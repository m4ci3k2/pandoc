{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, CPP, OverloadedStrings #-}
{-
Copyright (C) 2009-2010 John MacFarlane <jgm@berkeley.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Text.Pandoc.Templates
   Copyright   : Copyright (C) 2009-2010 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

A simple templating system with variable substitution and conditionals.
Example:

> renderTemplate [("name","Sam"),("salary","50,000")] $
>    "Hi, $name$.  $if(salary)$You make $$$salary$.$else$No salary data.$endif$"
> "Hi, John.  You make $50,000."

A slot for an interpolated variable is a variable name surrounded
by dollar signs.  To include a literal @$@ in your template, use
@$$@.  Variable names must begin with a letter and can contain letters,
numbers, @_@, and @-@.

The value of a variable will be indented to the same level as the
variable.

A conditional begins with @$if(variable_name)$@ and ends with @$endif$@.
It may optionally contain an @$else$@ section.  The if section is
used if @variable_name@ has a non-null value, otherwise the else section
is used.

Conditional keywords should not be indented, or unexpected spacing
problems may occur.

If a variable name is associated with multiple values in the association
list passed to 'renderTemplate', you may use the @$for$@ keyword to
iterate over them:

> renderTemplate [("name","Sam"),("name","Joe")] $
>   "$for(name)$\nHi, $name$.\n$endfor$"
> "Hi, Sam.\nHi, Joe."

You may optionally specify separators using @$sep$@:

> renderTemplate [("name","Sam"),("name","Joe"),("name","Lynn")] $
>   "Hi, $for(name)$$name$$sep$, $endfor$"
> "Hi, Sam, Joe, Lynn."
-}

module Text.Pandoc.Templates ( renderTemplate
                             , TemplateTarget(..)
                             , getDefaultTemplate ) where

import Data.Char (isAlphaNum)
import Control.Monad (guard, when)
import Data.Aeson (ToJSON(..), Value(..))
import qualified Data.Attoparsec.Text as A
import Data.Attoparsec.Text (Parser)
import Control.Applicative
import qualified Data.Text as T
import Data.Text (Text)
import Data.Monoid ((<>), Monoid(..))
import Data.List (intersperse)
import System.FilePath ((</>), (<.>))
import qualified Data.Map as M
import qualified Data.HashMap.Strict as H
import Data.Foldable (toList)
import qualified Control.Exception.Extensible as E (try, IOException)
#if MIN_VERSION_blaze_html(0,5,0)
import Text.Blaze.Html (Html)
import Text.Blaze.Internal (preEscapedString)
#else
import Text.Blaze (preEscapedString, Html)
#endif
import Text.Pandoc.UTF8 (fromStringLazy)
import Data.ByteString.Lazy (ByteString)
import Text.Pandoc.Shared (readDataFileUTF8)

-- | Get default template for the specified writer.
getDefaultTemplate :: (Maybe FilePath) -- ^ User data directory to search first
                   -> String           -- ^ Name of writer
                   -> IO (Either E.IOException String)
getDefaultTemplate user writer = do
  let format = takeWhile (`notElem` "+-") writer  -- strip off extensions
  case format of
       "native" -> return $ Right ""
       "json"   -> return $ Right ""
       "docx"   -> return $ Right ""
       "odt"    -> getDefaultTemplate user "opendocument"
       "markdown_strict" -> getDefaultTemplate user "markdown"
       "multimarkdown"   -> getDefaultTemplate user "markdown"
       "markdown_github" -> getDefaultTemplate user "markdown"
       _        -> let fname = "templates" </> "default" <.> format
                   in  E.try $ readDataFileUTF8 user fname

data Template = Literal Text
              | Subst (Value -> Template)
              | Empty

type Variable = [Text]

instance Monoid Template where
  mempty = Empty
  mappend Empty       x           = x
  mappend x           Empty       = x
  mappend (Literal x) (Literal y) = Literal (x <> y)
  mappend (Literal x) (Subst f)   = Subst (\c -> Literal x <> f c)
  mappend (Subst f)   (Literal x) = Subst (\c -> f c <> Literal x)
  mappend (Subst f)   (Subst g)   = Subst (\c -> f c <> g c)

class TemplateTarget a where
  toTarget :: String -> a

instance TemplateTarget String where
  toTarget = id

instance TemplateTarget ByteString where
  toTarget = fromStringLazy

instance TemplateTarget Html where
  toTarget = preEscapedString

renderTemplate :: TemplateTarget a
               => [(String,String)]  -- ^ Assoc. list of values for variables
               -> String             -- ^ Template
               -> a
renderTemplate assoc template =
  toTarget $ T.unpack $ renderTemplateNew templ ctx
  where ctx = toJSON $ toMap $ map toTexts assoc
        toTexts (s1, s2) = (T.pack s1, T.pack s2)
        toMap :: [(Text, Text)] -> M.Map Text Value
        toMap ts = M.fromList $
           map (\x -> (x, toVal [z | (y, z) <- ts, not (T.null z), y == x]))
                              $ map fst ts
        toVal []  = Null
        toVal [x] = toJSON x
        toVal xs  = toJSON xs
        templ = case compileTemplate (T.pack template) of
                     Left err -> error err
                     Right t  -> t

renderTemplateNew :: ToJSON a => Template -> a -> Text
renderTemplateNew template context = renderTemplate' template (toJSON context)

renderTemplate' :: Template -> Value -> Text
renderTemplate' (Literal b) _ = b
renderTemplate' (Subst f) ctx = renderTemplate' (f ctx) ctx
renderTemplate' Empty _ = mempty

compileTemplate :: Text -> Either String Template
compileTemplate template = A.parseOnly pTemplate template

var :: Variable -> Template
var = Subst . resolveVar

resolveVar :: Variable -> Value -> Template
resolveVar var' val =
  case multiLookup var' val of
       Just (Array vec) -> mconcat $ map (resolveVar []) $ toList vec
       Just (String t)  -> Literal $ T.stripEnd t
       Just (Number n)  -> Literal $ T.pack $ show n
       Just (Bool True) -> Literal "True"
       Just _           -> Empty
       Nothing          -> Empty

multiLookup :: [Text] -> Value -> Maybe Value
multiLookup [] x = Just x
multiLookup (v:vs) (Object o) = H.lookup v o >>= multiLookup vs
multiLookup _ _ = Nothing

lit :: Text -> Template
lit = Literal

cond :: Variable -> Template -> Template -> Template
cond var' ifyes ifno = Subst $ \val ->
  case resolveVar var' val of
       Empty -> ifno
       _     -> ifyes

iter :: Variable -> Template -> Template -> Template
iter var' template sep = Subst $ \val ->
  case multiLookup var' val of
       Just (Array vec) -> mconcat $ intersperse sep
                                   $ map (setVar template var')
                                   $ toList vec
       Just x           -> setVar template var' x
       Nothing          -> mempty

setVar :: Template -> Variable -> Value -> Template
setVar (Literal b) _   _   = Literal b
setVar Empty       _   _   = Empty
setVar (Subst f)   v   new = Subst $ \old -> f (replaceVar v new old)

replaceVar :: Variable -> Value -> Value -> Value
replaceVar []     new _          = new
replaceVar (v:vs) new (Object o) =
  Object $ H.adjust (\x -> replaceVar vs new x) v o
replaceVar _ _ old = old

--- parsing

pTemplate :: Parser Template
pTemplate = do
  sp <- A.option mempty pInitialSpace
  rest <- mconcat <$> many (pConditional <|>
                            pFor <|>
                            pNewline <|>
                            pVar <|>
                            pLit <|>
                            pEscapedDollar)
  return $ sp <> rest

pLit :: Parser Template
pLit = lit <$> A.takeWhile1 (\x -> x /='$' && x /= '\n')

pNewline :: Parser Template
pNewline = do
  A.char '\n'
  sp <- A.option mempty pInitialSpace
  return $ lit "\n" <> sp

pInitialSpace :: Parser Template
pInitialSpace = do
  sps <- A.takeWhile1 (==' ')
  let indentVar = if T.null sps
                     then id
                     else indent (T.length sps)
  v <- A.option mempty $ indentVar <$> pVar
  return $ lit sps <> v

pEscapedDollar :: Parser Template
pEscapedDollar = lit "$" <$ A.string "$$"

pVar :: Parser Template
pVar = var <$> (A.char '$' *> pIdent <* A.char '$')

pIdent :: Parser [Text]
pIdent = do
  first <- pIdentPart
  rest <- many (A.char '.' *> pIdentPart)
  return (first:rest)

pIdentPart :: Parser Text
pIdentPart = do
  first <- A.letter
  rest <- A.takeWhile (\c -> isAlphaNum c || c == '_' || c == '-')
  let id' = T.singleton first <> rest
  guard $ id' `notElem` reservedWords
  return id'

reservedWords :: [Text]
reservedWords = ["else","endif","for","endfor","sep"]

skipEndline :: Parser ()
skipEndline = A.skipWhile (`elem` " \t") >> A.char '\n' >> return ()

pConditional :: Parser Template
pConditional = do
  A.string "$if("
  id' <- pIdent
  A.string ")$"
  -- if newline after the "if", then a newline after "endif" will be swallowed
  multiline <- A.option False (True <$ skipEndline)
  ifContents <- pTemplate
  elseContents <- A.option Empty $
                      do A.string "$else$"
                         when multiline $ A.option () skipEndline
                         pTemplate
  A.string "$endif$"
  when multiline $ A.option () skipEndline
  return $ cond id' ifContents elseContents

pFor :: Parser Template
pFor = do
  A.string "$for("
  id' <- pIdent
  A.string ")$"
  -- if newline after the "for", then a newline after "endfor" will be swallowed
  multiline <- A.option False $ skipEndline >> return True
  contents <- pTemplate
  sep <- A.option Empty $
           do A.string "$sep$"
              when multiline $ A.option () skipEndline
              pTemplate
  A.string "$endfor$"
  when multiline $ A.option () skipEndline
  return $ iter id' contents sep

indent :: Int -> Template -> Template
indent 0   x           = x
indent _   Empty       = Empty
indent ind (Literal t) = Literal $ T.concat $ intersperse sep $ T.lines t
   where sep = "\n" <> T.replicate ind " "
indent ind (Subst f)   = Subst $ \val -> indent ind (f val)

