Changes wrt to standard pandoc
==============================

1. Code blocks which have "haskell" attribute set are made executable through
JS-interface to specified serverd (setting server URL: FIXME). 
2. If there is attribute "attach-file" on a code block which has haskell
attribute set:
	* if the value of that attribute is a name of file, it is attached silently to
	  haskell code and sent together (compressed) for interpretion together
	  with the specified file.
	* if the value of that attribute is a name of directory, all files in
	  it are attached as in the previous point. Technically a tar-gz archive
	  with maximum compression is always created even if only one file is
	  attached to simplify processing.
	* otherwise the attribute is silently ignored (FIXME: should be a
	  warning/error)
3. For the server see the (FIXME) other repository.
