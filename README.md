# HS2AST: Haskell Syntax Tree Extraction #

This is a script which reads in Haskell source code and dumps out abstract syntax trees (ASTs). It uses the Glasgow Haskell Compiler (GHC) to do the parsing, and also to perform unique renaming. This should allow names to be unified across files and modules.
