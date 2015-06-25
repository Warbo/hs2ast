# HS2AST: Haskell Syntax Tree Extraction #

This project turns Haskell source code into abstract syntax trees (ASTs). It uses the Glasgow Haskell Compiler (GHC), ensuring it's up to date with all language features and should allow names to be unified across packages and modules.

## Usage ##

HS2AST stores its output in the current working directory, so you probably want
to create a fresh directory first:

    mkdir /tmp/asts
    cd /tmp/asts

HS2AST takes Haskell filenames on stdin, separated by newlines. Only filenames ending in `.hs` or `.lhs` will be processed. For example:

    # Process a couple of files
    echo "/home/user/file1.hs\n/home/user/file2.hs" | HS2AST

    # Process all files in a project
    find /home/user/project | HS2AST

The resulting ASTs are grouped by *package*, then by *module*, then by *name*. For example, if a Cabal project called `my-project` contains a file like this:

    module MyProject.Util where

    helper x = x + 2

Then we should find a file called `my-project/MyProject.Util/helper` containing an AST for the `helper` function.
