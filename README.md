I. Introduction
===============
Dewhiten is a small utility for formatting white space in code source
files. It removes extraneous white space at the ends of lines,
extraneous blank lines at the end of files, and formats indentation
for blank lines elsewhere in a file.

Dewhiten was written in Haskell around June and July of 2011. Its
author is Elliot Cameron (http://www.3noch.com/). You can access the
git repository at http://github.com:CovenantEyes/Dewhiten.

Dewhiten is released under the GPLv3 license, which can be read at
http://www.gnu.org/licenses/gpl.html.

This software is provided WITHOUT ANY WARRANTY OR GUARANTEE OF ANY
KIND. USE AT YOUR OWN RISK.


II. Dependencies
================
Dewhiten has been tested with Glasgow Haskell Compiler versions
6.12.1, 7.0.3, and 7.0.4.

It requires the following cabal packages:

1. [`System.FilePath.Glob`](http://hackage.haskell.org/package/Glob)

   Install this package like this
       $ sudo cabal update
       $ sudo cabal install Glob


III. Building and Installing
============================
To build `dewhiten`, use the following command:

    $ make

Then run it like this:

    $ ./dewhiten

To install:

    $ sudo make install


If you have `runhaskell` or `runghc` installed, you can also run
`dewhiten` without compiling it like this:

    $ runhaskell Dewhiten

or

    $ runghc Dewhiten
