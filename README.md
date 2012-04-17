Introduction
============
Dewhiten is a small utility for formatting white space in code source
files. It removes extraneous white space at the ends of lines,
extraneous blank lines at the end of files, and formats indentation
for blank lines elsewhere in a file.

Dewhiten was written in [Haskell](http://haskell.org/) around June and
July of 2011. Its author is [Elliot Cameron](http://www.3noch.com/).
You can access the git repository at
http://github.com:CovenantEyes/Dewhiten.

This software is provided WITHOUT ANY WARRANTY OR GUARANTEE OF ANY
KIND. USE AT YOUR OWN RISK.


Dependencies
============
Dewhiten has been tested with
[Glasgow Haskell Compiler](http://www.haskell.org/ghc/) versions
6.12.1, 7.0.3, 7.0.4, and 7.4.1.

It requires the following [cabal](http://www.haskell.org/cabal/)
packages:

1.  [`System.FilePath.Glob`](http://hackage.haskell.org/package/Glob)

    Install this package like this:

        $ sudo cabal update
        $ sudo cabal install Glob


Building and Installing
=======================

To build `dewhiten`, you'll need
[GNU Make](http://www.gnu.org/s/make/) or some other `make` utility.
Build `dewhiten` like this:

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


Usage
=====
For help on using Dewhiten, run this after building it:

    $ ./dewhiten --help


Licensing
=========
Dewhiten is released under the
[GPLv3](http://www.gnu.org/licenses/gpl.html) license.
