---
layout: default
title: gitit
---

Gitit is a wiki program. Pages and uploaded files are stored in a git
or darcs repository and may be modified either by using the VCS's
command-line tools or through the wiki's web interface. Pandoc's
extended version of markdown is used as a markup language. Pages can
be exported in a number of different formats, including LaTeX, RTF,
OpenOffice ODT, and MediaWiki markup. Gitit can be configured to
display TeX math (using jsMath) and highlighted source code (using
highlighting-kate).

- [Live demo](http://gitit.johnmacfarlane.net)
- [README](http://github.com/jgm/gitit/README.markdown)
- [Repository](http://github.com/jgm/gitit)
- [Bug reports](http://code.google.com/p/gitit/issues/list?can=2&q=&colspec=ID+Type+Status+Priority+Milestone+Owner+Summary&cells=tiles)
- [Mailing list](http://groups.google.com/group/gitit-discuss)
- [Releases](http://hackage.haskell.org/cgi-bin/hackage-scripts/package/gitit)

To install the released version of gitit using
[cabal-install](http://hackage.haskell.org/trac/hackage/wiki/CabalInstall):

    cabal update
    cabal install pandoc -fhighlighting
    cabal install gitit

To install the development version:

    git clone git://github.com/jgm/gitit.git
    cd gitit
    cabal install pandoc -fhighlighting
    cabal install   # add -fplugins for plugins support

Then:

    mkdir mywiki
    cd mywiki
    gitit

and you're up and running on port 5001.

