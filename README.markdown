# Gitit

Gitit is a wiki program written in Haskell.  It uses [HAppS] for the web
server and session state, [git] for storage, history, search, diffs,
and merging, and [pandoc] for markup processing. Pages can be added,
changed, and removed either on the web or using git's command-line
tools. Gitit uses [pandoc]'s extended version of markdown as its markup
language.

[git]: http://git.or.cz  
[pandoc]: http://johnmacfarlane.net/pandoc
[HAppS]: http://happs.org

# Getting started

## Compiling and installing gitit

You'll need the [GHC] compiler and the [cabal-install] tool. GHC can
be downloaded [here]. For [cabal-install] on *nix, follow the [quick
install] instructions.

[GHC]: http://www.haskell.org/ghc/
[here]: http://www.haskell.org/ghc/
[cabal-install]:  http://hackage.haskell.org/trac/hackage/wiki/CabalInstall
[quick install]:  http://hackage.haskell.org/trac/hackage/wiki/CabalInstall#Quick Installation on Unix
[pcre]:  http://www.pcre.org/ 

If you want the syntax highlighting feature, you need to make sure
that pandoc is compiled with support for it.  First, make sure your system
has the [pcre] library installed.  Then:

    cabal install pandoc -fhighlighting

You can skip this step if you don't care about highlighting support.

You can now install the latest release of gitit:

    cabal update
    cabal install gitit

To install a version of gitit checked out from the repository,
change to the gitit directory and type:

    cabal install

The `cabal` tool will automatically install all of the required haskell
libraries. If all goes well, by the end of this process, the latest
release of gitit will be installed in your local `.cabal` directory. You
can check this by trying:

    gitit --version

If that doesn't work, check to see that `gitit` is in your local
cabal-install executable directory (usually `~/.cabal/bin`). And make
sure `~/.cabal/bin` is in your system path.

## Running gitit

To run gitit, you'll need [git] in your system path. Check this by doing

    git --version

Switch to the directory where you want to run gitit.  This should be a directory
where you have write access, since two directories, `static` and `wikidata`, will be
created here.  To start gitit, just type:

    gitit

If all goes well, gitit will do the following:

 1.  Create a git repository, `wikidata`, and add a default front page.
 2.  Create a `static` directory containing the scripts and CSS used by gitit.
 3.  Start a web server on port 5001.

Check that it worked:  open a web browser and go to <http://localhost:5001>.

## Configuration options

You can set some configuration options when starting gitit, using the
option `-f [filename]`.  A configuration file takes the following form:

    Config {
    repositoryPath  = "wikidata",
    staticDir       = "static",
    wikiBanner      = "<img src=\"/images/bann.png\" alt=\"banner\"",
    wikiTitle       = "Wiki",
    wikiFooter      = "Powered by Gitit",
    tableOfContents = False,
    maxUploadSize   = 100000,
    portNumber      = 5001,
    passwordSalt    = "l91snthoae8eou2340987",
    debugMode       = True
    accessQuestion  = Just ("Enter the access code (to request a code, contact me@foo.bar.com):", ["abcd"])
    }

For the most part, these options should be self-explanatory.

- The `wikiBanner` will be inserted before the top navigation bar on pages,
  and can be used to include a banner or wiki title.  It is raw HTML. Similarly,
  the `wikiFooter` is raw HTML that will be inserted at the bottom of the page.
- The `tableOfContents` boolean determines whether a table of contents,
  derived from the page's headers, will be included for every page.
- `maxUploadSize` (in bytes) limits the size of pages and uploads.
- The `passwordSalt` is used to encrypt passwords and should be
   changed for every new site.
- `debugMode` causes diagnostic information to be printed to the console.
- The `accessQuestion` is either `Nothing` (in which case anyone will be
  allowed to register for an account) or `Just (question, [ans1, ans2, ...])`
  (in which case anyone who registers must first answer the `question` with
  one of the provided answers).  One can deter automated spammers by using
  an `accessQuestion` with an easy and obvious answer.  Or one can use an
  `accessQuestion` to limit those who can edit a wiki to a trusted group.

# The `static` directory

If there is no wiki page or uploaded file corresponding to a request, gitit
always looks last in the `static` directory. So, for example, a file
`foo.jpg` in the `images` subdirectory of the `static` directory will be
accessible at the url `/images/foo.jpg`. Pandoc creates two subdirectories
of `static`, `stylesheets` and `javascripts`, which include the CSS and
scripts it uses.

# Changing the theme

To change the look of the wiki, modify `gitit.css` in `static/stylesheets`.

# Adding support for math

Gitit is designed to work with [jsMath] to display LaTeX math in HTML. 
Download `jsMath` and `jsMath Image Fonts` from the [jsMath download page].
You'll have two `.zip` archives. Unzip them both in the
`static/javascripts` directory (a new subdirectory, `jsMath`, will be
created).  You can test to see if math is working properly by clicking
"help" on the top navigation bar and looking for the math example
(the quadratic formula).

To write math on a wiki page, just enclose it in dollar signs, as in LaTeX:

    Here is a formula:  $\frac{1}{\sqrt{c^2}}$

You can write display math by enclosing it in double dollar signs:

    $$\frac{1}{\sqrt{c^2}}$$

[jsMath download page]: http://sourceforge.net/project/showfiles.php?group_id=172663
[jsMath]: http://www.math.union.edu/~dpvc/jsMath/

# Highlighted source code

If gitit was compiled against a version of pandoc that has highlighting support
(see above), you can get highlighted source code by using [delimited code blocks]:

    ~~~ {.haskell .numberLines}
    qsort []     = []
    qsort (x:xs) = qsort (filter (< x) xs) ++ [x] ++
                   qsort (filter (>= x) xs) 
    ~~~

To see what languages are available:

    pandoc -v

[delimited code blocks]: http://johnmacfarlane.net/pandoc/README.html#delimited-code-blocks

# Accessing the wiki via git

All the pages and uploaded files are stored in a git repository. By default, this
lives in the `wikidata` directory (though this can be changed through configuration
options).  So you can interact with the wiki using git command line tools:

    git clone ssh://my.server.edu/path/of/wiki/wikidata
    cd wikidata
    vim Front\ Page.page  # edit the page
    git commit -m "Added message about wiki etiquette" Front\ Page.page
    git push 

If you now look at the Front Page on the wiki, you should see your changes
reflected there.  Note that the pages all have the extension `.page`.

# Reporting bugs

There is no bug tracker as yet, so report bugs directly to the author,
jgm at berkeley . edu

# Acknowledgements

I borrowed some ideas about visual layout from Jeff Barczewski's fork
of Simon Rozet's `git-wiki`.

The code in `Gitit/State.hs` is based on http://hpaste.org/5957 by mightybyte,
as revised by dbpatterson.

