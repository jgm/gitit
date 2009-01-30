Gitit
=====

Gitit is a wiki program written in Haskell. It uses [HAppS][] for the
web server and [pandoc][] for markup processing. Pages and uploaded
files are stored in a [git][] or [darcs][] repository and may be modified either
by using the VCS's command-line tools or through the wiki's web interface.
By default, pandoc's extended version of markdown is used as a markup language,
but reStructuredText can also be used.  Pages can be exported in a
number of different formats, including LaTeX, RTF, OpenOffice ODT, and
MediaWiki markup. Gitit can be configured to display TeX math (using
[jsMath][]) and highlighted source code (using [highlighting-kate][]).

[git]: http://git.or.cz  
[darcs]: http://darcs.net
[pandoc]: http://johnmacfarlane.net/pandoc
[HAppS]: http://happs.org
[jsMath]: http://www.math.union.edu/~dpvc/jsMath/
[highlighting-kate]: http://johnmacfarlane.net/highlighting-kate/  

Getting started
===============

Compiling and installing gitit
------------------------------

You'll need the [GHC][] compiler and the [cabal-install][] tool. GHC can
be downloaded [here][]. Note that, starting with release 0.5, GHC 6.10
or higher is required. For [cabal-install][] on *nix, follow the [quick
install][] instructions.

[GHC]: http://www.haskell.org/ghc/
[here]: http://www.haskell.org/ghc/
[cabal-install]:  http://hackage.haskell.org/trac/hackage/wiki/CabalInstall
[quick install]:  http://hackage.haskell.org/trac/hackage/wiki/CabalInstall#Quick Installation on Unix
[pcre]:  http://www.pcre.org/ 

If you want the syntax highlighting feature, you need to make sure
that pandoc is compiled with support for it.  First, make sure your system
has the [pcre][] library installed.  Then:

    cabal update
    cabal install -fhighlighting pandoc gitit

If you don't care about highlighting support, you can just do:

    cabal install gitit

These commands will install the latest released version of gitit.
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

Running gitit
-------------

To run gitit, you'll need [git][] in your system path. Check this by doing

    git --version

You should also make sure that you are using a UTF-8 locale.
(To check this, type `locale`.)

Switch to the directory where you want to run gitit.  This should be a directory
where you have write access, since two directories, `static` and `wikidata`, and
two files, `gitit-users` and `template.html`, will be created here. To
start gitit, just type:

    gitit

If all goes well, gitit will do the following:

 1.  Create a git repository, `wikidata`, and add a default front page.
 2.  Create a `static` directory containing the scripts and CSS used by gitit.
 3.  Create a `template.html` file containing an (HStringTemplate) template
     for wiki pages.
 4.  Start a web server on port 5001.

Check that it worked:  open a web browser and go to http://localhost:5001.

Configuration options
---------------------

You can set some configuration options when starting gitit, using the
option `-f [filename]`.  A configuration file takes the following form:

    Config {
    repository          = Git "wikidata",
    defaultPageType     = Markdown,
    userFile            = "gitit-users",
    templateFile        = "template.html",
    staticDir           = "static",
    tableOfContents     = False,
    maxUploadSize       = 100000,
    portNumber          = 5001,
    debugMode           = True,
    frontPage           = "Front Page",
    noEdit              = ["Help", "Front Page"],
    noDelete            = ["Help", "Front Page"],
    accessQuestion      = Just ("Enter the access code (to request an access code, contact me@somewhere.org):", ["abcd"]),
    useRecaptcha        = False,
    recaptchaPublicKey  = "",
    recaptchaPrivateKey = "",
    mimeTypesFile       = "/etc/mime.types"
    }

- `repository` specifies the type and (relative) path of the repository
  in which the wiki's pages will be stored. If it does not exist, gitit
  will create it on startup.  Supported repository types are `Git` and
  `Darcs`.

- `defaultPageType` is the type of markup used to interpret pages in
  the wiki. Two values are currently supported: `Markdown` and `RST`.
  If `Markdown` is selected, [pandoc]'s syntax extensions (for footnotes,
  delimited code blocks, etc.) will be enabled.  Note that pandoc's
  reStructuredText parser is not complete, so some pages may
  not be rendered correctly if `RST` is selected.

- `userFile` is a file containing user login information (with hashed
  passwords).  If it does not exist, gitit will start with an empty list
  of users.  Gitit will write a new `userFile` on shutdown.

- `templateFile` is a file containing an HTML template for the wiki pages.
  If it does not exist, gitit will create a default template.  (For most
  purposes, this can be used just as it is, but some users may wish to
  customize the look of their wiki.)  `templateFile` is an
  `HStringTemplate` template.

- `staticDir` is the (relative) path of a directory in which static content
  (javascript, CSS, images) is stored.  If it does not exist, gitit will
  create it on startup.

- `tableOfContents` is either `False` or `True`.  If it is `True`, a table
  of contents (derived from the page's headers) will appear on each page.

- `maxUploadSize` (in bytes) sets a limit to the size of file uploads.

- `portNumber` is the number of the port on which the wiki will be served.

- `debugMode` is either `True` or `False`. If it is `True`, debug information
  will be printed to the console when gitit is running.

- `frontPage` is the name of the page that is designated as the "front" or
  "entrance" page of the wiki.  Any page may be designated.

- `noEdit` is a list of pages that cannot be edited.

- `noDelete` is a list of pages that cannot be deleted.

- `accessQuestion` provides primitive access control.  It is either `Nothing`,
  in which case anyone will be allowed to create an account and edit wiki pages,
  or `Just (question, [answer1, answer2, ...])`, where question is a prompt
  that will be displayed when a user tries to create an account, and
  `answer1, answer2, ...` are the valid responses. The user must provide a
  valid response in order to create an account. 

- `useRecaptcha` is either `True` or `False`. It specifies whether to
  use the [reCAPTCHA] service to provide captchas for user registration.

- `recaptchaPublicKey` and `recaptchaPrivateKey` are
  [reCAPTCHA] keys, which can be obtained free of charge at
  <http://recaptcha.net/api/getkey>.  The values of these fields are ignored
  if `useRecaptcha` is set to `False`.

- `mimeTypesFile` is the path of a file containing mime type associations.
  Each line of the file should contain a mime type, followed by some space,
  followed by a space-separated list of file extensions that map to that mime
  type.  If the file is not found, some simple defaults will be used.

[reCAPTCHA]: http://recaptcha.net

Configuring gitit
=================

The `static` directory
----------------------

If there is no wiki page or uploaded file corresponding to a request, gitit
always looks last in the `static` directory. So, for example, a file
`foo.jpg` in the `img` subdirectory of the `static` directory will be
accessible at the url `/img/foo.jpg`. Pandoc creates three subdirectories
of `static`, `css`, `img`, and `js`, which include the icons, stylesheets,
and javascripts it uses.

Note:  if you set `staticDir` to be a subdirectory of `repositoryPath`,
and then add the files in the static directory to your repository, you
can ensure that others who clone your wiki repository get these files
as well.  It will not be possible to modify these files using the web
interface, but they will be modifiable via git.

Changing the theme
------------------

To change the look of the wiki, modify `screen.css` in `static/css`.
To change the look of printed pages, modify `print.css`.
The logo picture can be changed by copying a new PNG file to
`static/img/logo.png`. For more radical changes, one can modify
`template.html`.

Adding support for math
-----------------------

Gitit is designed to work with [jsMath][] to display LaTeX math in HTML. 
Download `jsMath` and `jsMath Image Fonts` from the [jsMath download page][].
You'll have two `.zip` archives. Unzip them both in the
`static/js` directory (a new subdirectory, `jsMath`, will be
created).  You can test to see if math is working properly by clicking
"help" on the top navigation bar and looking for the math example
(the quadratic formula).

To write math on a wiki page, just enclose it in dollar signs, as in LaTeX:

    Here is a formula:  $\frac{1}{\sqrt{c^2}}$

You can write display math by enclosing it in double dollar signs:

    $$\frac{1}{\sqrt{c^2}}$$

[jsMath download page]: http://sourceforge.net/project/showfiles.php?group_id=172663

Highlighted source code
-----------------------

If gitit was compiled against a version of pandoc that has highlighting support
(see above), you can get highlighted source code by using [delimited code blocks][]:

    ~~~ {.haskell .numberLines}
    qsort []     = []
    qsort (x:xs) = qsort (filter (< x) xs) ++ [x] ++
                   qsort (filter (>= x) xs) 
    ~~~

To see what languages are available:

    pandoc -v

[delimited code blocks]: http://johnmacfarlane.net/pandoc/README.html#delimited-code-blocks

Accessing the wiki via git
==========================

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

Wiki links and formatting
=========================

For instructions on editing pages and creating links, see the "Help" page.

Gitit interprets links with empty URLs as wikilinks.  Thus, in markdown pages,
`[Front Page]()` creates an internal wikilink to the page `Front Page`.
In reStructuredText pages, `` `Front Page <>`_ `` has the same effect.

Character encodings
===================

Gitit assumes that the page files (stored in the git repository) are
encoded as UTF-8.  Even page names may be UTF-8 if the file system supports
this.  You should use a UTF-8 locale when running gitit.

Reporting bugs
==============

Bugs may be reported (and feature requests filed) at
<http://code.google.com/p/gitit/issues/list>.

Acknowledgements
================

Gwern Branwen helped to optimize Gitit.  Simon Michael contributed the patch for
RST support.

The visual layout is shamelessly borrowed from Wikipedia.

The stylesheets are influenced by Wikipedia's stylesheets and by the
bluetrip CSS framework (see BLUETRIP-LICENSE). Some of the icons in
`img/icons` come from bluetrip as well.

