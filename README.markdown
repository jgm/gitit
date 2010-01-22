Gitit
=====

Gitit is a wiki program written in Haskell. It uses [Happstack][] for
the web server and [pandoc][] for markup processing. Pages and uploaded
files are stored in a [git][], [darcs][], or [mercurial][] repository
and may be modified either by using the VCS's command-line tools or
through the wiki's web interface. By default, pandoc's extended version
of markdown is used as a markup language, but reStructuredText, LaTeX,
or HTML can also be used. Pages can be exported in a number of different
formats, including LaTeX, RTF, OpenOffice ODT, and MediaWiki markup.
Gitit can be configured to display TeX math (using [texmath][]) and
highlighted source code (using [highlighting-kate][]).

Other features include

* plugins: dynamically loaded page transformations written in Haskell
  (see "Network.Gitit.Interface")

* categories

* TeX math

* syntax highlighting of source code files and code snippets (using
  highlighting-kate)

* caching

* Atom feeds (site-wide and per-page)

* a library, "Network.Gitit", that makes it simple to include a gitit
  wiki in any happstack application

You can see a running demo at <http://gitit.johnmacfarlane.net>.

[git]: http://git.or.cz
[darcs]: http://darcs.net
[mercurial]: http://mercurial.selenic.com/
[pandoc]: http://johnmacfarlane.net/pandoc
[Happstack]: http://happstack.com
[highlighting-kate]: http://johnmacfarlane.net/highlighting-kate/
[texmath]: http://github.com/jgm/texmath/tree/master

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

Once you've got cabal-install, installing gitit is trivial:

    cabal update
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

Optional syntax highlighting support
------------------------------------

If pandoc was compiled with optional syntax highlighting support,
this will be available in gitit too.  This feature is recommended
if you plan to display source code on your wiki.

Highlighting support requires the [pcre][] library, so make sure that
is installed before continuing.

[pcre]:  http://www.pcre.org/ 

To install gitit with highlighting support, first ensure that pandoc
is compiled with highlighting support, then install gitit as above:

    cabal install pandoc -fhighlighting --reinstall
    cabal install gitit

Running gitit
-------------

To run gitit, you'll need `git` in your system path. (Or `darcs` or
`hg`, if you're using darcs or mercurial to store the wiki data.)

Gitit assumes that the page files (stored in the git repository) are
encoded as UTF-8. Even page names may be UTF-8 if the file system
supports this. So you should make sure that you are using a UTF-8 locale
when running gitit. (To check this, type `locale`.)

Switch to the directory where you want to run gitit. This should be a
directory where you have write access, since three directories, `static`,
`templates`, and `wikidata`, and two files, `gitit-users` and `gitit.log`,
will be created here. To start gitit, just type:

    gitit

If all goes well, gitit will do the following:

 1.  Create a git repository, `wikidata`, and add a default front page.
 2.  Create a `static` directory containing files to be treated as
     static files by gitit.
 3.  Create a `templates` directory containing HStringTemplate templates
     for wiki pages.
 4.  Start a web server on port 5001.

Check that it worked: open a web browser and go to
<http://localhost:5001>.

You can control the port that gitit runs on using the `-p` option:
`gitit -p 4000' will start gitit on port 4000. Additional runtime
options are described by `gitit -h`.

Using gitit
===========

Wiki links and formatting
-------------------------

For instructions on editing pages and creating links, see the "Help" page.

Gitit interprets links with empty URLs as wikilinks. Thus, in markdown
pages, `[Front Page]()` creates an internal wikilink to the page `Front
Page`. In reStructuredText pages, `` `Front Page <>`_ `` has the same
effect.

If you want to link to a directory listing for a subdirectory, use a
trailing slash:  `[foo/bar/]()` creates a link to the directory for
`foo/bar`.

Page metadata
-------------

Pages may optionally begin with a metadata block.  Here is an example:

    ---
    format: latex+lhs
    categories: haskell math
    toc: no
    title: Haskell and
      Category Theory
    ...

    \section{Why Category Theory?}

The metadata block consists of a list of key-value pairs, each on a
separate line. If needed, the value can be continued on one or more
additional line, which must begin with a space. (This is illustrated by
the "title" example above.) The metadata block must begin with a line
`---` and end with a line `...` optionally followed by one or more blank
lines. (The metadata block is a valid YAML document, though not all YAML
documents will be valid metadata blocks.)

Currently the following keys are supported:

format
:   Overrides the default page type as specified in the configuration file.
    Possible values are `markdown`, `rst`, `latex`, `html`, `markdown+lhs`,
    `rst+lhs`, `latex+lhs`.  (Capitalization is ignored, so you can also
    use `LaTeX`, `HTML`, etc.)  The `+lhs` variants indicate that the page
    is to be interpreted as literate Haskell.  If this field is missing,
    the default page type will be used.

categories
:   A space or comma separated list of categories to which the page belongs.

toc
:   Overrides default setting for table-of-contents in the configuration file.
    Values can be `yes`, `no`, `true`, or `false` (capitalization is ignored).

title
:   By default the displayed page title is the page name.  This metadata element
    overrides that default.

Highlighted source code
-----------------------

If gitit was compiled against a version of pandoc that has highlighting
support (see above), you can get highlighted source code by using
[delimited code blocks][]:

    ~~~ {.haskell .numberLines}
    qsort []     = []
    qsort (x:xs) = qsort (filter (< x) xs) ++ [x] ++
                   qsort (filter (>= x) xs) 
    ~~~

To see what languages your pandoc was compiled to highlight:

    pandoc -v

[delimited code blocks]: http://johnmacfarlane.net/pandoc/README.html#delimited-code-blocks

Configuring and customizing gitit
=================================

Configuration options
---------------------

Use the option `-f [filename]` to specify a configuration file:

    gitit -f my.conf

If this option is not used, gitit will use a default configuration.
To get a copy of the default configuration file, which you
can customize, just type:

    gitit --print-default-config > my.conf

The default configuration file is documented with comments throughout.

The `static` directory
----------------------

On receiving a request, gitit always looks first in the `static`
directory (or in whatever directory is specified for `static-dir` in
the configuration file). If a file corresponding to the request is
found there, it is served immediately. If the file is not found in
`static`, gitit next looks in the `static` subdirectory of gitit's data
file (`$CABALDIR/share/gitit-x.y.z/data`). This is where default css,
images, and javascripts are stored. If the file is not found there
either, gitit treats the request as a request for a wiki page or wiki
command.

So, you can throw anything you want to be served statically (for
example, a `robots.txt` file or `favicon.ico`) in the `static`
directory. You can override any of gitit's default css, javascript, or
image files by putting a file with the same relative path in `static`.
Note that gitit has a default `robots.txt` file that excludes all
URLs beginning with `/_`.

Note:  if you set `static-dir` to be a subdirectory of `repository-path`,
and then add the files in the static directory to your repository, you
can ensure that others who clone your wiki repository get these files
as well.  It will not be possible to modify these files using the web
interface, but they will be modifiable via git.

Using a VCS other than git
--------------------------

By default, gitit will store wiki pages in a git repository in the
`wikidata` directory.  If you'd prefer to use darcs instead of git,
you need to add the following field to the configuration file:

    repository-type: Darcs

If you'd prefer to use mercurial, add:

    repository-type: Mercurial

This program may be called "darcsit" instead of "gitit" when a darcs
backend is used.

Note:  we recommend that you use gitit/darcsit with darcs version
2.3.0 or greater.  If you must use an older version of darcs, then
you need to compile the filestore library without the (default)
maxcount flag, before (re)installing gitit:

    cabal install --reinstall filestore -f-maxcount
    cabal install --reinstall gitit

Otherwise you will get an error when you attempt to access your
repository.

Changing the theme
------------------

To change the look of the wiki, you can modify `custom.css` in
`static/css`.

To change the look of printed pages, copy gitit's default `print.css`
to `static/css` and modify it.

The logo picture can be changed by copying a new PNG file to
`static/img/logo.png`.

To change the footer, modify `templates/footer.st`.

For more radical changes, you can override any of the default
templates in `$CABALDIR/share/gitit-x.y.z/templates` by copying
the file into `templates` and modifying it. The `page.st` template is
the master template; it includes the others. Interpolated variables are
surrounded by `$`s, so `literal $` must be backslash-escaped.

Adding support for math
-----------------------

To write math on a markdown-formatted wiki page, just enclose it
in dollar signs, as in LaTeX:

    Here is a formula:  $\frac{1}{\sqrt{c^2}}$

You can write display math by enclosing it in double dollar signs:

    $$\frac{1}{\sqrt{c^2}}$$

Gitit can display TeX math in three different ways, depending on the
setting of `math` in the configuration file:

1.  `mathml` (default): Math will be converted to MathML using
    [texmath][]. This method works with IE+mathplayer, Firefox, and
    Opera, but not Safari.

2.  `jsMath`: Math will be rendered using the [jsMath][] javascript.
    If you want to use this method, download `jsMath` and `jsMath
    Image Fonts` from the [jsMath download page][]. You'll have two
    `.zip` archives. Unzip them both in the `static/js` directory (a new
    subdirectory, `jsMath`, will be created).  This works with all
    browsers, but is slower and not as nice looking as MathML.

3.  `raw`: Math will be rendered as raw LaTeX codes.

[jsMath download page]: http://sourceforge.net/project/showfiles.php?group_id=172663

Plugins
=======

Plugins are small Haskell programs that transform a wiki page after it
has been converted from Markdown or RST. See the example plugins in the
`plugins` directory. To enable a plugin, include the path to the plugin
(or its module name) in the `plugins` field of the configuration file.
(If the plugin name starts with `Network.Gitit.Plugin.`, gitit will assume that
the plugin is an installed module and will not look for a source file.)

Plugin support is enabled by default. However, plugin support makes
the gitit executable considerably larger and more memory-hungry.
If you don't need plugins, you may want to compile gitit without plugin
support.  To do this, unset the `plugins` Cabal flag:

    cabal install --reinstall gitit -f-plugins

Note also that if you compile gitit for executable profiling, attempts
to load plugins will result in "internal error: PAP object entered!"

Accessing the wiki through git
==============================

All the pages and uploaded files are stored in a git repository. By
default, this lives in the `wikidata` directory (though this can be
changed through configuration options). So you can interact with the
wiki using git command line tools:

    git clone ssh://my.server.edu/path/of/wiki/wikidata
    cd wikidata
    vim Front\ Page.page  # edit the page
    git commit -m "Added message about wiki etiquette" Front\ Page.page
    git push

If you now look at the Front Page on the wiki, you should see your changes
reflected there.  Note that the pages all have the extension `.page`.

If you are using the darcs or mercurial backend, the commands will
be slightly different.  See the documentation for your VCS for
details.

Caching
=======

By default, gitit does not cache content.  If your wiki receives a lot of
traffic or contains pages that are slow to render, you may want to activate
caching.  To do this, set the configuration option `use-cache` to `yes`.
By default, rendered pages and highlighted source files will be cached
in the `cache` directory. (Another directory can be specified by setting
the `cache-dir` configuration option.)

Cached pages are updated when pages are modified using the web
interface. They are not updated when pages are modified directly through
git or darcs. However, the cache can be refreshed manually by pressing
Ctrl-R when viewing a page, or by sending an HTTP GET or POST request to
`/_expire/path/to/page`, where `path/to/page` is the name of the page to
be expired.

Users who frequently update pages using git or darcs may wish to add a
hook to the repository that makes the appropriate HTTP request to expire
pages when they are updated. To facilitate such hooks, the gitit cabal
package includes an executable `expireGititCache`. Assuming you are
running gitit at port 5001 on localhost, and the environment variable
`CHANGED_FILES` contains a list of the files that have changed, you can
expire their cached versions using

    expireGititCache http://localhost:5001 $CHANGED_FILES

Or you can specify the files directly:

    expireGititCache http://localhost:5001 "Front Page.page" foo/bar/baz.c

This program will return a success status (0) if the page has been
successfully expired (or if it was never cached in the first place),
and a failure status (> 0) otherwise.

The cache is persistent through restarts of gitit.  To expire all cached
pages, simply remove the `cache` directory.

Using gitit with apache
=======================

Most users who run a public-facing gitit will want gitit to appear
at a nice URL like `http://wiki.mysite.com` or
`http://mysite.com/wiki` rather than `http://mysite.com:5001`.
This can be achieved using apache's `mod_proxy`.

Proxying to `http://wiki.mysite.com`
------------------------------------

Set up your DNS so that `http://wiki.mysite.com` maps to
your server's IP address. Make sure that the `mod_proxy` module is
loaded, and set up a virtual host with the following configuration:

    <VirtualHost *>
        ServerName wiki.mysite.com
        DocumentRoot /var/www/
        RewriteEngine On
        ProxyPreserveHost On
        ProxyRequests Off
    
        <Proxy *>
           Order deny,allow
           Allow from all
        </Proxy>
    
        ProxyPassReverse /    http://127.0.0.1:5001
        RewriteRule ^(.*) http://127.0.0.1:5001$1 [P]
    
        ErrorLog /var/log/apache2/error.log
        LogLevel warn
    
        CustomLog /var/log/apache2/access.log combined
        ServerSignature On
    
    </VirtualHost>

Reload your apache configuration and you should be all set.

Proxying to `http://mysite.com/wiki`
------------------------------------

Make sure the `mod_proxy`, `mod_headers`, `mod_proxy_http`,
and `mod_proxy_html` modules are loaded. `mod_proxy_html`
is an external module, which can be obtained [here]
(http://apache.webthing.com/mod_proxy_html/). It rewrites URLs that
occur in web pages. Here we will use it to rewrite gitit's links so that
they all begin with `/wiki/`.

First, tell gitit not to compress pages, since `mod_proxy_html` needs
uncompressed pages to parse. You can do this by setting the gitit
configuration option

    compress-responses: no

Second, modify the link in the `reset-password-message` in the
configuration file:  instead of

    http://$hostname$:$port$$resetlink$

set it to

    http://$hostname$/wiki$resetlink$

Restart gitit.

Now add the following lines to the apache configuration file for the
`mysite.com` server:

    # These commands will proxy /wiki/ to port 5001

    ProxyRequests Off

    <Proxy *>
      Order deny,allow
      Allow from all
    </Proxy>

    ProxyPass /wiki/ http://127.0.0.1:5001/

    <Location /wiki/>
      SetOutputFilter  proxy-html
      ProxyPassReverse /
      ProxyHTMLURLMap  /   /wiki/
      RequestHeader unset Accept-Encoding
    </Location>

Reload your apache configuration and you should be set.

For further information on the use of `mod_proxy_http` to rewrite URLs,
see the [`mod_proxy_html` guide].

[`mod_proxy_html` guide]: http://apache.webthing.com/mod_proxy_html/guide.html

Using gitit as a library
========================

By importing the module `Network.Gitit`, you can include a gitit wiki
(or several of them) in another happstack application. There are some
simple examples in the haddock documentation for `Network.Gitit`.

Reporting bugs
==============

Bugs may be reported (and feature requests filed) at
<http://code.google.com/p/gitit/issues/list>.

There is a mailing list for users and developers at
<http://groups.google.com/group/gitit-discuss>.

Acknowledgements
================

A number of people have contributed patches:

- Gwern Branwen helped to optimize gitit and wrote the
  InterwikiPlugin. He also helped with the Feed module.
- Simon Michael contributed the patch adding RST support.
- Henry Laxen added support for password resets and helped with
  the apache proxy instructions.
- Anton van Straaten made the process of page generation
  more modular by adding Gitit.ContentTransformer.
- Robin Green helped improve the plugin API and interface, and
  fixed a security problem with the reset password code.
- Thomas Hartman helped improve the index page, making directory
  browsing persistent, and fixed a bug in template recompilation.
- Justin Bogner improved the appearance of the preview button.
- Kohei Ozaki contributed the ImgTexPlugin.
- Michael Terepeta improved validation of change descriptions.
- mightybyte suggested making gitit available as a library,
  and contributed a patch to ifLoggedIn that was needed to
  make gitit usable with a custom authentication scheme.

I am especially grateful to the darcs team for using darcsit for
their public-facing wiki.  This has helped immensely in identifying
issues and improving performance.

Gitit's default visual layout is shamelessly borrowed from Wikipedia.
The stylesheets are influenced by Wikipedia's stylesheets and by the
bluetrip CSS framework (see BLUETRIP-LICENSE). Some of the icons in
`img/icons` come from bluetrip as well.

