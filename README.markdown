Gitit
=====

Gitit is a wiki program written in Haskell. It uses [Happstack][]
for the web server and [pandoc][] for markup processing. Pages and
uploaded files are stored in a [git][] or [darcs][] repository and may
be modified either by using the VCS's command-line tools or through the
wiki's web interface. By default, pandoc's extended version of markdown
is used as a markup language, but reStructuredText can also be used.
Pages can be exported in a number of different formats, including LaTeX,
RTF, OpenOffice ODT, and MediaWiki markup. Gitit can be configured to
display TeX math (using [jsMath][]) and highlighted source code (using
[highlighting-kate][]).

[git]: http://git.or.cz  
[darcs]: http://darcs.net
[pandoc]: http://johnmacfarlane.net/pandoc
[Happstack]: http://happstack.com
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

Optional plugins support
------------------------

Plugins are small Haskell programs that transform a wiki page after it
has been converted from Markdown or RST. See the example plugins in the
`plugins` directory. To enable a plugin, include the path to the plugin
(or its module name) in the `plugins` field of the configuration file.
(If the plugin name starts with `Gitit.Plugin.`, gitit will assume that
the plugin is an installed module and will not look for a source file.)

The gitit executable will be much larger if plugins support is compiled
in. Plugin support is disabled by default. To enable support for
plugins, pass the `plugins` flag to Cabal:

    cabal install gitit -fplugins

Note also that if you compile gitit for executable profiling, attempts
to load plugins will result in "internal error: PAP object entered!"

Running gitit
-------------

To run gitit, you'll need [git][] in your system path. (Or
[darcs][], if you're using darcs to store the wiki data.)

Gitit assumes that the page files (stored in the git repository) are
encoded as UTF-8. Even page names may be UTF-8 if the file system
supports this. So you should make sure that you are using a UTF-8 locale
when running gitit. (To check this, type `locale`.)

Switch to the directory where you want to run gitit. This should be a
directory where you have write access, since two directories, `static`
and `wikidata`, and two files, `gitit-users` and `template.html`, will
be created here. To start gitit, just type:

    gitit

If all goes well, gitit will do the following:

 1.  Create a git repository, `wikidata`, and add a default front page.
 2.  Create a `static` directory containing the scripts, images,
     and stylesheets used by gitit.
 3.  Create a `template.html` file containing an HStringTemplate template
     for wiki pages.
 4.  Start a web server on port 5001.

Check that it worked: open a web browser and go to
<http://localhost:5001>.

Wiki links and formatting
-------------------------

For instructions on editing pages and creating links, see the "Help" page.

Gitit interprets links with empty URLs as wikilinks. Thus, in markdown
pages, `[Front Page]()` creates an internal wikilink to the page `Front
Page`. In reStructuredText pages, `` `Front Page <>`_ `` has the same
effect.

Configuring gitit
=================

Configuration options
---------------------

You can set some configuration options when starting gitit, using the
option `-f [filename]`. To get a copy of the default configuration file,
which you can customize, just type:

    gitit --print-default-config > default.conf

The default configuration file is documented with comments throughout.

The `static` directory
----------------------

If there is no wiki page or uploaded file corresponding to a request,
gitit always looks last in the `static` directory. So, for example,
a file `foo.jpg` in the `img` subdirectory of the `static` directory
will be accessible at the url `/img/foo.jpg`. Pandoc creates three
subdirectories of `static`, `css`, `img`, and `js`, which include the
icons, stylesheets, and javascripts it uses.

Note:  if you set `static-dir` to be a subdirectory of `repository-path`,
and then add the files in the static directory to your repository, you
can ensure that others who clone your wiki repository get these files
as well.  It will not be possible to modify these files using the web
interface, but they will be modifiable via git.

Using darcs instead of git
--------------------------

By default, gitit will store wiki pages in a git repository in the
`wikidata` directory.  If you'd prefer to use darcs instead of git,
you need to add the following field to the configuration file:

    repository-type: Darcs

This program may be called "darcsit" instead of "gitit" when a darcs
backend is used.

Changing the theme
------------------

To change the look of the wiki, you can modify `screen.css` in
`static/css`.  But a better approach is to add a line to `template.html`
that imports your own custom stylesheet.  This line should go
after the line that links to `/css/screen.css`:

    <link href="/css/my-screen.css" rel="stylesheet" media="screen, projection" type="text/css" />

Then add `my-screen.css` to the `static/css` directory and customize
it as you see fit.  The advantage of this approach is that you won't
need to merge changes in `screen.css` when gitit is updated.  You can
just copy the revised `screen.css` into your `static/css` directory.

To change the look of printed pages, modify `print.css`.

The logo picture can be changed by copying a new PNG file to
`static/img/logo.png`.

For more radical changes, you can modify `template.html`.
Note that interpolated variables are surrounded by `$`s, so literal
`$` must be backslash-escaped.

Adding support for math
-----------------------

Gitit is designed to work with [jsMath][] to display LaTeX math in
HTML. Download `jsMath` and `jsMath Image Fonts` from the [jsMath
download page][]. You'll have two `.zip` archives. Unzip them both
in the `static/js` directory (a new subdirectory, `jsMath`, will be
created). You can test to see if math is working properly by clicking
"help" on the top navigation bar and looking for the math example (the
quadratic formula). Note that if you copied the `jsMath` directory into
`static` *after* starting gitit, you will have to restart gitit for the
change to be noticed. Gitit checks for the existence of the jsMath files
when it starts, and will not include links to them unless they exist.

To write math on a wiki page, just enclose it in dollar signs, as in LaTeX:

    Here is a formula:  $\frac{1}{\sqrt{c^2}}$

You can write display math by enclosing it in double dollar signs:

    $$\frac{1}{\sqrt{c^2}}$$

[jsMath download page]: http://sourceforge.net/project/showfiles.php?group_id=172663

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

To see what languages are available:

    pandoc -v

[delimited code blocks]: http://johnmacfarlane.net/pandoc/README.html#delimited-code-blocks

Accessing the wiki via git or darcs
===================================

All the pages and uploaded files are stored in a git or darcs
repository. By default, this lives in the `wikidata` directory (though
this can be changed through configuration options). So you can interact
with the wiki using git command line tools:

    git clone ssh://my.server.edu/path/of/wiki/wikidata
    cd wikidata
    vim Front\ Page.page  # edit the page
    git commit -m "Added message about wiki etiquette" Front\ Page.page
    git push 

If you now look at the Front Page on the wiki, you should see your changes
reflected there.  Note that the pages all have the extension `.page`.

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

Reporting bugs
==============

Bugs may be reported (and feature requests filed) at
<http://code.google.com/p/gitit/issues/list>.

Acknowledgements
================

A number of people have contributed patches:

- Gwern Branwen helped to optimize gitit.
- Simon Michael contributed the patch adding RST support.
- Henry Laxen added support for password resets and helped with
  the apache proxy instructions.
- Anton van Straaten made the process of page generation
  more modular by adding Gitit.ContentTransformer.
- Robin Green helped improve the plugin API and interface.
- Justin Bogner improved the appearance of the preview button.

Gitit's default visual layout is shamelessly borrowed from Wikipedia.
The stylesheets are influenced by Wikipedia's stylesheets and by the
bluetrip CSS framework (see BLUETRIP-LICENSE). Some of the icons in
`img/icons` come from bluetrip as well.

