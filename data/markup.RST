# Markup

This wiki's pages are written in [reStructuredText]. If you're
not familiar with reStructuredText, you should start by looking at
the [primer] and the [quick reference guide]. Note that not all
reStructuredText constructs are currently supported.  Use the
preview button if you're in doubt.

  [reStructuredText]: http://docutils.sourceforge.net/rst.html
  [primer]: http://docutils.sourceforge.net/docs/user/rst/quickstart.html
  [quick reference guide]: http://docutils.sourceforge.net/docs/user/rst/quickref.html

## Wiki links

Links to other wiki pages are formed this way: `` `Page Name <>`_ ``.
(Gitit converts markdown links with empty targets into wikilinks.)

To link to a wiki page using something else as the link text:
either `` `something else <Page+Name>`_ `` or

    `something else`_

    .. _`something else`: Page Name

Note that page names may contain spaces and some special
characters. They need not be CamelCase. CamelCase words are *not*
automatically converted to wiki links.

Wiki pages may be organized into directories. So, if you have
several pages on wine, you may wish to organize them like so:

    Wine/Pinot Noir
    Wine/Burgundy
    Wine/Cabernet Sauvignon

Note that a wiki link `` `Burgundy <>`_ `` that occurs inside the `Wine`
directory will link to `Wine/Burgundy`, and not to `Burgundy`. To
link to a top-level page called `Burgundy`, you'd have to use
`` `Burgundy </Burgundy>`_ ``.

To link to a directory listing for a subdirectory, use a trailing
slash:  `` `Wine/ <>`_ `` will link to a listing of the `Wine` subdirectory.
