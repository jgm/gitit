# Markdown

This wiki's pages are written in [pandoc]'s extended form of [markdown].
If you're not familiar with markdown, you should start by looking
at the [markdown "basics" page] and the [markdown syntax description].
Consult the [pandoc User's Guide] for information about pandoc's syntax
for footnotes, tables, description lists, and other elements not present
in standard markdown.

[pandoc]: http://pandoc.org
[pandoc User's Guide]: http://pandoc.org/README.html
[markdown]: http://daringfireball.net/projects/markdown
[markdown "basics" page]: http://daringfireball.net/projects/markdown/basics
[markdown syntax description]: http://daringfireball.net/projects/markdown/syntax 

Markdown is pretty intuitive, since it is based on email conventions.
Here are some examples to get you started:

<table>
<tr>
<td>`*emphasized text*`</td>
<td>*emphasized text*</td>
</tr>
<tr>
<td>`**strong emphasis**`</td>
<td>**strong emphasis**</td>
</tr>
<tr>
<td>`` `literal text` ``</td>
<td>`literal text`</td>
</tr>
<tr>
<td>`\*escaped special characters\*`</td>
<td>\*escaped special characters\*</td>
</tr>
<tr>
<td>`[external link](http://google.com)`</td>
<td>[external link](http://google.com)</td>
</tr>
<tr>
<td>`![folder](/img/icons/folder.png)`</td>
<td>![folder](/img/icons/folder.png)</td>
</tr>
<tr>
<td>Wikilink: `[Front Page]()`</td>
<td>Wikilink: [Front Page]()</td>
</tr>
<tr>
<td>`H~2~O`</td>
<td>H~2~O</td>
</tr>
<tr>
<td>`10^100^`</td>
<td>10^100^</td>
</tr>
<tr>
<td>`~~strikeout~~`</td>
<td>~~strikeout~~</td>
</tr>
<tr>
<td>
`$x = \frac{{ - b \pm \sqrt {b^2 - 4ac} }}{{2a}}$`
</td>
<td>
$x = \frac{{ - b \pm \sqrt {b^2 - 4ac} }}{{2a}}$^[If this looks like
code, it's because jsMath is
not installed on your system.  Contact your administrator to request it.]
</td>
</tr>
<tr>
<td>
`A simple footnote.^[Or is it so simple?]`
</td>
<td>
A simple footnote.^[Or is it so simple?]
</td>
</tr>
<tr>
<td>
<pre>
> an indented paragraph,
> usually used for quotations
</pre>
</td>
<td>

> an indented paragraph,
> usually used for quotations

</td>
<tr>
<td>
<pre>
    #!/bin/sh -e
    # code, indented four spaces
    echo "Hello world"
</pre>
</td>
<td>

    #!/bin/sh -e
    # code, indented four spaces
    echo "Hello world"

</td>
</tr>
<tr>
<td>
<pre>
* a bulleted list
* second item
    - sublist
    - and more
* back to main list
    1. this item has an ordered
    2. sublist
        a) you can also use letters
        b) another item
</pre>
</td>
<td>

* a bulleted list
* second item
    - sublist
    - and more
* back to main list
    1. this item has an ordered
    2. sublist
        a) you can also use letters
        b) another item

</td>
</tr>
<tr>
<td>
<pre>
Fruit        Quantity
--------  -----------
apples         30,200
oranges         1,998
pears              42

Table:  Our fruit inventory
</pre>
</td>
<td>

Fruit        Quantity
--------  -----------
apples         30,200
oranges         1,998
pears              42

Table:  Our fruit inventory

</td>
</tr>
</table>

For headings, prefix a line with one or more `#` signs:  one for a major heading,
two for a subheading, three for a subsubheading.  Be sure to leave space before
and after the heading.

    # Markdown

    Text...
 
    ## Some examples...
   
    Text...

## Wiki links

Links to other wiki pages are formed this way:  `[Page Name]()`.
(Gitit converts markdown links with empty targets into wikilinks.)

To link to a wiki page using something else as the link text:
`[something else](Page Name)`.

Note that page names may contain spaces and some special characters.
They need not be CamelCase.  CamelCase words are *not* automatically
converted to wiki links.

Wiki pages may be organized into directories.  So, if you have
several pages on wine, you may wish to organize them like so:

    Wine/Pinot Noir
    Wine/Burgundy
    Wine/Cabernet Sauvignon

Note that a wiki link `[Burgundy]()` that occurs inside the `Wine`
directory will link to `Wine/Burgundy`, and not to `Burgundy`.
To link to a top-level page called `Burgundy`, you'd have to use
`[Burgundy](/Burgundy)`.

To link to a directory listing for a subdirectory, use a trailing
slash: `[Wine/]()` will link to a listing of the `Wine` subdirectory.
