---
title: "GSOC: Improve pandoc's figure handlings."
---

# Introduction

<!-- Narrative style
	- Like a guide for others to use. Withuot becoming a tutorial.
-->

<!-- What's GSOC. -->

> [Google Summer of Code](https://summerofcode.withgoogle.com/) is  a global
> program focused on bringing more student developers into open source software
> development. Students work with an open source organization on a 10 week
> programming project during their break from school.


<!-- Who we are. -->

[@argen0](https://github.com/argent0)
[@tarleb](https://github.com/tarleb)
[@cderv](https://github.com/cderv)
[@apreshill](https://github.com/apreshill)

<!-- Summer of Haskell

Anything that improves the Haskell ecosystem is valid.

Projects should benefit as many people as possible
-->
## The Haskell Open Source Oraganization



<!-- The pandoc proposal, a summary -->

<!-- About pandoc -->
## Pandoc

One of `haskell.org`'s proposals was put up by the people working in pandoc.

Pandoc is ...
* [lua filters](https://pandoc.org/lua-filters.html)

## Pandoc's proposal

[Pandoc's proposal](https://summer.haskell.org/ideas.html#pandoc-figures)
for GSoC aimed to improve support for figures. To achieve
this, it set two main goals:

1. Extending pandoc's internal representation to capture information about
   figures.

2. Implementing full figure support for, at least, one input and output format.

It also included an optional goal:

3. Implementing an interface usable by Lua.


## My proposal

* [My proposal](https://github.com/argent0/gsoc-2021-proposal)
	- My previous contributions.

* [Prior work](https://github.com/jgm/pandoc-types/pull/83)

# On Figures

To start improving pandoc's support for figures, it's important to agree on what
we mean by *figures*.

For this project we considered a figure to be:

> A part of the text document that is not part of the main flow of the text. It
> may contain a descriptive *caption* and it may be *referenced* in the main
> text.

This concept of a figure is inspired by various document formats that deal with
different aspects of digital documentation:

* HTML
  [`<figure>`](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figure)
  tag, "self-contained content, with an optional caption. It is referenced as a
  single unit".

* JATS
  [`<fig>`](https://jats.nlm.nih.gov/archiving/tag-library/1.1/element/fig.html)
  tag, "Block of graphic or textual material that is identified as a figure,
  usually bearing a caption and a label".

* LaTeX
  [floats](https://en.wikibooks.org/wiki/LaTeX/Floats,_Figures_and_Captions)
  , "not part of the normal stream of text, but separate entities, positioned in
  a part of the page to themselves".

It also aligns well with Pandoc's internal representation philosophy that
prioritizes *content* over *format*.

It's important to highlight that figures aren't always images or pictures.

# Pandoc's orignial handling of figures.

Despite its internal representation lacking an *ad hoc* representation of
figures, pandoc's was capable of producing "figure" elements in its output. This
was achieved by treating an specific **construction** in a particular way.

```
[Para [Image ("", [], [("alt", "alt1")]) [Str "capt"] ("src", "fig:tit")]]
```

For example, here is pandoc 2.14 using the construction to generate HTML, LaTeX
and XWiki figures:

```
$ pandoc -f native -t html
[Para [Image ("", [], [("alt", "alt1")]) [Str "capt"] ("src", "fig:tit")]]
<figure>
<img src="src" title="tit" alt="alt1" alt="capt" /><figcaption aria-hidden="true">capt</figcaption>
</figure>

$ pandoc -f native -t latex
[Para [Image ("", [], [("alt", "alt1")]) [Str "capt"] ("src", "fig:tit")]]
\begin{figure}
\centering
\includegraphics{src}
\caption{capt}
\end{figure}

$ pandoc -f native -t xwiki
[Para [Image ("", [], [("alt", "alt1")]) [Str "capt"] ("src", "fig:tit")]]
[[image:src||alt="capt" title="fig:tit"]]
```

The logic used by these `Writers` was:

*A paragraph containing a single inline image whose title has the `fig:` prefix,
is a figure*

This approach provides some benefit with its simplicity and the fact that it is
already in place. But it also presents issues.  In particular: it *leaves room
for interpretation on how to use it* to produce the "figure" element. Consider,
for example, the `title` element, the `tit` part of the `fig:tit`, in the
outputs above:

* It becomes the `title` attribute of the `img` tag in HTML. This attribute is
  what dictates the tooltip that shows up when hovering the mouse over the
  image.
* It is absent from the LaTeX output.
* It becomes the `title` in the XWiki output.

The issue being: the same piece of text filling widely different roles.

I believe this issue arises when the author of a `Writer` needs to interpret
the **construction** without explicitly defined roles for its parts. And that
compounded with what the output format can actually handle.

Nevertheless, the current figure support is very adequate for handling
figures consisting of an image and a "caption".

# The `SimpleFigure` construction

To address some of the issues with the previous handling of figures, we
introduced, along with some helper functions, the `SimpleFigure` [pattern
synonym](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/pattern_synonyms.html)

```haskell
-- | Constructor for a figure with a single image.
--
-- It can be used to construct a figure:
--
-- >>> SimpleFigure nullAttr [] (T.pack "", T.pack "title")
-- Para [Image ("",[],[]) [] ("","fig:title")]
--
--
-- It can be used to pattern match:
--
-- >>> let img = Para [Image undefined undefined (undefined, T.pack "title")]
-- >>> case img of { SimpleFigure _ _ _ -> True; _ -> False }
-- False
-- >>> let fig = Para [Image undefined undefined (undefined, T.pack "fig:title")]
-- >>> case fig of { SimpleFigure _ _ tit -> snd tit; _ -> T.pack "" }
-- "title"
pattern SimpleFigure :: Attr -> [Inline] -> Target -> Block
pattern SimpleFigure attributes figureCaption tgt <-
    Para [Image attributes figureCaption
        (isFigureTarget -> Just tgt)]  where
  SimpleFigure attributes figureCaption tgt =
    Para [Image attributes figureCaption (second ("fig:" <>) tgt)]
```

This is very much like adding a new constructor to the `Block` type but with
some differences.

Some benefits of this approach are:

* It is backward compatible with the previous construction.
* It formalizes, without enforcing, the roles for the construction's elements.
* It makes it easier to find where are figures handled in the code for both
  `Readers` and `Writers`.

  Here is an example diff from the RST `Reader` in the [pandoc pull
  request](https://github.com/jgm/pandoc/pull/7364):

  ```diff
  - return $ B.para (B.imageWith (imgAttr "figclass") src "fig:"
  -             caption) <> legend
  + return $ B.simpleFigureWith
  +     (imgAttr "figclass") caption src "" <> legend
  ```

Of course, there are also some limitations:

* It doesn't *enforce* the construction on new code, as an *actual constructor*
  would do. For example, code with non-exhaustive patterns matches won't rise a
  warning if `SimpleFigure` is not handled.

* It lacks an explicit `alt-text` field. This can be included using attributes,
  but I would have liked to give it more importance.

The code for the `SimpleFigure` pattern synonym was submitted with the
[pandoc-types pull request](https://github.com/jgm/pandoc-types/pull/90).
Its use on `Readers` and `Writers` was submitted with the [pandoc pull
request](https://github.com/jgm/pandoc/pull/7364).

I believe this is a modest improvement of the previous code. It provides an
explicit representation for figures. It could go a little further with an
explicit constructor; but keeping the behavior backward compatible prevents
breaking some workflows down the line, for example someone using a `lua-filter`.

Along the way, two other pull requests where submitted and merged to pandoc's
[main repository](https://github.com/jgm/pandoc):

* [The first](https://github.com/jgm/pandoc/pull/7417) dealing with duplicated
  `alt-text` tags in the HTML output.

* [The second](https://github.com/jgm/pandoc/pull/7425) closely related, dealing
  with HTML figure accessibility.

# The `Figure` constructor

The `SimpleFigure` constructor only addressed one type of figures, the ones with
only one image in them. To address the general case of the concept of a docment
within a document detailed in the section on figures, we introduced the
following `Block` constructor:

```haskell
data Block =
	-- ...
	-- | Figure, with attributes, caption and caption position, width
	-- (optional), and content (list of blocks)
	| Figure Attr Caption [Block]
	-- ...
```

This includes other helper functions andd tests and is part of the [pandoc-types
pull request](https://github.com/jgm/pandoc/pull/7364). It's based on the
previous work by [@despresc](https://github.com/jgm/pandoc-types/pull/83).
Modifications include: the removal of the `CaptionPos` argument that can be
specified as an attribute; and the code involving tables.

Tests for this constructor are also included with the pull request.

This constructor allows pandoc to capture the semantics of figures from
different formats into its internal representation. It also helps when writing
output in formats that support figures.

## Reading input with figures

Reading figures is now supported for two formats natively and one format through
a Lua filter. It has to be enabled with a new [pandoc
extension](https://pandoc.org/MANUAL.html#extensions): `native_figures`.

```
#### Extension: `native_figures` ####

Use pandoc's native `Figure` element for content inside `<figure>` tags, in the
case of HTML, or `figure` environments, in case of LaTeX. This, in turn, allows
some writers to produce more accurate representations of figures. It also
allows the use of the `Figure` element in filters, for custom figure output.

This extension can be enabled/disabled for the following formats:

input formats
: `latex` `html`
```

The choice of an extension was made to introduce the new behaviour with minimal
disruption of the old one.

### Reading HTML 5 figures

This version of pandoc can now read figures from HTML into its internal
representation:

**After**
```
$ pandoc -f html+native_figures -t native
<figure class="important">
  <img src="../media/rId25.jpg" />
  <ul> <li> ITEM </li> </ul>
  <figcaption> CAP2 </figcaption>
</figure>
^D
[Figure ("",["important"],[]) (Caption Nothing [Plain [Str "CAP2"]])\
	[ Plain [Image ("",[],[]) [] ("../media/rId25.jpg","")]
	, BulletList [[Plain [Str "ITEM"]]]]]
```

The difference with the old handling is that, now, elements inside a figure are
not limited to only one image:

**Before**
```
$ pandoc-before -f html -t native
...
^D
[Para [Image ("",[],[]) [Str "CAP2"] ("../media/rId25.jpg","fig:")]]
```

Notice the missing `ITEM`. Handling of attributes and classes has also become
more granular and accurte. Notice the missing `important` class above.

### Reading LaTeX figures

It can also read LaTeX figures:

**After**
```
pandoc -f latex+native_figures -t native
\begin{figure}
  \begin{subfigure}[b]{0.5\textwidth}
    \begin{subfigure}[b]{0.5\textwidth}
      \centering
      \includegraphics{test/media/rId25.jpg}
      \caption{CAP1.1}
    \end{subfigure}
    \begin{subfigure}[b]{0.5\textwidth}
      \centering
      \includegraphics{test/media/rId25.jpg}
      \caption{CAP1.2}
    \end{subfigure}
    \caption{CAP1}
    \label{fig:inner1}
  \end{subfigure}
  \begin{subfigure}[b]{0.5\textwidth}
    \includegraphics{test/media/rId25.jpg}
    \caption{CAP2}
    \label{fig:inner2}
  \end{subfigure}
  \caption{CAP}
  \label{fig:outer}
\end{figure}
^D
[Figure ("fig:outer",[],[]) (Caption Nothing [Plain [Str "CAP"]]) 
	[ Figure ("fig:inner1",[],[]) (Caption Nothing [Plain [Str "CAP1"]])
		[ Figure ("",[],[]) (Caption Nothing [Plain [Str "CAP1.1"]])
			[Plain [Image ("",[],[]) [] ("test/media/rId25.jpg","")]]
		, Figure ("",[],[]) (Caption Nothing [Plain [Str "CAP1.2"]])
			[Plain [Image ("",[],[]) [] ("test/media/rId25.jpg","")]]]
	, Figure ("fig:inner2",[],[]) (Caption Nothing [Plain [Str "CAP2"]])
		[Plain [Image ("",[],[]) [] ("test/media/rId25.jpg","")]]]]
```

It respects the figure and subfigure hierarchies, labels and captions
accurately.

**Before**

```
pandoc -f latex -t native
...
^D
[Para [Image ("",[],[]) [Str "CAP1.1"] ("test/media/rId25.jpg","fig:")]
,Para [Image ("",[],[]) [Str "CAP1.2"] ("test/media/rId25.jpg","fig:")]
,Para [Image ("fig:inner2",[],[]) [Str "CAP2"] ("test/media/rId25.jpg","fig:")]]
```

Figures with sub-figures where flattened into a list of figures using the
construction mentioned in the original figure handling section. Some captions
and labels are lost.

### Markdown figures

Handling of markdown figures was implemented through a Lua filter. We opted for
this approach because there is yet no agreed-upon syntax for figures in markdown
(for example commonmark). Pandoc has its own markdown flavor but we have also
opted to propose our syntax by implementing it as a filter. The code of the
filter has been submitted in the [lua-filters pull
request](https://github.com/pandoc/lua-filters/pull/187).

I've written two filters that represent two possible markdown syntax extensions
to represent figures. Both use pandoc's markdown `div` sytax with special
classes.

#### The explcit caption syntax

```markdown
::: { .figure }

content.

:::: {.caption }
caption
::::

:::
```

In this syntax a figure is a pandoc div with a `figure` class and the caption,
if present, is a div with a `caption` class.

Here is an example that converts this to HTML

```
$ pandoc -f markdown -t html --lua-filter=../lua-filters/markdown-figures/md-figure-explicit.lua
...
```

```html
<figure>
<p>content.</p>
<figcaption><p>caption</p></figcaption>
</figure>
```

#### The implicit caption syntax

```markdown
::: { .figure }
figure content

figure caption.
:::
```

This, more concise, syntax uses the last paragraph inside the div as the caption
for the figure.

```
$ pandoc -f markdown -t html --lua-filter=../lua-filters/markdown-figures/md-figure-implicit.lua
...
```

```html
<figure>
<p>figure content</p>
<figcaption><p>figure caption.</p></figcaption>
</figure>
```

## Generating output with figures.

Once figures can be described in pandoc's internal representation, it is the
`Writers` that translate them into various output formats. Not all output
formats can represent figures, and for those that can, we have decided to focus
on the ones that would make the least intrusive modifications first.

Next, I'll briefly enumerate the output resulting from the `Figure` constructor
in various formats.

### HTML


```
% pandoc -f native -t html5
[Figure ("fig-id",[],[]) (Caption Nothing [Plain [Str "caption"]]) [Para [Str "content"]]]

^D
```
```html
<figure id="fig-id">
<p>content</p>
<figcaption>caption</figcaption>
</figure>
```

Figures are represented as `<figure>` tags.

### Org mode format

```
% pandoc -f native -t org
[Figure ("fig-id",[],[]) (Caption Nothing []) [Para [Str "content"]]]
^D
```
```org
<<fig-id>>
content
```

Emac's org mode adds an anchor to the content of the figure.

### Textile

```
% pandoc -f native -t textile
[Figure ("fig-id",[],[]) (Caption Nothing []) [Para [Image ("",[],[]) [] ("foo.png", "")]]]
^D
```
```html
<figure id="fig-id">

!foo.png!


</figure>
```

The textile format constructs an HTML5 figure.

### Texinfo

```
% pandoc -f native -t texinfo
[Figure ("fig-id",[],[])
	(Caption Nothing [Para [Str "Caption"]])
	[Para [Image ("",[],[]) [] ("foo.png", "fig:")]]]

^D
```
```texinfo
@node Top
@top Top

@float Figure
@image{foo,,,Caption,png}
@caption{Caption}
@end float
```

GNU Info's figures.

### RST

```
% pandoc -f native -t rst
[Figure ("fig-id",[],[]) (Caption Nothing [Para [Str "Caption"]])
	[Para [Image ("",[],[]) [] ("foo.png", "fig:")]]]

^D
```
```rst
.. container:: float
   :name: fig-id

   .. figure:: foo.png
      :alt: 
```

Figures are represented as containers.

### Markdown


```
% pandoc -f native -t markdown
[Figure ("fig-id",[],[]) (Caption Nothing [Para [Str "Caption"]])
	[Para [Image ("",[],[]) [] ("foo.png", "fig:")]]]
^D
```
```markdown
::: {#fig-id .figure}
![](foo.png)
:::
```

Figures are represented as a pandoc div ith the `.figure` class.

### MediaWiki


```
% pandoc -f native -t mediawiki
[Figure ("fig-id",[],[]) (Caption Nothing [Para [Str "Caption"]])
	[Para [Image ("",[],[]) [] ("foo.png", "fig:")]]]

^D
```
```mediawiki
<div id="fig-id" class="figure">

[[File:foo.png|thumb|none]]


</div>
```

Figures are represented as a div with the `figure` class.

### Jats


```
% pandoc -f native -t jats
[Figure ("fig-id",[],[]) (Caption Nothing [Para [Str "Caption"]]) [Para [Str "Text"],
Para [Image ("fig-id-2",[],[]) [] ("foo.png", "fig:")]]]

^D
```
```jats
<boxed-text id="fig-id">
  <p>Text</p>
  <fig id="fig-id-2">
    <graphic mimetype="image" mime-subtype="png" xlink:href="foo.png" xlink:title="" />
  </fig>
</boxed-text>
```

Figures are represented with the `boxed-text` tag in JATS.

### XWiki

```
% pandoc -f native -t xwiki
[Figure ("fig-id",[],[]) (Caption Nothing []) [Para [Str "content"]]]

^D
```
```xwiki
(((
{{id name="fig-id" /}}content
)))
```

Figures are represented as groups.

# Results

## On Haskell community goals
<!-- How we address the multiple goals of the multiple involved organizations -->

How does this improves the Haskell ecosystem:

* Pancoc = application with a large *indirect* impact on users.
* Pandoc = written in Haskell.
* Then: Improving pandoc => Improving Haskell.



<!--
-- vim: spell
-->
