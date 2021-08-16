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

z
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


# Results
<!-- How we address the multiple goals of the multiple involved organizations -->

## On Haskell community goals

How does this improves the Haskell ecosystem:

* Pancoc = application with a large *indirect* impact on users.
* Pandoc = written in Haskell.
* Then: Improving pandoc => Improving Haskell.


<!--
-- vim: spell
-->

<script>
//setTimeout(function(){
//	window.location.reload(1);
//	}, 1000);
</script>
