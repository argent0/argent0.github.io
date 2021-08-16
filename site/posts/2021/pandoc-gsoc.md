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
## Haskell

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

<!-- How we address the multiple goals of the multiple involved organizations -->
# Results

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
