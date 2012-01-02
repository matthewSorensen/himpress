This looks like a very nice place to start that Haskell-presentation super-tool I've
long been threatening to write. As of now, the grand plan is something like:

1) Build my own version of the main style-sheet - incorporating a nice modern Haskell mode. Something...solarized...?

2) Some monstrosity, linking pandoc, Hscolor, and my own slide-layout DSL + tool

3) ????

4) PROFIT

Full insanity would be some way to start referencing definitions in the slide (wait until the base library has anchors),
and clever linking of libraries, documentation, and source, like Agda.

Even worse would be some way of getting haddock to render in a presentation like this. Could use something like a connectivity graph to
fully lay-out the module, starting with types/classes and then fanning out to secondary definitions... This is a horrid idea, and shouldn't be 
attempted.

Todo:

+ My own version of the CSS
+ Switch over to highlighting-kate highlighting library.
+ Build a mostly-blank template presentation
+ Update from upstream

~~impress.js~~ himpress
============

It's a presentation framework based on the power of CSS3 transforms and 
transitions in modern browsers and inspired by the idea behind prezi.com.

*WARNING*

impress.js may not help you if you have nothing interesting to say ;)



DEMO
------

impress.js demo: [http://bartaz.github.com/impress.js]


If you have used impress.js in your presentation and would like to have it listed here,
please contact me via GitHub or send me a pull request to updated `README.md` file.



BROWSER SUPPORT
-----------------

Impress.js is developed with current webkit-based browsers in mind (Chrome,
Safari), but *should* work also in other browsers supporting CSS3 3D transforms
and transitions (Firefox, IE10).

Additionally for the animations to run smoothly it's required to have hardware
acceleration support in your browser. This depends on the browser, your operating
system and even kind of graphic hardware you have in your machine.

It's actively developed with newest Chromium and tested in Firefox Aurora.

I don't really expect it to run smoothly in non-webkit-based browser.
If it does, just let me know, I'll glad to hear that!

For browsers not supporting CSS3 3D transforms impress.js adds `impress-not-supported`
class on `#impress` element, so fallback styles can be applied.


### Mobile

Mobile browsers are currently not supported. Even iOS and Android browsers that support
CSS 3D transforms are forced into fallback view at this point.

Anyway, I'm really curious to see how modern mobile devices such as iPhone or iPad can
handle such animations, so future mobile support is considered.



LICENSE
---------

Copyright 2011 Bartek Szopka. Released under MIT License.

