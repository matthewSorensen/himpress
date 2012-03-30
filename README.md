Himpress
--------

[**H***askell* **impress** *.js*](http://matthewsorensen.github.com/himpress),

 + a tool for generating [impress.js](https://github.com/bartaz/impress.js) presentations from an amalgam of various markdowns, markups, source code, and presentation-specific commands.
 + the end result of my attempts to avoid the annoyance that is {PowerPoint, OpenOffice/LibreOffice Impress, GoogleDocs, Prezi, Keynote, etc}.
 + the extension of the "Emacs über alles" principle to building presentations.

Incipient Creeping Featurism
--------

 + New css. Optimized for standard low-res projectors and somewhat standard-looking slides - although impress.js is awesome for [3-d "Prezi" style presentations](http://bartaz.github.com/impress.js), such presentations are really finicky to lay out, and are sorta distracting.
 + A slightly more intuitive model for representing 3-d transformations and scaling.
 + Switch to [highlighting-kate](http://hackage.haskell.org/package/highlighting-kate-0.5.0.5) for syntax highlighting - although I don't really like this package.
 + A server to control presentations from a phone/other browser.
 + A better/higher-level set of presentation commands.

LICENSE
---------

Impress.js (*js/impress.js*, and *css/impress.css*) is released under the MIT license and GPL,
copyright 2011-2012 Bartek Szopka.

Himpress itself is released under the 3-clause BSD license (see LICENSE file), 
copyright 2012- Matthew Sorensen.

The Haskell color theme is blatantly stolen from Ethan Schoonover's Solarized palette.
