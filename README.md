Introduction
------------
This is my new Emacs configuration (old one can be [found here](https://github.com/ryanwersal/hipplej-emacs)). It currently is still based largely on [Justin Hipple's Emacs configuration](https://github.com/hipplej/hipplej-emacs) but I am hoping it will prove to grow more distinct over time.

Why a new config?
-----------------
There are a couple motivations for the new configuration. The largest was that for the past 6-8 months I have been using [Sublime Text 2](http://www.sublimetext.com/). It has mostly been a good experience except I have run into a handful of impactful performance issues largely related to installed plugins (I'm looking at you [SublimeRope](http://www.sublimetext.com/)!) which were admittedly related to conditions outside their control. 

Between that and my growing affection for Clojure (and probably Lisps in general), I've decided to "come back home" to Emacs. 

Goals
-----
There are a few long term goals that I wish to work towards. Some are old goals made new again and others are directly inspired by my favorite features from Sublime Text:

- ~~Add in a Minimap feature~~ (Added thanks to [MiniMap](http://www.emacswiki.org/emacs/MiniMap).)
- Handle multi selection in a vaguely ST2 way (including the "select next instance of this token" functionality.) (Looking at [multiple-cursors.el](https://github.com/magnars/multiple-cursors.el) currently.)
- Add some form of the "Go to Anything" panel from ST2. (Currently investigating [Helm](https://github.com/emacs-helm/helm).)
- Actually get Clang working with Emacs for code completion etc. (And hopefully code navigation as well!)
- Get something similar to Clang working for Python. (Looking at [ropemacs](http://rope.sourceforge.net/ropemacs.html).)
