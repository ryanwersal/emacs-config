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
- ~~Add some form of the "Go to Anything" panel from ST2.~~ ([Helm](https://github.com/emacs-helm/helm) is proving to be quite capable so far.)
- Actually get Clang working with Emacs for code completion etc. (And hopefully code navigation as well!)
- Get something similar to Clang working for Python. (Looking at [ropemacs](http://rope.sourceforge.net/ropemacs.html).)
- ~~Implement a Base16 Dark theme (or find someone who has done so already).~~ (A start can be found in my [base16-emacs repo](https://github.com/ryanwersal/base16-emacs/blob/master/base16-dark-theme.el).)

Installation Notes
------------------
The only step is to clone this repo into ~/.emacs.d and you should be good to go! With Windows you have a couple extra steps. Namely, you have to install [Everything](http://www.voidtools.com/download.php) as per the [wiki](https://github.com/emacs-helm/helm/wiki#wiki-windowsspecificity). I ended up having to download both the installer and the standalone es.exe. I just placed all of it in Everything's install directory, added it to the PATH, and everything was set.