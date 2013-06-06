This mode provides syntax highlighting for LESS CSS files, plus
optional support for `flymake-mode' and compilation of .less files
to .css files at the time they are saved: use
`less-css-compile-at-save' to enable the latter.

Command line utility "lessc" is required if enabling flymake or
setting `less-css-compile-at-save' to t.  To install "lessc" using
the Node.js package manager, run "npm install less"

Also make sure the "lessc" executable is in emacs' PATH, example:
(setq exec-path (cons (expand-file-name "~/.gem/ruby/1.8/bin") exec-path))
or customize `less-css-lessc-command' to point to your "lessc" executable.

`less-css-mode' is derived from `css-mode', and indentation of
nested blocks may not work correctly with versions of `css-mode'
other than that bundled with recent Emacs.

You can specify per-file values for `less-css-compile-at-save',
`less-css-output-file-name' or `less-css-output-directory' using a
variables header at the top of your .less file, e.g.:

// -*- less-css-compile-at-save: t; less-css-output-directory: "../css" -*-

If you don't need CSS output but would like to be warned of any
syntax errors in your .less source, enable `flymake-mode': support
is provided for .less files, but note that the less compiler is a
little slow, so there can be a delay of several seconds between
editing and receiving feedback on any error.

Credits

The original code for this mode was, in large part, written using
Anton Johansson's scss-mode as a template -- thanks Anton!
https://github.com/antonj
