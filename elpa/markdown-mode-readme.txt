markdown-mode is a major mode for editing [Markdown][]-formatted
text files in GNU Emacs.  markdown-mode is free software, licensed
under the GNU GPL.

 [Markdown]: http://daringfireball.net/projects/markdown/

The latest stable version is markdown-mode 1.9, released on January 25, 2013:

   * [markdown-mode.el][]
   * [Screenshot][][^theme]
   * [Release notes][]

 [markdown-mode.el]: http://jblevins.org/projects/markdown-mode/markdown-mode.el
 [screenshot]: http://jblevins.org/projects/markdown-mode/screenshots/20110812-001.png
 [release notes]: http://jblevins.org/projects/markdown-mode/rev-1-9

[^theme]: The theme used in the screenshot is
  [color-theme-twilight](https://github.com/crafterm/twilight-emacs).

markdown-mode is also available in several package managers, including:

   * Debian and Ubuntu Linux: [emacs-goodies-el][]
   * RedHat and Fedora Linux: [emacs-goodies][]
   * OpenBSD: [textproc/markdown-mode][]
   * Arch Linux (AUR): [emacs-markdown-mode-git][]
   * MacPorts: [markdown-mode.el][macports-package] ([pending][macports-ticket])
   * FreeBSD: [textproc/markdown-mode.el][freebsd-port]

 [emacs-goodies-el]: http://packages.debian.org/emacs-goodies-el
 [emacs-goodies]: https://admin.fedoraproject.org/pkgdb/acls/name/emacs-goodies
 [textproc/markdown-mode]: http://pkgsrc.se/textproc/markdown-mode
 [emacs-markdown-mode-git]: http://aur.archlinux.org/packages.php?ID=30389
 [macports-package]: https://trac.macports.org/browser/trunk/dports/editors/markdown-mode.el/Portfile
 [macports-ticket]: http://trac.macports.org/ticket/35716
 [freebsd-port]: http://svnweb.freebsd.org/ports/head/textproc/markdown-mode.el

The latest development version can be downloaded directly
([markdown-mode.el][devel.el]) or it can be obtained from the
(browsable and clonable) Git repository at
<http://jblevins.org/git/markdown-mode.git>.  The entire repository,
including the full project history, can be cloned via the Git protocol
by running

    git clone git://jblevins.org/git/markdown-mode.git

 [devel.el]: http://jblevins.org/git/markdown-mode.git/plain/markdown-mode.el

Dependencies:

markdown-mode requires easymenu, a standard package since GNU Emacs
19 and XEmacs 19, which provides a uniform interface for creating
menus in GNU Emacs and XEmacs.

Installation:

Make sure to place `markdown-mode.el` somewhere in the load-path and add
the following lines to your `.emacs` file to associate markdown-mode
with `.text` files:

    (autoload 'markdown-mode "markdown-mode"
       "Major mode for editing Markdown files" t)
    (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))

There is no consensus on an official file extension so change `.text` to
`.mdwn`, `.md`, `.mdt`, or whatever you call your markdown files.

Customization:

Although no configuration is *necessary* there are a few things
that can be customized.  The `M-x customize-mode` command
provides an interface to all of the possible customizations:

  * `markdown-command' - the command used to run Markdown (default:
    `markdown`).  This variable may be customized to pass
    command-line options to your Markdown processor of choice.

  * `markdown-command-needs-filename' - by default, `markdown-mode'
    will pass the Markdown content to `markdown-command' using
    standard input (`stdin`).  If `markdown-command' does not
    accept standard input, then `markdown-command-needs-filename'
    should be set to non-nil, which instructs `markdown-mode' to
    pass the name of the file as the final command-line argument.
    Note that with this option, you will only be able to run
    `markdown-command' from buffers which are visiting a file.

  * `markdown-open-command' - the command used for calling a standalone
    Markdown previewer which is capable of opening Markdown source files
    directly (default: `nil').  This command will be called
    with a single argument, the filename of the current buffer.
    A representative program is the Mac app [Marked][], a
    live-updating MultiMarkdown previewer which has a command line
    utility at `/usr/local/bin/mark`.

  * `markdown-hr-strings' - list of strings to use when inserting
    horizontal rules, in decreasing order of priority.

  * `markdown-bold-underscore' - set to a non-nil value to use two
    underscores for bold instead of two asterisks (default: `nil').

  * `markdown-italic-underscore' - set to a non-nil value to use
    underscores for italic instead of asterisks (default: `nil').

  * `markdown-indent-function' - the function to use for automatic
    indentation (default: `markdown-indent-line').

  * `markdown-indent-on-enter' - set to a non-nil value to
    automatically indent new lines when the enter key is pressed
    (default: `t')

  * `markdown-follow-wiki-link-on-enter' - set to a non-nil value
    to automatically open a linked document in a new buffer if the
    cursor is an wiki link
    (default: `t')

  * `markdown-wiki-link-alias-first' - set to a non-nil value to
    treat aliased wiki links like `[[link text|PageName]]`.
    When set to nil, they will be treated as `[[PageName|link text]]'.

  * `markdown-uri-types' - a list of protocols for URIs that
    `markdown-mode' should highlight.

  * `markdown-enable-math' - syntax highlighting for
    LaTeX fragments (default: `nil').

  * `markdown-css-path' - CSS file to link to in XHTML output.

  * `markdown-content-type' - when set to a nonempty string, an
    `http-equiv` attribute will be included in the XHTML `<head>`
    block.  If needed, the suggested values are
    `application/xhtml+xml` or `text/html`.

  * `markdown-coding-system' - used for specifying the character
    set identifier in the `http-equiv` attribute (see
    `markdown-content-type').  When set to `nil',
    `buffer-file-coding-system' will be used (and falling back to
    `iso-8859-1' when unavailable).  Common settings are `utf-8'
    and `iso-latin-1'.

  * `markdown-xhtml-header-content' - additional content to include
    in the XHTML `<head>` block.

  * `markdown-command-needs-filename' - set to non-nil if
    `markdown-command' does not accept input from stdin (default: nil).
     Instead, it will be passed a filename as the final command-line
     argument.  As a result, you will only be able to run Markdown
     from buffers which are visiting a file.

  * `markdown-xhtml-standalone-regexp' - a regular expression which
    `markdown-mode' uses to determine whether the output of
    `markdown-command' is a standalone XHTML document or an XHTML
    fragment (default: `"^\\(\<\\?xml\\|\<!DOCTYPE\\|\<html\\)"`).  If
    this regular expression not matched in the first five lines of
    output, `markdown-mode' assumes the output is a fragment and
    adds a header and footer.

  * `markdown-link-space-sub-char' - a character to replace spaces
    when mapping wiki links to filenames (default: `_`).
    For example, use an underscore for compatibility with the
    Python Markdown WikiLinks extension or a hyphen for compatibility
    with GitHub wiki links.

Additionally, the faces used for syntax highlighting can be modified to
your liking by issuing `M-x customize-group RET markdown-faces`
or by using the "Markdown Faces" link at the bottom of the mode
customization screen.

[Marked]: https://itunes.apple.com/us/app/marked/id448925439?ls=1&mt=12&partnerId=30&siteID=GpHp3Acs1Yo
