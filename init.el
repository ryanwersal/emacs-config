;;; init --- My Emacs initialization file
;;; Commentary:
;;; Nothing too out of the ordinary here.
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar is-windows-p (eq system-type 'windows-nt))
(defvar is-osx-p (eq system-type 'darwin))
(defvar is-linux-p (eq system-type 'gnu/linux))

(defvar default-font-name
  (cond (is-windows-p "PragmataPro-8")
		(is-osx-p "PragmataPro-12")
		(is-linux-p "PragmataPro-9")
		(t "PragmataPro-10")))

(defun libdir-file (file)
  "Create a valid path to FILE that is located in the Emacs lib folder (~/.emacs.d)."
  (concat (expand-file-name "~/.emacs.d") "/" file))

(defun highlight-fixme-tokens ()
  "Highlight fixme tokens in comments."
  (font-lock-add-keywords nil '(("\\<\\(IMPROVEME\\|FIXME\\|TODO\\|BUG\\|NOTE\\|HACK\\)[:\(]" 1 font-lock-warning-face t))))

(defun confirm-exit ()
  "Prompt prior to exit."
  (interactive)
  (if (yes-or-no-p "Do you want to exit? ")
	  (save-buffers-kill-emacs)))
(global-set-key (kbd "C-x C-c") 'confirm-exit)
(global-unset-key (kbd "C-z"))

(defun z-normalize-newlines ()
  (interactive)
  (set-buffer-file-coding-system 'iso-latin-1-unix))

(defun z-normalize-whitespace ()
  (interactive)
  (let ((beg (region-beginning))
		(end (region-end)))
	(whitespace-cleanup-region beg end)
	(cond (indent-tabs-mode (tabify beg end))
		  (t (untabify beg end)))))

(defun st2-like-beginning-of-line ()
  "Reproduce ST2 beginning of line functionality.
Go to the position of the first non-whitespace character.
If already there, go to actual beginning of line."
  (interactive)
  (let ((col (current-column)))
	  (back-to-indentation)
	  (if (= col (current-column)) (move-beginning-of-line nil))))
(global-set-key (kbd "C-a") 'st2-like-beginning-of-line)

(defun open-line-and-indent ()
  "'newline-and-indent' from middle of line without breaking that line."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))
(global-set-key (kbd "<S-return>") 'open-line-and-indent)
(global-set-key (kbd "RET") 'newline-and-indent)

(defun eval-and-replace ()
  "Replace preceding sexp with its result."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
	  (prin1 (eval (read (current-kill 0)))
			 (current-buffer))
	(error (message "Invalid expression")
		   (insert (current-kill 0)))))
(global-set-key (kbd "C-c e") 'eval-and-replace)

(defun open-fogbugz ()
  (interactive)
  (browse-url
   (concat
	"https://fogbugz.zuerchertech.com/default.asp?"
	(url-hexify-string (if mark-active
						   (buffer-substring (region-beginning) (region-end))
						 (read-string "FogBugz #: "))))))
(global-set-key (kbd "C-c C-f") 'open-fogbugz)

(defun byte-recompile ()
  "Recompile .emacs.d directory."
  (interactive)
  (byte-recompile-directory (libdir-file ".") 0))

(global-set-key (kbd "C-c C-/") 'comment-or-uncomment-region)

(global-set-key (kbd "C-x i") 'imenu)

(global-set-key (kbd "C-c i") 'indent-region)
(global-set-key (kbd "C-c w") 'z-normalize-whitespace)
(global-set-key (kbd "C-c l") 'goto-line)
(global-set-key (kbd "C-c x") 'execute-extended-command)

(setq-default fill-column 100)
(global-set-key (kbd "C-c f") 'fill-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default buffer-file-coding-system 'iso-latin-1-unix
			  tab-width 4
			  indent-tabs-mode t)
(add-to-list 'default-frame-alist `(font . ,default-font-name))

(setq-default frame-title-format
			  '((:eval
				 (concat
				  (projectile-project-name)
				  " - "
				  (if (buffer-file-name)
					  (abbreviate-file-name (buffer-file-name))
					"%b")))))
(setq-default icon-title-format 'frame-title-format)

;; Typing replaces selected region.
(delete-selection-mode t)

;; Allow y/n instead of yes/no at prompts.
(fset 'yes-or-no-p 'y-or-n-p)

(global-hl-line-mode +1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bell-volumne 0)
 '(c-basic-offset 4)
 '(c-default-style (quote ((c-mode . "bsd") (c++-mode . "bsd") (java-mode . "java") (other . "bsd"))))
 '(get-frame-for-buffer-default-instance-limit nil)
 '(gutter-buffers-tab-visible-p nil)
 '(inhibit-startup-screen t)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(paren-mode (quote paren) nil (paren))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(sound-load-list nil)
 '(tool-bar-mode nil)
 '(visible-bell t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(global-auto-revert-mode 1)

(set-default 'sentence-end-double-space nil) ;; Don't require double spaces after periods anywhere.

;; Make help more helpful (and less intrusive).
(global-set-key (kbd "C-c C-h") 'help-command)
(global-set-key (kbd "C-c C-h a") 'apropos)

;; Configure whitespace-mode
(setq whitespace-style (quote (face spaces tabs newline space-mark tab-mark trailing)))
(setq whitespace-display-mappings
	  '(
		(space-mark 32 [183] [46])
		(newline-mark 10 [182 10])
		(tab-mark 9 [8594 9] [92 9])
	   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup load-path
(defvar search-paths '("plugins" "plugins/zmonitor.el"
					   "themes" "themes/base16-emacs"))
(dolist (path search-paths) (add-to-list 'load-path (libdir-file path)))

;; Configure ELPA
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
						 ("marmalade" . "http://marmalade-repo.org/packages/")
						 ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

;; Maximize window
(require 'maxframe)
(add-hook 'window-setup-hook 'maximize-frame t)

;; Enable syntax highlighting/theme.
(require 'base16-default-dark-theme)

;; Key-chord
(require 'key-chord)
(key-chord-mode 1)

;; Configure Helm
(require 'helm-config)
(helm-mode 1)
(setq helm-idle-delay 0.1)
(setq helm-input-idle-delay 0.1)
(global-set-key (kbd "C-x C-f") 'helm-for-files)
(global-set-key (kbd "C-x b") 'helm-buffers-list)

(require 'helm-swoop)
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)

(require 'dash)

;; Configure company-mode
(require 'company)
(setq company-idle-delay 0.25)

(defun is-unneeded-company-backend? (backend)
  (let ((unneeded-backends '(company-bbdb company-eclim company-semantic
							 company-xcode company-ropemacs company-capf
							 company-oddmuse)))
	(if (-contains? unneeded-backends backend) nil backend)))
(setq company-backends (-filter 'is-unneeded-company-backend? company-backends))

(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-h") 'company-show-doc-buffer)
(define-key company-active-map (kbd "<tab>") 'company-complete-selection)

;; Configure yasnippet with helm-c-yasnippet.
(require 'yasnippet)
(require 'helm-c-yasnippet)
(setq yas/snippet-dirs (libdir-file "snippets")) ;; Only load my snippets.
(yas-global-mode t)
(global-set-key (kbd "C-c y") 'helm-c-yas-complete)

(autoload 'js2-mode "js2-mode" "Major mode for editing javascript files" t)
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(autoload 'cmake-mode "cmake-mode" "Major mode for editing CMake files" t)

(require 'expand-region)
(global-set-key (kbd "C-h") 'er/expand-region)

(require 'ace-jump-mode)
(autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t)
(key-chord-define-global "jk" 'ace-jump-mode)

(require 'recentf)
(setq recentf-max-saved-items 50)
(recentf-mode +1)

(require 'visual-regexp)
(global-set-key (kbd "C-x C-r") 'vr/query-replace)

(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

(require 'zencoding-mode)

(require 'json-mode)

(require 'ido-vertical-mode)
(ido-vertical-mode 1)

(require 'projectile)
(require 'helm-projectile)
(projectile-global-mode)
(setq projectile-enable-caching t)
(global-set-key (kbd "C-c h") 'helm-projectile)

(require 'tagedit)
(tagedit-add-paredit-like-keybindings)
(tagedit-add-experimental-features)

(require 'powerline)
(powerline-default-theme)

(require 'clojure-mode)
(define-key clojure-mode-map (kbd "C-:") nil)
(define-key clojure-mode-map (kbd "C-;") 'clojure-toggle-keyword-string)

(require 'win-switch)
(setq win-switch-idle-time (* 10 60)
	  win-switch-other-window-first nil)
(global-set-key (kbd "C-x o") 'win-switch-dispatch)

(require 'zmonitor)

;; vc-mode
(setq vc-stay-local nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configure Modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (if is-linux-p (add-hook 'prog-mode-hook 'flyspell-prog-mode)) ;; Spell check comments.
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'prog-mode-hook
		  (lambda ()
			(subword-mode) ;; Make each part of CamelCase a word.
			(whitespace-mode)
			(highlight-fixme-tokens)))

(add-hook 'text-mode-hook
		  (lambda ()
			(whitespace-mode)))

(add-hook 'css-mode-hook
		  (lambda ()
			(subword-mode)
			(whitespace-mode)
			(highlight-fixme-tokens)))

(add-hook 'sgml-mode-hook
		  (lambda ()
			(subword-mode)
			(zencoding-mode)
			(whitespace-mode)
			(tagedit-mode 1)))

(add-hook 'c-mode-common-hook
		  (lambda ()
			;; Make it easier to jump between .h/.cpp files
			(local-set-key (kbd "C-c o") 'ff-find-other-file)
			(setq tab-width 4
				  indent-tabs-mode t)))

(add-hook 'python-mode-hook
		  (lambda ()
			(setq tab-width 4
				  python-indent-offset 4
				  indent-tabs-mode t)))

(add-hook 'html-mode-hook
		  (lambda ()
			(setq tab-width 2
				  sqml-basic-offset 2
				  indent-tabs-mode nil)))

(add-hook 'js2-mode-hook
		  (lambda ()
			(setq indent-tabs-mode nil
				  js2-basic-offset 2)))

(add-hook 'js-mode-hook
		  (lambda ()
			(setq indent-tab-mode nil
				  js-indent-level 2
				  tab-width 2)))

(add-hook 'coffee-mode-hook
		  (lambda ()
			(setq indent-tab-mode nil
				  coffee-tab-width 2
				  tab-width 2)))

(add-hook 'clojure-mode-hook
		  (lambda ()
			(paredit-mode 1)))

(add-hook 'emacs-lisp-mode-hook
		  (lambda ()
			(paredit-mode 1)))

(add-hook 'zencoding-mode-hook
		  (lambda ()
			(setq zencoding-indentation 2)))

(add-hook 'less-css-mode-hook
		  (lambda ()
			(setq less-css-indent-level 2
				  css-indent-offset 2
				  indent-tabs-mode nil
				  tab-width 2)))

(add-hook 'diff-mode-hook
		  (lambda ()
			(whitespace-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configure file type/mode associations.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))

(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))

(add-to-list 'auto-mode-alist '("\\.pro\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.pri\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.conf\\'" . shell-script-mode))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

(add-to-list 'auto-mode-alist '("\\.wsgi\\'" . python-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start Emacs Server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(server-start)

(provide 'init)
;;; init.el ends here
