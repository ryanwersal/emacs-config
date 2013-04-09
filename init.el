;;; init --- My Emacs initialization file
;;; Commentary:
;;; Nothing too out of the ordinary here.
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar is-windows-p (eq system-type 'windows-nt))
(defvar is-linux-p (eq system-type 'gnu/linux))

(defvar office-email-address "ryan.wersal@zuerchertech.com")
(defvar home-email-address "ryan@ryanwersal.com")

(defvar my-email-address
  (cond (is-windows-p office-email-address)
		(t home-email-address)))

(defvar default-font-name
  (cond (is-windows-p "PragmataPro-8")
		(is-linux-p "PragmataPro-8")
		(t "PragmataPro-10")))

(defun libdir-file (file)
  "Create a valid path to FILE that is located in the Emacs lib folder (~/.emacs.d)."
  (concat (expand-file-name "~/.emacs.d") "/" file))

(defun confirm-exit ()
  "Prompt before exitting."
  (interactive)
  (if (yes-or-no-p "Do you want to exit? ")
	  (save-buffers-kill-emacs)))
(global-set-key (kbd "C-x C-c") 'confirm-exit)

(defun confirm-suspend ()
  "Prompt before suspending."
  (interactive)
  (if (yes-or-no-p "Do you want to suspend? ")
	  (suspend-emacs)))
(global-set-key (kbd "C-z") 'confirm-suspend)

(defun start-msys-shell ()
  "Attempt to create a good shell on Windows."
  (interactive)
  (setq shell-file-name "bash.exe")
  (defvar explicit-shell-file-name shell-file-name)
  (defvar explicit-bash.exe-args '("--login" "-i"))
  (defvar w32-quote-process-args t)
  (setenv "SHELL" explicit-shell-file-name)
  (setq shell-command-switch "-c")
  (shell))

(defun start-shell ()
  "Start the proper shell based on platform."
  (interactive)
  (if is-windows-p
	  (start-msys-shell)
	(ansi-term "/bin/bash")))
(global-set-key (kbd "<f5>") 'start-shell)

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
  "Essentially like 'newline-and-indent' but can be done in the middle of another line."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))
(global-set-key (kbd "<S-return>") 'open-line-and-indent)

(global-set-key (kbd "C-S-k") 'scroll-down-line)
(global-set-key (kbd "C-S-j") 'scroll-up-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'default-frame-alist `(font . ,default-font-name))

;; Configure tabs
(setq-default tab-width 4)
(setq-default indent-tabs-mode t)

;; Configure title bar
(setq-default frame-title-format
			  '((:eval (if (buffer-file-name)
						   (abbreviate-file-name (buffer-file-name))
						 "%b"))))
(setq-default icon-title-format 'frame-title-format)

;; Set appropriate email
(setq user-mail-address my-email-address)

;; Fully setup PATH
(defun append-to-path (dir)
  "Add DIR to path and 'exec-path'."
  (setenv "PATH" (concat (getenv "PATH") ":" dir))
  (setq exec-path (append exec-path 'dir)))
(if is-linux-p (dolist (dir '("~/bin")) (append-to-path dir)))

;; Typing replaces selected region.
(delete-selection-mode t)

;; Allow y/n instead of yes/no at prompts.
(fset 'yes-or-no-p 'y-or-n-p)

;; Always show line & column numbers.
(global-linum-mode t)
(setq line-number-mode t
	  column-number-mode t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bell-volumne 0)
 '(c-basic-offset 4)
 '(c-default-style (quote ((c-mode . "bsd") (c++-mode . "bsd") (java-mode . "java") (other . "bsd"))))
 '(column-number-mode t)
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
 '(visible-bell t)
 '(flycheck-flake8rc (libdir-file "configs/.flake8rc")) ; flycheck: Set flake8 config file.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(global-auto-revert-mode 1)

(electric-indent-mode t)
(electric-pair-mode t)

(set-default 'sentence-end-double-space nil) ;; Don't require double spaces after periods anywhere.

;; Make help more helpful (and less intrusive).
(global-set-key (kbd "<f1>") 'help-command)
(global-set-key (kbd "<f1> a") 'apropos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup load-path
(dolist (path '("themes" "themes/base16" "packages" "packages/minimap")) (add-to-list 'load-path (libdir-file path)))

;; Configure ELPA
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
						 ("marmalade" . "http://marmalade-repo.org/packages/")
						 ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

;; Maximize window and start with 50/50 vertical split.
(require 'maxframe)
(add-hook 'window-setup-hook 'maximize-frame t)
(add-hook 'window-setup-hook 'split-window-horizontally)

;; Enable syntax highlighting/theme.
(require 'base16-dark-theme)

;; Enable autocompletion.
(require 'auto-complete-config)
(ac-config-default)
(setq ac-use-menu-map t)
(define-key ac-menu-map (kbd "C-n") 'ac-next)
(define-key ac-menu-map (kbd "C-p") 'ac-previous)
(setq ac-auto-show-menu 0.1)

;; Enable fancy window switching.
(require 'switch-window)

;; ST2 like Minimap
(require 'minimap)

;; Configure Helm/Anything.el just for find-files and buffer selection for now.
(require 'helm-config)
(helm-mode 1)
(setq helm-idle-delay 0.1)
(setq helm-input-idle-delay 0.1)
(global-set-key (kbd "C-x C-f") 'helm-for-files)
(global-set-key (kbd "C-x b") 'helm-buffers-list)

;; Configure yasnippet with helm-c-yasnippet.
(require 'yasnippet)
(require 'helm-c-yasnippet)
(setq yas/snippet-dirs (libdir-file "snippets")) ;; Only load my snippets.
(yas-global-mode t)
(global-set-key (kbd "C-c y") 'helm-c-yas-complete)

(require 'flycheck)
(add-hook 'prog-mode-hook 'flycheck-mode)
(add-hook 'text-mode-hook 'flycheck-mode)

(autoload 'js2-mode "js2-mode" "Major mode for editing javascript files" t)

(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)

(require 'expand-region)
(global-set-key (kbd "C-h") 'er/expand-region)

(require 'highlight-symbol)
(global-set-key (kbd "<C-f3>") 'highlight-symbol-at-point)
(global-set-key (kbd "<M-f3>") 'highlight-symbol-remove-all)
(global-set-key (kbd "<f3>") 'highlight-symbol-next)
(global-set-key (kbd "<S-f3>") 'highlight-symbol-prev)
(global-set-key (kbd "<C-M-f3>") 'highlight-symbol-query-replace)

(require 'ace-jump-mode)
(autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

(require 'recentf)
(setq recentf-max-saved-items 200)
(recentf-mode +1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configure Modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'prog-mode-hook 'flyspell-prog-mode) ;; Spell check comments.

(add-hook 'c-mode-common-hook
		  (lambda ()
			;; Highlight certain tokens in comments.
			(font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|BUG\\|NOTE\\):" 1 font-lock-warning-face t)))
			;; Make it easier to jump between .h/.cpp files
			(local-set-key (kbd "C-c o") 'ff-find-other-file)))

(add-hook 'python-mode-hook
		  (lambda ()
			;; Highlight certain tokens in comments.
			(font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|BUG\\|NOTE\\):" 1 font-lock-warning-face t)))
			;; Fully configure tab settings
			(setq tab-width 4
				  py-indent-offset 4
				  python-indent 4)))

(add-hook 'clojure-mode-hook
		  (lambda ()
			(paredit-mode 1)))

(add-hook 'sql-mode-hook
		  (lambda ()
			(auto-complete-mode)))

(add-hook 'markdown-mode-hook
		  (lambda ()
			(auto-complete-mode)
			(flyspell-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configure file type/mode associations.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))

(add-to-list 'auto-mode-alist '("\\.pro\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.pri\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.conf\\'" . shell-script-mode))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start Emacs Server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'server)
(unless (server-running-p)
  (server-start))

(provide 'init)
;;; init.el ends here
