;; -*-emacs-lisp-*-

(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar is-windows-p (eq system-type 'windows-nt))
(defvar is-linux-p (eq system-type 'gnu/linux))

(defvar office-email-address "ryan.wersal@zuerchertech.com")
(defvar home-email-address "ryan@ryanwersal.com")

(defvar default-email-address
  (cond (is-windows-p office-email-address)
	(home-email-address)))

(defvar default-font-name
  (cond (is-windows-p "PragmataPro-8")
		(is-linux-p "PragmataPro-8")
		("PragmataPro-10")))

(defun libdir-file (file)
  (concat (expand-file-name "~/.emacs.d") "/" file))

;; Close Emacs with a confirmation to prevent accidental closings.
(defun confirm-exit ()
  (interactive)
  (if (yes-or-no-p "Do you want to exit? ")
      (save-buffers-kill-emacs)))
(global-set-key (kbd "C-x C-c") 'confirm-exit)

;; Confirm before we minimize/suspend Emacs.
(defun confirm-suspend ()
  (interactive)
  (if (yes-or-no-p "Do you want to suspend? ")
	  (suspend-emacs)))
(global-set-key (kbd "C-z") 'confirm-suspend)

;; Start correct shell per platform.
(defun start-shell ()
  (interactive)
  (if is-windows-p
      (shell)
    (ansi-term "/bin/bash")))
(global-set-key (kbd "<f5>") 'start-shell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-default-font default-font-name)

;; Configure tabs
(setq-default tab-width 4)
(setq-default indent-tabs-mode t)

;; Configure title bar
(setq-default 
 frame-title-format
 (list '((buffer-file-name 
	  "Emacs - %f"
	  (dired-directory
	   dired-directory
	   (revert-buffer-function " %b" ("%b - Dir: " default-directory)))))))

(setq-default icon-title-format 'frame-title-format)			   

;; Set appropriate email
(setq user-mail-address 'default-email-address)

;; Fully setup PATH
(if is-linux-p
    (progn (setenv "PATH" (concat (getenv "PATH") ":~/bin"))
	   (setq exec-path (append exec-path '("~/bin")))))

;; Typing replaces selected region.
(delete-selection-mode t)

;; Allow y/n instead of yes/no at prompts.
(fset 'yes-or-no-p 'y-or-n-p)

;; Always show line numbers.
(global-linum-mode t)

;; Configure the basic filetypes and their modes.
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))

(add-to-list 'auto-mode-alist '("\\.pro\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.pri\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.conf\\'" . shell-script-mode))

(custom-set-variables
 '(bell-volumne 0)
 '(c-basic-offset 4)
 '(c-default-style (quote ((c-mode . "bsd") (c++-mode . "bsd") (java-mode . "java") (other . "bsd"))))
 '(column-number-mode t)
 '(get-frame-for-buffer-default-instance-limit nil)
 '(gutter-buffers-tab-visible-p nil)
 '(make-backup-files nil)
 '(paren-mode (quote paren) nil (paren))
 '(show-paren-mode t)
 '(sound-load-list nil)
 '(tool-bar-mode nil)
 '(visible-bell t)
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configure ELPA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(mapcar #'(lambda (path) (add-to-list 'load-path (libdir-file path))) '("themes" "themes/base16"))

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
						 ("marmalade" . "http://marmalade-repo.org/packages/")
						 ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Maximize window and start with 50/50 vertical split.
(require 'maxframe)
(add-hook 'window-setup-hook 'maximize-frame t)
(add-hook 'window-setup-hook 'split-window-horizontally)

;; Enable syntax highlighting/theme.
(require 'tomorrow-night-bright-theme)

;; Enable autocompletion.
(require 'auto-complete-config)
(ac-config-default)
(setq ac-use-menu-map t)
(define-key ac-menu-map (kbd "C-n") 'ac-next)
(define-key ac-menu-map (kbd "C-p") 'ac-previous)
(setq ac-auto-show-menu 0.1)

;; Snippets!
(require 'yasnippet)
(yas-global-mode t)
(yas/load-directory (libdir-file "elpa/yasnippet-0.8.0/snippets"))
(yas/load-directory (libdir-file "snippets"))

;; Enable fancy window switching.
(require 'switch-window)

;; IDO goodness. Yes please!
(require 'ido)
(ido-mode t)
(setq ido-ignore-buffers '("\\` " "^\*Mess" "^\*Back" "^\*scratch" ".*Completion" "^\*Ido")
	  ido-everywhere t
	  ido-case-fold t
	  ido-enable-flex-matching t
	  ido-max-prospects 10
	  ido-confirm-unique-completion t
	  ido-auto-merge-werk-directories-length -1)

;; Port over some Sublime Text goodness that I've come to really enjoy.
;; Start with the minimap
(require 'minimap)

;; Configure Helm/Anything.el just for find-files uses for now.
(require 'helm-config)
(setq helm-idle-delay 0.1)
(setq helm-input-idle-delay 0.1)
(global-set-key (kbd "C-x C-f") 'helm-for-files)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configure Modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'c-mode-common-hook
	  (lambda ()
	    ;; Make these patterns more evident in code.
	    (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|BUG\\|NOTE\\):" 1 font-lock-warning-face t)))
	    ;; Make it easier to jump between .h/.cpp files
	    (local-set-key (kbd "C-c o") 'ff-find-other-file)))

(add-hook 'python-mode-hook
	  (lambda ()
	    (setq tab-width 4
		  py-indent-offset 4
		  python-indent 4)))

(add-hook 'clojure-mode-hook
		  (lambda ()
			(paredit-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start Emacs Server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(server-start)
