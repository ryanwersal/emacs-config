;;; init --- My Emacs initialization file
;;; Commentary:
;;; Nothing too out of the ordinary here.
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar is-windows-p (eq system-type 'windows-nt))
(defvar is-linux-p (eq system-type 'gnu/linux))

(defvar office-email-address-p "ryan.wersal@zuerchertech.com")
(defvar home-email-address-p "ryan@ryanwersal.com")

(defvar my-email-address-p
  (cond (is-windows-p office-email-address-p)
		(t home-email-address-p)))

(defvar default-font-name-p
  (cond (is-windows-p "PragmataPro-8")
		(is-linux-p "PragmataPro-8")
		(t "PragmataPro-10")))

(defun libdir-file (file)
  "Create a valid path to FILE that is located in the Emacs lib folder (~/.emacs.d)."
  (concat (expand-file-name "~/.emacs.d") "/" file))

;; Close Emacs with a confirmation to prevent accidental closings.
(defun confirm-exit ()
  "Prompt before exitting."
  (interactive)
  (if (yes-or-no-p "Do you want to exit? ")
	  (save-buffers-kill-emacs)))
(global-set-key (kbd "C-x C-c") 'confirm-exit)

;; Confirm before we minimize/suspend Emacs.
(defun confirm-suspend ()
  "Prompt before suspending."
  (interactive)
  (if (yes-or-no-p "Do you want to suspend? ")
	  (suspend-emacs)))
(global-set-key (kbd "C-z") 'confirm-suspend)

;; Start correct shell per platform.
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-frame-font default-font-name-p)

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
(setq user-mail-address my-email-address-p)

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

;; Always show line numbers.
(global-linum-mode t)

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

;; Configure Helm/Anything.el just for find-files and buffer selection for now.
(require 'helm-config)
(setq helm-idle-delay 0.1)
(setq helm-input-idle-delay 0.1)
(global-set-key (kbd "C-x C-f") 'helm-for-files)
(global-set-key (kbd "C-x b") 'helm-buffers-list)

;; Configure yasnippet with helm-c-yasnippet.
(require 'yasnippet)
(require 'helm-c-yasnippet)
(yas-global-mode t)
(yas/load-directory (libdir-file "elpa/yasnippet-0.8.0/snippets"))
(yas/load-directory (libdir-file "snippets"))
(global-set-key (kbd "C-c y") 'helm-c-yas-complete)

(require 'flycheck)
(add-hook 'prog-mode-hook 'flycheck-mode)
(add-hook 'text-mode-hook 'flycheck-mode)

(autoload 'js2-mode "js2-mode" "Major mode for editing javascript files" t)

(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)

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
;; Configure file type/mode associations.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))

(add-to-list 'auto-mode-alist '("\\.pro\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.pri\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.conf\\'" . shell-script-mode))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Additional Keybinds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-c C-w") 'whitespace-mode)
(global-set-key (kbd "C-c C-c") 'whitespace-cleanup-region)
(global-set-key (kbd "RET") 'newline-and-indent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start Emacs Server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(server-start)

(provide 'init)
;;; init.el ends here