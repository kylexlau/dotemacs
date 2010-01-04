;; my variable
(defvar ntp (string= "windows-nt" (symbol-name system-type))
  "If Emacs runs on a Windows system.")

(defvar linuxp (string= "gnu/linux" (symbol-name system-type))
  "If Emacs runs on a Linux system.")

(defvar macosp (string= "darwin" (symbol-name system-type))
  "If Emacs runs on a Mac OS system.")

(defvar cygwinp (string= "cygwin" (symbol-name system-type))
  "If Emacs runs on a Cygwin platform.")

;; fix ~/.emacs.d/server is unsafe on w32
(when (and (= emacs-major-version 23) (equal window-system 'w32))
  (defun server-ensure-safe-dir (dir) "Noop" t))

;; encoding
(when (not ntp)
  (prefer-coding-system 'utf-8)
  (set-language-environment 'utf-8))

;; i'm not a novice anymore.
(setq disabled-command-function nil)

;; get rid of bars
(when window-system
  (when (not macosp) (menu-bar-mode -1))
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  )

;; frame title
(setq frame-title-format
      '("Emacs@%b " (buffer-file-name ("("buffer-file-name")"))))

;; transparency 90%
(modify-frame-parameters (selected-frame)
			 '((alpha . 90)))

(column-number-mode 1)
(size-indication-mode 1)
(show-paren-mode 1)
(transient-mark-mode 1)
(auto-image-file-mode 1)
(global-font-lock-mode 1)

(setq font-lock-maximum-decoration t)
(setq show-trailing-whitespace t)
(setq query-replace-highlight t)
(setq search-highlight t)
(setq debug-on-error t)
(setq backup-inhibited t)
(setq auto-save-default t)

;; fset and set-variable
(fset 'yes-or-no-p 'y-or-n-p)
(set-variable 'confirm-kill-emacs 'yes-or-no-p)

;; extensions
(require 'color-theme)
(color-theme-clarity)

;; ido
(require 'ido)
(ido-mode 1)

;; desktop
(require 'desktop)
(desktop-save-mode 1)
(setq desktop-restore-eager 50)

;; dired
(require 'dired-x)

(dired-omit-mode 1)
(setq dired-omit-files "^#\\|^\\..*")
(setq dired-omit-extensions
      '(".svn/" "CVS/" ".o" "~" ".bin" ".lbin"
	".so" ".a" ".ln" ".log" ".aux" ".toc" ".out"))

(setq dired-guess-shell-gnutar "gtar")
(setq dired-recursive-copies 'top)
(setq dired-recursive-deletes 'top)
(setq dired-dwim-target t)
