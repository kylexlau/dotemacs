;;; display

(defun my-setup-window-system (&rest frame)
  (when window-system
    (setq frame-title-format '(buffer-file-name "%f" ("%b")))
    (set-frame-size (selected-frame) 120 40)
    (tool-bar-mode -1)
    (tooltip-mode -1)
    (mouse-wheel-mode 1)
    )
  )

(add-hook 'after-make-frame-functions 'my-setup-window-system t)

;; frame ui
(add-to-list 'default-frame-alist '(width . 120))
(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(alpha . 90))

;; minor modes
(tool-bar-mode -1)
(tooltip-mode -1)
(mouse-wheel-mode 1)
(global-hl-line-mode 1)
(global-font-lock-mode 1)
(show-paren-mode 1)
(transient-mark-mode 1)
(auto-image-file-mode 1)
(delete-selection-mode 1)

; mode line
(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode 1)
(display-time-mode 1)

;; empty line
(setq-default indicate-empty-lines t)

;; trailing whitespace
(setq-default show-trailing-whitespace t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; variables
(setq font-lock-maximum-decoration t
      debug-on-error t
      search-highlight t
      query-replace-highlight t
      use-dialog-box nil
      visible-bell t
      echo-keystrokes 0.1
      inhibit-startup-message t
      )

;;; linum-mode
(dolist (mode-hook '(
		     c-mode-hook
		     c++-mode-hook
		     go-mode-hook
		     python-mode-hook
		     ruby-mode-hook
		     emacs-lisp-mode-hook
		     sql-mode-hook
		     ))
  (add-hook mode-hook (lambda() (linum-mode 1))))

;;; user info
(setq user-full-name "kyle lau")
(setq user-mail-address "kylexlau@gmail.com")

;;; encodings
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)

;; y-or-n
(fset 'yes-or-no-p 'y-or-n-p)
(set-variable 'confirm-kill-emacs 'yes-or-no-p)

;; make emacs use the clipboard
(setq x-select-enable-clipboard t)

;; i'm not a novice anymore.
(setq disabled-command-function nil)

;; backup files
(setq
 backup-by-copying t
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)

(setq create-lockfiles nil)

(setq emacs-temporary-file-directory
      (expand-file-name "~/.emacs.d/tmp"))
(setq backup-directory-alist
      `((".*" . ,emacs-temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-temporary-file-directory t)))
