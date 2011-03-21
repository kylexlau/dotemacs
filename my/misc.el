;; user info
(setq user-full-name "kyle x lau")
(setq user-mail-address "kylexlau@gmail.com")

;; encoding
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)


(add-hook 'message-mode-hook
	  (lambda ()
            (setq fill-column 72)
            (turn-on-auto-fill)))

;; fset and set-variable
;(defalias 'yes-or-no-p 'y-or-n-p)
(fset 'yes-or-no-p 'y-or-n-p)
(set-variable 'confirm-kill-emacs 'yes-or-no-p)

;; delete trailing whitespaces before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; make emacs use the clipboard
(setq x-select-enable-clipboard t)

;; i'm not a novice anymore.
(setq disabled-command-function nil)

