;; fullscreen shortcut
(global-set-key (kbd "<C-s-268632070>") 'ns-toggle-fullscreen)

(set-fringe-style 0) ; for OS X linum-mode

(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

(set-fontset-font "fontset-default" 'han '("STHeiti"))

(require 'dired)

(define-key dired-mode-map "w"
  (function
   (lambda ()
     (interactive)
     (shell-command (concat "/usr/bin/open " (dired-get-filename)))
     )))

;; flyspell mode
;; `brew install aspell` first
(setq ispell-program-name "aspell")
(add-to-list 'exec-path "/usr/local/bin")
