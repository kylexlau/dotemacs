;; Load Dired X when Dired is loaded.
(add-hook 'dired-load-hook '(lambda () (require 'dired-x)))

;; Enable toggling of uninteresting files.
(setq dired-omit-files-p t)

(setq dired-omit-files
          (concat dired-omit-files "\\|^\\..+$"))

