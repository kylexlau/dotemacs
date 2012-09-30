;; dired-x
(require 'dired-x)
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..+$"))
(dired-omit-mode t)


;; ido
(require 'ido)
(ido-mode t)
