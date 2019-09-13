;; dired-x
(require 'dired-x)
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..+$"))
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))

;; ido
(require 'ido)
(ido-mode t)
