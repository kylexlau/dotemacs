(when macosp
  (progn
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier 'super)
    )))

(when macosp
  (define-key dired-mode-map "w"
    (function
     (lambda ()
       (interactive)
       (shell-command (concat "/usr/bin/open " (dired-get-filename)))
       ))))

