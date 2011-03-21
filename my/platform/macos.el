(set-frame-font "Courier New-14")

(set-fontset-font (frame-parameter nil 'font)
		  'han '("STSong" . "unicode-bmp"))

(setq default-frame-alist
      '(
	;;(top . 0) (left . 0)
	(width . 80) (height . 40)
	(font . "Courier New-14")))

(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

(define-key dired-mode-map "w"
  (function
   (lambda ()
     (interactive)
     (shell-command (concat "/usr/bin/open " (dired-get-filename)))
     )))

