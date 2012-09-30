;(set-frame-font "Bitstream Vera Sans Mono-10.5")
;(set-frame-font "DejaVu Sans Mono-10.5")
(set-frame-font "Meslo LG S-10.5")
;(set-frame-font "Monaco-11")
;(set-frame-font "Lucida Console-10.5")
;(set-frame-font "Inconsolata-11")

(set-fontset-font (frame-parameter nil 'font)
		    'han '("Microsoft YaHei-10.5" . "unicode-bmp")
		    )

;;; fullscrenn for linux
(defun my-toggle-fullscreen ()
  (interactive)
  (setq my-fullscreen-p (not my-fullscreen-p))
  (if my-fullscreen-p
      (my-non-fullscreen)
    (my-fullscreen)))

(when linuxp
  (defun my-toggle-fullscreen ()
    "Full screen frame."
    (interactive)
    (x-send-client-message
     nil 0 nil "_NET_WM_STATE" 32
     '(2 "_NET_WM_STATE_FULLSCREEN" 0))
    ))

;;; fullscreen function that can't get back
(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
			 (if (equal 'fullboth current-value)
			     (if (boundp 'old-fullscreen) old-fullscreen nil)
			   (progn (setq old-fullscreen current-value)
				  'fullboth)))))

(global-set-key [f11] 'my-toggle-fullscreen)
