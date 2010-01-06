(defvar my-fullscreen-p t 
  "Check if fullscreen is on or off")

(defun my-non-fullscreen ()
  (interactive)
  (if (fboundp 'w32-send-sys-command)
      ;; WM_SYSCOMMAND restore #xf120
      (w32-send-sys-command 61728)
    (progn (set-frame-parameter nil 'width 82)
	   (set-frame-parameter nil 'fullscreen 'fullheight))))

(defun my-fullscreen ()
  (interactive)
  (if (fboundp 'w32-send-sys-command)
      ;; WM_SYSCOMMAND maximaze #xf030
      (w32-send-sys-command 61488)
    (set-frame-parameter nil 'fullscreen 'fullboth)))

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

(defun k/check-file(file)
  "check if a file is in load-path."
  (locate-file file load-path))

;; (defun alpha()
;; (let ((p (string-to-int (read-string "Foo: " nil 'my-history))))
;;   (message (int-to-string p))
;;   (modify-frame-parameters (selected-frame)
;; 			   '((alpha . 80)))
;;   )
