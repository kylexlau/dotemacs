(set-frame-font "Consolas-10.5")

(setq default-frame-alist 
      '((width . 80)
	(height . 25)
	(font . "Consolas-10.5")))

;; fix ~/.emacs.d/server is unsafe on w32
(when (and (= emacs-major-version 23) (equal window-system 'w32))
  (defun server-ensure-safe-dir (dir) "Noop" t))
