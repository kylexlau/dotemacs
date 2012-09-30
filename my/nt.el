;(add-to-list 'load-path (concat emacs-el-dir "/elisp"))

(set-frame-font "Consolas-10.5")

(setq default-frame-alist
      '((width . 80)
	(height . 25)
	(font . "Consolas-10.5")))

;; fix ~/.emacs.d/server is unsafe on w32
(when (and (= emacs-major-version 23) (equal window-system 'w32))
  (defun server-ensure-safe-dir (dir) "Noop" t))

;;; fullscreen for w32
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

