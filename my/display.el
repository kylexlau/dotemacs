;;; color-theme
(require 'color-theme)
(color-theme-clarity)

;;; frame title
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  ;; (tooltip-mode -1)
  ;; (mouse-wheel-mode 1)
  )

;;; transparency 90%
(modify-frame-parameters (selected-frame)
			 '((alpha . 90)))

;;; minor modes
(global-font-lock-mode 1)
(global-hl-line-mode 1)
;; mode line
(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode 1)
(display-time-mode 1)

(show-paren-mode 1)
(transient-mark-mode 1)
(auto-image-file-mode 1)
(delete-selection-mode 1)

(blink-cursor-mode 1)
(setq-default cursor-type '(bar . 10))
(setq-default indicate-empty-lines t)

;;; variables
(setq font-lock-maximum-decoration t
      debug-on-error t
      search-highlight t
      query-replace-highlight t
      show-trailing-whitespace t

      use-dialog-box nil
      visible-bell t
      echo-keystrokes 0.1
      inhibit-startup-message t
      ;; truncate-partial-width-windows nil
      )
