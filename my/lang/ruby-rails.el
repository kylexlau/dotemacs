;; ruby mode
(if (functionp 'ruby-electric-mode)
    (add-hook 'ruby-mode-hook 'ruby-electric-mode))

(if (functionp 'textmate-mode)
    (add-hook 'ruby-mode-hook 'textmate-mode))

(add-hook 'ruby-mode-hook
	  '(lambda ()
	     (setq tab-width 2)
	     (setq outline-regexp " *\\(def \\|class\\|module\\)")
	     (define-key ruby-mode-map "\t" 'org-cycle)
	     (outline-minor-mode 1)
	     ))
