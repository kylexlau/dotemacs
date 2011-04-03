(add-hook 'outline-mode-hook
	  (lambda ()
	    (define-key outline-minor-mode-map "\t" 'org-cycle)))
