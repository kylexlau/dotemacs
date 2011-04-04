(add-hook 'outline-mode-hook
	  (lambda ()
	    (define-key outline-mode-map "\t" 'org-cycle)))

(add-hook 'outline-minor-mode-hook
	  (lambda ()
	    (define-key outline-minor-mode-map "\t" 'org-cycle)))
