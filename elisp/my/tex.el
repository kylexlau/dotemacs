(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))

(add-hook 'LaTeX-mode-hook
	  '(lambda() 
	     (outline-minor-mode 1)))
