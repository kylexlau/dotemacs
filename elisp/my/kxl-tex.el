;;; etexshow for context
(autoload 'etexshow "etexshow.el"  "Browswer for ConTeXt commands." t)

(setq etexshow-xml-files-alist 
      '(("~/prj/emacs/elisp/etexshow/cont-en.xml" . 
 	 "/tmp/cont-en.cache")))

(setq etexshow-comment-file "~/prj/emacs/elisp/etexshow/cont-en-comments.xml")

(global-set-key [f7] 'etexshow)
(add-hook 'etexshow-mode-hook '(lambda () 
				 (local-set-key [f7] 'etexshow-quit)))

;;; auctex, not in git repo
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))
(add-to-list 'auto-mode-alist '("\\.context\\'" . context-mode))

(add-hook 'LaTeX-mode-hook
	  '(lambda() 
	     (outline-minor-mode 1)))


