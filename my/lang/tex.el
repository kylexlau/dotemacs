;; auctex
(add-to-list 'auto-mode-alist '("\\.tex\\'" . ConTeXt-mode))

(add-hook 'ConTeXt-mode-hook
	  '(lambda()
	     (outline-minor-mode t)
	     (flyspell-mode t)
	     (auto-fill-mode t)
	     ))

(add-hook 'plain-tex-mode-hook
	  '(lambda()
	     (outline-minor-mode 1)
	     (auto-fill-mode 1)
	     (flyspell-mode 1)
	     ))
