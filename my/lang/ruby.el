;; ruby-mode
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

;; ruby-electric-mode
(add-hook 'ruby-mode-hook 'ruby-electric-mode)
