;; ruby-mode
(autoload 'ruby-mode "ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

;; ruby-electric-mode
(require 'ruby-electric)
(add-hook 'ruby-mode-hook 'ruby-electric-mode)
