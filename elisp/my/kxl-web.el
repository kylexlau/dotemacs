;; php
(autoload 'php-mode "php-mode")
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))

;; CSS
(autoload 'css-mode "css-mode")
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
(setq css-indent-offset 2)

;; Javascript
(autoload 'javascript-mode "javascript")
(add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode))

