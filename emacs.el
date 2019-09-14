;; setup load path
(add-to-list 'load-path (concat (file-name-directory (or (buffer-file-name) load-file-name))))

;; loading
(load "my/settings")
(load "my/bindings")
(load "my/functions")
(load "my/platforms")
(load "my/auto-mode")
(load "my/basic-modes")
(load "my/pkgs")
(load "my/lang/md")
