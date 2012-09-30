;; setup load path
(add-to-list 'load-path
	     (concat
	      (file-name-directory (or (buffer-file-name) load-file-name))
	      "my"))

;; loading
(load "settings")
(load "bindings")
(load "functions")
(load "platforms")
(load "auto-mode")
(load "basic-modes")
