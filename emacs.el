(progn (cd "~/repo/emacs/")
       (normal-top-level-add-subdirs-to-load-path))

;; my settings
(load "base")
(load "font")
(load "func")
(load "keybinding")
(load "orgmode")
(load "md")
(load "rails-ide")

