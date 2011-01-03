(progn (cd "d:/repo/emacs/")
       (normal-top-level-add-subdirs-to-load-path))

(load "base")
(load "font")
(load "func")
;(load "keybinding")
(load "orgmode")

;(when (not cygwinp)
;  (load-my "python"))


;(load "tex")
