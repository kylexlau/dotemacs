(add-to-list 'load-path "~/prj/emacs/elisp")

(progn (cd "~/prj/emacs/elisp")
       (normal-top-level-add-subdirs-to-load-path))

(load-library "base")
(load-library "font")
(load-library "func")
(load-library "org")

(when (not cygwinp)
  (load-library "python"))

(load-library "keybinding")
(load-library "tex")

(message-box "all library loaded...")
