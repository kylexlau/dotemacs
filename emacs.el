(add-to-list 'load-path "~/prj/emacs/elisp")

(progn (cd "~/prj/emacs/elisp")
       (normal-top-level-add-subdirs-to-load-path))

(defun load-my (lib)
  (load-file (concat "~/prj/emacs/elisp/my/" lib ".el"))
  )

(load-my "base")
(load-my "font")
(load-my "func")
(load-my "orgmode")

(when (not cygwinp)
  (load-my "python"))

(load-my "keybinding")
(load-my "tex")
