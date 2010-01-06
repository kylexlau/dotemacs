(add-to-list 'load-path "~/prj/emacs/elisp")

(progn (cd "~/prj/emacs/elisp")
       (normal-top-level-add-subdirs-to-load-path))

(load-library "kxl-base")
(load-library "kxl-font")
(load-library "kxl-func")
(load-library "kxl-org")
(load-library "kxl-py")
(load-library "kxl-tex")
(load-library "kxl-key")


(message-box "all library loaded...")
