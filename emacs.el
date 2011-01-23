(progn (cd "~/repo/emacs/")
       (normal-top-level-add-subdirs-to-load-path))

(load "base")
(load "font")
(load "func")
(load "keybinding")
(load "orgmode")

;;; markdown
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)

(push '("\\.md" . markdown-mode) auto-mode-alist)

