(push "/usr/share/emacs23/site-lisp/gnu-smalltalk-el" load-path)
(push '("\\.st\\'" . smalltalk-mode) auto-mode-alist)
(push "\\.star\\'" inhibit-first-line-modes-regexps)

(autoload 'smalltalk-mode "smalltalk-mode" "" t)
(autoload 'gst-mode "gst-mode" "" t)

