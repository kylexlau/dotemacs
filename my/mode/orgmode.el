(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))

(add-hook 'org-mode-hook
	  (lambda ()
	    (auto-fill-mode 1)
	    (when linuxp
	      (flyspell-mode 1))
	    (outline-minor-mode 1)
	    (setq show-trailing-whitespace nil) ; don't need on org-mode
	    ))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cr" 'org-remember)

;; clocking
(setq org-clock-persist t)
(org-clock-persistence-insinuate)

;; remember
(setq org-default-notes-file "~/.notes")
(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)

(setq org-directory "~/Dropbox/gtd/")
(setq org-agenda-files (quote ("~/Dropbox/gtd/diary.txt" "~/Dropbox/gtd/todo.txt")))

;; template
(setq org-remember-templates
      '(
	("Diary" ?d "* %U %? :DIARY: \n"  "~/Dropbox/gtd/diary.txt")
	("Notes" ?n "* %U %^{Title} :NOTES: \n " "~/Dropbox/gtd/diary.txt")
	("TODO"  ?t "* TODO %?\nAdded %T" "~/Dropbox/gtd/todo.txt")
	))
