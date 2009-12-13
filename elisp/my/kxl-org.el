(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))

(add-hook 'org-mode-hook
	  (lambda ()
	    (auto-fill-mode 1)
	    (flyspell-mode 1)  ; turn on will make typing slow on MacOSX
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

(when (not ntp)
  (setq org-directory "~/Dropbox/gtd/")
  (setq org-agenda-files (quote ("~/Dropbox/gtd/gtd.txt" "~/Dropbox/gtd/diary.txt")))
  )

;; template
(when (not ntp)
  (setq org-remember-templates
	'(
	  ("Diary" ?d "* %U %? :DIARY: \n"  "~/Dropbox/gtd/diary.txt")
	  ("Review" ?r "* %U Daily Review :DR: \n%[~/.daily_review.txt]\n" "~/Dropbox/gtd/diary.txt")
	  ("Book" ?b "* %U %^{Title} :READING: \n%[~/.booktemp.txt]\n" "~/Dropbox/gtd/diary.txt")
	  ("Film" ?f "* %U %^{Title} :FILM: \n%[~/.film_temp.txt]\n" "~/Dropbox/gtd/diary.txt")
	  ("Clipboard" ?c "* %U %^{Headline} %^g\n%c\n%?"  "~/Dropbox/gtd/diary.txt")
	  ("Notes" ?n "* %U %^{Title} :NOTES \n %?" "~/Dropbox/gtd/diary.txt")
	  ("TODO" ?t "** TODO %? \nAdded @ %T" "~/Dropbox/gtd/gtd.txt" "Tasks")
	  )))

;; for Windows
(when ntp
  (setq org-directory "~/doc/My Dropbox/gtd/")
  (setq org-agenda-files (quote ("~/doc/My Dropbox/gtd/gtd.txt" "~/doc/My Dropbox/gtd/diary.txt")))
  )

;; template
(when ntp
  (setq org-remember-templates
	'(
	  ("Diary" ?d "* %U %? :DIARY: \n"  "~/doc/My Dropbox/gtd/diary.txt")
	  ("Notes" ?n "* %U %^{Title} :NOTES: \n " "~/doc/My Dropbox/gtd/diary.txt")
	  ("TODO" ?t "** TODO %? \nAdded @ %T" "~/doc/My Dropbox/gtd/diary.txt" "TODOs")
	  ("Programming" ?p "** TODO %? \nAdded @ %T" "~/doc/My Dropbox/gtd/programming.txt" "todolist")
	  )))
