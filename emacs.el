;;; keybinding
(global-set-key (kbd "C-c o") 'speedbar-select-attached-frame)
(global-set-key (kbd "C-=") 'hippie-expand)

;;; my variables
(defvar ntp (string= "windows-nt" (symbol-name system-type))
  "If Emacs runs on a Windows system.")

(defvar linuxp (string= "gnu/linux" (symbol-name system-type))
  "If Emacs runs on a Linux system.")

(defvar macosp (string= "darwin" (symbol-name system-type))
  "If Emacs runs on a Mac OS system.")

;;; my functions
(defun k/full()
  " full screen function for window."
  (interactive)

  (when ntp
    (defvar my-fullscreen-p t "Check if fullscreen is on or off")
    (defun my-non-fullscreen ()
      (interactive)
      (if (fboundp 'w32-send-sys-command)
	  ;; WM_SYSCOMMAND restore #xf120
	  (w32-send-sys-command 61728)
	(progn (set-frame-parameter nil 'width 82)
	       (set-frame-parameter nil 'fullscreen 'fullheight))))

    (defun my-fullscreen ()
      (interactive)
      (if (fboundp 'w32-send-sys-command)
	  ;; WM_SYSCOMMAND maximaze #xf030
	  (w32-send-sys-command 61488)
	(set-frame-parameter nil 'fullscreen 'fullboth)))

    (defun my-toggle-fullscreen ()
      (interactive)
      (setq my-fullscreen-p (not my-fullscreen-p))
      (if my-fullscreen-p
	  (my-non-fullscreen)
	(my-fullscreen)))
    )

  (when linuxp
    (defun my-toggle-fullscreen ()
      "Full screen frame."
      (interactive)
      (x-send-client-message
       nil 0 nil "_NET_WM_STATE" 32
       '(2 "_NET_WM_STATE_FULLSCREEN" 0))
      ))

  (global-set-key [f11] 'my-toggle-fullscreen)
  )

(defun k/check-file(file)
  "check if a file is in load-path."
  (locate-file file load-path))
;;; built-ins
(defun k/init()
  " init my configuration. "
  (interactive)
  ;; server

  ;; fix ~/.emacs.d/server is unsafe on w32
  (when (and (= emacs-major-version 23) (equal window-system 'w32))
    (defun server-ensure-safe-dir (dir) "Noop" t))

  ;; (server-start)

  ;; load path
  (add-to-list 'load-path "~/.emacs.d/elisp")
  (progn (cd "~/.emacs.d/elisp")
	 (normal-top-level-add-subdirs-to-load-path))

  (add-to-list 'load-path "~/prj/emacs/elisp")
  (progn (cd "~/prj/emacs/elisp")
	 (normal-top-level-add-subdirs-to-load-path))

  ;; encoding
  (when (not ntp)
    (prefer-coding-system 'utf-8)
    (set-language-environment 'utf-8))

  ;; i'm not a novice anymore.
  (setq disabled-command-function nil)
  )

(defun k/macos()
  " Mac OS related configuration. "
  (interactive)
  (when macosp
    (progn
      (setq mac-command-modifier 'meta)
      (setq mac-option-modifier 'super)
    )))

(defun k/dired()
  "dired mode"
  (interactive)

  ;; omit mode
  (require 'dired-x)
  (dired-omit-mode 1)
  (setq dired-omit-files "^#\\|^\\..*")  
  (setq dired-omit-extensions
	'(".svn/" "CVS/" ".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".log" ".aux" ".toc" ".out"))

  (setq dired-guess-shell-gnutar "gtar")
  (setq dired-recursive-copies 'top)
  (setq dired-recursive-deletes 'top)
  (setq dired-dwim-target t)

  (when macosp
    (define-key dired-mode-map "w"
      (function
       (lambda ()
	 (interactive)
	 (shell-command (concat "/usr/bin/open " (dired-get-filename)))
	 ))))
  )

(defun k/ui()
  " ui related configuration. "
  (interactive)
  ;; bars
  (when window-system
    (when (not macosp) (menu-bar-mode -1))
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    )

  ;; font
  (when macosp (set-frame-font "Courier New-14"))
  (when ntp (set-frame-font "Consolas-11"))
  (when linuxp 
    (set-frame-font "Consolas-8")

    (setq default-frame-alist
	  '(
	    (top . 0) (left . 0)
	    (width . 80) (height . 25)
	    (font . "Consolas-8")))
	) ; small font for my eeepc 1000he

  (when ntp 
    (set-frame-font "Consolas-12")
    (setq default-frame-alist 
	  '((width . 80)
	    (height . 30)
	    (font . "Consolas-12"))))

  (when macosp
    (set-fontset-font (frame-parameter nil 'font)
		      'han '("STSong" . "unicode-bmp"))
    )

  (when linuxp
    (set-fontset-font (frame-parameter nil 'font)
		      'han '("WenQuanYi Zen Hei" . "unicode-bmp"))
    )

  (when macosp
    (setq default-frame-alist
	  '(
	    ;;(top . 0) (left . 0)
	    (width . 80) (height . 40)
	    (font . "Courier New-14"))))

  ;; frame title
  (setq frame-title-format
      '("Emacs@%b " (buffer-file-name ("("buffer-file-name")"))))

  ;; transparency
  (modify-frame-parameters (selected-frame)
			   '((alpha . 90)))

  (setq show-trailing-whitespace t)

  ;; minor modes
  (column-number-mode 1)
  (size-indication-mode 1)
  (show-paren-mode 1)
  (auto-image-file-mode 1)
  (transient-mark-mode 1)

  ;; font lock
  (global-font-lock-mode 1)
  (setq font-lock-maximum-decoration t)

  ;; search
  (setq query-replace-highlight t)
  (setq search-highlight t)

  ;; debug
  (setq debug-on-error t)

  ;; yes/no to y/n
  (fset 'yes-or-no-p 'y-or-n-p)
  (set-variable 'confirm-kill-emacs 'yes-or-no-p)
  )

(defun k/org()
  "org-mode related."
  (interactive)
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
  (add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))
  (add-hook 'org-mode-hook
	    (lambda ()
	      (auto-fill-mode 1)
	      (flyspell-mode 0)  ; turn on will make typing slow on MacOSX
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

  )

(defun k/file()
  "file management."
  (interactive)
  
  ;; backup
  (setq backup-inhibited t)

  ;; auto save
  (setq auto-save-default nil)

  ;; ido
  (require 'ido)
  (ido-mode 1)

  ;; desktop
  (require 'desktop)
  (desktop-save-mode 1)
  (setq desktop-restore-eager 50)
  )

;;; extensions
(defun k/cth()
  " color-theme. "
  (interactive)
  (when (k/check-file "color-theme.el")
    (require 'color-theme)
    (color-theme-initialize)
    (color-theme-clarity))
)

(defun k/web()
  " web development. "
  (interactive)

  ;; php
  (autoload 'php-mode "php-mode")
  (add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))

  ;; CSS
  (autoload 'css-mode "css-mode")
  (add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
  (setq css-indent-offset 2)

  ;; Javascript
  (autoload 'javascript-mode "javascript")
  (add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode))

  ;; markdown
  (autoload 'markdown-mode "markdown")
  (add-to-list 'auto-mode-alist '("\\.text$" . markdown-mode))
  )

(defun k/yas()
  " yasnippet. "
  (interactive)
  (when (k/check-file "yasnippet.el")
    (require 'yasnippet)
    (yas/initialize)
    (yas/load-directory "~/prj/emacs/elisp/snippets")
    (setq yas/prompt-functions '(yas/dropdown-prompt
				 yas/completing-prompt
				 yas/ido-prompt
				 yas/x-prompt
				 yas/no-prompt))

    ))

(defun k/tex()
  "LaTeX."
  (interactive)
  (when (k/check-file "auctex.el")
    (load "auctex.el" nil t t)
    (load "preview-latex.el" nil t t)
    (add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))
    (add-hook 'LaTeX-mode-hook '(lambda() (outline-minor-mode 1)))
    )
)

(defun k/textile()
  "Textile mode."
  (interactive)
  (when (k/check-file "textile-mode.el")
    (require 'textile-mode)
    (add-to-list 'auto-mode-alist '("\\.textile\\'" . textile-mode))
    )
  )

(defun k/company()
  "Company.el."
  (interactive)
  (when (k/check-file "company.el")
    (autoload 'company-mode "company" "company mode." t)
    (setq company-idle-delay t)
    (setq company-idle-delay 0.2)
    (setq company-minimum-prefix-length 1)
;    (setq company-show-numbers nil)
    (dolist (hook (list
		   'emacs-lisp-mode-hook
		   'lisp-mode-hook
		   'lisp-interaction-mode-hook
		   'scheme-mode-hook
		   'c-mode-hook
		   'c++-mode-hook
		   'java-mode-hook
		   'haskell-mode-hook
		   'asm-mode-hook
		   'emms-tag-editor-mode-hook
		   'sh-mode-hook
		   ))
      (add-hook hook 'company-mode))
    )
  )

(defun k/perl()
  "Perl mode."
  (interactive)
  (defalias 'perl-mode 'cperl-mode)
  )

(defun k/plsql()
  "pl/sql mode."
  (interactive)
  (when (k/check-file "plsql.el")
    (autoload 'plsql-mode "plsql" "PL/SQL Mode" t)
    (setq auto-mode-alist 
	  (cons (cons "\\.sql$" 'plsql-mode) auto-mode-alist))
    (setq plsql-indent 2)
    ))

(defun k/lua()
  "lua mode."
  (interactive)
  (when (k/check-file "lua-mode.el")
    (autoload 'lua-mode "lua-mode" "Lua editing mode." t)
    (setq auto-mode-alist 
	  (cons '("\\.lua$" . lua-mode) auto-mode-alist))
    (add-hook 'lua-mode-hook 'hs-minor-mode)
    )
  )

(defun k/python()
  "python mode. "
  (interactive)
  (require 'python-mode)
  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
  (add-to-list 'interpreter-mode-alist '("python" . python-mode))

  (require 'smart-operator)
  (add-hook 'python-mode-hook
	    (lambda ()
	      (set-variable 'py-indent-offset 2)
	      (set-variable 'indent-tabs-mode nil)
	      (define-key py-mode-map (kbd "RET") 'newline-and-indent)
	      (smart-operator-mode-on)
	      ))

  ;; pymacs / rope
  (autoload 'pymacs-apply "pymacs")
  (autoload 'pymacs-call "pymacs")
  (autoload 'pymacs-eval "pymacs")
  (autoload 'pymacs-exec "pymacs")
  (autoload 'pymacs-load "pymacs")

  (pymacs-load "ropemacs" "rope-")
  (setq ropemacs-enable-autoimport t)

  ;; autocomplete.el
  (require 'auto-complete)
  (defun prefix-list-elements (list prefix)
    (let (value)
      (nreverse
       (dolist (element list value)
	 (setq value (cons (format "%s%s" prefix element) value))))))

  (defvar ac-source-rope
    '((candidates
       . (lambda ()
	   (prefix-list-elements (rope-completions) ac-target))))
    "Source for Rope")

  (defun ac-python-find ()
    "Python `ac-find-function'."
    (require 'thingatpt)
    (let ((symbol (car-safe (bounds-of-thing-at-point 'symbol))))
      (if (null symbol)
	  (if (string= "." (buffer-substring (- (point) 1) (point)))
	      (point)
	    nil)
	symbol)))

  (defun ac-python-candidate ()
    "Python `ac-candidates-function'"
    (let (candidates)
      (dolist (source ac-sources)
	(if (symbolp source)
	    (setq source (symbol-value source)))
	(let* ((ac-limit (or (cdr-safe (assq 'limit source)) ac-limit))
	       (requires (cdr-safe (assq 'requires source)))
	       cand)
	  (if (or (null requires)
		  (>= (length ac-target) requires))
	      (setq cand
		    (delq nil
			  (mapcar (lambda (candidate)
				    (propertize candidate 'source source))
				  (funcall (cdr (assq 'candidates source)))))))
	  (if (and (> ac-limit 1)
		   (> (length cand) ac-limit))
	      (setcdr (nthcdr (1- ac-limit) cand) nil))
	  (setq candidates (append candidates cand))))
      (delete-dups candidates)))

  (add-hook 'python-mode-hook
	    (lambda ()
	      (auto-complete-mode 1)
	      (set (make-local-variable 'ac-sources)
		   (append ac-sources '(ac-source-rope) '(ac-source-yasnippet)))
	      (set (make-local-variable 'ac-find-function) 'ac-python-find)
	      (set (make-local-variable 'ac-candidate-function) 'ac-python-candidate)
	      (set (make-local-variable 'ac-auto-start) nil)))

  ;; ryan' python tab completion
  (defun ryan-python-tab ()
    (interactive)
    (if (eql (ac-start) 0)
	(indent-for-tab-command)))

  (defadvice ac-start (before advice-turn-on-auto-start activate)
    (set (make-local-variable 'ac-auto-start) t))

  (defadvice ac-cleanup (after advice-turn-off-auto-start activate)
    (set (make-local-variable 'ac-auto-start) nil))

  (define-key py-mode-map "\t" 'ryan-python-tab)
  )

;;; k/func
(defun k/func()
  (interactive)
  (k/init)
  (k/ui)
  (k/cth)
  (k/macos)
  (k/dired)
  (k/file)
  (k/org)

  (k/full)
  (k/web)
  (k/yas)
  (k/tex)
  (k/textile)
  (k/perl)
  (k/plsql)
  (k/lua)
)


(k/func)

;;; emacs.el ends here
