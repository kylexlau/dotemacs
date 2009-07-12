;;; keybinding
(global-set-key (kbd "C-=") 'hippie-expand)

;;; built-ins
(defun k/init()
  " init my configuration. "
  ;; server
  (server-start)

  ;; load path
  (add-to-list 'load-path "~/.emacs.d/elisp")
  (progn (cd "~/.emacs.d/elisp")
	 (normal-top-level-add-subdirs-to-load-path))

  ;; variable for system type
  (defvar ntp (string= "windows-nt" (symbol-name system-type))
    "If Emacs runs on a Windows system.")

  (defvar linuxp (string= "gnu/linux" (symbol-name system-type))
    "If Emacs runs on a Linux system.")

  (defvar macosp (string= "darwin" (symbol-name system-type))
    "If Emacs runs on a Mac OS system.")
  )

(defun k/macos()
  " Mac OS related configuration. "
  (interactive)
  (when macosp
    (progn
      (setq mac-command-modifier 'meta)
      (setq mac-option-modifier 'super)
      ;; Mac Open/Execute from dired
      (require 'dired-x)
      (define-key dired-mode-map "w"
	(function
	 (lambda ()
	   (interactive)
	   (shell-command (concat "/usr/bin/open " (dired-get-filename)))))))
    ))

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
  (when ntp (set-frame-font "Courier New-12"))
  (when linuxp (set-frame-font "Bitstream Vera Sans Mono-12"))

  (when macosp
    (set-fontset-font (frame-parameter nil 'font)
		      'han '("STSong" . "unicode-bmp"))
    )

  (when linuxp
    (set-fontset-font (frame-parameter nil 'font)
		      'han '("WenQuanYi Zen Hei" . "unicode-bmp"))
    )

  (setq default-frame-alist
	'(
	  ;;(top . 0) (left . 0)
	  (width . 80) (height . 40)
	  (font . "Courier New-14")))

  ;; encoding
  (prefer-coding-system 'utf-8)

  ;; frame title
  (setq frame-title-format
      '("Emacs@%b " (buffer-file-name ("("buffer-file-name")"))))

  (column-number-mode 1)
  (setq show-trailing-whitespace t)

  ;; transparency
  (modify-frame-parameters (selected-frame)
			   '((alpha . 90)))

  ;; minor modes
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


(defun k/out()
  "outline related."
  (interactive)

  (require 'outline)

  (define-key outline-minor-mode-map (kbd "<tab>") 'org-cycle)
  (define-key outline-minor-mode-map (kbd "\C-u <tab>") 'org-shifttab)

  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)

  (add-hook 'ruby-mode-hook
	    '(lambda ()
	       (outline-minor-mode 1)
	       (setq outline-regexp " *\\(def \\|class\\|module\\)")
	       (hide-sublevels 1)))

  (add-hook 'php-mode-hook
	    '(lambda ()
	       (outline-minor-mode 1)
	       (setq outline-regexp " *\\(private funct\\|public funct\\|funct\\|class\\|#head\\)")
	       (hide-sublevels 1)))

  (add-hook 'python-mode-hook
	    '(lambda ()
	       (outline-minor-mode 1)
	       (setq outline-regexp " *\\(def \\|clas\\|#hea\\)")
	       (hide-sublevels 1)))
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
  (when macosp
    (setq org-directory "~/Dropbox/gtd/")
    (setq org-agenda-files (quote ("~/Dropbox/gtd/gtd.txt" "~/Dropbox/gtd/diary.txt")))
    )

  (setq org-default-notes-file "~/.notes")
  (setq remember-annotation-functions '(org-remember-annotation))
  (setq remember-handler-functions '(org-remember-handler))
  (add-hook 'remember-mode-hook 'org-remember-apply-template)

  ;; template
  (when macosp
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

  ;; dired
  (require 'dired-x)
  (setq dired-recursive-copies 'top)
  (setq dired-recursive-deletes 'top)
  (setq dired-dwim-target t)

  ;; dired omit
  (add-hook 'dired-mode-hook
	    (lambda ()
	      (setq dired-omit-files "^#\\|^\\..*")
	      (dired-omit-mode 1)))
  (setq dired-omit-extensions
	'(".svn/" "CVS/" ".o" "~" ".bin" ".lbin" ".so" ".a" ".ln"))
  )


;;; functions
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
;;; extensions
(defun k/colth()
  " color-theme. "
  (interactive)
  (when (k/check-file "color-theme.el")
    (require 'color-theme)
    (load "color-theme-library.el")
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
    (yas/load-directory "~/.emacs.d/snippets")
    )

;;; k/func
(defun k/func()
  (interactive)
  (k/init)
  (k/ui)
  (k/colth)
  (k/macos)
  (k/out)
  (k/org)
  (k/file)

  (k/full)
  (k/web)
  (k/yas)
)

(k/func)

;;; k.el ends here

