;;; my dotemacs file
;; by Kyle x lau (www.xlau.org)

;;; Changelogs:

;; <2009-06-18 Thu 18:00>  add mac os x specific settings
;;                         add macosp variable
;; <2009-06-19 Fri 15:00>  add remember template for words
;; <2009-06-21 Sun 02:00>  add remember template for TODO and Diary
;;                         add org-agenda-files varibale setting
;; <2009-06-22 Mon 14:00>  add Chinese font setting
;; <2009-06-24 Wed 11:30>  add yasnippet
;;                         make load path setting shorter by (progn ...)
;; <2009-06-24 Wed 12:00>  add auto-complete-mode
;; <2009-06-25 Thu 12:30>  add (column-number-mode 1)
;; <2009-06-25 Thu 17:00>  add cedet and erb
;;                         add font configurations for new frame
;; <2009-06-26 Fri 13:00>  add anything
;;                         add auto-install
;;                         add more remember templates
;; <2009-06-26 Fri 14:40>  add twit.el
;; <2009-06-27 Sat 10:00>  delete tinypair and pabbrev

;;; Commentary:

;; TODO: using emacs --daemon to start emacs.

;; Syntax highlighting: font-lock-mode, global-font-lock-mode

;; Folding: outline-mode, outline-minor-mode

;; go-to-symbol(Textmate)
;; that's a quite useful feature
;; list all the symbol, goto some symbol

;; Auto completion: yasnippet, auto-complete
;; Snippets.

;; Project management: file and buffer, anything, speedbar

;; GTD
;; org-mode, remember-mode, planner-mode, muse-mode and more

;;; load path
;; server
(server-start)

(add-to-list 'load-path "~/.emacs.d/elisp")
(progn (cd "~/.emacs.d/elisp")
       (normal-top-level-add-subdirs-to-load-path))

;; cedet and ecb load-path
(add-to-list 'load-path "~/.emacs.d/cedet")
(progn (cd "~/.emacs.d/cedet")
       (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'load-path "~/.emacs.d/ecb")
(add-to-list 'load-path "~/.emacs.d/auto-install")

;; Some variables to determine Operating System
(defvar ntp (string= "windows-nt" (symbol-name system-type))
  "If Emacs runs on a Windows system.")

(defvar linuxp (string= "gnu/linux" (symbol-name system-type))
  "If Emacs runs on a Linux system.")

(defvar macosp (string= "darwin" (symbol-name system-type))
  "If Emacs runs on a Mac OS system.")

;;; Mac OS X
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
  )

;; auto save
(setq auto-save-interval 60)

;;; basic
;; backup file
(setq backup-by-copying t)
(setq backup-directory-alist
      (list (cons "." "~/.emacs.d/backup")))

;; ui option
;;font
;(set-frame-font "Monaco-14")
;(set-frame-font "Droid Sans Mono-14")
(set-frame-font "Courier New-14")
(set-fontset-font (frame-parameter nil 'font)
                  'han '("STsong" . "unicode-bmp"))

(setq default-frame-alist
      '(
	;;(top . 0) (left . 0)
	(width . 80) (height . 40)
	(font . "Courier New-14")))

(defun my-new-frame-setting ()
;  (set-frame-font "Courier New-14")
  (let ((fontset (frame-parameter nil 'font)))
    (dolist
      (charset '(han symbol cjk-misc bopomofo))
      (set-fontset-font fontset charset '("STSong" . "unicode-bmp")))))

(add-to-list 'after-make-frame-functions
             (lambda (new-frame)
               (select-frame new-frame)
               (if window-system
                 (my-new-frame-setting))))

;; encoding
(prefer-coding-system 'utf-8)
(require 'unicad)

; get rid of bars
(when window-system
  (when (not macosp) (menu-bar-mode -1))
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  )

;; scrool-bar
;(set-scroll-bar-mode 'right)
;(modify-frame-parameters nil '((scroll-bar-width . 8)))

;; title
(setq frame-title-format
      '("Emacs@%b " (buffer-file-name ("("buffer-file-name")"))))

;; display time on modeline
(display-time-mode 1)

(setq display-time-mode t)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)

;; column number
(column-number-mode 1)
(setq show-trailing-whitespace t)

;; transparency
(modify-frame-parameters (selected-frame)
			 '((alpha . 90)))

;; disabled function
(put 'narrow-to-region 'disabled nil)

;; settings
(show-paren-mode 1)
(auto-image-file-mode 1)
(transient-mark-mode t)
(setq debug-on-error t)
(setq query-replace-highlight t)
(setq search-highlight t)
(setq font-lock-maximum-decoration t)
(setq python-indent 2)

;; yes/no to y/n
(fset 'yes-or-no-p 'y-or-n-p)
(set-variable 'confirm-kill-emacs 'yes-or-no-p)

;; Global Keybindings
(global-set-key [\C-f12] 'speedbar)

;;; Folding
(require 'outline)
(define-key outline-minor-mode-map (kbd "<tab>") 'org-cycle)
(define-key outline-minor-mode-map (kbd "\C-u <tab>") 'org-shifttab)
(add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)

;;; org-mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))
(add-hook 'org-mode-hook
	  (lambda ()
;	    (setq fill-column 72) ; default 70 is OK
	    (auto-fill-mode 1)
	    (flyspell-mode 0)  ; turn on will make typing slow on MacOSX
	    (outline-minor-mode 1)
	    (setq show-trailing-whitespace nil) ; don't need on org-mode
	    ))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cr" 'org-remember)

;; org-mode-hook



;; ipsell
(when macosp
  (setq ispell-program-name "/sw/bin/ispell"))

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
	  ("Diary" ?d "* %U %^{Headline} :DIARY: \n%?"  "~/Dropbox/gtd/diary.txt")
	  ("Review" ?r "* %U Daily Review :DR: \n%[~/.daily_review.txt]\n" "~/Dropbox/gtd/diary.txt")
	  ("Book" ?b "* %U %^{Title} :READING: \n%[~/.booktemp.txt]\n" "~/Dropbox/gtd/diary.txt")
 	  ("Film" ?f "* %U %^{Title} :FILM: \n%[~/.film_temp.txt]\n" "~/Dropbox/gtd/diary.txt")
	  ("Clipboard" ?c "* %U %^{Headline} %^g\n%c\n%?"  "~/Dropbox/gtd/diary.txt")
	  ("Notes" ?n "* %U %^{Title} :NOTES \n %?" "~/Dropbox/gtd/diary.txt")
	  ("TODO" ?t "** TODO %? \nAdded @ %T" "~/Dropbox/gtd/gtd.txt" "Tasks")
	  ("Words" ?w "" "~/Dropbox/gtd/word.txt")
	  )))

;;; modes
(require 'color-theme)
(load "color-theme-library.el")
(color-theme-clarity)
;(color-theme-wheat)
;(load "color-theme-colorful-obsolescence")
;(color-theme-colorful-obsolescence)

;; utility

;; TODO: I should find a better pair utility replace
;; this ugly one.
;; (require 'tinypair)
;; (tinypair-pair-type-select)

;; auto-complete is better for this purpose
;; (require 'pabbrev)

;(require 'setnu)

;;; buffer and file
(require 'ibuffer)
(global-set-key (kbd "C-c C-b") 'ibuffer)
(require 'bs)
(global-set-key (kbd "C-x C-b") 'bs-show)
(require 'ido)
(ido-mode 'buffer)

;; desktop
(require 'desktop)
(desktop-save-mode 1)
(setq desktop-restore-eager 50)

;; recentf
(require 'recentf)
(setq recentf-mode t)

;;; anything
(require 'anything-config)
;(require 'anything-match-plugin)
(setq anything-sources
      (list anything-c-source-buffers
            anything-c-source-file-name-history
	    anything-c-source-files-in-current-dir
            anything-c-source-info-pages
            anything-c-source-man-pages
	    anything-c-source-file-cache
            anything-c-source-emacs-commands))
(global-set-key "\C-x\C-a" 'anything)

;;; dired mode
(require 'dired-x) ; OS X required it before
(setq dired-recursive-copies 'top)
(setq dired-recursive-deletes 'top)
(setq dired-dwim-target t)
(add-hook 'dired-mode-hook
          (lambda ()
            (setq dired-omit-files "^#\\|^\\..*")
            (dired-omit-mode 1)))
(setq dired-omit-extensions
      '(".svn/" "CVS/" ".o" "~" ".bin" ".lbin" ".so" ".a" ".ln"

        ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".dvi"
        ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f"
        ".sparcf" ".fasl" ".ufsl" ".fsl" ".dxl" ".pfsl" ".dfsl"
        ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky"
        ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs"
        ".pyc" ".pyo" ".idx" ".lof" ".lot" ".glo" ".blg" ".bbl"
        ".cp" ".cps" ".fn" ".fns" ".ky" ".kys" ".pg" ".pgs" ".tp"
        ".tps" ".vr" ".vrs"))

;;; auto complete
;; auto-complete
(when (require 'auto-complete nil t)
  (require 'auto-complete-yasnippet)
  (require 'auto-complete-emacs-lisp)
  (require 'auto-complete-css)
  (require 'auto-complete-python)

  (global-auto-complete-mode t)

  (define-key ac-complete-mode-map "\t" 'ac-expand)
  (define-key ac-complete-mode-map "\r" 'ac-complete)
  (define-key ac-complete-mode-map "\C-n" 'ac-next)
  (define-key ac-complete-mode-map "\C-p" 'ac-previous)

  (setq ac-auto-start 3)
  (setq ac-dwim t)

  (set-default 'ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-words-in-buffer))

  (setq ac-modes
      (append ac-modes
	      '(eshell-mode org-mode html-mode)))

;;(add-to-list 'ac-trigger-commands 'org-self-insert-command)

  (add-hook 'emacs-lisp-mode-hook
	    (lambda ()
	      (setq ac-sources
		    '(ac-source-yasnippet ac-source-abbrev ac-source-words-in-buffer ac-source-symbols))))

  (add-hook 'eshell-mode-hook
	    (lambda ()
	      (setq ac-sources
		    '(ac-source-yasnippet ac-source-abbrev ac-source-files-in-current-dir ac-source-words-in-buffer)))))

;; (add-hook 'ruby-mode-hook
;; 	    (lambda ()
;; 	      (setq ac-omni-completion-sources '(("\\.\\=" ac-source-rcodetools))))))

;; Initialize Yasnippet
;; Don't map TAB to yasnippet
;; In fact, set it to something we'll never use because
;; we'll only ever trigger it indirectly.
(require 'yasnippet)
(setq yas/trigger-key (kbd "C-c <kp-multiply>"))
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets")

;;; auto install
(require 'auto-install)

;;; web development
;; nxhtml-mode, seems it's for Windows only.
;(load "~/.emacs.d/nxhtml/autostart.el")

;; PHP Mode
(autoload 'php-mode "php-mode")
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))

;; CSS Mode
(autoload 'css-mode "css-mode")
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
(setq css-indent-offset 2)

;; Javascript Mode
(autoload 'javascript-mode "javascript")
(add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode))

;;; cedet and ecb
;(load "cedet.el")
;(global-ede-mode 1)                      ; Enable the Project management system
; (semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion
; (global-srecode-minor-mode 1)            ; Enable template insertion menu
; (require 'ecb)
;(require 'ecb-autoloads)

;;; twitter
(require 'twit)

;; my lib
(load "~/.emacs.d/mylib.el")

;;; .emacs file ends here.
