;;; .emacs --- my dot emacs file
;;; TODO
;;  using emacs daemon
;;; basic setting
(require 'cl)
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir "~/.emacs.d/elisp")
	   (default-directory my-lisp-dir))
      (setq load-path (cons my-lisp-dir load-path))
      (normal-top-level-add-subdirs-to-load-path)))

(defvar elisp-d (expand-file-name "~/emacs/elisp")
  "Elisp(single file) directory. Put in version control.")

(add-to-list 'load-path elisp-d)

(setq Info-default-directory-list
      (cons (expand-file-name "~/.info")
	    Info-default-directory-list))

(setq custom-file (concat elisp-d "/cus.el"))

(server-start)
;;; emacs daemon
(defun k-emd()
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (setq x-select-enable-clipboard t)
)

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (when window-system
		(k-emd)))))

;;; color theme
(require 'color-theme)
(color-theme-initialize)
(color-theme-clarity)
;;; ui
(setq inhibit-startup-message t)

;; get rid of bars
(when window-system
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

;; scrool-bar
;;(set-scroll-bar-mode 'right)
;;(modify-frame-parameters nil '((scroll-bar-width . 8)))

(setq frame-title-format '("Emacs@%b " (buffer-file-name ("("buffer-file-name")"))))

;; mouse action
(mouse-avoidance-mode 'jump)
(global-font-lock-mode t)
(show-paren-mode t)
(setq show-paren-style 'parenthesis)
(setq show-paren-delay 0.1)
(blink-cursor-mode -1)
(setq cursor-in-echo-area nil)
(setq line-number-mode t)   ; Put line number in display
(setq column-number-mode t) ; Put column number in display

;;(display-time)
(setq display-time-mode t)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time-mode 1)
(setq-default show-trailing-whitespace t)

(modify-frame-parameters (selected-frame)
			 '((alpha . 90)))

;;; font
;;(set-default-font "Bitstream Vera Sans Mono-10")
;;(set-default-font "Consolas-11")
;;(set-default-font "Monaco-11")
;;(set-default-font "Inconsolata")
(set-default-font "Courier-11")
(add-to-list 'default-frame-alist '(font . "Courier-11"))

;;; user pref

(fset 'yes-or-no-p 'y-or-n-p)
(auto-save-mode 1)
(setq auto-save-interval 30)
(setq make-backup-files nil)
(setq auto-image-file-mode t)
(setq visible-bell t)
(setq x-select-enable-clipboard t)
(setq shell-command-completion-mode t)
(setq require-final-newlipne t)
(setq scroll-step 1
      scroll-margin 3)
(setq kill-ring-max 300)
;;(setq kill-whole-line t)
;;(setq next-line-add-newlines nil)
(setq default-major-mode 'text-mode)
(setq enable-recursive-minibuffers t)

;;; backup/autosave
(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))

;;; enable those functions
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'capitalize-region 'disabled nil)

(put 'set-goal-column 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'eval-expression 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;;; keybinding
;(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-xm" 'set-mark-command)
(global-set-key	"\C-w"  'backward-kill-word)
(global-set-key	"\C-x\C-k" 'kill-region)
(global-set-key	"\C-c\C-q" 'fill-paragraph)
(global-set-key	"\M-p" 'beginning-of-buffer)
(global-set-key	"\M-n" 'end-of-buffer)
(global-set-key	[home] 'beginning-of-buffer)
(global-set-key	[end] 'end-of-buffer)
;(global-set-key "\C-cq" 'k-unfill-paragraph)
(global-set-key	"\C-c;" 'k-comment-dwim)
(global-set-key	[f11] 'k-fullscreen)

;;; buffers
(require 'ibuffer)
(global-set-key (kbd "C-c C-b") 'ibuffer)

(require 'bs)
(global-set-key (kbd "C-x C-b") 'bs-show)

(require 'ido)
(ido-mode 'buffer)

(load "desktop")
(desktop-save-mode t)
(setq desktop-restore-eager 30)

;;; dired mode
(require 'dired-x)
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
;;; org mode
(require 'org)
(setq default-major-mode 'org-mode)
(setq auto-mode-alist (cons '("\\.txt$" . org-mode) auto-mode-alist))

;; org mode's hooks
(add-hook 'org-mode-hook
	  (lambda ()
;	    (setq fill-column 72) ; default is 70
	    (auto-fill-mode 1)
	    (flyspell-mode 1)
	    (outline-minor-mode 1)
	    ))
;; keybinding for outline mode
(define-key org-mode-map (kbd "C-c t") 'org-todo)
(define-key outline-minor-mode-map (kbd "<tab>") 'org-cycle)
(define-key outline-minor-mode-map (kbd "<backtab>") 'org-shifttab)


;;; rst mode
(require 'rst)

(add-hook 'rst-mode-hook
	  (lambda ()
	    (setq tab-width 3)	  ; default is 8.
	    (setq indent-tabs-mode nil)	; default is t.
	    (setq fill-column 80) ; default is 70, python recommeded 80.
	    (auto-fill-mode 1)
	    (flyspell-mode 1)
	    ))

(setq auto-mode-alist
      (append '(("\\.rst$" . rst-mode)
                ("\\.rest$" . rst-mode)) auto-mode-alist))

;;; c mode
(defun my-c-mode()
  (define-key c-mode-map [return] 'newline-and-indent)
  (define-key c-mode-map (kbd "C-c C-r") 'compile)
  (define-key c-mode-map (kbd "C-x M-g") 'gdb)
  (interactive)
;  (c-turn-on-eldoc-mode)
  (c-set-style "K&R")
  (setq c-basic-offset 2)
  (c-toggle-auto-newline 1)
  (c-toggle-hungry-state 1)
  (which-function-mode 1)
  (set (make-local-variable 'compile-command)
       (concat (concat "gcc -Wall -g -o " (concat (file-name-sans-extension buffer-file-name)) " ") buffer-file-name))
  )

(add-hook 'c-mode-hook 'my-c-mode)

(load "google-c-style.el")
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;;; slime mode
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(require 'slime)
(slime-setup)
;;; stumwm mode
(load "stumpwm-mode.el")
;;; sawfish mode
(autoload 'sawfish-mode "sawfish" "sawfish-mode" t)
(setq auto-mode-alist (cons '("\\.sawfishrc$"  . sawfish-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.jl$"         . sawfish-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.sawfish/rc$" . sawfish-mode) auto-mode-alist))

;;; sdcv mode
(load "sdcv-mode.el")
(global-set-key (kbd "C-c d") 'sdcv-search)
;;; elisp lib
(defvar switch-major-mode-history nil)
(defun switch-major-mode (mode)
  "Switch to a major mode."
  (interactive
   (list
    (intern
     (completing-read "Switch to mode: "
                      obarray (lambda (s)
                                (and (fboundp s)
                                     (string-match "-mode$" (symbol-name s))))
                      t nil 'switch-major-mode-history)))
   )
  (setq switch-major-mode-history
        (cons (symbol-name major-mode) switch-major-mode-history))
  (funcall mode))

;; fill-buffer
(defun fill-buffer ()
  "Fill current buffer (see `fill-region' for details)."
  (interactive)
  (fill-region (point-min) (point-max))
  )

;; anti M-q unfill.
(defun unfill-paragraph ()
  "Unfill a paragraph."
  (interactive)
  (let ((fillcol fill-column))
    (setq fill-column 99999)
    (call-interactively 'fill-paragraph)
    (setq fill-column fillcol)))

;; comment-dwim, do what i mean!
(defun k-comment-dwim (&optional n)
  "Comment do-what-i-mean."
  (interactive "p")
  (apply 'comment-or-uncomment-region
         (if (and mark-active transient-mark-mode)
             (list (region-beginning) (region-end))
           (if (> n 0)
               (list (line-beginning-position) (line-end-position n))
             (list (line-beginning-position n) (line-end-position))))))

;; from steve yegge
;; Never understood why Emacs doesn't have this function.
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
	(filename (buffer-file-name)))
    (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
	  (message "A buffer named '%s' already exists!" new-name)
	(progn
	  (rename-file name new-name 1)
	  (rename-buffer new-name)
	  (set-visited-file-name new-name)
	  (set-buffer-modified-p nil)))))
  )

;; Never understood why Emacs doesn't have this function, either.
(defun move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
	 (filename (buffer-file-name))
	 (dir
	  (if (string-match dir "\\(?:/\\|\\\\)$")
	      (substring dir 0 -1) dir))
	 (newname (concat dir "/" name)))
    (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
      (progn
 	(copy-file filename newname 1)
 	(delete-file filename)
 	(set-visited-file-name newname)
 	(set-buffer-modified-p nil) t)))
  )

(defun k-numerotate-line ()
  "Ddd line number to file content."
    (interactive)
    (let ((P (point))
  	(max (count-lines (point-min)(point-max)))
  	(line 1))
      (goto-char (point-min))
      (while (< line max)
        (insert (format "%04d " line))
        (beginning-of-line 2)
        (setq line (+ line 1)))
      (goto-char P)))

;; interactive with browser, dict
(defun k-net-dict ()
"Look up the word under cursor in a browser."
 (interactive)
 (browse-url
  (concat "http://dict.yodao.com/search?q="
          (thing-at-point 'word)))
)

(defun k-net-stardict ()
  "Look up the word under cursor in a browser."
  (interactive)
  (browse-url
   (concat "http://stardict.cn/query.php?q="
	   (thing-at-point 'word)))
  )


(defun k-insert-time ()
  "Insert time, for diary writing."
  (interactive)
  (insert (format-time-string "%Y-%m-%d-%R")))

(defun k-goto-minibuffer ()
  "Move cursor to minibuffer window"
  (interactive)
  (select-window (minibuffer-window)))

(defun k-fullscreen ()
  "Full screen frame."
  (interactive)
  (x-send-client-message
   nil 0 nil "_NET_WM_STATE" 32
   '(2 "_NET_WM_STATE_FULLSCREEN" 0))
)

(defun k-maximized ()
  "Maximize Emacs."
  (interactive)
  (x-send-client-message
   nil 0 nil "_NET_WM_STATE" 32
   '(1 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
  (x-send-client-message
   nil 0 nil "_NET_WM_STATE" 32
   '(1 "_NET_WM_STATE_MAXIMIZED_VERT" 0)))

;;; Local Variables:
;;; mode:outline-minor
;;; End:
