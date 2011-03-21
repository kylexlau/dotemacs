;;; install
;; pyemacs
;; git clone git://github.com/pinard/Pymacs.git

;; pyflakes
;; sudo apt-get install pyflakes

;; rope and ropemacs
;; hg clone http://bitbucket.org/agr/rope
;; hg clone http://bitbucket.org/agr/ropemacs
;; hg clone http://bitbucket.org/agr/ropemode

;; auto-complete
;; http://www.emacswiki.org/emacs/auto-complete.el

;; auto-complete-yasnippet
;; http://www.cx4a.org/pub/auto-complete-yasnippet.el

;; python-mode
;; https://launchpad.net/python-mode

;; smart-operator-mode
;; http://xwl.appspot.com/ref/smart-operator.el
;; http://www.emacswiki.org/emacs/smart-operator.el

;;; python-mode
(require 'python-mode)
(require 'smart-operator)

(autoload 'python-mode "python-mode" "Python Mode." t)

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(add-hook 'python-mode-hook
      (lambda ()
	(set-variable 'py-indent-offset 2)
	;(set-variable 'py-smart-indentation nil)
	(set-variable 'indent-tabs-mode nil)
	(define-key py-mode-map (kbd "RET") 'newline-and-indent)
	;(define-key py-mode-map [tab] 'yas/expand)
	;(setq yas/after-exit-snippet-hook 'indent-according-to-mode)
	(smart-operator-mode-on)
	))

;;; pymacs

(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)

(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t)

;;; yasnippet
(require 'yasnippet)
(yas/initialize)
(setq yas/trigger-key (kbd "C-c <kp-multiply>"))
(yas/load-directory "~/prj/emacs/elisp/snippets")
(setq yas/prompt-functions '(yas/dropdown-prompt
			     yas/completing-prompt
			     yas/ido-prompt
			     yas/x-prompt
			     yas/no-prompt))

;; Backward compatibility
;; for auto-complete-yasnippet.el and anything-c-yasnippet.el
(unless (fboundp 'yas/snippet-table)
  (defalias 'yas/snippet-table 'yas/snippet-table-get-create)
  (defalias 'yas/snippet-table-parent 'yas/snippet-table-parents))

;;; auto-completion
;;  Integrates:
;;   1) Rope
;;   2) Yasnippet
;;   all with AutoComplete.el
(require 'auto-complete)
(require 'auto-complete-config)
(require 'auto-complete-yasnippet)

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

;;; tab completion
;; Ryan's python specific tab completion
(defun ryan-python-tab ()
  ; Try the following:
  ; 1) Do a yasnippet expansion
  ; 2) Do a Rope code completion
  ; 3) Do an indent
  (interactive)
  (if (eql (ac-start) 0)
      (indent-for-tab-command)))

(defadvice ac-start (before advice-turn-on-auto-start activate)
  (set (make-local-variable 'ac-auto-start) t))
(defadvice ac-cleanup (after advice-turn-off-auto-start activate)
  (set (make-local-variable 'ac-auto-start) nil))
(define-key py-mode-map "\t" 'ryan-python-tab)

;; (require 'pycomplete)

;;; company mode
(require 'company-pysmell)
(autoload 'company-mode "company" "company mode." t)
(setq company-idle-delay t)
(setq company-idle-delay 0.3)
(setq company-echo-delay 0.3)
(setq company-minimum-prefix-length 4)
(setq company-show-numbers 1)

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
	       'python-mode
	       ))
  (add-hook hook 'company-mode))

(require 'pysmell)
(require 'company-ropemacs)

;;; syntax checking
;; Auto Syntax Error Hightlight
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
		       'flymake-create-temp-inplace))
	   (local-file (file-relative-name
			temp-file
			(file-name-directory buffer-file-name))))
      (list "pyflakes" (list local-file))))

  (when linuxp
    (add-to-list 'flymake-allowed-file-name-masks
		 '("\\.py\\'" flymake-pyflakes-init)))
  )

(add-hook 'find-file-hook 'flymake-find-file-hook)

;;; outline minor mode
(require 'outline)
(define-key outline-minor-mode-map (kbd "\C-c <tab>") 'org-cycle)
(define-key outline-minor-mode-map (kbd "\C-u <tab>") 'org-shifttab)

(dolist (hook (list
	       'emacs-lisp-mode-hook
	       'python-mode-hook
	       ))
  (add-hook hook 'outline-minor-mode))

;;; pyide.el ends here.
