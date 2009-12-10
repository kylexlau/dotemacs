;;; python-mode
(require 'python-mode)
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;;; Initialize Pymacs
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)

;; Initialize Rope
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t)

;;; Initialize Yasnippet                                                        
; Don't map TAB to yasnippet. In fact, set it to something we'll never
; use because. we'll only ever trigger it indirectly.  
(require 'yasnippet)
(setq yas/trigger-key (kbd "C-c <kp-multiply>"))
(yas/initialize)
(yas/load-directory "~/prj/emacs/elisp/snippets")

;;; Auto-completion
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)

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

;;; Ryan's python specific tab completion                                                                        
(defun ryan-python-tab ()
; Try the following: 1) Do a yasnippet expansion 2) Do a Rope code
; completion 3) Do an indent (interactive)
  (if (eql (ac-start) 0)
      (indent-for-tab-command)))

(defadvice ac-start (before advice-turn-on-auto-start activate)
  (set (make-local-variable 'ac-auto-start) t))

(defadvice ac-cleanup (after advice-turn-off-auto-start activate)
  (set (make-local-variable 'ac-auto-start) nil))

(define-key python-mode-map "\t" 'ryan-python-tab)

;;; company mode
(require 'company-pysmell)
(autoload 'company-mode "company" "company mode." t)
(setq company-idle-delay t)
(setq company-idle-delay 0.005)
(setq company-minimum-prefix-length 1)
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
	       'python-mode-hook
	       ))
  (add-hook hook 'company-mode))

;;; outline minor mode
(require 'outline)
(define-key outline-minor-mode-map (kbd "\C-c <tab>") 'org-cycle)
(define-key outline-minor-mode-map (kbd "\C-u <tab>") 'org-shifttab)

(dolist (hook (list
	       'emacs-lisp-mode-hook
	       'python-mode-hook
	       ))
  (add-hook hook 'outline-minor-mode))
