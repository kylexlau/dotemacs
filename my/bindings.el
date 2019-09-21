;;; key bindings
(global-set-key (kbd "C-c C-k") 'kill-compilation) ;; Kill compilation buffer.
(global-set-key (kbd "C-c C-l") 'linum-mode)       ;; Toggle linum-mode.

(global-set-key "\C-x\C-b" 'ibuffer)
(global-set-key "\C-co" 'speedbar-select-attached-frame)

;; org-mode
;(global-set-key "\C-cl" 'org-store-link)
;(global-set-key "\C-ca" 'org-agenda)
;(global-set-key "\C-cb" 'org-iswitchb)
;(global-set-key "\C-cr" 'org-remember)

;; git
(global-set-key "\C-xg" 'magit-status)

;; completion
(global-set-key (kbd "C-=") 'hippie-expand)

;; outline mode
(autoload 'org "org")
(add-hook 'outline-mode-hook
	  (lambda ()
	    (define-key outline-mode-map "\t" 'org-cycle)))

(add-hook 'outline-minor-mode-hook
	  (lambda ()
	    (define-key outline-minor-mode-map "\t" 'org-cycle)))

;; tmux
(global-set-key (kbd "C-x x") (lambda ()
                            (interactive)
                            (shell-command (concat "tmux split-window -v -p 50 -c " default-directory))
			    ))
