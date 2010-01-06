(global-set-key [f11] 'my-toggle-fullscreen)
(global-set-key (kbd "C-c o") 'speedbar-select-attached-frame)
(global-set-key (kbd "C-=") 'hippie-expand)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cr" 'org-remember)

(define-key outline-minor-mode-map (kbd "\C-c <tab>") 'org-cycle)
(define-key outline-minor-mode-map (kbd "\C-u <tab>") 'org-shifttab)

(global-set-key (kbd "\C-c s") 'ispell-word)

(global-set-key [f7] 'etexshow)
(add-hook 'etexshow-mode-hook '(lambda () 
				 (local-set-key [f7] 'etexshow-quit)))
