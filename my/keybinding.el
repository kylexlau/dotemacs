(global-set-key [f11] 'my-toggle-fullscreen)
(global-set-key (kbd "C-c o") 'speedbar-select-attached-frame)
(global-set-key (kbd "C-=") 'hippie-expand)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cr" 'org-remember)

(global-set-key (kbd "C-c M-q") 'unfill-paragraph)

(define-key outline-minor-mode-map (kbd "\C-c <tab>") 'org-cycle)
(define-key outline-minor-mode-map (kbd "\C-u <tab>") 'org-shifttab)
