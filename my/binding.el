(global-set-key [f11] 'my-toggle-fullscreen)
(global-set-key "\C-x\C-b" 'ibuffer)
(global-set-key "\C-co" 'speedbar-select-attached-frame)
(global-set-key "\C-c\M-q" 'unfill-paragraph)

;; org-mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cr" 'org-remember)

;; git
(global-set-key "\C-xg" 'magit-status)

;; completion
(global-set-key (kbd "C-=") 'hippie-expand)
