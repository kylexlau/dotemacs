(add-to-list 'el-get-recipe-path (concat dotemacs-my-dir "/el-get/recipes"))

(setq el-get-sources
      '(
        el-get

	ecb

	auctex

        ;; dired-plus
        ;; ido-hacks

        color-theme
        paredit
        yasnippet

        ruby-mode
        rinari
        ruby-electric
        ruby-compilation
        flymake-ruby
	yari
        rvm
	magit
;	magithub

        yaml-mode
        rhtml-mode
        css-mode

        emacs-textmate

	textile-mode

	haml-mode
	scss-mode
;	sass-mode
        ))

;(el-get 'sync)
(el-get 'wait)
