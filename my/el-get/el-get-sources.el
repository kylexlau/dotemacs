(add-to-list 'el-get-recipe-path (concat my-dir "/el-get/recipes"))

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

        yaml-mode
        rhtml-mode
        css-mode

        emacs-textmate

        ))

;(el-get 'sync)
(el-get 'wait)
