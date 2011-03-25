(add-to-list 'el-get-recipe-path (concat my-dir "/elget/recipes"))

(setq el-get-sources
      '(
	el-get

;	dired-plus
	ido-hacks

	color-theme
        yaml-mode
        rvm
	paredit
	yasnippet

	ruby-mode
	ruby-electric
	flymake-ruby
        ))

(el-get 'sync)
