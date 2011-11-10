(add-to-list 'el-get-recipe-path (concat dotemacs-my-dir "/el-get/recipes"))

(setq el-get-sources
      '(
	el-get paredit
	ruby-mode ruby-electric ruby-compilation flymake-ruby
	))

(el-get 'wait)
