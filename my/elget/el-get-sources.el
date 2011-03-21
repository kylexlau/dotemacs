(add-to-list 'el-get-recipe-path (concat my-dir "/elget/recipes"))

(setq el-get-sources
      '(
	el-get
	color-theme
        yaml-mode
        rvm

        (:name ruby-mode        :type elpa)
	(:name ruby-electric    :type elpa)
        (:name ruby-compilation :type elpa)
        (:name inf-ruby         :type elpa)
        ))

(el-get 'sync)
