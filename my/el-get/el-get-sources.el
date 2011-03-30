(add-to-list 'el-get-recipe-path (concat my-dir "/elget/recipes"))

(setq el-get-sources
      '(
        el-get

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
        yaml-mode
        rhtml-mode
        rvm
        css-mode
        emacs-textmate
        ))

;(el-get 'sync)
(el-get 'wait)
