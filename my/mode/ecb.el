(add-to-list 'load-path "~/.emacs.d/el-get/ecb")

(setq stack-trace-on-error nil)

(require 'ecb)

(set 'ecb-activate-hook nil)
(add-hook 'ecb-activate-hook
	  (lambda ()
	    (set-frame-size (selected-frame) 105 35)
	    ))

(add-hook 'ecb-deactivate-hook
	  (lambda ()
	    (set-frame-size (selected-frame) 80 35)
	    ))
