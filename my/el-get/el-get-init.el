(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(if (require 'el-get nil t)
    (progn
      (message "el-get is already installed, try M-x el-get-update")
      (load "el-get/el-get-sources"))
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp)
     (load "el-get/el-get-sources"))))

(setq package-archives
      '(("original" . "http://tromey.com/elpa/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
	))
