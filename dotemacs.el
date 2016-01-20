;;; add MELPA repository
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;;; Font setting
;; English Font
(set-face-attribute 'default nil :font "Consolas 13")

;; Chinese Font
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
                    charset (font-spec :family "SimHei"
                                       :size 12.5)))
(setq face-font-rescale-alist '("SimHei" . 1.2))
