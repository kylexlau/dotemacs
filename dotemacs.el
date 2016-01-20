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

;;; File backups
(setq
 backup-by-copying t
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)

(setq emacs-temporary-file-directory
      (expand-file-name "~/.emacs.d/tmp"))
(setq backup-directory-alist
      `((".*" . ,emacs-temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-temporary-file-directory t)))

;;; UI setting
(tool-bar-mode -1)
