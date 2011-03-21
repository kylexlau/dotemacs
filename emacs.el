;;; Thanks https://github.com/senny/emacs-configs

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; platform variable
(defvar ntp (string= "windows-nt" (symbol-name system-type))
  "If Emacs runs on a Windows system.")

(defvar linuxp (string= "gnu/linux" (symbol-name system-type))
  "If Emacs runs on a Linux system.")

(defvar macosp (string= "darwin" (symbol-name system-type))
  "If Emacs runs on a Mac OS system.")

(defvar cygwinp (string= "cygwin" (symbol-name system-type))
  "If Emacs runs on a Cygwin platform.")

(cond
  (ntp (load "my/platform/nt"))
  (linuxp (load "my/platform/linux"))
  (macosp (load "my/platform/macos"))
)

;; Load path
(setq emacs-el-dir (file-name-directory (or (buffer-file-name) load-file-name))
      my-dir (concat emacs-el-dir "my")
)

(add-to-list 'load-path emacs-el-dir)
(add-to-list 'load-path my-dir)

;; load my configs
(load "my/display")
(load "my/defun")
(load "my/misc")
(load "my/backup")
(load "my/binding")

;; el-get to manage el packages
(load "my/elget/el-get-init")

;;; load languages configs
(mapc 'load
      (directory-files (concat my-dir "/lang") t ".*elc$"))

;;; load mode configs
(mapc 'load
      (directory-files (concat my-dir "/mode") t ".*elc$"))

;;; local settings
(if (file-exists-p (concat my-dir "/local.el")) (load "local"))
