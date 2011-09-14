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

;; Load path
(setq dotemacs-dir (file-name-directory (or (buffer-file-name) load-file-name)))
(setq dotemacs-my-dir (concat dotemacs-dir "my"))
(setq dotemacs-elisp-dir (concat dotemacs-dir "elisp"))

(add-to-list 'load-path dotemacs-dir)
(add-to-list 'load-path dotemacs-my-dir)
(add-to-list 'load-path dotemacs-elisp-dir)

;; el-get to manage el packages
(when (not ntp) (load "my/el-get/el-get-init"))

;; load platform config
(cond
  (ntp (load "my/platform/nt"))
  (linuxp (load "my/platform/linux"))
  (macosp (load "my/platform/macos"))
)

;; load my configs
(load "my/display")
(load "my/defun")
(load "my/misc")
(load "my/binding")
(load "my/ske")

;; baload mode configs
(mapc 'load
      (directory-files (concat dotemacs-my-dir "/mode") t ".*elc$"))

;; load languages configs
(mapc 'load
      (directory-files (concat dotemacs-my-dir "/lang") t ".*elc$"))

;; local settings
(if (file-exists-p (concat dotemacs-my-dir "/local.el")) (load "local"))

(server-start)

