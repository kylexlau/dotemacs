;; platform variable
(defvar ntp (string= "windows-nt" (symbol-name system-type))
  "If Emacs runs on a Windows system.")

(defvar linuxp (string= "gnu/linux" (symbol-name system-type))
  "If Emacs runs on a Linux system.")

(defvar macosp (string= "darwin" (symbol-name system-type))
  "If Emacs runs on a Mac OS system.")

(defvar cygwinp (string= "cygwin" (symbol-name system-type))
  "If Emacs runs on a Cygwin platform.")

;; load platform config
(cond
  (ntp (load "nt"))
  (linuxp (load "linux"))
  (macosp (load "macosx"))
)

