(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("Tromey" . "http://tromey.com/elpa/")))

(package-initialize)

(setq my-required-packages
      '(inf-ruby))

(dolist (package my-required-packages)
  (when (not (package-installed-p package))
    (package-refresh-contents)
    (package-install package)))
