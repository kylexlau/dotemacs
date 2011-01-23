(when macosp 
  (set-frame-font "Courier New-14")

  (set-fontset-font (frame-parameter nil 'font)
		    'han '("STSong" . "unicode-bmp"))
  
  (setq default-frame-alist
	'(
	  ;;(top . 0) (left . 0)
	  (width . 80) (height . 40)
	  (font . "Courier New-14")))
  )

(when linuxp 
  (set-frame-font "DejaVu Sans Mono-10.5")
  
  (set-fontset-font (frame-parameter nil 'font)
		    'han '("Microsoft YaHei-10.5" . "unicode-bmp"))

  (setq default-frame-alist
	'(
	  (top . 0) (left . 0)
	  (width . 80) (height . 25)
	  (font . "DejaVu Sans Mono-10.5")))
  )

(when ntp 
  (set-frame-font "Consolas-10.5")
  
  (setq default-frame-alist 
	'((width . 80)
	  (height . 25)
	  (font . "Consolas-10.5")))
  )
