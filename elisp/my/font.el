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
  (set-frame-font "Consolas-8")
  
  (set-fontset-font (frame-parameter nil 'font)
		    'han '("WenQuanYi Zen Hei" . "unicode-bmp"))

  (setq default-frame-alist
	'(
	  (top . 0) (left . 0)
	  (width . 80) (height . 25)
	  (font . "Consolas-8")))
  )

(when ntp 
  (set-frame-font "Consolas-10.5")
  
  (setq default-frame-alist 
	'((width . 80)
	  (height . 25)
	  (font . "Consolas-10.5")))
  )
