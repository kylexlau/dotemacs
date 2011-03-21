(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

(setq auto-mode-alist 
      (cons '("\\.lua$" . lua-mode) auto-mode-alist))

(add-hook 'lua-mode-hook 'hs-minor-mode)
