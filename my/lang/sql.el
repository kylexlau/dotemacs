(autoload 'plsql-mode "plsql" "PL/SQL Mode" t)
(setq auto-mode-alist 
      (cons (cons "\\.sql$" 'plsql-mode) auto-mode-alist))
(setq plsql-indent 2)
