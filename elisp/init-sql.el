(use-package sql
  :mode
  (("\.sql$" . sql-mode)
   ("\.sqltmpl$" . sql-mode))
  )
(use-package sql-indent
  :commands (sql-indent-mode)
  :init
  (add-hook 'sql-mode-hook 'sql-indent-mode)
  (add-hook 'sql-interactive-mode-hook 'sql-indent-mode)
  :config
  (setq sql-indent-offset 2)
  )
(use-package sqlup-mode
  :commands (sqlup-mode)
  :init
  (add-hook 'sql-mode-hook 'sqlup-mode)
  (add-hook 'sql-interactive-mode-hook 'sqlup-mode)
  :config
  )

(provide 'init-sql)
