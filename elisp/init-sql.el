(use-package sql
  :mode
  (("\.sql$" . sql-mode)
   ("\.sqltmpl$" . sql-mode))
  :config
  (use-package sql-indent
    :ensure t
    :commands (sql-indent-mode)
    :init
    (load-library "sql-indent")
    (setq sql-indent-offset 2)
    )
  (use-package sqlup-mode
    :ensure t
    :commands (sqlup-mode)
    :init
    (add-hook 'sql-mode-hook 'sqlup-mode)
    (add-hook 'sql-interactive-mode-hook 'sqlup-mode)
    )
  (with-eval-after-load 'evil
    (evil-leader/set-key-for-mode 'sql-mode
      "mc" 'sqlup-capitalize-keywords-in-region
      "mi" 'sql-indent-buffer
      )
    )
  )

(provide 'init-sql)
