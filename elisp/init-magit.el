(use-package magit
  :ensure t
  :commands (magit-status
             magit-log
             magit-commit
             magit-stage-file)
  :config
(progn
  (add-hook 'magit-mode-hook 'xah-fly-insert-mode-activate)
  (add-hook 'magit-status-mode-hook 'xah-fly-command-mode-activate)))

(provide 'init-magit)
