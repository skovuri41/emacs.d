(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config
  (setq flycheck-idle-change-delay 5)
  (add-hook 'after-init-hook #'global-flycheck-mode)
  )

(provide 'init-flycheck)
