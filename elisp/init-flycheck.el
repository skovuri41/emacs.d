(use-package flycheck
  :ensure t
  :diminish flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))
(provide 'init-flycheck)
