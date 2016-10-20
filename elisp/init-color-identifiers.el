(use-package color-identifiers-mode
  :diminish color-identifiers-mode
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-color-identifiers-mode))

(provide 'init-color-identifiers)
