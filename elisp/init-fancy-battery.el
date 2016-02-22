(use-package fancy-battery
  :ensure t
  :config
  (setq-default fancy-battery-show-percentage t)
  (add-hook 'after-init-hook #'fancy-battery-mode))

(provide 'init-fancy-battery)
