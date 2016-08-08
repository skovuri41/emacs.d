(use-package fancy-battery
  :if *is-a-mac*
  :ensure t
  :config
  (setq-default fancy-battery-show-percentage t)
  (add-hook 'after-init-hook #'fancy-battery-mode))

(provide 'init-fancy-battery)
