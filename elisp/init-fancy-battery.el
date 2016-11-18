(use-package fancy-battery
  :if *is-a-mac*
  :ensure t
  :defer 5
  :config
  (validate-setq fancy-battery-show-percentage t)
  (setq battery-update-interval 60)
  (validate-setq fancy-battery-mode-line "")
  (fancy-battery-mode)
  ;; (add-hook 'after-init-hook #'fancy-battery-mode)
  )

(provide 'init-fancy-battery)
