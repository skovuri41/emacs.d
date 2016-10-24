(use-package imenu-anywhere
  :ensure t
  :config
  (setq imenu-auto-rescan t)
  )

(use-package imenu-list
  :ensure t
  :defer t
  :init
  (progn
    (setq imenu-list-focus-after-activation t
          imenu-list-auto-resize t)))

(provide 'init-imenu-anywhere)
