(use-package helm
  :ensure t
  :defer t
  :diminish helm-mode
  :init
  (setq helm-buffers-fuzzy-matching t)
  :config
  (helm-mode 1)
  (setq helm-autoresize-mode t)
  (setq helm-buffer-max-length 40)
  (bind-key "S-SPC" 'helm-toggle-visible-mark helm-map)
  (bind-key "C-k" 'helm-find-files-up-one-level helm-find-files-map))
(use-package helm-descbinds )
(use-package helm-gtags )
(use-package helm-projectile)
(use-package helm-swoop)


(provide 'init-helm)
