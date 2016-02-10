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
(use-package helm-company
  :ensure t
  :config
  (eval-after-load 'company
    '(progn
       (define-key company-mode-map (kbd "C-:") 'helm-company)
       (define-key company-active-map (kbd "C-:") 'helm-company)))
  )
(use-package helm-ag
  :ensure helm-ag
  :commands (helm-ag helm-projectile-ag))


(provide 'init-helm)
