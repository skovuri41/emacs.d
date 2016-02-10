
(use-package projectile
  :init (projectile-global-mode)
  :config
  (progn
    (evil-leader/set-key "pf" 'projectile-find-file)
    (evil-leader/set-key "pa" 'projectile-ag)
    (evil-leader/set-key "pk" 'projectile-kill-buffers)
    (evil-leader/set-key "pm" 'projectile-command-map)))

(provide 'init-projectile)
