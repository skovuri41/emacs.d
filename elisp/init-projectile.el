
(use-package projectile
  :init (projectile-global-mode)
  :config
  (progn
    (evil-leader/set-key "pf" 'projectile-find-file)
    (evil-leader/set-key "pa" 'projectile-ag)
    (evil-leader/set-key "pk" 'projectile-kill-buffers)
    (evil-leader/set-key "pm" 'projectile-command-map)

    (add-to-list 'projectile-globally-ignored-files ".DS_Store")
    (add-to-list 'projectile-globally-ignored-files "*.pyc")
    (add-to-list 'projectile-globally-ignored-files "*.python-version")
    (add-to-list 'projectile-globally-ignored-files "*.egg-info")
    (add-to-list 'projectile-globally-ignored-directories "__pycache__")
    (add-to-list 'projectile-globally-ignored-directories ".env")
    (add-to-list 'projectile-globally-ignored-directories ".venv")
    (add-to-list 'projectile-globally-ignored-directories ".cask")
    (add-to-list 'projectile-globally-ignored-directories ".cache")

    ))

(provide 'init-projectile)
