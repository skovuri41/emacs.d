(use-package flycheck
  :ensure t
  :diminish flycheck-mode 
  :config
  (setq flycheck-idle-change-delay 5)
  (add-hook 'after-init-hook #'global-flycheck-mode)
  ;; Flycheck mode:
  (add-hook 'flycheck-mode-hook
            (lambda ()
              (evil-define-key 'normal flycheck-mode-map (kbd "]e") 'flycheck-next-error)
              (evil-define-key 'normal flycheck-mode-map (kbd "[e") 'flycheck-previous-error)))
  )

(provide 'init-flycheck)
