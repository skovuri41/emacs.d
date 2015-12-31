(use-package expand-region
  :ensure t
  :defer t
  :after (evil)
  :config
    (define-key evil-normal-state-map (kbd "+") 'er/expand-region)
    (define-key evil-visual-state-map (kbd "x") 'er/expand-region)
    (define-key evil-insert-state-map (kbd "M-=") 'er/expand-region)
    (define-key evil-visual-state-map (kbd "X") 'er/contract-region)
    (define-key evil-visual-state-map (kbd "+") 'er/expand-region)
    (define-key evil-visual-state-map (kbd "-") 'er/contract-region)
    (setq expand-region-contract-fast-key "z"))
(provide 'init-expand-region)
