(use-package highlight-tail
  :ensure t
  :config
  (progn
    (setq highlight-tail-steps 8)
    (setq highlight-tail-timer 0.05)))

(provide 'highlight-tail)
