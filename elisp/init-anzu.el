(use-package anzu
  :ensure t
  :diminish anzu-mode
  :bind (("C-M-%" . anzu-query-replace)
         ("M-%" . anzu-query-replace-regexp))
  :config
  (progn
    (use-package thingatpt)
    (setq anzu-mode-lighter "")
    (set-face-attribute 'anzu-mode-line nil :foreground "yellow")
    (global-anzu-mode)))

(provide 'init-anzu)
