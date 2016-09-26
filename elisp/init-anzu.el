(use-package anzu
  :ensure t
  :diminish anzu-mode
  :bind (("C-M-%" . anzu-query-replace)
         ("M-%" . anzu-query-replace-regexp))
  :config
  (progn
    (use-package thingatpt)
    (validate-setq anzu-mode-lighter "")
    (global-set-key [remap query-replace] 'anzu-query-replace)
    (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
    (validate-setq anzu-replace-threshold 1000)
    (validate-setq anzu-search-threshold 1000)
    (validate-setq anzu-replace-to-string-separator " =>")
    (set-face-attribute 'anzu-mode-line nil :foreground "yellow")
    (global-anzu-mode)))

(provide 'init-anzu)
