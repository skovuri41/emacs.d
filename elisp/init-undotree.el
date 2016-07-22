(use-package undo-tree
  :defer t
  :ensure t
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)
    (add-hook 'undo-tree-visualizer-mode-hook
              'my-undo-tree-visualizer-settings)
    (defun my-undo-tree-visualizer-settings ()
      (interactive)
      (define-key undo-tree-visualizer-mode-map (kbd "C-c C-k") 'undo-tree-visualizer-quit)
      (define-key undo-tree-visualizer-mode-map (kbd "C-k") 'undo-tree-visualizer-quit)
      ;; (define-key undo-tree-visualizer-mode-map (kbd "k") 'undo-tree-visualizer-quit)
      (define-key undo-tree-visualizer-mode-map (kbd "C-g") 'undo-tree-visualizer-abort))
    ))

;; (setq undo-tree-mode-lighter "")

(provide 'init-undotree)
