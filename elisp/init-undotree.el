(use-package undo-tree
  :defer t
  :ensure t
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)

    ;; Keep region when undoing in region
    (defadvice undo-tree-undo (around keep-region activate)
      (if (use-region-p)
          (let ((m (set-marker (make-marker) (mark)))
                (p (set-marker (make-marker) (point))))
            ad-do-it
            (goto-char p)
            (set-mark m)
            (set-marker p nil)
            (set-marker m nil))
        ad-do-it))

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
