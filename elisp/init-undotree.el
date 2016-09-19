(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)
    (setq undo-tree-mode-lighter "")
    (setq undo-tree-auto-save-history t
          undo-tree-history-directory-alist
          `(("." . ,(expand-file-name "undo" user-emacs-directory))))

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

    (defun my-undo-tree-visualizer-settings ()
      (interactive)
      (define-key undo-tree-visualizer-mode-map (kbd "C-c C-k") 'undo-tree-visualizer-quit)
      (define-key undo-tree-visualizer-mode-map (kbd "C-k") 'undo-tree-visualizer-quit)
      (define-key undo-tree-visualizer-mode-map (kbd "C-g") 'undo-tree-visualizer-abort)
      (bind-keys :map undo-tree-visualizer-mode-map
                 ("RET" . undo-tree-visualizer-quit)
                 ("q" . undo-tree-visualizer-quit)
                 ("h" . undo-tree-visualize-switch-branch-left)
                 ("j" . undo-tree-visualize-redo)
                 ("k" . undo-tree-visualize-undo)
                 ("l" . undo-tree-visualize-switch-branch-right)))

    (add-hook 'undo-tree-visualizer-mode-hook
              'my-undo-tree-visualizer-settings)

    ))

;; (setq undo-tree-mode-lighter "")

(provide 'init-undotree)
