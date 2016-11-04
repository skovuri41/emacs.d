(use-package composable
  :diminish composable-mode
  :ensure t
  :config
  (progn
    (unbind-key (kbd "p") composable-object-mode-map)
    (unbind-key (kbd "n") composable-object-mode-map)
    (bind-keys :map composable-object-mode-map
               ((kbd "j") . next-line)
               ((kbd "d") . er/mark-defun)
               ((kbd "k") . previous-line))

    (composable-def '(delete-region))
    (composable-mode)
    (composable-mark-mode)))

(provide 'init-composable)
