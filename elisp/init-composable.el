(use-package composable
  :diminish composable-mode
  :ensure t
  :config
  (progn
    (unbind-key (kbd "p") composable-object-mode-map)
    (unbind-key (kbd "s") composable-object-mode-map)
    (unbind-key (kbd "u") composable-object-mode-map)
    (unbind-key (kbd "y") composable-object-mode-map)
    (unbind-key (kbd "l") composable-object-mode-map)

    (bind-keys :map composable-object-mode-map
               ("j" . next-line)
               ("l" . forward-char)
               ("h" . backward-char)
               ("d" . er/mark-defun)
               ("P" . er/mark-inside-pairs)
               ("p" . er/mark-outside-pairs)
               ("q" . er/mark-inside-quotes)
               ("t" . evilmi-jump-items)
               ("Q" . er/mark-outside-quotes)
               ("'" . avy-goto-char)
               ("." . er/mark-text-sentence)
               ("s" . composable-mark-symbol)
               ("B" . mark-whole-buffer)
               ("b" . backward-word)
               ("=" . er/expand-region)
               ("k" . previous-line))

    (composable-def '(delete-region))
    (composable-mode)
    (composable-mark-mode)))

(provide 'init-composable)
