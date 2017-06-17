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
               ("b" . backward-word)
               ("B" . mark-whole-buffer)
               ("d" . er/mark-defun)
               ("E" . er/mark-email)
               ("g" . mark-buffer-before-point)
               ("G" . mark-buffer-after-point)
               ("h" . backward-char)
               ("j" . next-line)
               ("k" . previous-line)
               ("l" . forward-char)
               ("L" . link-hint-copy-multiple-links)
               ("n" . xah-copy-file-path)
               ("P" . er/mark-inside-pairs)
               ("p" . er/mark-outside-pairs)
               ("q" . er/mark-inside-quotes)
               ("Q" . er/mark-outside-quotes)
               ("s" . composable-mark-symbol)
               ("t" . evilmi-jump-items)
               ("u" . er/mark-url)
               ("'" . avy-goto-char)
               ("." . er/mark-text-sentence)
               ("=" . er/expand-region)
               ("," . er/contract-region))

    (composable-def '(delete-region))
    (composable-mode)
    (composable-mark-mode)))

(provide 'init-composable)
