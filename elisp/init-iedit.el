(use-package iedit
  :ensure t
  :config
  (define-key iedit-mode-occurrence-keymap (kbd "M-n") 'iedit-next-occurrence)
  (define-key iedit-mode-occurrence-keymap (kbd "M-p") 'iedit-prev-occurrence))

(provide 'init-iedit)
