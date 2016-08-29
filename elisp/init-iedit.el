(use-package iedit
  :ensure t
  :config
  (global-set-key (kbd "M-[ 5 n") 'iedit-mode)
  (global-set-key (kbd "C-;") 'iedit-mode)
  (define-key iedit-mode-occurrence-keymap (kbd "M-n") 'iedit-next-occurrence)
  (define-key iedit-mode-occurrence-keymap (kbd "M-p") 'iedit-prev-occurrence))

(provide 'init-iedit)
