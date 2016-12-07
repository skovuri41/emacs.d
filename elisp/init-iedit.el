(use-package iedit
  :ensure t
  :commands iedit-mode
  :config (progn
            (setq iedit-log-level 0)
            (custom-set-faces
             '(iedit-occurrence ((t (:foreground "green")))))
            (define-key iedit-mode-keymap "\C-h" nil)
            (define-key iedit-mode-occurrence-keymap (kbd "M-n") 'iedit-next-occurrence)
            (define-key iedit-mode-occurrence-keymap (kbd "M-p") 'iedit-prev-occurrence))
  :init (setq iedit-toggle-key-default nil))

(provide 'init-iedit)
