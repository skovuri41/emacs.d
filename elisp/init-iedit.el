(use-package iedit
  :ensure t
  :config
  (global-set-key (kbd "M-[ 5 n") 'iedit-mode);iterm2 key ^; : ^[[5n
  (global-set-key (kbd "C-;") 'iedit-mode);iterm2 key ^; : ^[[5n
  )

(provide 'init-iedit)
