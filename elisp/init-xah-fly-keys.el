(use-package key-chord
  :ensure t
  :init
  (progn
    (key-chord-mode 1)
    (setq key-chord-two-keys-delay 0.2)
    ))

(use-package xah-fly-keys
  :load-path "elisp/"
  :config
  (progn
    (key-chord-define-global "kj" 'xah-fly-command-mode-activate)
    (key-chord-define minibuffer-local-map "kj" (kbd "C-g") )
    (xah-fly-keys 1)
    ))

(provide 'init-xah-fly-keys)
