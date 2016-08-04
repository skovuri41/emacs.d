(use-package key-chord
  :ensure t
  :init
  (progn
    (key-chord-mode 1)
    ))

(use-package xah-fly-keys
  :ensure t
  :init
  (progn
    (key-chord-define-global "kj" 'xah-fly-command-mode-activate)
    (xah-fly-keys 1)
    ))

(provide 'init-xah-fly-keys)
