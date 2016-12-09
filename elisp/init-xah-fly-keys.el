(use-package key-chord
  :ensure t
  :init
  (progn
    (shut-up (key-chord-mode 1))
    (setq key-chord-two-keys-delay 0.3)))

(use-package xah-fly-keys
  :load-path "elisp/"
  :config
  (progn
    (xah-fly-keys 1)
    (defun kj-chord-dwim ()
      "quit or command mode or M-x"
      (interactive)
      (cond
       ((use-region-p) (progn
                         (xah-fly-command-mode-activate)
                         (keyboard-quit)))
       ((eq t xah-fly-insert-state-q) (xah-fly-command-mode-activate))))
    (key-chord-define-global "kj" 'kj-chord-dwim)
    (key-chord-define-global "df" 'counsel-M-x)
    (key-chord-define minibuffer-local-map "kj" (kbd "C-g"))
    (key-chord-define ivy-minibuffer-map "kj" (kbd "C-g"))
    (key-chord-define company-active-map "kj" 'company-abort)
    ))

(provide 'init-xah-fly-keys)
