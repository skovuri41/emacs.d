(use-package discover-my-major
  :ensure t
  :defer t
  :config
  (evil-leader/set-key "Hm" 'discover-my-major)
  (evil-leader/set-key "Hh" 'help)
  (evil-leader/set-key "Hf" 'describe-function)
  (evil-leader/set-key "Hk" 'describe-key)
  (evil-leader/set-key "Hv" 'describe-variable))

(provide 'init-help)
