(use-package avy
  :config
  (progn
    (evil-leader/set-key
      "jw" 'avy-goto-word-1
      "jc" 'avy-goto-char-2
      "jl" 'avy-goto-line)))

(provide 'init-avy)
