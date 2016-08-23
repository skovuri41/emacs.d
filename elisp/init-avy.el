(use-package avy
  :ensure t
  :config
  (progn
    (defhydra hydra-avy (:color teal)
      ("j" avy-goto-char "char")
      ("k" avy-goto-word-1 "word")
      ("l" avy-goto-line "line")
      ("s" avy-goto-char-timer "timer")
      ("f" counsel-find-file "find-file")
      ("q" nil))
    )
  )

(provide 'init-avy)
