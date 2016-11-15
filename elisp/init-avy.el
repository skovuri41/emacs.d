(use-package avy
  :ensure t
  :config
  (progn
    (defhydra hydra-avy (:color teal)
      ("c" avy-goto-char "char")
      ("w" avy-goto-word-1 "word")
      ("l" avy-goto-line "line")
      ("t" avy-goto-char-timer "timer")
      ("f" counsel-find-file "find-file")
      ("q" nil))
    ;;* Lispy
    (csetq avy-keys-alist
           `((lispy-ace-symbol . ,aw-keys)))

    (setq avy-background t)))

(provide 'init-avy)
