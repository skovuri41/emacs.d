(use-package avy
  :ensure t
  :init
  (progn (bind-key "M-s M-s" 'avy-isearch isearch-mode-map)
         (bind-key "C-'" 'avy-isearch isearch-mode-map))
  :config
  (progn
    (defun avy-isearch ()
      "Override to allow avy-background to work as configured."
      (interactive)
      (avy-with avy-isearch
        (avy--process
         (avy--regex-candidates isearch-string)
         (avy--style-fn avy-style))
        (isearch-done)))

    (defhydra hydra-avy (:color teal)
      ("b" avy-pop-mark "char")
      ("c" avy-goto-char "char")
      ("w" avy-goto-word-1 "word")
      ("ll" avy-goto-line "goto line")
      ("ly" avy-copy-line "copy line")
      ("lm" avy-move-line "move line")
      ("ln" goto-line-with-feedback "goto line number")
      ("t" avy-goto-char-timer "timer")
      ("f" counsel-find-file "find-file")
      ("q" nil))
    ;;* Lispy
    (csetq avy-keys-alist
           `((lispy-ace-symbol . ,aw-keys)))

    (setq avy-background t)))

(use-package avy-zap
  :ensure t
  :bind (("M-z" . avy-zap-to-char-dwim)
         ("M-Z" . avy-zap-up-to-char-dwim)))

(provide 'init-avy)
