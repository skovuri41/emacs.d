(use-package aggressive-indent
  :ensure t
  :diminish aggressive-indent-mode
  :config
  (progn
    (add-hook 'clojure-mode-hook #'aggressive-indent-mode)
    (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
    (add-hook 'css-mode-hook #'aggressive-indent-mode)
    ;; (add-hook 'js2-mode-hook #'aggressive-indent-mode)
    )
  )
(provide 'init-aggressive-indent)
