(use-package yasnippet
  :ensure t
  :diminish Eldoc
  :init
  (progn
    (yas-global-mode 1)
    (use-package clojure-snippets)))

(provide 'init-yasnippet)
