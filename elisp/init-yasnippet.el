(use-package yasnippet
  :ensure t
  :defer t
  :init
  (progn
    ;; (yas-reload-all)
    ;; (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
    (setq yas-snippet-dirs '("~/.emacs.d/snippets" yas-installed-snippets-dir))
    (setq tab-always-indent 'complete)
    (yas-global-mode 1)
    (setq yas-prompt-functions '(yas-completing-prompt
                                 yas-ido-prompt
                                 yas-dropdown-prompt))
    (define-key yas-minor-mode-map (kbd "<escape>") 'yas-exit-snippet)
    (use-package clojure-snippets)))

(provide 'init-yasnippet)
