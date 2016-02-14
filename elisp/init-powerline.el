(use-package powerline
  :ensure t
  :init
  (progn
    (require 'powerline)
    ;; (powerline-center-evil-theme)
    (use-package powerline-evil
      :config
      (progn
        (powerline-evil-vim-color-theme)))
    (use-package diminish
      :config
      (progn
        (eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
        (eval-after-load "simple" '(diminish 'auto-fill-function))
        (eval-after-load "eldoc" '(diminish 'eldoc-mode))
        (eval-after-load "guide-key" '(diminish 'guide-key-mode))
        (eval-after-load "highlight-parentheses" '(diminish 'highlight-parentheses-mode))
        (eval-after-load "elisp-slime-nav" '(diminish 'elisp-slime-nav-mode " sln"))
        (eval-after-load "projectile" '(diminish 'projectile-mode " prj"))
        (eval-after-load "company" '(diminish 'company-mode " cmp"))
        (eval-after-load "cider" '(diminish 'cider-mode " cid"))
        (eval-after-load "typed-clojure-mode" '(diminish 'typed-clojure-mode " typ"))
        (eval-after-load "org-indent" '(diminish 'org-indent-mode))
        (eval-after-load "evil-org" '(diminish 'evil-org-mode))
        (eval-after-load "smartparens" '(diminish 'smartparens-mode))
        (eval-after-load "super-save" '(diminish 'super-save-mode))
        (eval-after-load "flycheck" '(diminish 'flycheck-mode))
        (eval-after-load "drag-stuff" '(diminish 'drag-stuff-mode))))))

(powerline-default-theme)
(powerline-vim-theme)
(powerline-nano-theme)
(powerline-center-evil-theme)
(provide 'init-powerline)
