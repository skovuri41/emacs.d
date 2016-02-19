(use-package spaceline
  :ensure t
  :config
  (progn
    (use-package spaceline-config
      :init
      (setq spaceline-workspace-numbers-unicode t)
      (setq spaceline-window-numbers-unicode t)
      (setq powerline-height 25)
      ;; (setq spaceline-highlight-face-func 'spaceline-highlight-face-modified)
      (set-face-attribute 'spaceline-evil-emacs nil :background "#be84ff")
      (set-face-attribute 'spaceline-evil-insert nil :background "#5fd7ff")
      (set-face-attribute 'spaceline-evil-motion nil :background "#ae81ff")
      (set-face-attribute 'spaceline-evil-normal nil :background "#a6e22e")
      (set-face-attribute 'spaceline-evil-replace nil :background "#f92672")
      (set-face-attribute 'spaceline-evil-visual nil :background "#fd971f")
      (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
      :config
      (spaceline-spacemacs-theme)
      ;; (spaceline-emacs-theme)
      (spaceline-toggle-flycheck-warning-off)
      (spaceline-toggle-flycheck-error-off)
      (spaceline-toggle-flycheck-info-off)
      (setq powerline-default-separator 'arrow-fade))))


(provide 'init-spaceline)
