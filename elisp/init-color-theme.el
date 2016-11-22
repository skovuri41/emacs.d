(use-package zenburn-theme
  :disabled t
  :ensure t
  :config
  (load-theme 'zenburn t))

(use-package oceanic-theme
  :disabled t
  :ensure t
  :config
  (load-theme 'oceanic t))

(use-package atom-one-dark-theme
  :disabled t
  :ensure t
  :config
  (load-theme 'atom-one-dark t))

(use-package tangotango-theme
  :disabled t
  :ensure t
  :config
  (load-theme 'tangotango t))

(use-package material-theme
  :ensure t
  :disabled t
  :init (load-theme 'material-light t))

(use-package spacemacs-theme
  :ensure t
  :disabled t
  :defer 2
  :config
  (load-theme 'spacemacs-dark t)
  )

;; (set-face-background 'powerline-active1 "#5d4d7a")
;; (set-face-background 'powerline-active2 "#5d4d7a")
;; (set-face-foreground 'powerline-active1 "#eee8d5")
;; (set-face-foreground 'powerline-active2 "#eee8d5")
(use-package color-themes
  :config
  (setq packages-appearance '(monokai-theme solarized-theme
                                            zenburn-theme base16-theme molokai-theme tango-2-theme
                                            gotham-theme sublime-themes rainbow-delimiters waher-theme
                                            ample-theme material-theme zerodark-theme
                                            color-theme-modern leuven-theme spacemacs-theme
                                            gruvbox-theme zenburn-theme forest-blue-theme flatland-theme
                                            afternoon-theme birds-of-paradise-plus-theme apropospriate-theme noctilux-theme))
  (ensure-packages-installed appearance-packages))

(use-package cycle-themes
  :ensure t
  :disabled t
  :init (setq cycle-themes-theme-list
              '(apropospriate-dark spacemacs-dark  zenburn afternoon material))
  :config (cycle-themes-mode))

(provide 'init-color-theme)
