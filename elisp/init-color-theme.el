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

(setq packages-appearance
      '(monokai-theme solarized-theme zenburn-theme base16-theme molokai-theme
                      tango-2-theme gotham-theme sublime-themes rainbow-delimiters waher-theme
                      ample-theme material-theme zerodark-theme color-theme-modern leuven-theme
                      spacemacs-theme gruvbox-theme forest-blue-theme flatland-theme afternoon-theme))

(ensure-packages-installed packages-appearance)

(provide 'init-color-theme)
