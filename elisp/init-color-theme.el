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
  :disabled t
  :quelpa (spacemacs-theme :fetcher github :repo "surya46584/spacemacs-theme"))

(use-package zerodark-theme
  ;; :if *is-gnu-linux*
  ;; :quelpa (zerodark-theme :fetcher github :repo "surya46584/zerodark-theme")
  :ensure t
  :config
  (setq zerodark-use-paddings-in-mode-line nil))

(use-package color-themes
  :defer 5
  :config
  (progn
    (defvar packages-appearance)
    (setq packages-appearance '(monokai-theme solarized-theme
                                              zenburn-theme
                                              molokai-theme
                                              tango-2-theme
                                              rainbow-delimiters
                                              material-theme
                                              leuven-theme
                                              zenburn-theme
                                              spacemacs-theme
                                              apropospriate-theme
                                              birds-of-paradise-plus-theme))
    (ensure-packages-installed packages-appearance)))

(provide 'init-color-theme)
