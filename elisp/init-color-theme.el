;; Fully unload previous theme before loading a new one
;; (defadvice load-theme
;;     (before theme-dont-propagate activate)
;;   (mapc #'disable-theme custom-enabled-themes))

(use-package zenburn-theme
  :disabled t
  :ensure t
  :config
  (load-theme 'zenburn t))

;; (use-package gruvbox-theme
;;   :disabled t
;;   :ensure t
;;   :config (load-theme 'gruvbox 'no-confirm))

;; ;; tangotango-theme
;; (use-package tangotango-theme
;;   :disabled t
;;   :ensure t
;;   :config
;;   (load-theme 'tangotango t))

;; ;; leuven-theme
;; (use-package leuven-theme
;;   :disabled t
;;   :ensure t
;;   :config
;;   (load-theme 'leuven t))

;; ;; monokai-theme
;; (use-package monokai-theme
;;   :disabled t
;;   :ensure t
;;   :config
;;   (load-theme 'monokai t))

;; ;; molokai-theme
;; (use-package molokai-theme
;;   :disabled t
;;   :ensure t
;;   :config
;;   (load-theme 'molokai t))

(use-package spacemacs-theme
  :ensure t
  :disabled t
  :defer 2
  :config
  (load-theme 'spacemacs-dark t)
  )

(provide 'init-color-theme)
