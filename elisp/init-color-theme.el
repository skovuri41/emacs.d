;; Fully unload previous theme before loading a new one
(defadvice load-theme
  (before theme-dont-propagate activate)
    (mapc #'disable-theme custom-enabled-themes))



(use-package gruvbox-theme
  :ensure t
  :config (load-theme 'gruvbox 'no-confirm))

(provide 'init-color-theme)
