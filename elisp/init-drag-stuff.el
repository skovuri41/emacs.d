(use-package drag-stuff
  :diminish drag-stuff-mode
  :init (drag-stuff-global-mode 1)
  :bind (("M-N" . drag-stuff-down)
         ("M-P" . drag-stuff-up)))

(use-package move-text
  :config
  ;; (move-text-default-bindings)
  )

(provide 'init-drag-stuff)

