(use-package drag-stuff
  :init (drag-stuff-global-mode 1)
  :diminish drag-stuff-mode
  :bind (("M-N" . drag-stuff-down)
         ("M-P" . drag-stuff-up)))

(provide 'init-drag-stuff)

