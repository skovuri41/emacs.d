(use-package fancy-narrow
  :config
  (evil-leader/set-key
    "nr" 'fancy-narrow-to-region
    "np" 'fancy-narrow-to-page
    "nf" 'fancy-narrow-to-defun
    "nw" 'fancy-widen))

(provide 'init-fancy-narrow)
