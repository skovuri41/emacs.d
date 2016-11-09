(use-package guide-key
  :init (guide-key-mode 1)
  :disabled t
  :config
  (progn
    (setq guide-key/idle-delay 1)
    (setq guide-key/recursive-key-sequence-flag t)
    (setq guide-key/popup-window-position 'bottom)
    ))
(provide 'init-guidekey)


