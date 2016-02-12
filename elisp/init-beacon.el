(use-package beacon
  ;; Whenever the window scrolls a light will shine on top of your
  ;; cursor so you know where it is.
  ;; Homepage: https://github.com/Malabarba/beacon
  :ensure t
  :diminish beacon-mode
  :init
  (progn
    (beacon-mode)))
(provide 'init-beacon)
