(use-package beacon
  ;; Whenever the window scrolls a light will shine on top of your
  ;; cursor so you know where it is.
  ;; Homepage: https://github.com/Malabarba/beacon
  :ensure t
  :diminish beacon-mode
  :init
  (progn
    (setq  beacon-blink-when-buffer-changes t
           beacon-blink-when-window-changes t
           beacon-blink-when-window-scrolls t
           beacon-blink-duration 1)
    (beacon-mode 1)))
(provide 'init-beacon)
