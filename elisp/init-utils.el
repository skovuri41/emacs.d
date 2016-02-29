(use-package symon
  :if window-system
  :disabled t
  :init
  (setq symon-refresh-rate 2
        symon-delay 5)
  (symon-mode 1)
  :config
  (setq symon-sparkline-type 'bounded))

(use-package vlf-setup)


(provide 'init-utils)
