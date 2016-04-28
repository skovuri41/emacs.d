(use-package symon
  :ensure t
  :init
  (setq symon-refresh-rate 2
        symon-delay 5)
  (symon-mode 1)
  :config
  (setq symon-sparkline-type 'bounded))

(use-package vlf-setup)

(use-package typit
  :ensure t)

(provide 'init-utils)
