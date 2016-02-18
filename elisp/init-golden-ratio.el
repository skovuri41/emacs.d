(use-package golden-ratio
  :ensure t
  :diminish golden-ratio-mode
  :config
  (setq golden-ratio-exclude-modes
        '( "sr-mode" "neotree-mode""ediff-mode" "gnus-summary-mode" ))
  (setq golden-ratio-extra-commands
        (append golden-ratio-extra-commands
                '(evil-window-left
                  evil-window-right
                  evil-window-up
                  evil-window-down
                  ace-window)))
  (golden-ratio-mode 1))
(provide 'init-golden-ratio)
