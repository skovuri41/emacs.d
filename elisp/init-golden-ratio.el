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

(use-package golden-ratio-scroll-screen
  :ensure t
  :config
  (global-set-key [remap scroll-down-command] 'golden-ratio-scroll-screen-down)
  (global-set-key [remap scroll-up-command] 'golden-ratio-scroll-screen-up)
  (evil-declare-motion 'gold-ratio-scroll-screen-down)
  (evil-declare-motion 'gold-ratio-scroll-screen-up))


(provide 'init-golden-ratio)
