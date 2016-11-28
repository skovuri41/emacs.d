(use-package golden-ratio
  :ensure t
  :diminish golden-ratio-mode
  :config
  (setq golden-ratio-exclude-modes
        '("calendar--mode" "sr-mode" "neotree-mode" "ediff-mode" "gnus-summary-mode" "ranger-mode" "dired-mode" ))
  (setq golden-ratio-extra-commands
        (append golden-ratio-extra-commands
                '(ivy-switch-buffer
                  cider-popup-buffer-quit-function
                  ace-swap-window
                  aw-flip-window
                  ace-window
                  ace-delete-window
                  ace-select-window
                  ace-maximize-window
                  avy-pop-mark
                  quit-window
                  select-window-0
                  select-window-1
                  select-window-2
                  select-window-3
                  select-window-4
                  select-window-5
                  select-window-6
                  select-window-7
                  select-window-8
                  select-window-9
                  pdf-outline-follow-link
                  pdf-outline-select-pdf-window)))
  (add-to-list 'golden-ratio-exclude-buffer-regexp "^\\*Ilist\\*")
  (setq golden-ratio-recenter t)
  (golden-ratio-mode 1))

(use-package golden-ratio-scroll-screen
  :ensure t
  :disabled t
  :config
  (global-set-key [remap scroll-down-command] 'golden-ratio-scroll-screen-down)
  (global-set-key [remap scroll-up-command] 'golden-ratio-scroll-screen-up))


(provide 'init-golden-ratio)
