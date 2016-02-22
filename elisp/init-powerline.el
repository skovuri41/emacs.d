(use-package powerline
  :ensure t
  :init
  (progn
    (require 'powerline)
    ;; (powerline-center-evil-theme)
    (use-package powerline-evil
      :config
      (progn
        (powerline-evil-vim-color-theme)))))

;; (powerline-default-theme)
;; (powerline-vim-theme)
;; (powerline-nano-theme)
;; (powerline-center-evil-theme)
(provide 'init-powerline)
