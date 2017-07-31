(use-package smooth-scrolling
  :defer t
  :ensure t
  :init
  (smooth-scrolling-mode)
  :config
  (setq smooth-scroll-margin 6
        scroll-margin 3
        scroll-step 1
        scroll-conservatively 10000
        scroll-preserve-screen-position 1
        scroll-up-aggressively 0.01
        scroll-down-aggressively 0.01
        auto-window-vscroll nil))

(provide 'init-smooth-scrolling)
