(use-package smooth-scrolling
  :defer t
  :ensure t
  :init
  (smooth-scrolling-mode)
  :config
  (setq smooth-scroll-margin 6
        scroll-margin 3
        scroll-step 1
        scroll-conservatively 101
        scroll-preserve-screen-position t
        auto-window-vscroll nil))

(provide 'init-smooth-scrolling)
