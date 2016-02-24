;; which-key
(use-package which-key
  :diminish which-key-mode
  :init
  (progn
    ;;(which-key-setup-minibuffer)
    ;;(which-key-setup-side-window-bottom)
    (which-key-setup-side-window-right-bottom)
    (setq which-key-max-description-length nil)
    (which-key-mode 1)))

(provide 'init-whichkey)
