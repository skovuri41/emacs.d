;; which-key
(use-package which-key
  :diminish which-key-mode
  :init
  (progn
    ;;(which-key-setup-minibuffer)
    ;;(which-key-setup-side-window-bottom)
    (which-key-setup-side-window-right-bottom)
    (which-key-mode 1)))	

(provide 'init-whichkey)
