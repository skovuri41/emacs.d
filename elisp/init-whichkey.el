;; which-key
(use-package which-key
  :diminish which-key-mode
  :init
  (progn
    ;; (which-key-setup-minibuffer)
    ;; (which-key-setup-side-window-bottom)
    (which-key-setup-side-window-right-bottom)
    ;; (which-key-setup-side-window-right)
    ;; (setq which-key-popup-type 'side-window)
    ;; (setq which-key-side-window-location '(right,bottom))
    ;; (setq which-key-side-window-max-width 0.33)
    ;; (setq which-key-side-window-max-height 0.25)
    (setq which-key-sort-order 'which-key-prefix-then-key-order)
    ;; (setq which-key-max-description-length nil)
    ;; (which-key-add-key-based-replacements
    ;;   "SPC w" "Windows"
    ;;   "SPC p" "Project"
    ;;   "SPC f" "Files"
    ;;   "SPC b" "Buffer")
    ;; (which-key-add-key-based-replacements
    ;; "SPC +" "Add entry to journal"
    ;; "SPC =" "View today's journal")
    (which-key-mode 1)))

(provide 'init-whichkey)
