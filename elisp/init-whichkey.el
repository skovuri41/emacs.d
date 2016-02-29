;; which-key
(use-package which-key
  :diminish which-key-mode
  :init
  (progn
    ;;(which-key-setup-minibuffer)
    ;;(which-key-setup-side-window-bottom)
    (which-key-setup-side-window-right-bottom)
    (setq which-key-max-description-length nil)
    (which-key-add-key-based-replacements
      "SPC W" "Workgroups"
      "SPC w" "Windows"
      "SPC p" "Project"
      "SPC g" "Magit"
      "SPC f" "Files"
      "SPC m" "Mode"
      "SPC b" "Buffer"
      "SPC /" "Comment"
      "SPC h" "Helm")
    (which-key-mode 1)))

(provide 'init-whichkey)
