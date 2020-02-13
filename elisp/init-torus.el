(use-package torus
  :ensure t
  :demand
  :bind-keymap ("s-t" . torus-map)
  :bind
  (("C-^" . torus-alternate)
   ("s-/" . torus-search-history)
   :map torus-map
   ("t" . torus-copy-to-circle))
  :hook ((emacs-startup . torus-start)
	 (kill-emacs . torus-quit))
  :custom
  ((torus-prefix-key "s-t")
   (torus-binding-level 3)
   (torus-verbosity 2)
   (torus-dirname "~/.emacs.d/torus/")
   (torus-load-on-startup t)
   (torus-save-on-exit t)
   (torus-autoread-file "~/.emacs.d/torus/last.el")
   (torus-autowrite-file torus-autoread-file)
   (torus-history-maximum-elements 30)
   (torus-maximum-horizontal-split 3)
   (torus-maximum-vertical-split 4)
   ;; (torus-display-tab-bar t)
   (torus-display-tab-bar nil)
   (torus-separator-torus-circle " >> ")
   (torus-separator-circle-location " > ")
   (torus-prefix-separator "/")
   (torus-join-separator " & "))
  :config
  (torus-init)
  (torus-install-default-bindings))


(provide 'init-torus)
