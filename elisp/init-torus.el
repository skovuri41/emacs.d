(use-package torus
  :demand
  :bind-keymap ("s-t" . torus-map)
  :bind
  (("C-^" . torus-alternate)
   ("s-/" . torus-search-history)
   :map torus-map
   ("t" . torus-copy-to-circle))
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
   (torus-prefix-separator " : ")
   (torus-join-separator " - "))
  :config
  (torus-init)
  (torus-install-default-bindings))

(provide 'init-torus)
