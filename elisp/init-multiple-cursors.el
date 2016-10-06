(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click))
  :bind (:map region-bindings-mode-map
              ("a" . mc/mark-all-like-this)
              ("p" . mc/mark-previous-like-this)
              ("n" . mc/mark-next-like-this)
              ("P" . mc/unmark-previous-like-this)
              ("N" . mc/unmark-next-like-this)
              ("[" . mc/cycle-backward)
              ("]" . mc/cycle-forward)
              ("m" . mc/mark-more-like-this-extended)
              ("h" . mc-hide-unmatched-lines-mode)
              ("\\" . mc/vertical-align-with-space)
              ("#" . mc/insert-numbers) ; use num prefix to set the starting number
              ("^" . mc/edit-beginnings-of-lines)
              ("$" . mc/edit-ends-of-lines))
  :init
  (progn
    (setq mc/list-file (locate-user-emacs-file "mc-lists"))))

(provide 'init-multiple-cursors)
