(use-package quickrun
  :ensure t
  :commands (quickrun
             quickrun-region
             quickrun-with-arg
             quickrun-shell
             quickrun-compile-only
             quickrun-replace-region
             helm-quickrun)
  :bind
  (:map evil-leader--default-map
        ("qr" . quickrun)
        ("qR" . quickrun-with-arg))
  :config
  (setq quickrun-focus-p nil)
  (setq quickrun-focus-p nil)
  (add-to-list 'quickrun-file-alist '("\\.gvy$" . "groovy")))
(provide 'init-quickrun)
