(use-package quickrun
  :ensure t
  :commands (quickrun
             quickrun-region
             quickrun-with-arg
             quickrun-shell
             quickrun-compile-only
             quickrun-replace-region
             helm-quickrun)
  :config
  (setq quickrun-focus-p nil)
  (setq quickrun-focus-p nil)
  (add-to-list 'quickrun-file-alist '("\\.gvy$" . "groovy")))
(provide 'init-quickrun)
