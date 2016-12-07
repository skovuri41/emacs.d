(use-package quickrun
  :ensure t
  :commands (quickrun
             quickrun-region
             quickrun-with-arg
             quickrun-shell
             quickrun-compile-only
             quickrun-replace-region
             helm-quickrun)
  :init (add-hook 'quickrun/mode-hook 'linum-mode)
  :config
  (setq quickrun-focus-p nil)
  (setq quickrun-focus-p nil)

  (defun doom|quickrun-after-run ()
    "Ensures window is scrolled to BOF"
    (with-selected-window (get-buffer-window quickrun/buffer-name)
  ;; Ensures window is scrolled to BOF
  (add-hook 'quickrun-after-run-hook 'doom|quickrun-after-run)
  (add-to-list 'quickrun-file-alist '("\\.gvy$" . "groovy")))
(provide 'init-quickrun)
