(use-package super-save
  :ensure t
  :diminish super-save-mode
  :config
  (setq super-save-auto-save-when-idle t
        super-save-idle-duration 5
        super-save-triggers (append super-save-triggers '("magit-status")))
  (add-to-list 'super-save-triggers "evil-insert-state-exit-hook")
  (super-save-mode 1)
  )


(provide 'init-super-save)
;;; init-super-save.el ends here

