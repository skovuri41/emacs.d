(use-package super-save
  :ensure t
  :diminish super-save-mode
  :config
  (setq super-save-auto-save-when-idle t
        super-save-idle-duration 15
        ;; super-save-triggers (append super-save-triggers '("magit-status"))
        )
  ;; (add-to-list 'super-save-triggers "eyebrowse-switch-to-window-config")
  ;; (add-to-list 'super-save-triggers "completing-read")
  ;; (add-to-list 'super-save-triggers "ivy--read")
  (super-save-mode 1)
  ;; you can probably switch off the built-in auto-save-mode (unless you really care about its backups):
  (setq auto-save-default nil)
  )


(provide 'init-super-save)
;;; init-super-save.el ends here

