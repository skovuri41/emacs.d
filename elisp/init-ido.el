(use-package ido
  :config
  (progn
    ;;(evil-leader/set-key "bs" 'ido-switch-buffer)
    (evil-leader/set-key "bk" 'ido-kill-buffer)
    (evil-leader/set-key "ff" 'ido-find-file)
    ;; Use the current window when visiting files and buffers with ido
    (setq ido-default-file-method 'selected-window)
    (setq ido-default-buffer-method 'selected-window)

    ))

(use-package flx-ido
  :init (flx-ido-mode 1)
  :config
  (setq ido-use-faces nil)
  ;; disable ido faces to see flx highlights.
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil)
  )

(use-package ido-vertical-mode
  :init (ido-vertical-mode 1)
  :config
  (progn
    (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)))

(use-package smex
  :defer t
  :bind (
         ;; ("M-x" . smex)
         ("M-X" . smex-major-mode-commands))
  :config
  (progn
    (smex-initialize)))

(provide 'init-ido)
