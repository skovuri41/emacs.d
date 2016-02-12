
(use-package ido
  :config
  (progn
    ;;(evil-leader/set-key "bs" 'ido-switch-buffer)
    (evil-leader/set-key "bk" 'ido-kill-buffer)
    (evil-leader/set-key "bn" 'xah-new-empty-buffer)
    (evil-leader/set-key "ff" 'ido-find-file)))

(use-package flx-ido
  :init (flx-ido-mode 1)
  :config (setq ido-use-faces nil))

(use-package ido-vertical-mode
  :init (ido-vertical-mode 1)
  :config
  (progn
    (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)))
;(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)  
(use-package smex
  :defer t
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands))
  :config
  (progn
        (smex-initialize)))
(provide 'init-ido)
