(use-package ag
  :ensure t
  :commands (ag ag-mode ag-files ag-regexp)
  :init
  (progn
    (setq ag-highlight-search t)
    (setq ag-reuse-buffers t)
    (defun setup-ag ()
      "Function called to set my ag stuff up."
      (toggle-truncate-lines t)
      (linum-mode 0)
      (switch-to-buffer-other-window "*ag search*")
      )
    (add-hook 'ag-mode-hook 'setup-ag)
    (after 'evil
      (evil-set-initial-state 'ag-mode 'normal)
      ;; unbind kill window
      )
    )
  (progn
    ;;(define-key ag-mode-map (kbd "k") 'nil)
    (add-hook 'ag-mode-hook
              (lambda ()
                (wgrep-ag-setup)))))
(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t))

(use-package wgrep-ag
  :ensure t
  :commands (wgrep-ag-setup))

(provide 'init-ag)
