(use-package window-numbering
  :ensure t
  :commands (window-numbering-mode)
  :config
  (defun window-numbering-install-mode-line (&optional ignored))
  :init
  (add-hook 'after-init-hook 'window-numbering-mode))


(provide 'init-window-numbering)
