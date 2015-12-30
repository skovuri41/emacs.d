;; highlight the current line when you have been idle for a while
(use-package hl-line+
  :ensure t
  :init (progn
          (global-hl-line-mode -1)
          (toggle-hl-line-when-idle +1)))


(provide 'init-hl-line)
 
