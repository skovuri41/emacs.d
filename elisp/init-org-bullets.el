(use-package org-bullets
  :ensure t
  :init
  ;; (setq org-bullets-bullet-list
  ;;       '("►" "◉" "★" "○" "◇" "◉" "○" ))
  (setq org-bullets-bullet-list '("■" "►" "◆" "▶"))
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(provide 'init-org-bullets)
