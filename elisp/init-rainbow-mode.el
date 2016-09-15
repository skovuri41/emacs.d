;;;; rainbow-mode
;; Colorize color names in buffers
(use-package rainbow-mode
  :diminish rainbow-mode
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode)
  ;; (dolist (hook '(css-mode-hook
  ;;                 html-mode-hook
  ;;                 js-mode-hook
  ;;                 emacs-lisp-mode-hook
  ;;                 org-mode-hook
  ;;                 text-mode-hook
  ;;                 ))
  ;;   (add-hook hook 'rainbow-mode))
  )

(provide 'init-rainbow-mode)
