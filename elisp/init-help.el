(use-package discover-my-major
  :ensure t
  :init
  ;;(evil-leader/set-key "Hm" 'discover-my-major)
  ;;(evil-leader/set-key "Hh" 'help)
  ;; (evil-leader/set-key "Hf" 'describe-function)
  ;;  (evil-leader/set-key "Hw" 'popup-which-function)
  ;; (evil-leader/set-key "Hk" 'describe-key)
  ;; (evil-leader/set-key "Hv" 'describe-variable)
  )

(require 'help-mode)

(define-key help-mode-map "j" 'next-line)
(define-key help-mode-map "k" 'previous-line)
(define-key help-mode-map "H" 'describe-mode)
(define-key help-mode-map "h" 'backward-char)
(define-key help-mode-map "L" 'help-go-back)
(define-key help-mode-map "l" 'forward-char)
(define-key help-mode-map "v" 'recenter-top-bottom)
(define-key help-mode-map (kbd "C-M-i") nil)
(define-key help-mode-map "c" 'counsel-ace-link)

(use-package help-fns+
  :ensure t
  :init
  (autoload 'describe-keymap "help-fns+"))

(use-package know-your-http-well :ensure t :defer t)

(provide 'init-help)
