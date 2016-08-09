(use-package eval-sexp-fu
  :ensure t
  :init
  (progn
    (setq eval-sexp-fu-flash-duration 0.4)
    (define-key lisp-interaction-mode-map (kbd "C-c C-c") 'eval-sexp-fu-eval-sexp-inner-list)
    (define-key lisp-interaction-mode-map (kbd "C-c C-e") 'eval-sexp-fu-eval-sexp-inner-sexp)
    (define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-sexp-fu-eval-sexp-inner-list)
    (define-key emacs-lisp-mode-map (kbd "C-c C-e") 'eval-sexp-fu-eval-sexp-inner-sexp)
    )
  :config
  (defface eval-sexp-fu-flash1
    '((((class color)) (:background "#A0FF76" :foreground "white" :bold t))
      (t (:inverse-video t)))
    "Face for highlighting sexps during evaluation."
    :group 'eval-sexp-fu)
  (setq eval-sexp-fu-flash-face 'eval-sexp-fu-flash1))

(provide 'init-eval-sexp-fu)

