(use-package eval-sexp-fu
  :ensure t
  :init
  (progn
    (setq eval-sexp-fu-flash-duration 0.4)
    (custom-set-faces
     '(eval-sexp-fu-flash
       ((((class color)) (:background "#A0FF76" :foreground "white")))))
    (define-key lisp-interaction-mode-map (kbd "C-c C-c") 'eval-sexp-fu-eval-sexp-inner-list)
    (define-key lisp-interaction-mode-map (kbd "C-c C-e") 'eval-sexp-fu-eval-sexp-inner-sexp)
    (define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-sexp-fu-eval-sexp-inner-list)
    (define-key emacs-lisp-mode-map (kbd "C-c C-e") 'eval-sexp-fu-eval-sexp-inner-sexp)
    ))

(use-package cider-eval-sexp-fu
  :defer t
  )

(provide 'init-eval-sexp-fu)

