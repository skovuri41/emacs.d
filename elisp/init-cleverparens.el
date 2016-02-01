(use-package evil-cleverparens
      :interpreter ("evil-cleverparens" . evil-cleverparens-mode) 
      :init
      (add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode)
      (add-hook 'clojure-mode-hook #'evil-cleverparens-mode)
      :config
      (progn 
        (defun support-iterm2 ()
          (defvar evil-cp-additional-bindings-local
            '(("M-t" . sp-transpose-sexp)
              ("M-k" . evil-cp-drag-backward)
              ("M-j" . evil-cp-drag-forward)
              ("M-J" . sp-join-sexp)
              ("M-s" . sp-splice-sexp)
              ("M-S" . sp-split-sexp)
              ("M-R" . evil-cp-raise-form)
              ("M-r" . sp-raise-sexp)
              ("M-a" . evil-cp-insert-at-end-of-form)
              ("M-i" . evil-cp-insert-at-beginning-of-form)
              ("M-w" . evil-cp-copy-paste-form)
              ("M-y" . evil-cp-yank-sexp)
              ("M-d" . evil-cp-delete-sexp)
              ("M-c" . evil-cp-change-sexp)
              ("M-Y" . evil-cp-yank-enclosing)
              ("M-D" . evil-cp-delete-enclosing)
              ("M-C" . evil-cp-change-enclosing)
              ("M-q" . sp-indent-defun)
              ("M-o" . evil-cp-open-below-form)
              ("M-O" . evil-cp-open-above-form)
              ("M-v" . sp-convolute-sexp)
              ("M-(" . evil-cp-wrap-next-round)
              ("M-)" . evil-cp-wrap-previous-round)
              ("M-[" . evil-cp-wrap-next-square)
              ;;("M-]" . evil-cp-wrap-previous-square)
              ("M-{" . evil-cp-wrap-next-curly)
              ("M-}" . evil-cp-wrap-previous-curly)
              )
            "Alist containing additional functionality for
  evil-cleverparens via a modifier key (using the meta-key by
  default). Only enabled in evil's normal mode.")

          (evil-cp--populate-mode-bindings-for-state
           evil-cp-additional-bindings 'normal nil)
          (evil-cp--populate-mode-bindings-for-state
           evil-cp-additional-bindings-local 'normal
           evil-cleverparens-use-additional-bindings)
          (message "suppport-iterm2")
          )
        (support-iterm2)
        (evil-define-key 'normal evil-cleverparens-mode-map  (kbd "M-w") nil)
        (evil-define-key 'normal evil-cleverparens-mode-map  (kbd "M-[") nil)
        )
      )
(provide 'init-cleverparens)
