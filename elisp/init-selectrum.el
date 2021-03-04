;; -*- lexical-binding: t; -*-

(use-package selectrum
  :ensure t
  :straight t
  :bind (("C-x M-r" . selectrum-repeat))
  :init (selectrum-mode))

(use-package selectrum-prescient
  :ensure t
  :after selectrum
  :config (selectrum-prescient-mode 1))

;; (use-package prescient
;;   :ensure t
;;   :defer t
;;   :config (prescient-persist-mode 1))

;;(use-package prescient
;;  :straight t
;;  :defer t
;;  :config
;;  (setq prescient-sort-length-enable nil)
;;  (setq prescient-save-file (concat user-emacs-directory "prescient-save.el"))
;;  (prescient-persist-mode))

(use-package ivy-prescient
  :straight t
  :after counsel
  :config
  (ivy-prescient-mode)
  (setq ivy-initial-inputs-alist ivy-prescient--old-initial-inputs-alist))

(use-package orderless
  :ensure t
  :after selectrum
  :config (setq orderless-matching-styles '(orderless-regexp
                                            orderless-initialism
                                            orderless-prefixes)
                selectrum-refine-candidates-function #'orderless-filter
                selectrum-highlight-candidates-function #'orderless-highlight-matches))

(use-package embark
  :ensure t
  :bind (("C-c o" . embark-act)
         :map minibuffer-local-map
         ("M-o" . embark-act)
         ("C-M-o" . embark-act-noexit))
  :config (progn
            ;; Source: https://github.com/raxod502/selectrum/wiki/Additional-Configuration#minibuffer-actions-with-embark
            (progn
              (require 'selectrum)
              (defun current-candidate+category ()
                (when selectrum-active-p
                  (cons (selectrum--get-meta 'category)
                        (selectrum-get-current-candidate))))

              (add-hook 'embark-target-finders #'current-candidate+category)

              (defun current-candidates+category ()
                (when selectrum-active-p
                  (cons (selectrum--get-meta 'category)
                        (selectrum-get-current-candidates
                         ;; Pass relative file names for dired.
                         minibuffer-completing-file-name))))

              (add-hook 'embark-candidate-collectors #'current-candidates+category)
              ;; No unnecessary computation delay after injection.
              (add-hook 'embark-setup-hook 'selectrum-set-selected-candidate))))

(use-package marginalia
  :ensure t
  :after selectrum
  :demand t                     ; :demand applies to :bind but not
                                ; :after.  We want to eagerly load
                                ; marginalia once selectrum is loaded.
  :bind (:map minibuffer-local-map
         ("C-o" . marginalia-cycle))
  :config (marginalia-mode 1))

(use-package consult
  :ensure t
  :bind (("C-x C-r" . consult-recent-file)
         ([remap yank-pop] . consult-yank-pop)
         ([remap goto-line] . consult-goto-line)))

;; (use-package consult-selectrum
;;   :ensure t
;;   :after consult)

(provide 'init-selectrum)
