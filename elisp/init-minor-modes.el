(use-package yaml-mode
  :mode ("\\.yml$" . yaml-mode))

(use-package markdown-mode
  :mode (("\\.markdown$" . markdown-mode)
         ("\\.md$" . markdown-mode)))
(use-package json-mode
  :defer t)

(use-package js2-mode
  :defer t)

(use-package scratch
  :ensure t)

;;;; Modes ;;;;
(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)
(add-hook 'clojure-mode-hook 'prettify-symbols-mode)
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))


(use-package with-editor
  :ensure t
  :init
  (progn
    (add-hook 'shell-mode-hook  'with-editor-export-editor)
    (add-hook 'eshell-mode-hook 'with-editor-export-editor)))

(use-package smartscan
  :ensure t
  :init
  (add-hook #'prog-mode-hook #'smartscan-mode)
  :config
  (define-key smartscan-map (kbd "M-n") nil)
  (define-key smartscan-map (kbd "M-p") nil)
  (define-key smartscan-map (kbd "M-'") nil)
  (define-key evil-normal-state-map "gn" 'smartscan-symbol-go-forward)
  (define-key evil-normal-state-map "gp" 'smartscan-symbol-go-backward))

;;; Visible mark
(use-package visible-mark
  :ensure t
  ;; :init
  ;; (defface visible-mark-active
  ;;   '((((type tty) (class mono)))
  ;;     (t (:background "magenta"))) "")
  :config
  (setq visible-mark-max 2)
  (setq visible-mark-faces `(visible-mark-face1 visible-mark-face2))
  (global-visible-mark-mode 1))

(use-package bug-hunter                            ; Search init file for bugs
  :ensure t)

(provide 'init-minor-modes)
