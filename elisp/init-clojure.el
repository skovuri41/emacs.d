(use-package clojure-mode
  :mode (("\\.edn$" . clojure-mode)
         ("\\.cljc$" . clojure-mode)
         ("\\.clj$" . clojure-mode))
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))
    (add-to-list 'magic-mode-alist '(".* boot" . clojure-mode))

    (use-package clj-refactor
      :init
      (add-hook 'clojure-mode-hook (lambda () (clj-refactor-mode 1)))
      )

    (define-clojure-indent
      (defroutes 'defun)
      (GET 2)
      (POST 2)
      (PUT 2)
      (DELETE 2)
      (HEAD 2)
      (ANY 2)
      (context 2)
      (let-routes 1))

    (define-clojure-indent
      (form-to 1))

    (define-clojure-indent
      (match 1)
      (are 2)
      (checking 2)
      (async 1))

    (define-clojure-indent
      (select 1)
      (insert 1)
      (update 1)
      (delete 1))

    (define-clojure-indent
      (run* 1)
      (fresh 1))

    (define-clojure-indent
      (extend-freeze 2)
      (extend-thaw 1))

    (define-clojure-indent
      (go-loop 1))

    (define-clojure-indent
      (this-as 1)
      (specify 1)
      (specify! 1))

    (setq clojure--prettify-symbols-alist
          '(("fn" . ?λ)
            ("not=" . ?≠)
            ("identical?" . ?≡)))

    (defun toggle-nrepl-buffer ()
      "Toggle the nREPL REPL on and off"
      (interactive)
      (if (string-match "cider-repl" (buffer-name (current-buffer)))
          (delete-window)
        (cider-switch-to-repl-buffer)))

    (defun cider-save-and-refresh ()
      (interactive)
      (save-buffer)
      (call-interactively 'cider-refresh))

    (defun cider-send-and-evaluate-sexp ()
      "Sends the s-expression located before the point or the active
       region to the REPL and evaluates it. Then the Clojure buffer is
       activated as if nothing happened."
      (interactive)
      (if (not (region-active-p))
          (cider-insert-last-sexp-in-repl)
        (cider-insert-in-repl
         (buffer-substring (region-beginning) (region-end)) nil))
      (cider-switch-to-repl-buffer)
      (cider-repl-closing-return)
      (cider-switch-to-last-clojure-buffer)
      (message ""))


    (defun clj-mode-keys-setup ()
      "for 'clojure mode'"

      (bind-keys :map clojure-mode-map
                 ("C-x C-e" . cider-eval-last-sexp)
                 ("C-c C-r" . cider-repl-reset)
                 ("C-c C-v" . cider-send-and-evaluate-sexp))

      (bind-keys :prefix-map local-clojure-mode-keymap
                 :prefix "C-c j"
                 ;; ("e b"  . cider-eval-buffer)
                 ;; ("e e"  . cider-eval-last-sexp)
                 ;; ("e r"  . cider-eval-region)
                 ;; ("e f"  . cider-eval-defun-at-point)
                 ;; ("e s"  . cider-send-and-evaluate-sexp)
                 ;; ("c t"  . cider-test-run-tests)
                 ;; ("c d"  . cider-doc)
                 ("e" . cider-eval-commands-map)
                 ("t" . cider-test-commands-map)
                 ;; ("d" . hydra-clojure-docs/body)
                 ;; ("d" . cider-doc-map)
                 ("c c" . cider-connect)
                 ("c j" . cider-jack-in)
                 ("c l" . cider-jack-in-clojurescript)
                 ("c f" . cider-save-and-refresh)
                 ("c r" . toggle-nrepl-buffer)
                 ("r" . hydra-cljr-help-menu/body)
                 ;; ("rai" . cljr-add-import-to-ns)
                 ;; ("rar" . cljr-add-require-to-ns)
                 ;; ("rau" . cljr-add-use-to-ns)
                 ;; ("rrr" . cljr-remove-unused-requires)
                 ;; ("rsn" . cljr-sort-ns)
                 ;; ("rtf" . cljr-thread-first-all)
                 ;; ("rtl" . cljr-thread-last-all)
                 ;; ("rcc" . cljr-cycle-coll)
                 ;; ("rcp" . cljr-cycle-privacy)
                 ;; ("rcs" . clojure-toggle-keyword-string)
                 ;; ("rfe" . cljr-create-fn-from-example)
                 ))
    (add-hook 'clojure-mode-hook #'clj-mode-keys-setup)))

(use-package cider
  :commands (cider cider-connect cider-jack-in)
  :init
  (progn
    (add-hook 'cider-mode-hook #'eldoc-mode)
    (add-hook 'cider-repl-mode-hook #'eldoc-mode)
    (add-hook 'cider-repl-mode-hook 'company-mode)
    (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'cider-mode-hook 'company-mode)
    (add-hook 'cider-repl-mode-hook 'subword-mode))
  :config
  (progn
    (setq cider-repl-history-file "~/.emacs.d/nrepl-history")
    (setq cider-repl-pop-to-buffer-on-connect nil)
    ;; (setq cider-repl-use-clojure-font-lock nil)
    (setq cider-repl-use-clojure-font-lock t)
    (setq cider-repl-pop-to-buffer-on-connect nil)
    (setq cider-repl-wrap-history t)
    (setq cider-repl-history-size 1000)
    (setq cider-repl-shortcut-dispatch-char ?\;)
    (setq cider-repl-result-prefix ";; => ")
    (setq cider-auto-select-error-buffer t)
    (setq cider-show-error-buffer t)
    (setq nrepl-popup-stacktraces nil)
    (setq nrepl-hide-special-buffers t)
    (setq nrepl-buffer-name-show-port t)
    (setq nrepl-buffer-name-separator "-")
    (setq cider-prompt-save-file-on-load nil)
    (setq cider-popup-stacktraces-in-repl t)
    (setq cider-prefer-local-resources t)
    (setq cider-interactive-eval-result-prefix ";; => ")
    (setq cider-stacktrace-fill-column 80)
    (setq cider-test-show-report-on-success t)
    (require 'cider-buddy)
    ))

(use-package typed-clojure-mode
  :disabled t
  :config
  (add-hook 'clojure-mode-hook 'typed-clojure-mode))

(use-package flycheck-clojure
  :ensure t
  :defer t
  :config
  (eval-after-load 'flycheck '(flycheck-clojure-setup)))

(use-package 4clojure
  :ensure t
  :defer t)

(use-package cider-eval-sexp-fu
  :ensure t
  :config
  (progn
    (defun config-init-cider-eval-sexp-fu ()
      (require 'cider-eval-sexp-fu))
    (add-hook 'cider-mode-hook 'config-init-cider-eval-sexp-fu)))

;;; clojurescript (build from emacs, and pop up stacktrack when
;;; there's a error):
(use-package cljsbuild-mode
  :diminish cljsbuild-mode)

(use-package html-to-hiccup
  :ensure t
  :config
  ;; (define-key clojure-mode-map (kbd "H-h") 'html-to-hiccup-convert-region)
  )

(provide 'init-clojure)
