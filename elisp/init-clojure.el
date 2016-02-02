(use-package clojure-mode
  :mode (("\\.edn$" . clojure-mode)
         ("\\.cljc$" . clojure-mode)
         ("\\.clj$" . clojure-mode))
  :config
  (progn
    (add-hook 'clojure-mode-hook #'smartparens-strict-mode)

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
        (cider-switch-to-relevant-repl-buffer)))

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

    (evil-leader/set-key "eb" 'cider-eval-buffer)
    (evil-leader/set-key "ee" 'cider-eval-last-sexp)
    (evil-leader/set-key "er" 'cider-eval-region)
    (evil-leader/set-key "ef" 'cider-eval-defun-at-point)
    (evil-leader/set-key "es" 'cider-send-and-evaluate-sexp)

    (evil-leader/set-key "cd" 'cider-doc)
    (evil-leader/set-key "cc" 'cider-connect)
    (evil-leader/set-key "ct" 'cider-test-run-tests)
    (evil-leader/set-key "cr" 'toggle-nrepl-buffer)
    (evil-leader/set-key "cf" 'cider-save-and-refresh)))

(use-package cider
  :commands (cider cider-connect cider-jack-in)
  :init
  (progn
    (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
    (add-hook 'cider-repl-mode-hook 'company-mode)
    (add-hook 'cider-mode-hook 'company-mode)
    (add-hook 'cider-repl-mode-hook 'subword-mode))
  :config
  (progn
    (setq nrepl-hide-special-buffers t)
    (setq cider-popup-stacktraces-in-repl t)
    (setq cider-repl-history-file "~/.emacs.d/nrepl-history")
    (setq cider-repl-pop-to-buffer-on-connect nil)
    (setq cider-repl-use-clojure-font-lock nil)
    (setq cider-auto-select-error-buffer t)
    (setq cider-show-error-buffer t)
    (setq cider-repl-use-clojure-font-lock t)
    (setq nrepl-hide-special-buffers t)
    (setq nrepl-popup-stacktraces nil)
    (setq cider-prompt-save-file-on-load nil)
    (setq cider-refresh-before-fn "reloaded.repl/suspend")
    (setq cider-refresh-after-fn "reloaded.repl/resume")))

(use-package typed-clojure-mode
  :init
  (add-hook 'clojure-mode-hook 'typed-clojure-mode)
  :config
  (progn
    (evil-leader/set-key "tc" 'typed-clojure-check-ns)))

(use-package clj-refactor
  :init
  (add-hook 'clojure-mode-hook (lambda () (clj-refactor-mode 1)))
  :config
  (progn
    (evil-leader/set-key "rai" 'cljr-add-import-to-ns)
    (evil-leader/set-key "rar" 'cljr-add-require-to-ns)
    (evil-leader/set-key "rau" 'cljr-add-use-to-ns)
    (evil-leader/set-key "rrr" 'cljr-remove-unused-requires)
    (evil-leader/set-key "rsn" 'cljr-sort-ns)
    (evil-leader/set-key "rtf" 'cljr-thread-first-all)
    (evil-leader/set-key "rtl" 'cljr-thread-last-all)
    (evil-leader/set-key "rcc" 'cljr-cycle-coll)
    (evil-leader/set-key "rcp" 'cljr-cycle-privacy)
    (evil-leader/set-key "rcs" 'clojure-toggle-keyword-string)
    (evil-leader/set-key "rfe" 'cljr-create-fn-from-example)))

(use-package ob-clojure
  :init
  (setq org-babel-clojure-backend 'cider))

(provide 'init-clojure)
