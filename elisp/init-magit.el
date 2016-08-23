(use-package magit
  :ensure t
  :commands (magit-status
             magit-log
             magit-commit
             magit-stage-file)
  :config
  (progn
    (add-hook 'magit-mode-hook 'xah-fly-insert-mode-activate)
    (add-hook 'magit-status-mode-hook 'xah-fly-insert-mode-activate)

    (dolist (map (list magit-status-mode-map
                       magit-log-mode-map
                       magit-diff-mode-map
                       magit-staged-section-map))
      (define-key map "j" 'magit-section-forward)
      (define-key map "k" 'magit-section-backward)
      (define-key map "n" nil)
      (define-key map "p" nil)
      (define-key map "v" 'recenter-top-bottom)
      (define-key map "i" 'magit-section-toggle))

    ;; (eval-after-load 'magit-blame
    ;;   '(progn
    ;;      (define-key magit-blame-mode-map "n" nil)
    ;;      (define-key magit-blame-mode-map "p" nil)
    ;;      (define-key magit-blame-mode-map "j" 'magit-blame-next-chunk)
    ;;      (define-key magit-blame-mode-map "k" 'magit-blame-previous-chunk)))

    ;; ;;* Maps
    ;; ;;** Status
    ;; (define-key magit-status-mode-map "j" 'magit-goto-next-section)
    ;; (define-key magit-status-mode-map "k" 'magit-goto-previous-section)
    ;; (define-key magit-status-mode-map "h" 'magit-goto-previous-section)
    ;; (define-key magit-status-mode-map "\C-k" 'magit-discard-item)
    ;; (define-key magit-status-mode-map "\C-d" 'magit-discard-item)
    ;; (define-key magit-status-mode-map "d" 'magit-discard-item)
    ;; (define-key magit-status-mode-map "i" 'magit-toggle-section)
    ;; (define-key magit-status-mode-map (kbd "M-m") 'lispy-mark-symbol)
    ;; ;; (define-key magit-status-mode-map "C" 'ora-magit-commit-add-log)
    ;; (define-key magit-status-mode-map "v" (lambda () (interactive) (magit-visit-item t)))
    ;; (define-key magit-status-mode-map "V" 'projectile-find-file)
    ;; ;; (define-key magit-status-mode-map "h" 'ora-magit-find-main-file)

    ;; ;;** Log
    ;; (define-key magit-log-mode-map "j" 'magit-goto-next-section)
    ;; (define-key magit-log-mode-map "k" 'magit-goto-previous-section)
    ;; ;; (define-key magit-log-mode-map (kbd "M-w") 'ora-magit-copy-item-as-kill)
    ;; ;; (define-key magit-log-mode-map "n" 'ora-magit-copy-item-as-kill)
    ;; ;; (define-key magit-log-mode-map "v" 'ora-magit-visit)
    ;; ;; (define-key magit-log-mode-map "o" 'ora-magit-visit-item-other-window)

    ;; ;;** Commit
    ;; (define-key magit-commit-mode-map "i" 'magit-toggle-section)
    ;; (define-key magit-commit-mode-map "j" 'magit-goto-next-section)
    ;; (define-key magit-commit-mode-map "k" 'magit-goto-previous-section)
    ;; ;; (define-key magit-commit-mode-map "n" 'ora-magit-copy-item-as-kill)
    ;; ;; (define-key magit-commit-mode-map "C" 'ora-magit-commit-add-log)
    ;; ;; (define-key magit-commit-mode-map "o" 'ora-magit-visit-item-other-window)

    ;; ;;** Diff
    ;; (define-key magit-diff-mode-map "i" 'magit-toggle-section)
    ;; (define-key magit-diff-mode-map "j" 'magit-goto-next-section)
    ;; (define-key magit-diff-mode-map "k" 'magit-goto-previous-section)
    ;; (define-key magit-diff-mode-map "o"
    ;;   (lambda () (interactive) (magit-visit-item t)))

    ;; ;;** Manager
    ;; (define-key magit-branch-manager-mode-map "j" 'magit-goto-next-section)
    ;; (define-key magit-branch-manager-mode-map "k" 'magit-goto-previous-section)
    ;; (define-key magit-branch-manager-mode-map "d" 'magit-discard-item)
    ;; (define-key magit-branch-manager-mode-map "u" 'magit-diff-working-tree)


    ))

(provide 'init-magit)
