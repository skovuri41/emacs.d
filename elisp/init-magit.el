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


    (eval-after-load 'magit-blame
      '(progn
         (define-key magit-blame-mode-map "n" nil)
         (define-key magit-blame-mode-map "p" nil)
         (define-key magit-blame-mode-map "j" 'magit-blame-next-chunk)
         (define-key magit-blame-mode-map "k" 'magit-blame-previous-chunk)))

    (define-key magit-refs-mode-map "j" 'magit-section-forward)
    (define-key magit-refs-mode-map "k" 'magit-section-backward)
    (define-key magit-refs-mode-map "i" 'magit-section-toggle)

    (define-key magit-status-mode-map (kbd "M-m") 'lispy-mark-symbol)


    (setq magit-status-headers-hook
          '(magit-insert-repo-header
            magit-insert-remote-header
            magit-insert-head-header
            magit-insert-tags-header))


    ;; ;;* Maps
    ;; magit-status-mode-map
    ;; magit-log-mode-map
    ;; magit-commit-mode-map
    ;; magit-diff-mode-map

    ))

(provide 'init-magit)
