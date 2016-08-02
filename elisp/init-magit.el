(use-package magit
  :ensure t
  :commands (magit-status
             magit-log
             magit-commit
             magit-stage-file)
  :init
  (progn
    (evil-leader/set-key
      "gs" 'magit-status
      "gS" 'magit-stage-file
      "gC" 'magit-commit
      ;; "gt" 'git-timemachine
      "gt" 'git-timemachine-toggle
      "gl" 'magit-log-all
      "gL" 'magit-log-buffer-file
      "gg" 'hydra-git-gutter/body
      ))

  ;; (progn
  ;;   (evil-set-initial-state 'magit-log-edit-mode 'insert)
  ;;   (evil-set-initial-state 'magit-status-mode 'emacs)
  ;;   (evil-add-hjkl-bindings magit-log-mode-map 'emacs)
  ;;   (evil-add-hjkl-bindings magit-commit-mode-map 'emacs)
  ;;   )

  (progn
    (evil-set-initial-state 'magit-mode 'normal)
    (evil-set-initial-state 'magit-status-mode 'normal)
    (evil-set-initial-state 'magit-diff-mode 'normal)
    (evil-set-initial-state 'magit-log-mode 'normal)
    (evil-set-initial-state 'magit-process-mode 'normal)
    )
  )



(provide 'init-magit)
