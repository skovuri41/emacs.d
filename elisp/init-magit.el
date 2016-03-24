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
      "gt" 'git-timemachine
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
    (evil-define-key 'normal magit-mode-map
      (kbd "j") 'magit-goto-next-section
      (kbd "k") 'magit-goto-previous-section
      (kbd "c") 'magit-commit
      (kbd "s") 'magit-stage-item
      (kbd "u") 'magit-unstage-item
      (kbd "p") 'magit-toggle-section
      (kbd "r") 'magit-refresh
      (kbd "q") 'kill-buffer-and-window)

    (evil-define-key 'normal magit-log-mode-map
      (kbd "j") 'magit-goto-next-section
      (kbd "k") 'magit-goto-previous-section)

    (evil-define-key 'normal magit-diff-mode-map
      (kbd "j") 'magit-goto-next-section
      (kbd "k") 'magit-goto-previous-section)

    (evil-define-key 'normal magit-process-mode-map
      (kbd "j") 'evil-next-line
      (kbd "k") 'evil-previous-line))

  )



(provide 'init-magit)
