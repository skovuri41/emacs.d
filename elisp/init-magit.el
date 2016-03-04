(use-package magit
  :ensure t
  :commands (magit-status
             magit-log
             magit-commit
             magit-stage-file)
  :init (progn
          (evil-leader/set-key
            "gs" 'magit-status
            "gS" 'magit-stage-file
            "gC" 'magit-commit
            "gt" 'git-timemachine
            "gl" 'magit-log-all
            "gL" 'magit-log-buffer-file
            "gg" 'hydra-git-gutter/body
            )))

(provide 'init-magit)
