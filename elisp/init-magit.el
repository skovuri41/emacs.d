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
            "gn" 'git-gutter+-next-hunk
            "gp" 'git-gutter+-previous-hunk
            "gu" 'global-git-gutter+-mode
            "ge" 'git-gutter+-stage-hunks
            "gc" 'git-gutter+-commit
            "gm" 'git-gutter+-stage-and-commit
            )))

(provide 'init-magit)
