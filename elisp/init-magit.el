(use-package magit
  :init
  (progn
    (evil-leader/set-key "gs" 'magit-status)
    (evil-leader/set-key "gt" 'git-timemachine)))
(provide 'init-magit)


