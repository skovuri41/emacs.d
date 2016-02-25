(use-package popwin
  :ensure t
  :config
  (progn
    (defvar popwin:special-display-config-backup popwin:special-display-config)
    (setq display-buffer-function 'popwin:display-buffer)
    (push '("*Help*" :stick t) popwin:special-display-config)
    (push '("*Register Preview*" :noselect t) popwin:special-display-config)
    (push '(magit-status-mode :stick t :noselect t :position right) popwin:special-display-config)
    (push '(ag-mode :stick t) popwin:special-display-config)
    (push '("*magit-commit*" :noselect t :height 0.3 :width 80 :stick t) popwin:special-display-config)
    (push '("*magit-diff*" :noselect t :height 0.3 :width 80) popwin:special-display-config)
    (push '("*magit-edit-log*" :noselect t :height 0.2 :width 80) popwin:special-display-config)
    (push '("*magit-process*" :noselect t :height 0.2 :width 80) popwin:special-display-config)
    (push '(slime-repl-mode :stick t) popwin:special-display-config)
    (push '(cider-repl-mode :stick t) popwin:special-display-config)
    (push '("*compilation*" :stick t) popwin:special-display-config)
    (push '(Man-mode :stick t :height 20) popwin:special-display-config)
    (push '(direx:direx-mode :position left :width 40 :dedicated t :noselect t)
          popwin:special-display-config)
    (push '("*Completions*" :stick t :noselect t) popwin:special-display-config)
    (push "*vc-diff*" popwin:special-display-config)
    (push "*vc-change-log*" popwin:special-display-config)
    (push '(" *undo-tree*" :width 0.3 :position right) popwin:special-display-config)
    (push "*Ibuffer*" popwin:special-display-config)
    (popwin-mode t))
  )

(provide 'init-popwin)
