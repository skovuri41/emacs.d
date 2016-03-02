(use-package popwin
  :ensure t
  :config
  (progn
    (defvar popwin:special-display-config-backup popwin:special-display-config)
    (setq display-buffer-function 'popwin:display-buffer)
    (push '("*helm*" :height 20) popwin:special-display-config)
    (push '("^\*helm .+\*$" :regexp t :height 20) popwin:special-display-config)
    (push '("*Help*" :stick t) popwin:special-display-config)
    (push '("*Register Preview*" :noselect t) popwin:special-display-config)
    (push '(magit-status-mode :stick t :noselect t :position right) popwin:special-display-config)
    (push '(ag-mode :stick t) popwin:special-display-config)
    (push '("*HTTP Response*" :noselect t :stick t) popwin:special-display-config)
    (push '("*magit-commit*" :noselect t :height 0.3 :width 80 :stick t) popwin:special-display-config)
    (push '("*magit-diff*" :noselect t :height 0.3 :width 80) popwin:special-display-config)
    (push '("*magit-edit-log*" :noselect t :height 0.2 :width 80) popwin:special-display-config)
    (push '("*magit-process*" :noselect t :height 0.2 :width 80) popwin:special-display-config)
    ;; git-gutter
    (push '("*git-gutter:diff*" :width 0.5 :stick t)
          popwin:special-display-config)
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
    (push '("*Org Agenda*") popwin:special-display-config)
    (push '(cider-inspector-mode) popwin:special-display-config)
    (push '(cider-popup-buffer-mode) popwin:special-display-config)
    (push '("*cider grimoire*") popwin:special-display-config)
    (push '("*cider-error*") popwin:special-display-config)
    (push '("*cider-result*") popwin:special-display-config)
    (push '("*cider-apropos*" :noselect t) popwin:special-display-config)
    (push '("*cider-macroexpansion*" :noselect t) popwin:special-display-config)
    (push '("*cider-error*" :noselect t :height 20) popwin:special-display-config)
    (push '("*cider-doc*" :noselect t :height 20) popwin:special-display-config)
    (push '(cider-macroexpansion-mode :noselect t) popwin:special-display-config)
    (popwin-mode t))
  )

(use-package popup-switcher
  :ensure t
  :config
  (define-key popup-menu-keymap [escape] 'keyboard-quit)
  (setq psw-popup-position 'center)
  (setq psw-use-flx t)
  (setq psw-popup-menu-max-length 15)
  (set-face-foreground 'popup-menu-face "firebrick")
  (set-face-background 'popup-menu-face "grey")
  (set-face-background 'popup-menu-selection-face "yellow")
  ;; (set-face-foreground 'popup-menu-selection-face "blue")
  (setq psw-use-flx t))

(provide 'init-popwin)
