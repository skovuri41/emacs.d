(use-package workgroups2
  :ensure t
  :init
  (progn
    (defhydra hydra-workgroups (global-map "C-x w" :hint nil)
      "
      _n_: switch to workgroup
      _c_: create new workgroup
      _s_: save workgroup
      _r_: reload session
      "
      ("n" wg-switch-to-workgroup)
      ("c" wg-create-workgroup)
      ("r" wg-reload-session)
      ("s" wg-save-wconfig)
      ("q" nil :exit t))
    (setq wg-session-file  (expand-file-name ".emacs_workgroups" user-emacs-directory))
    (setq wg-emacs-exit-save-behavior           'save)      ; Options: 'save 'ask nil
    (setq wg-workgroups-mode-exit-save-behavior 'save)      ; Options: 'save 'ask nil
    (defun wg-change-modeline () nil)
    (setq wg-mode-line-display-on nil)          ; Default: (not (featurep 'powerline))
    ;; (setq wg-flag-modified t)                 ; Display modified flags as well
    ;; (setq wg-mode-line-decor-left-brace "["
    ;;       wg-mode-line-decor-right-brace "]"  ; how to surround it
    ;;       wg-mode-line-decor-divider ":")
    )
  :config
  (progn
    (defun wg-change-modeline () nil)
    (evil-leader/set-key
      "Wf" 'wg-open-session
      "Ws" 'wg-save-session
      "Wc" 'wg-create-workgroup
      "WC" 'wg-clone-workgroup
      "WW" 'wg-switch-to-workgroup
      "W TAB" 'wg-switch-to-previous-workgroup
      "Wn" 'wg-switch-to-workgroup-left
      "Wp" 'wg-switch-to-workgroup-right
      "Wj" 'wg-switch-to-workgroup-at-index
      "W1" 'wg-switch-to-workgroup-at-index-1
      "W2" 'wg-switch-to-workgroup-at-index-2
      "W3" 'wg-switch-to-workgroup-at-index-3
      "W4" 'wg-switch-to-workgroup-at-index-4
      "W5" 'wg-switch-to-workgroup-at-index-5
      "W6" 'wg-switch-to-workgroup-at-index-6
      "W7" 'wg-switch-to-workgroup-at-index-7
      "W8" 'wg-switch-to-workgroup-at-index-8
      "W9" 'wg-switch-to-workgroup-at-index-9
      "W0" 'wg-switch-to-workgroup-at-index-0
      "WA" 'wg-rename-workgroup
      "WR" 'wg-revert-workgroup
      "Wk" 'wg-kill-workgroup
      "W C-k" 'wg-kill-workgroup-and-buffers
      "WS" 'wg-save-wconfig
      "Wu" 'wg-undo-wconfig-change
      "WU" 'wg-redo-wconfig-change
      "W C-r" 'wg-revert-all-workgroups
      "W!" 'wg-reset
      "W?" 'wg-help
      )

    (workgroups-mode 1)
    (wg-open-session wg-session-file)
    )
  )

(provide 'init-workgroups2)
