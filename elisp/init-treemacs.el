(use-package treemacs
  :ensure t
  ;; :defer t
  :init
  ;; (progn
  ;;   (unbind-key "w" treemacs-mode-map)
  ;;   (unbind-key "u" treemacs-mode-map)
  ;;   (unbind-key "h" treemacs-mode-map))
  :config
  (progn
    (progn
      (unbind-key "w" treemacs-mode-map)
      (unbind-key "u" treemacs-mode-map)
      (unbind-key "i" treemacs-mode-map)
      (unbind-key "h" treemacs-mode-map))
    (setq treemacs-collapse-dirs (if (executable-find "python") 3 0)
          treemacs-deferred-git-apply-delay 0.5
          treemacs-file-event-delay 5000
          treemacs-follow-after-init t
          treemacs-follow-recenter-distance 0.1
          treemacs-goto-tag-strategy 'refetch-index
          treemacs-indentation 2
          treemacs-indentation-string " "
          treemacs-is-never-other-window nil
          treemacs-no-png-images nil
          treemacs-project-follow-cleanup nil
          treemacs-persist-file (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-recenter-after-file-follow nil
          treemacs-recenter-after-tag-follow nil
          treemacs-show-hidden-files t
          treemacs-silent-filewatch nil
          treemacs-silent-refresh nil
          treemacs-sorting 'alphabetic-desc
          treemacs-space-between-root-nodes t
          treemacs-tag-follow-cleanup t
          treemacs-tag-follow-delay 1.5
          treemacs-display-in-side-window nil
          treemacs-width 35)

    (defun my/treemacs-quit-buffer ()
      (interactive)
      (progn (bury-buffer)
             (xah-fly-keys 1)
             (xah-fly-command-mode-activate)))

    (add-hook 'treemacs-mode-hook
              '(lambda ()
                 (xah-fly-keys-off)))

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    (treemacs-resize-icons 14)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t))
  :bind
  (:map treemacs-mode-map
        ("j" . treemacs-next-line)
        ("k" . treemacs-previous-line)
        ("u" . treemacs-root-up)
        ("h" . treemacs-goto-parent-node)
        ("i" . treemacs-TAB-action)
        ("ww" . ace-window)
        ("q" . my/treemacs-quit-buffer)))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)


(provide 'init-treemacs)
