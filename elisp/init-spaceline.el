(use-package spaceline
  :ensure t
  :config
  (progn
    (use-package spaceline-config
      :config
      (setq spaceline-workspace-numbers-unicode t)
      (setq spaceline-window-numbers-unicode t)
      (setq powerline-height 18)
      ;; (spaceline-spacemacs-theme)
      (spaceline-emacs-theme)
      (spaceline-toggle-flycheck-warning-off)
      (spaceline-toggle-flycheck-error-off)
      (spaceline-toggle-flycheck-info-off)
      (setq powerline-default-separator 'arrow-fade)

      (spaceline-define-segment *macro-recording
        "Show when recording macro"
        (format "%s ▶" (string ?m ))
        :when (and active defining-kbd-macro)
        :face highlight-face
        :skip-alternate t)

      (defface mode-line-is-modified nil "Face for mode-line modified symbol")
      (defface mode-line-buffer-file nil "Face for mode-line buffer file path")

      (spaceline-define-segment *buffer-modified
        (concat
         (when buffer-file-name
           (concat
            (when (buffer-modified-p) "[+]")
            (unless (file-exists-p buffer-file-name) "[!]")))
         (if buffer-read-only "[RO]"))
        :face mode-line-is-modified
        :when (not (string-prefix-p "*" (buffer-name)))
        :skip-alternate t
        :tight t)

      (defun xah-fly-keys-state ()
        (if xah-fly-insert-state-q
            " I "
          " C "
          ))

      (require 'all-the-icons)
      (defun xah-fly-keys-state-face ()
        (if xah-fly-insert-state-q
            'all-the-icons-green
          'all-the-icons-lred
          ))

      (spaceline-define-segment *xah-fly-keys-state
        (propertize (xah-fly-keys-state)
                    'face (xah-fly-keys-state-face)
                    'help-echo (format "Xah Fly keys: %s"
                                       "xah-fly-keys" )))

      (spaceline-define-segment config-modeline-version-control
        "Version control information."
        (when (bound-and-true-p vc-mode)
          (let ((sym (when (buffer-file-name)
                       (pcase (vc-state (buffer-file-name))
                         ((\` up-to-date) " ")
                         ((\` edited) (propertize " *" 'face 'all-the-icons-dred))
                         ((\` added) (propertize " +" 'face 'all-the-icons-green))
                         ((\` unregistered) (propertize " ??" 'face 'all-the-icons-silver))
                         ((\` removed) (propertize " -" 'face 'all-the-icons-red))
                         ((\` needs-merge) (propertize " M" 'face 'all-the-icons-yellow))
                         ((\` needs-update) (propertize " X" 'face 'all-the-icons-orange))
                         ((\` ignored) " ")
                         (_ " Unk"))))
                (desc (replace-regexp-in-string "Git[.?:-]" "" vc-mode)))
            (powerline-raw (concat sym desc " ")))))

      (spaceline-define-segment config-modeline-ace-window-number
        (when (and (featurep 'ace-window)
                   (> (length (aw-window-list)) 1))
          (when-let ((pos (cl-position (selected-window) (aw-window-list)))
                     (key (nth pos aw-keys)))
            (propertize (char-to-string key) 'face 'all-the-icons-lred))))

      (spaceline-install
       '(*macro-recording
         config-modeline-ace-window-number
         ((workspace-number window-number) :separator " | ")
         (*xah-fly-keys-state :separator " | ")
         (buffer-modified buffer-size buffer-id remote-host)
         major-mode
         ((flycheck-error flycheck-warning flycheck-info) :when active)
         (((minor-modes :separator " ") process) :when active)
         *buffer-modified
         (config-modeline-version-control :when active)
         (org-pomodoro :when active)
         (org-clock :when active)
         (battery :when active))
       `(selection-info
         ((*selection-info buffer-encoding-abbrev point-position line-column) :separator " | ")
         (global :when active)
         buffeir-position
         hud))

      )))

(provide 'init-spaceline)
