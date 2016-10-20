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

      (defface xah-fly-keys-face1
        '((t (:foreground "#FC5C94" :distant-foreground "#A20C41")))
        "Face for xah keys in the modeline."
        :group 'spaceline)

      (defface xah-fly-keys-face2
        '((t (:foreground "chartreuse3" :distant-foreground "darkgreen")))
        "Face for xah keys the modeline."
        :group 'spaceline)

      (defun xah-fly-keys-state ()
        (if xah-fly-insert-state-q
            " I "
          " C "
          ))

      (defun xah-fly-keys-state-face ()
        (if xah-fly-insert-state-q
            'xah-fly-keys-face2
          'xah-fly-keys-face1
          ))

      (spaceline-define-segment *xah-fly-keys-state
        (propertize (xah-fly-keys-state)
                    'face (xah-fly-keys-state-face)
                    'help-echo (format "Xah Fly keys: %s"
                                       "xah-fly-keys" )))
      (spaceline-install
       '(*macro-recording
         ((workspace-number window-number) :separator " | ")
         (*xah-fly-keys-state :separator " | ")
         (buffer-modified buffer-size buffer-id remote-host)
         major-mode
         ((flycheck-error flycheck-warning flycheck-info) :when active)
         (((minor-modes :separator " ") process) :when active)
         *buffer-modified
         (version-control :when active)
         (org-pomodoro :when active)
         (org-clock :when active)
         (battery :when active))
       `(selection-info
         ((*selection-info buffer-encoding-abbrev point-position line-column) :separator " | ")
         (global :when active)
         buffeir-position
         hud))

      ;; (setq mode-line-format (default-value 'mode-line-format))

      )))

(defadvice vc-mode-line (after strip-backend () activate)
  "Remove the Git string from the 'vc-mode-line'."
  (when (stringp vc-mode)
    (let ((noback (replace-regexp-in-string
                   (format "^ %s\\(:\\|-\\)?" (vc-backend buffer-file-name))
                   " " vc-mode)))
      (setq vc-mode noback))))


(provide 'init-spaceline)
