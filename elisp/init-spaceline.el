(use-package spaceline
  :ensure t
  :config
  (progn
    (use-package spaceline-config
      :init
      (setq spaceline-workspace-numbers-unicode t)
      (setq spaceline-window-numbers-unicode t)
      (setq powerline-height 25)
      (set-face-attribute 'spaceline-evil-emacs nil :background "#be84ff")
      (set-face-attribute 'spaceline-evil-insert nil :background "#5fd7ff")
      (set-face-attribute 'spaceline-evil-motion nil :background "#ae81ff")
      (set-face-attribute 'spaceline-evil-normal nil :background "#a6e22e")
      (set-face-attribute 'spaceline-evil-replace nil :background "#f92672")
      (set-face-attribute 'spaceline-evil-visual nil :background "#fd971f")
      ;; (set-face-attribute 'mode-line nil :font "Source Code Pro for Powerline-12")
      (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
      :config
      ;; (spaceline-spacemacs-theme)
      ;; (spaceline-emacs-theme)
      (spaceline-toggle-flycheck-warning-off)
      (spaceline-toggle-flycheck-error-off)
      (spaceline-toggle-flycheck-info-off)
      (setq powerline-default-separator 'arrow-fade)

      (spaceline-define-segment *macro-recording
        "Show when recording macro"
        (format "%s ▶" (char-to-string evil-this-macro))
        :when (and active defining-kbd-macro)
        :face highlight-face
        :skip-alternate t)

      (defun narf--col-at-pos (pos)
        (save-excursion (goto-char pos) (current-column)))
      (spaceline-define-segment *selection-info
        "Information about the size of the current selection, when applicable.
         Supports both Emacs and Evil cursor conventions."
        (let ((reg-beg (region-beginning))
              (reg-end (region-end)))
          (let* ((lines (count-lines reg-beg (min (1+ reg-end) (point-max))))
                 (chars (- (1+ reg-end) reg-beg))
                 (cols (1+ (abs (- (narf--col-at-pos reg-end)
                                   (narf--col-at-pos reg-beg)))))
                 (evil (eq 'visual evil-state))
                 (rect (or (bound-and-true-p rectangle-mark-mode)
                           (and evil (eq 'block evil-visual-selection))))
                 (multi-line (or (> lines 1) (eq 'line evil-visual-selection))))
            (cond
             (rect (format "%dx%dB" lines (if evil cols (1- cols))))
             (multi-line
              (if (and (eq evil-state 'visual) (eq evil-this-type 'line))
                  (format "%dL" lines)
                (format "%dC %dL" chars lines)))
             (t (format "%dC" (if evil chars (1- chars)))))))
        :when (eq 'visual evil-state)
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

      (spaceline-install
       '(*macro-recording
         ((window-number) :separator " | ")
         ((evil-state) :face highlight-face :separator " | ")
         (buffer-modified buffer-size buffer-id remote-host)
         major-mode
         ((flycheck-error flycheck-warning flycheck-info) :when active)
         (((minor-modes :separator " ") process) :when active)
         *buffer-modified
         (version-control :when active)
         (battery :when active))
       `(selection-info
         ((*selection-info buffer-encoding-abbrev point-position line-column) :separator " | ")
         (global :when active)
         buffer-position
         hud))
      (setq mode-line-format (default-value 'mode-line-format))
      )))

(defadvice vc-mode-line (after strip-backend () activate)
  "Remove the Git string from the 'vc-mode-line'."
  (when (stringp vc-mode)
    (let ((noback (replace-regexp-in-string
                   (format "^ %s\\(:\\|-\\)?" (vc-backend buffer-file-name))
                   " " vc-mode)))
      (setq vc-mode noback))))


(provide 'init-spaceline)
