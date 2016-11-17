(use-package spaceline
  :ensure t
  :config
  (progn
    (use-package spaceline-config
      :config
      (require 'all-the-icons)
      (setq spaceline-workspace-numbers-unicode t)
      (setq spaceline-window-numbers-unicode t)
      (setq powerline-height 18)
      ;; (spaceline-spacemacs-theme)
      (spaceline-emacs-theme)
      (spaceline-toggle-flycheck-warning-off)
      (spaceline-toggle-flycheck-error-off)
      (spaceline-toggle-flycheck-info-off)
      (validate-setq powerline-default-separator 'arrow-fade)

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
        :tight nil)

      (defun xah-fly-keys-state ()
        (if xah-fly-insert-state-q
            " I "
          " C "
          ))

      (defun xah-fly-keys-state-face ()
        (if xah-fly-insert-state-q
            'all-the-icons-green
          'all-the-icons-lred
          ))

      (spaceline-define-segment *xah-fly-keys-state
        (propertize (xah-fly-keys-state)
                    'face (xah-fly-keys-state-face)
                    'help-echo (format "Xah Fly Mode: %s"
                                       (xah-fly-keys-state))))

      (spaceline-define-segment config-modeline-ace-window-number
        (when (and (featurep 'ace-window)
                   (> (length (aw-window-list)) 1))
          (when-let ((pos (cl-position (selected-window) (aw-window-list)))
                     (key (nth pos aw-keys)))
            (propertize (char-to-string key) 'face 'all-the-icons-lred))))

      (spaceline-define-segment
          *projectile "An `all-the-icons' segment for current `projectile' project"
          (if (and (fboundp 'projectile-project-name)
                   (projectile-project-name))
              (propertize (format " %s " (concat (projectile-project-name)))
                          'face '(:height 1.0 :inherit)
                          'display '(raise 0.0)
                          'help-echo "Switch Project"
                          'mouse-face '(:box 1)
                          'local-map (make-mode-line-mouse-map
                                      'mouse-1 (lambda () (interactive) (projectile-switch-project)))))
          :tight nil)

      (defun spaceline---git-vc ()
        "Function to return the Spaceline formatted GIT Version Control text."
        (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
          (concat
           (propertize (all-the-icons-alltheicon "git") 'face '(:height 1.1 :inherit) 'display '(raise 0.1))
           (propertize (format "  %s" (all-the-icons-octicon "git-branch"))
                       'face `(:family ,(all-the-icons-octicon-family) :height 1.0 :inherit)
                       'display '(raise 0.2))
           (propertize (format " %s" branch) 'face `(:height 0.9 :inherit) 'display '(raise 0.2)))))

      (defun spaceline---svn-vc ()
        "Function to return the Spaceline formatted SVN Version Control text."
        (let ((revision (cadr (split-string vc-mode "-"))))
          (concat
           (propertize (format " %s" (all-the-icons-faicon "cloud")) 'face `(:height 1.2) 'display '(raise -0.1))
           (propertize (format " · %s" revision) 'face `(:height 0.9)))))

      (spaceline-define-segment
          *vc-icon
        "An `all-the-icons' segment for the current Version Control icon"
        (when vc-mode
          (cond ((string-match "Git[:-]" vc-mode) (spaceline---git-vc))
                ((string-match "SVN-" vc-mode) (spaceline---svn-vc))
                (t (propertize (format "%s" vc-mode)))))
        :when active)

      (defun spaceline--get-temp ()
        "Function to return the Temperature formatted for ATI Spacline."
        (let ((temp (yahoo-weather-info-format yahoo-weather-info "%(temperature)")))
          (unless (string= "" temp) (format "%s°C" (round (string-to-number temp))))))

      (spaceline-define-segment
          *weather "Weather"
          (let* ((weather (yahoo-weather-info-format yahoo-weather-info "%(weather)"))
                 (temp (spaceline--get-temp))
                 (help (concat "Weather is '" weather "' and the temperature is " temp))
                 (icon (all-the-icons-icon-for-weather (downcase weather))))
            (concat
             (if (> (length icon) 1)
                 (propertize icon 'help-echo help 'face `(:height 0.9 :inherit) 'display '(raise 0.1))
               (propertize icon
                           'help-echo help
                           'face `(:height 0.9 :family ,(all-the-icons-wicon-family) :inherit)
                           'display '(raise 0.0)))
             (propertize " " 'help-echo help)
             (propertize (spaceline--get-temp) 'face '(:height 0.9 :inherit) 'help-echo help)))
          :when (and active (boundp 'yahoo-weather-info) yahoo-weather-mode)
          :tight nil)

      (defvar spaceline--upgrades nil)
      (defun spaceline--count-upgrades ()
        "Function to count the number of package upgrades needed."
        (let ((buf (current-buffer)))
          (package-list-packages-no-fetch)
          (with-current-buffer "*Packages*"
            (setq spaceline--upgrades (length (package-menu--find-upgrades))))
          (switch-to-buffer buf)))
      (advice-add 'package-menu-execute :after 'spaceline--count-upgrades)

      (spaceline-define-segment
          *package-updates "An `all-the-icons' spaceline segment to indicate number of package updates needed"
          (let ((num (or spaceline--upgrades (spaceline--count-upgrades))))
            (propertize
             (concat
              (propertize (format "%s" (all-the-icons-octicon "package"))
                          'face `(:family ,(all-the-icons-octicon-family) :height 1.1 :inherit)
                          'display '(raise 0.1))
              (propertize (format " %d updates " num) 'face `(:height 0.9 :inherit) 'display '(raise 0.2)))
             'help-echo "Open Packages Menu"
             'mouse-face '(:box 1)
             'local-map (make-mode-line-mouse-map
                         'mouse-1 (lambda () (interactive) (package-list-packages)))))
          :when (and active (> (or spaceline--upgrades (spaceline--count-upgrades)) 0)))

      (spaceline-install
       '(*macro-recording
         config-modeline-ace-window-number
         ((workspace-number window-number) :separator " | ")
         (*xah-fly-keys-state :separator " | ")
         *projectile
         (buffer-modified buffer-size buffer-id remote-host)
         major-mode
         ((flycheck-error flycheck-warning flycheck-info) :when active)
         (((minor-modes :separator " ") process) :when active)
         *buffer-modified
         *vc-icon
         (*package-updates :when active)
         (org-pomodoro :when active)
         (org-clock :when active)
         (battery :when active)
         *weather)
       `(selection-info
         ((*selection-info buffer-encoding-abbrev point-position line-column) :separator " | ")
         (global :when active)
         buffer-position
         hud)))))

(provide 'init-spaceline)
