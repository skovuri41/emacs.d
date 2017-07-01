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

      (spaceline-define-segment
          *process "An `all-the-icons' segment for the current process"
          (let ((icon (all-the-icons-icon-for-buffer)))
            (concat
             (when (or (symbolp icon) mode-line-process)
               (propertize (format-mode-line "%m") 'face `(:height 0.8 :inherit) 'display '(raise 0.2)))
             (when mode-line-process
               (propertize (format-mode-line mode-line-process) 'face '(:height 0.7 :inherit) 'display '(raise 0.2))))))

      (spaceline-define-segment
          *position "An `all-the-icons' segment for the Row and Column of the current point"
          (propertize (if (eq 'pdf-view-mode major-mode)
                          (spaceline--pdfview-page-number)
                        "%l:%2c") 'face `(:height 0.9 :inherit) 'display '(raise 0.1)))

      (spaceline-define-segment
          *region-info "An `all-the-icons' segment for the currently marked region"
          (when mark-active
            (let ((words (count-lines (region-beginning) (region-end)))
                  (chars (count-words (region-end) (region-beginning))))
              (concat
               (propertize (format "%s " (all-the-icons-octicon "pencil") words chars)
                           'face `(:family ,(all-the-icons-octicon-family) :inherit) 'display '(raise 0.1))
               (propertize (format "(%s, %s)" words chars)
                           'face `(:height 0.9 :inherit))))))

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

      (spaceline-define-segment
          *major-mode "An `all-the-icons' segment for the current buffer mode"
          (let ((icon (all-the-icons-icon-for-buffer)))
            (unless (symbolp icon) ;; This implies it's the major mode
              (propertize icon
                          'help-echo (format "Major-mode: `%s`" major-mode)
                          'display '(raise 0.0)
                          'face `(:height 1.0 :family ,(all-the-icons-icon-family-for-buffer) :inherit)))))

      (spaceline-define-segment
          *buffer-id "An `all-the-icons' segment for the current buffer id"
          (if (fboundp 'projectile-project-root)
              (let* ((buf (or (buffer-file-name) (buffer-name)))
                     (proj (ignore-errors (projectile-project-root)))
                     (name (if (buffer-file-name)
                               (or (cadr (split-string buf proj))
                                   (format-mode-line "%b"))
                             (format-mode-line "%b"))))
                (propertize (format "%s" name)
                            'face `(:height 0.8 :inherit)
                            'display '(raise 0.2)
                            'help-echo (format "Major-mode: `%s`" major-mode)))
            (propertize (format-mode-line "%b ") 'face '(:height 0.8 :inherit) 'display '(raise 0.1))))

      (defun xah-fly-keys-state ()
        (if xah-fly-insert-state-q
            " I "
          " C "))

      (defun xah-fly-keys-state-face ()
        (if xah-fly-insert-state-q
            'all-the-icons-green
          'all-the-icons-lred))

      (spaceline-define-segment *xah-fly-keys-state
        (propertize (xah-fly-keys-state)
                    'face (xah-fly-keys-state-face)
                    'help-echo (format "Xah Fly Mode: %s"
                                       (xah-fly-keys-state)))
        :tight t)

      (spaceline-define-segment *ace
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
          (unless (string= "" temp) (format "%s°F" (round (string-to-number temp))))))

      (spaceline-define-segment
          *weather "Weather"
          (let* ((weather (yahoo-weather-info-format yahoo-weather-info "%(weather)"))
                 (temp (spaceline--get-temp))
                 (help (concat "Weather is '" weather "' and the temperature is " temp))
                 (icon (all-the-icons-icon-for-weather (downcase weather))))
            (propertize
             (concat
              (if (> (length icon) 1)
                  (propertize icon 'help-echo help 'face `(:height 0.9 :inherit) 'display '(raise 0.1))
                (propertize icon
                            'help-echo help
                            'face `(:height 0.9 :family ,(all-the-icons-wicon-family) :inherit)
                            'display '(raise 0.0)))
              (propertize " " 'help-echo help)
              (propertize (spaceline--get-temp) 'face '(:height 0.9 :inherit) 'help-echo help))
             'mouse-face '(:box 1)
             'local-map (make-mode-line-mouse-map
                         'mouse-1 (lambda () (interactive) (wttrin-query yahoo-weather-location))))
            )
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
      ;; (spaceline--count-upgrades)
      (add-hook 'after-init-hook #'spaceline--count-upgrades)
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

      (spaceline-define-segment
          *battery "Show battery information"
          (let* ((charging? (equal "AC" (cdr (assoc ?L fancy-battery-last-status))))
                 (percentage (string-to-int (cdr (assoc ?p fancy-battery-last-status))))
                 (time (format "%s" (cdr (assoc ?t fancy-battery-last-status))))
                 (icon-set (if charging? 'alltheicon 'faicon))
                 (icon-alist
                  (cond
                   (charging? '((icon . "charging") (inherit . success) (height . 1.2) (raise . -0.1)))
                   ((> percentage 95) '((icon . "full") (inherit . success)))
                   ((> percentage 70) '((icon . "three-quarters")))
                   ((> percentage 35) '((icon . "half")))
                   ((> percentage 15) '((icon . "quarter") (inherit . warning)))
                   (t '((icon . "empty") (inherit . error)))))
                 (icon-f (all-the-icons--function-name icon-set))
                 (family (funcall (all-the-icons--family-name icon-set))))
            (let-alist icon-alist
              (concat
               (if .inherit
                   (let ((fg (face-attribute .inherit :foreground)))
                     (propertize (funcall icon-f (format "battery-%s" .icon))
                                 'face `(:height ,(or .height 1.0) :family ,family :foreground ,fg)
                                 'display `(raise ,(or .raise 0.0))))
                 (propertize (funcall icon-f (format "battery-%s" .icon))
                             'face `(:family ,family :inherit)
                             'display '(raise 0.0)))
               " "
               (if .inherit
                   (let ((fg (face-attribute .inherit :foreground)))
                     (propertize (if charging? (format "%s%%%%" percentage) time) 'face `(:height 0.9 :foreground ,fg)))
                 (propertize time 'face '(:height 0.9 :inherit)))
               )))
          :global-override fancy-battery-mode-line :when (and active (fboundp 'fancy-battery-mode) fancy-battery-mode))

      (spaceline-define-segment
          *time "Time"
          (let* ((hour (string-to-number (format-time-string "%I")))
                 (icon (all-the-icons-wicon (format "time-%s" hour) :v-adjust 0.0)))
            (propertize
             (concat
              (propertize (format-time-string "%H:%M ") 'face `(:height 0.9 :inherit) 'display '(raise 0.1))
              (propertize (format "%s" icon)
                          'face `(:height 0.8 :family ,(all-the-icons-wicon-family) :inherit)
                          'display '(raise 0.1)))
             'help-echo "world time"
             'mouse-face '(:box 1)
             'local-map (make-mode-line-mouse-map
                         'mouse-1 (lambda () (interactive) (display-time-world))))))

      (spaceline-define-segment *iedit
        "Show the number of iedit regions matches + what match you're on."
        (propertize
         (let ((this-oc (let (message-log-max) (iedit-find-current-occurrence-overlay)))
               (length (or (ignore-errors (length iedit-occurrences-overlays)) 0)))
           (format
            " %s/%s "
            (save-excursion
              (unless this-oc
                (iedit-prev-occurrence)
                (setq this-oc (iedit-find-current-occurrence-overlay)))
              (if this-oc
                  ;; NOTE: Not terribly reliable
                  (- length (-elem-index this-oc iedit-occurrences-overlays))
                "-"))
            length))
         'face `(:foreground "green") ) :when (and (boundp 'iedit-mode) iedit-mode))

      (spaceline-install
        '(*macro-recording
          *iedit
          *ace
          ((workspace-number window-number) :separator " | ")
          *xah-fly-keys-state
          *projectile
          (*process *buffer-modified *buffer-id remote-host)
          *major-mode
          ((flycheck-error flycheck-warning flycheck-info) :when active)
          (((minor-modes :separator " ") process) :when active)
          *vc-icon
          (org-pomodoro :when active)
          (org-clock :when active))
        `(*region-info
          ((buffer-encoding-abbrev *position) :separator " | ")
          *battery
          *time
          buffer-position
          hud))
      ;; *weather
      ;; (global :when active)
      ;; (*package-updates :when active)

      )))

(use-package spaceline-all-the-icons
  :after spaceline
  :disabled t
  :config
  (progn
    (spaceline-all-the-icons--setup-neotree)
    (spaceline-all-the-icons--setup-anzu)
    (spaceline-all-the-icons-theme)

    ;; Configuration
    (setq spaceline-highlight-face-func 'spaceline-highlight-face-default
          spaceline-all-the-icons-icon-set-modified 'chain
          spaceline-all-the-icons-icon-set-window-numbering 'circle
          spaceline-all-the-icons-separator-type 'none
          spaceline-all-the-icons-primary-separator "")

    ;; Toggles
    (spaceline-toggle-all-the-icons-buffer-size-off)
    (spaceline-toggle-all-the-icons-buffer-position-off)
    (spaceline-toggle-all-the-icons-vc-icon-off)
    (spaceline-toggle-all-the-icons-vc-status-off)
    ;; (spaceline-toggle-all-the-icons-git-status-off)
    (spaceline-toggle-all-the-icons-flycheck-status-off)
    ;; (spaceline-toggle-all-the-icons-time-off)
    ;; (spaceline-toggle-all-the-icons-battery-status-off)
    ;; (spaceline-toggle-hud-off)
    ))

(provide 'init-spaceline)
