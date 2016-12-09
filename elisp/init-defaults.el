(setq inhibit-startup-message t)
(blink-cursor-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)

;; Disable visible bell
(setq visible-bell nil)

;; Disable audible bell as well
(setq ring-bell-function 'ignore)

;; Buffer settings
(setq default-indicate-empty-lines t)
(setq require-final-newline t)

;; Allow pasting selection outside of Emacs
(setq x-select-enable-clipboard t)

;; window focus follows the mouse pointer
(setq  mouse-autoselect-window t)

;; copy by just selecting with mouse
(setq mouse-drag-copy-region t)

;;make mouse scrolling smooth
(setq  mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))))

(setq recenter-positions '(top middle bottom))

(setq use-dialog-box nil)

;;Suppress symbolic link warnings
(setq find-file-visit-truename t)

(use-package validate
  :ensure t)

(use-package autorevert
  :init (global-auto-revert-mode)
  :config
  (validate-setq auto-revert-verbose nil
                 global-auto-revert-non-file-buffers t)
  (when (eq system-type 'darwin)
    ;; File notifications aren't supported on OS X
    (validate-setq auto-revert-use-notify nil))
  :diminish (auto-revert-mode . ""))

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Hide the mouse while typing:
(setq make-pointer-invisible t)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Real emacs knights don't use shift to mark things
(setq shift-select-mode nil)

;; Transparently open compressed files
(auto-compression-mode t)

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8 please
;(set-keyboard-coding-system 'iso-latin-1)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)
(set-charset-priority 'unicode)

;; Show active region
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; Remove text in active region if inserting text
(delete-selection-mode 1)

;; Don't highlight matches with jump-char - it's distracting
(setq jump-char-lazy-highlight-face nil)

;; Always display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Lines should be 80 characters wide, not 72
(setq fill-column 100)

(setq debug-on-error t)
;; (setq debug-on-quit t)

(validate-setq
 kill-ring-max 200                           ; More killed items
 kill-do-not-save-duplicates t               ; No duplicates in kill ring
 ;; Save the contents of the clipboard to kill ring before killing
 save-interprogram-paste-before-kill t)

(use-package recentf
  :init
  (setq recentf-save-file "~/.emacs.d/.recentf")
  :config
  (defmacro with-suppressed-message (&rest body)
    "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
    (declare (indent 0))
    (let ((message-log-max nil))
      `(with-temp-message (or (current-message) "") ,@body)))
  (setq recentf-max-saved-items 1000)
  (setq recentf-auto-cleanup 'never)
  (setq delete-old-versions t)
  (setq delete-auto-save-files t)
  (setq backup-directory-alist
        '(("." . "~/backups")))
  (setq recentf-exclude
        '("/TAGS$" "/var/tmp/" "~/.emacs.d/.recentf"  ".recentf" "/auto-install/" "/elpa/"
          "\\.mime-example" "\\.ido.last" "COMMIT_EDITMSG"
          ".gz" "~$" "/tmp/" "/ssh:" "/sudo:" "/scp:"))

  (validate-setq
   recentf-max-menu-items 15
   recentf-exclude (list "/\\.git/.*\\'"     ; Git contents
                         "/elpa/.*\\'"       ; Package files
                         "company-statistics-cache.el"
                         ))
  ;; (run-with-idle-timer (* 5 60) t 'recentf-save-list)
  (run-with-idle-timer (* 5 60) t '(lambda ()
                                     (with-suppressed-message (recentf-save-list))))
  (use-package recentf-ext
    :ensure t))

(save-place-mode +1)
(setq save-place-version-control "never")
(setq save-place-forget-unreadable-files nil)

(use-package winner
  :config
  (setq winner-boring-buffers '("*Completions*"
                                "*Compile-Log*"
                                "*inferior-lisp*"
                                "*Fuzzy Completions*"
                                "*Apropos*"
                                "*Help*"
                                "*cvs*"
                                "*Buffer List*"
                                "*Ibuffer*"
                                ))
  (winner-mode 1))

;; Never insert tabs
(set-default 'indent-tabs-mode nil)
;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Easily navigate sillycased words
(global-subword-mode 1)

;;quickly pop mark several times
(setq set-mark-command-repeat-pop t)

;; Don't break lines for me, please
(setq-default truncate-lines t)

;;keep cursor at same position when scrolling
(setq scroll-preserve-screen-position 1)

;; Allow recursive minibuffers
(setq enable-recursive-minibuffers t)
(setq minibuffer-auto-raise nil)

;; Don't be so stingy on the memory, we have lots now. It's the distant future.
(setq gc-cons-threshold (* 20 1024 1024))
(setq large-file-warning-threshold (* 25 1024 1024))

;; enable erase-buffer command
(put 'erase-buffer 'disabled nil)

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(use-package uniquify
  :config
  (setq uniquify-separator "/")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*")
  (setq uniquify-buffer-name-style 'post-forward uniquify-separator ":"))

(when (boundp 'global-prettify-symbols-mode)
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (push '("lambda" . ?λ) prettify-symbols-alist)))
  (global-prettify-symbols-mode +1))

;; savehist
(use-package savehist
  :config
  (progn
    (setq savehist-additional-variables
          ;; also save my search entries
          '(search-ring regexp-search-ring kill-ring)
          savehist-file "~/.emacs.d/savehist")
    (setq savehist-save-minibuffer-history t
          savehist-autosave-interval 180)
    (savehist-mode)))

;; Match fringe colour to background colour
(defun my/set-fringe-bg()
  (set-face-attribute 'fringe nil
                      :foreground (face-foreground 'default)
                      :background (face-background 'default)))

;; Indicate where a buffer starts and stops
(setq-default indicate-buffer-boundaries 'right)

(setq initial-major-mode 'fundamental-mode)
(setq  initial-scratch-message nil)
(use-package scratch-message
  :ensure t
  :init
  (scratch-message-mode))

(use-package scratch
  :ensure t
  :init
  (autoload 'scratch "scratch" nil t))

(defmacro after (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
     '(progn ,@body)))

;; make sure $PATH is set correctly
(when(eq system-type 'darwin)
  (use-package exec-path-from-shell
    :ensure t
    :config
    (progn
      (setq exec-path-from-shell-variables '("PATH"
                                             "MANPATH"))
      (exec-path-from-shell-initialize))))

(use-package exec-path-from-shell
  :ensure exec-path-from-shell
  :disabled t
  :config
  (progn
    (exec-path-from-shell-copy-env "PATH")
    (exec-path-from-shell-initialize)))

(use-package hungry-delete
  :ensure t
  :config
  (progn
    (setq hungry-delete-chars-to-skip " \t\r\f\v")
    (setq hungry-delete-delete-backslash-eol t)
    (global-hungry-delete-mode nil)
    (add-hook 'prog-mode-hook 'hungry-delete-mode)))

;; Note: for every project, run the following command:
;; ctags -e -R .

(use-package ctags-update
  :ensure t
  :diminish ctags-auto-update-mode
  :config
  (add-hook 'prog-mode-hook 'turn-on-ctags-auto-update-mode)
  (setq ctags-update-delay-seconds (* 30 60)) ; every 1/2 hour
  (ctags-auto-update-mode 1))

(use-package visual-fill-column
  :ensure t)

(setq display-time-day-and-date t)
(setq display-time-string-forms
      '((format "%s:%s  "
                24-hours minutes)
        (if display-time-day-and-date
            (format "%s %s %s " dayname monthname day) "")))
(setq display-time-interval 30)
(display-time-mode 1)

(setq frame-title-format
      '("emacs@" (:eval (system-name)) ": "(:eval (if (buffer-file-name)
                                                      (abbreviate-file-name (buffer-file-name))
                                                    "%b")) " [%*]"))
(when (executable-find "hunspell")
  (setq ispell-program-name "hunspell")
  (setq-default ispell-program-name "hunspell")
  (setq ispell-extra-args '("-d en_US"))
  (setq ispell-really-hunspell t))
(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))
(add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))
;; No flyspell.
(eval-after-load "flyspell"
  '(defun flyspell-mode (&optional arg)))

;; Autocomplete with a dictionary and hippie-expand
;; The actual expansion function
(defun try-expand-by-dict (old)
  ;; old is true if we have already attempted an expansion
  (unless (bound-and-true-p ispell-minor-mode)
    (ispell-minor-mode 1))

  ;; english-words.txt is the fallback dicitonary
  (if (not ispell-alternate-dictionary)
      (setq ispell-alternate-dictionary (file-truename "~/.emacs.d/misc/english-words.txt")))
  (let ((lookup-func (if (fboundp 'ispell-lookup-words)
                         'ispell-lookup-words
                       'lookup-words)))
    (unless old
      (he-init-string (he-lisp-symbol-beg) (point))
      (if (not (he-string-member he-search-string he-tried-table))
          (setq he-tried-table (cons he-search-string he-tried-table)))
      (setq he-expand-list
            (and (not (equal he-search-string ""))
                 (funcall lookup-func (concat (buffer-substring-no-properties (he-lisp-symbol-beg) (point)) "*")))))
    (if (null he-expand-list)
        (if old (he-reset-string))
      (he-substitute-string (car he-expand-list))
      (setq he-expand-list (cdr he-expand-list))
      t)))

;; Hippie expand.
(setq hippie-expand-try-functions-list
      '(yas-hippie-try-expand
        try-expand-all-abbrevs
        try-expand-dabbrev
        try-expand-dabbrev-from-kill
        try-expand-dabbrev-all-buffers
        try-complete-file-name
        try-complete-lisp-symbol
        try-expand-by-dict
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        ))

(defun my/setup-osx-fonts ()
  (interactive)
  (when (eq system-type 'darwin)
    (set-fontset-font "fontset-default" 'symbol "Monaco")
    ;;(set-default-font "Fantasque Sans Mono")
    ;;(set-default-font "Monaco")
    ;;(set-default-font "Anonymous Pro")
    ;;(set-default-font "Inconsolata")
    (set-default-font "Bitstream Vera Sans Mono")
    ;;(set-default-font "Menlo")
    ;;(set-default-font "Source Code Pro")
    ;;(set-default-font "Mensch")
    (set-face-attribute 'default nil :height 120)
    (set-face-attribute 'fixed-pitch nil :height 120)
    ;; Anti-aliasing
    (setq mac-allow-anti-aliasing t)))


(defun my/setup-x11-fonts ()
  (interactive)
  (when (eq window-system 'x)
    ;; Font family
    (set-frame-font "DejaVu Sans Mono")
    ;; (set-frame-font "Ubuntu Mono")
    ;; (set-frame-font "Hack")
    ;; (set-frame-font "Fantasque Sans Mono")
    ;; (set-frame-font "Anonymous Pro")
    ;; (set-frame-font "Inconsolata")
    (set-face-attribute 'default nil :height 105)))


(use-package better-defaults)

(setq show-paren-style 'parenthesis)
(setq show-paren-priority -50) ; without this matching parens aren't highlighted in region
(setq show-paren-delay 0)
;; (set-face-attribute 'show-paren-match nil :weight 'normal :foreground "lemon chiffon" :background "default")

(require 'autoinsert)
;; (setq auto-insert-directory "~/.emacs.d/template/")
(auto-insert-mode)
(setq auto-insert-query nil)
(add-hook 'find-file-hook 'auto-insert)
(add-hook 'find-file-not-found-hooks 'auto-insert)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(mapc (lambda (x)
        (add-to-list 'completion-ignored-extensions x))
      '(".aux" ".bbl" ".blg" ".exe"
        ".log" ".meta" ".out" ".pdf"
        ".synctex.gz" ".tdo" ".toc"
        "-pkg.el" "-autoloads.el"
        "Notes.bib" "auto/"))


(setq custom-file "~/.emacs.d/emacs-customizations.el")
(shut-up (load custom-file 'noerror))

(validate-setq display-time-world-list '(
                                         ("America/New_York"      "New York")
                                         ("Europe/London"         "London")
                                         ("Asia/Calcutta"         "New Delhi")
                                         ("Asia/Calcutta"         "Hyderabad")
                                         ("Asia/Singapore"        "Singapore")
                                         ("Asia/Tokyo"            "Tokyo")
                                         ("Australia/Melbourne"   "Melbourne")
                                         ("America/Los_Angeles"   "San Franscisco")
                                         ))
(require 'init-ibuffer)

(eval-when-compile (require 'use-package))

(use-package compile
  :diminish compilation-in-progress
  :config
  (setq compilation-ask-about-save nil)
  (setq compilation-scroll-output 'next-error)
  (setq compilation-skip-threshold 2))

(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

(setq package-enable-at-startup nil)

;; note: cpu expensive, disable later if find any issues
;; (setq auto-revert-check-vc-info t)

(setq load-prefer-newer t)

(provide 'init-defaults)
