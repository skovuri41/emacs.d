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

;;make mouse scrolling smooth
(setq  mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))))

;;Suppress symbolic link warnings
(setq find-file-visit-truename t)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

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
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top
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

;; Save a list of recent files visited. (open recent file with C-x f)
(recentf-mode 1)
(setq recentf-max-saved-items 1000) ;; just 20 is too recent
(setq delete-old-versions t) ;; dont ask to delete excess backup versions

;; Remember the current position of files when re-opening them
(use-package saveplace
  :defer t
  :init
  (setq-default save-place t)
  (setq save-place-file (expand-file-name ".saveplaces" user-emacs-directory)))


;; Undo/redo window configuration with C-c <left>/<right>
(winner-mode 1)

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

;; Don't be so stingy on the memory, we have lots now. It's the distant future.
(setq gc-cons-threshold 20000000)

;; enable erase-buffer command
(put 'erase-buffer 'disabled nil)

;; Represent undo-history as an actual tree (visualize with C-x u)
(setq undo-tree-mode-lighter "")
(require 'undo-tree)
(global-undo-tree-mode)

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(use-package uniquify
  :config
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*")
  (setq uniquify-buffer-name-style 'post-forward uniquify-separator ":"))

(when (boundp 'global-prettify-symbols-mode)
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (push '("lambda" . ?λ) prettify-symbols-alist)))
  (global-prettify-symbols-mode +1))

;; A saner ediff
(use-package ediff
  :config
  (progn
    (setq ediff-diff-options "-w")
    (setq ediff-split-window-function 'split-window-horizontally)
    (setq ediff-window-setup-function 'ediff-setup-windows-plain)))


;; Match fringe colour to background colour
(defun my/set-fringe-bg()
  (set-face-attribute 'fringe nil
                      :foreground (face-foreground 'default)
                      :background (face-background 'default)))

;; Indicate where a buffer starts and stops
(setq-default indicate-buffer-boundaries 'right)

(setq initial-major-mode 'org-mode)
(setq  initial-scratch-message nil)

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
    (exec-path-from-shell-copy-env "PATH")))

(use-package hungry-delete
  :config
  (progn
    (setq hungry-delete-chars-to-skip " \t\r\f\v")
    (global-hungry-delete-mode nil)
    (add-hook 'prog-mode-hook 'hungry-delete-mode)))

;; Note: for every project, run the following command:
;; ctags -e -R .

(use-package ctags-update
  :ensure t
  :config
  (add-hook 'prog-mode-hook  'turn-on-ctags-auto-update-mode)
  (ctags-auto-update-mode)
  :diminish ctags-auto-update-mode)

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
      t)
    ))

(setq hippie-expand-try-functions-list
      '(;; try-expand-dabbrev
        ;; try-expand-dabbrev-all-buffers
        try-expand-by-dict))

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

;; (when (eq system-type 'darwin)
;;   (add-hook 'after-init-hook #'my/setup-osx-fonts))


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

;; (when (eq window-system 'x)
;;   (add-hook 'after-init-hook #'my/setup-x11-fonts))

(use-package better-defaults)


(setq show-paren-priority -50) ; without this matching parens aren't highlighted in region
(setq show-paren-delay 0)
(set-face-attribute 'show-paren-match nil :weight 'normal :foreground "lemon chiffon" :background "default")


(provide 'init-defaults)
