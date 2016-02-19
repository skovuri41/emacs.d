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
(setq show-trailing-whitespace t)

;; Allow pasting selection outside of Emacs
(setq x-select-enable-clipboard t)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

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
(setq save-place-file (expand-file-name "saveplace" user-emacs-directory))
(setq-default save-place t)
(require 'saveplace)


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

;; Don't break lines for me, please
(setq-default truncate-lines t)

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

(use-package uniquify
  :config
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*")
  (setq uniquify-buffer-name-style 'post-forward uniquify-separator ":"))

;; A saner ediff
(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(setq initial-major-mode 'org-mode)
(setq  initial-scratch-message nil)

(defmacro after (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
     '(progn ,@body)))


;; make sure $PATH is set correctly
(use-package exec-path-from-shell
  :ensure exec-path-from-shell
  :disabled t
  :config
  (progn
    (exec-path-from-shell-copy-env "PATH")))

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

(provide 'init-defaults)
