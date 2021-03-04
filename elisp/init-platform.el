;;; ;; Platform specific settings
(defvar *is-a-mac*)
(defvar *is-carbon-emacs*)
(defvar *is-cocoa-emacs*)
(defvar *is-gnu-linux*)
(setq
 *is-a-mac* (eq system-type 'darwin)
 *is-carbon-emacs* (and *is-a-mac* (eq window-system 'mac))
 *is-cocoa-emacs* (and *is-a-mac* (eq window-system 'ns))
 *cygwin* (eq system-type 'cygwin)
 *is-gnu-linux* (eq system-type 'gnu/linux))

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(ac-cider . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(clojure-mode . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(clj-refactor . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(cider-eval-sexp-fu. "melpa-stable") t)

(when *is-gnu-linux*
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
  (setq exec-path (append exec-path '("/usr/local/bin")))
  (setq default-directory (getenv "HOME"))
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives
               '("melpa-stable" . "http://stable.melpa.org/packages/"))
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  (add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
  (add-to-list 'package-pinned-packages '(ac-cider . "melpa-stable") t)
  (add-to-list 'package-pinned-packages '(clojure-mode . "melpa-stable") t)
  (add-to-list 'package-pinned-packages '(clj-refactor . "melpa-stable") t)
  (add-to-list 'package-pinned-packages '(cider-eval-sexp-fu. "melpa-stable") t)

  (setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
  ;; Bootstrap `use-package'
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  ;; ;; Install all packages required
  (load-file (expand-file-name "elisp/init-elpa-list.el" user-emacs-directory))

  (package-initialize)
  ;; ;; Fetch packages the first time
  (unless (file-exists-p package-user-dir)
    (package-refresh-contents))
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p)))
  (setq frame-resize-pixelwise t)
  ;; (set-face-italic 'tabbar-unselected nil)
  (set-frame-position (selected-frame) 0 0)
  ;; (set-frame-size (selected-frame) 1800 920 t)
  ;; (setq
  ;;  ;; font
  ;;  ;; default-frame-alist '((font . "Monaco-10"))
  ;;  ;; default-frame-alist '((font . "Fira Code-11"))
  ;;  ;;default-frame-alist '((font . "Hasklig-12"))
  ;;  default-frame-alist '((font . "JetBrains Mono-11"))
  ;;  ;; ;; default-frame-alist '((font . "Hasklig-12")
  ;;  ;; ;;                       (width . 120) ;character
  ;;  ;; ;;                       (height . 40))
  ;;  ;;                                      ; lines
  ;;  ;; ;; make emacs use the clipboard
  ;;  select-enable-clipboard t)
  ;; (set-frame-font "JetBrainsMono 11" nil t)
  (set-face-attribute 'default
                      nil
                      :font "JetBrainsMono"
                      :height 120
                      :weight 'normal)
  ;; (set-face-attribute 'default
  ;;                     nil
  ;;                     :font "JetBrainsMono"
  ;;                     :height 105
  ;;                     :weight 'medium)

  ;; (set-frame-font "Fira Code 11" nil t)

  (defun linux-copy (beg end)
    (interactive "r")
    (call-process-region beg end  "xclip" nil nil nil "-selection" "c"))

  (defun linux-paste ()
    (interactive)
    (if (region-active-p) (delete-region (region-beginning) (region-end)) nil)
    (call-process "xsel" nil t nil "-b"))

  (setq select-enable-clipboard t)
  (setq interprogram-paste-function 'x-selection-value))

(when *is-a-mac*

  ;; with no self upgrade
  (package-initialize)
  (setq
   ;; for multilingual environments
   default-input-method "MacOSX"
   ;; ;; font
   ;; default-frame-alist '((font . "Monaco-13")
   ;;                       (width . 120)  ;character
   ;;                       (height . 52)) ; lines
   ;; ;; Work around a bug on OS X where system-name is FQDN
   system-name (car (split-string system-name "\\."))
   ;; make emacs open in existing frames
   ;;ns-pop-up-frames nil
   interprogram-cut-function 'paste-to-osx
   interprogram-paste-function 'copy-from-osx
   mac-command-modifier nil)
  (require 'init-fira)
  (require 'init-cask)
  (require 'init-pbcopy))

;; (message
;;     (mapconcat (quote identity)
;;         (sort (font-family-list) #'string-lessp) "\n"))

(provide 'init-platform)
