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

(when *is-gnu-linux*
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
  (setq exec-path (append exec-path '("/usr/local/bin")))
  (setq default-directory (getenv "HOME"))
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives
               '("melpa-stable" . "http://stable.melpa.org/packages/"))
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

  (use-package quelpa
    :defer 10
    :ensure t
    :init
    (setq quelpa-update-melpa-p nil))

  ;; Enable the use of Quelpa with use-package.
  (use-package quelpa-use-package
    :defer 10
    :ensure t
    :config
    (quelpa-use-package-activate-advice))

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
  (set-frame-size (selected-frame) 1920 1080 t)
  (setq
   ;; font
   ;; default-frame-alist '((font . "Monaco-10"))
   ;; default-frame-alist '((font . "Fira Code-11"))
   default-frame-alist '((font . "Hasklig-12"))
   ;; default-frame-alist '((font . "Hasklig-12")
   ;;                       (width . 120) ;character
   ;;                       (height . 40))
                                        ; lines
   ;; make emacs use the clipboard
   x-select-enable-clipboard t))

(when *is-a-mac*
  ;; Bootstrap quelpa
  ;; (if (require 'quelpa nil t)
  ;;     (quelpa-self-upgrade)
  ;;   (with-temp-buffer
  ;;     (url-insert-file-contents
  ;;      "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
  ;;     (eval-buffer)))

  ;; with no self upgrade
  (package-initialize)
  (unless (require 'quelpa nil t)
    (with-temp-buffer
      (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
      (eval-buffer)))

  ;; Make Quelpa prefer MELPA-stable over melpa. This is optional but
  ;; highly recommended.
  ;;
  ;; (setq quelpa-stable-p t)

  (quelpa
   '(quelpa-use-package
     :fetcher github
     :repo "quelpa/quelpa-use-package"))
  
  (require 'quelpa-use-package)

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

(provide 'init-platform)
