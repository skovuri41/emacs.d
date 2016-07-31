;; Platform specific settings
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

(when *is-a-mac*
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

  ;; (setq
  ;;  ;; font
  ;;  default-frame-alist '((font . "Monaco-13")
  ;;                        (width . 120)    ;character
  ;;                        (height . 52)))  ; lines


  )

(when *is-a-mac*
  (require 'init-fira)
  )

(when *is-gnu-linux*
  (setq
   ;; font
   default-frame-alist '((font . "Monaco-10"))
   ;; make emacs use the clipboard
   x-select-enable-clipboard t))

(when *is-a-mac*
  (require 'init-cask)
  )

(provide 'init-platform)
