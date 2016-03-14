(when *is-a-mac*
  (setq
   ;; for multilingual environments
   default-input-method "MacOSX"
   ;; font
   default-frame-alist '((font . "Monaco-13")
                         (width . 120)  ;character
                         (height . 52)) ; lines
   ;; Work around a bug on OS X where system-name is FQDN
   system-name (car (split-string system-name "\\."))
   ;; make emacs open in existing frames
   ;;ns-pop-up-frames nil
   interprogram-cut-function 'paste-to-osx
   interprogram-paste-function 'copy-from-osx
   mac-command-modifier nil))

(when *is-gnu-linux*
  (setq
   ;; font
   default-frame-alist '((font . "Monaco-10"))
   ;; make emacs use the clipboard
   x-select-enable-clipboard t))

(provide 'init-platform)
