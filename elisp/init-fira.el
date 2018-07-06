;; Font setup with Fira ;;

(when *is-a-mac*
  ;; (set-frame-font "Fira Code")
  (setq
   default-frame-alist '((font . "Fira Code-14")))
  ;; (setq
  ;;  default-frame-alist '((font . "Fira Code-15")
  ;;                        (width . 80)  ;character
  ;;                        (height . 24)))
  ;; (set-frame-position (selected-frame) 0 0)
  ;; (set-frame-size (selected-frame) 91 63)
  (setq frame-resize-pixelwise t)
  (set-frame-position (selected-frame) 0 0)
  ;; (set-frame-size (selected-frame) 1920 1080 t)
  (modify-all-frames-parameters '((fullscreen . maximized)))
  )


;; Enable ligatures
(mac-auto-operator-composition-mode)

(provide 'init-fira)
