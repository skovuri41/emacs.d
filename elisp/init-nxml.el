(use-package nxml-mode
  :mode
  (("\.xml$" . nxml-mode)
   ("\.xsl$" . nxml-mode)
   ("\.xhtml$" . nxml-mode)
   ("\.page$" . nxml-mode))
  :config
  (setq nxml-child-indent 2)                  
  (setq nxml-attribute-indent 2)              
  (setq indent-tabs-mode nil)
  (setq nxml-slash-auto-complete-flag t)     
  (setq tab-width 2)


  (custom-set-faces
   ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
   ;; Your init file should contain only one such instance.
   '(nxml-comment-content-face ((t (:foreground "yellow4"))))
   '(nxml-comment-delimiter-face ((t (:foreground "yellow4"))))
   '(nxml-delimited-data-face ((t (:foreground "lime green"))))
   '(nxml-delimiter-face ((t (:foreground "grey"))))
   '(nxml-element-local-name-face ((t (:inherit nxml-name-face :foreground "medium turquoise"))))
   '(nxml-name-face ((t (:foreground "rosy brown"))))
   '(nxml-tag-slash-face ((t (:inherit nxml-name-face :foreground "grey")))))
  )

(provide 'init-nxml)
