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

  (defun nxml-pretty-print-buffer ()
    "pretty print the XML in a buffer."
    (interactive)
    (nxml-pretty-print-region (point-min) (point-max)))

  ;; XML pretty print
  (defun pretty-print-xml-region (begin end)
    (interactive "r")
    (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char) (insert "\n"))
      (indent-region begin end))
    (message "Ah, much better!"))

  (defun nxml-kill-tag-contents ()
    "Copy the contents between two tags"
    ;; (interactive "*p\ncCopy tag contents: ") ; this expects arguments input
    (interactive)
    (nxml-backward-up-element)
    (kill-region
     (progn (search-forward ">")
            (point))
     (progn (nxml-backward-up-element)
            (nxml-forward-element)
            (search-backward "</")
            (point))))

  (defun nxml-copy-tag-contents ()
    "Copy the contents between two tags"
    ;; (interactive "*p\ncCopy tag contents: ") ; this expects arguments input
    (interactive)
    (nxml-backward-up-element)
    (copy-region-as-kill
     (progn (search-forward ">") (point))
     (progn (nxml-backward-up-element)
            (nxml-forward-element)
            (search-backward "</")
            (point))))


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
