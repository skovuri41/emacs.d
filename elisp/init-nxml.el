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

  ;; See: http://sinewalker.wordpress.com/2008/06/26/pretty-printing-xml-with-emacs-nxml-mode/
  (defun pp-xml-region (beg end)
    "Pretty format XML markup in region. The function inserts
     linebreaks to separate tags that have nothing but whitespace
     between them.  It then indents the markup by using nxml's
     indentation rules."
    (interactive "r")
    (unless (use-region-p)
      (setq beg (point-min)
            end (point-max)))
    ;; Use markers because our changes will move END
    (setq beg (set-marker (make-marker) begin)
          end (set-marker (make-marker) end))
    (save-excursion
      (goto-char beg)
      (while (search-forward-regexp "\>[ \\t]*\<" end t)
        (backward-char) (insert "\n"))
      (nxml-mode)
      (indent-region begin end)))

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

  (use-package tidy
    :ensure t
    :disabled t
    :config
    (add-hook 'nxml-mode-hook (lambda () (tidy-build-menu nxml-mode-map)))
    (defun sanityinc/tidy-buffer-xml (beg end)
      "Run \"tidy -xml\" on the region from BEG to END, or whole buffer."
      (interactive "r")
      (unless (use-region-p)
        (setq beg (point-min)
              end (point-max)))
      (shell-command-on-region beg end "tidy -xml -q -i" (current-buffer) t "*tidy-errors*" t)))


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
