(use-package pdf-tools
  :ensure t
  :mode (("\\.pdf$" . pdf-view-mode))
  :config (pdf-tools-install))

(require 'pdf-view)

(defun pdf-view-go-to-last-bookmark ()
  (interactive)
  (when (member (concat "last-" (buffer-name)) (mapcar #'first bookmark-alist))
    (bookmark-jump (concat "last-" (buffer-name)))))

(add-hook 'pdf-view-mode-hook 'pdf-view-go-to-last-bookmark)
(add-hook 'pdf-view-mode-hook 'pdf-annot-minor-mode)
(add-hook 'pdf-view-mode-hook 'pdf-outline-minor-mode)

(defun pdf-view-save-bookmark ()
  (interactive)
  (when (eq major-mode 'pdf-view-mode)
    (bookmark-set (concat "last-" (buffer-name)))))

(defun pdf-view-center-pdf ()
  (interactive)
  (image-forward-hscroll 10))

(advice-add #'pdf-view-enlarge :after (lambda (&rest args) (pdf-view-center-pdf)))
(advice-add #'interleave--sync-pdf-page-current :after (lambda (&rest args) (pdf-view-center-pdf)))

(use-package interleave)

(setq auto-revert-interval 0.5)
(auto-revert-set-timer)

(define-key pdf-view-mode-map "j" 'pdf-view-next-line-or-next-page)
(define-key pdf-view-map-mode "k" 'pdf-view-previous-line-or-previous-page)
(define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward-regexp)

(define-key pdf-outline-minor-mode-map (kbd "k") 'previous-line)
(define-key pdf-outline-minor-mode-map (kbd "j") 'next-line)
(define-key pdf-outline-minor-mode-map (kbd "i") 'outline-toggle-children)

(provide 'init-pdfview)
