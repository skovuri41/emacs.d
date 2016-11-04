(use-package pdf-tools
  :if (eq system-type 'darwin)
  :ensure t
  :init (pdf-tools-install)
  :bind (:map pdf-view-mode-map
              ("." . hydra-pdftools/body)
              ("<s-spc>" . pdf-view-scroll-down-or-next-page)
              ("g" . pdf-view-first-page)
              ("G" . pdf-view-last-page)
              ("l" . image-forward-hscroll)
              ("h" . image-backward-hscroll)
              ("e" . pdf-view-goto-page)
              ("u" . pdf-view-revert-buffer)
              ("al" . pdf-annot-list-annotations)
              ("ad" . pdf-annot-delete)
              ("aa" . pdf-annot-attachment-dired)
              ("am" . pdf-annot-add-markup-annotation)
              ("as" . pdf-annot-add-squiggly-markup-annotation)
              ("ao" . pdf-annot-add-strikeout-markup-annotation)
              ("au" . pdf-annot-add-underline-markup-annotation)
              ("ah" . pdf-annot-add-highlight-markup-annotation)
              ("at" . pdf-annot-add-text-annotation)
              ("y" . pdf-view-kill-ring-save)
              ("$" . pdf-misc-display-metadata)
              ("s" . pdf-occur)
              ("b" . pdf-view-set-slice-from-bounding-box)
              ("Q" . pdf-outline-quit-and-kill)
              ("r" . pdf-view-reset-slice))
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :config
  (progn
    (use-package pdf-annot)
    (use-package pdf-links)
    (use-package pdf-info)
    (use-package pdf-misc)
    (use-package pdf-sync)
    (use-package pdf-occur)
    (use-package pdf-outline)

    (add-hook 'pdf-view-mode-hook
              (lambda ()
                (pdf-misc-size-indication-minor-mode)
                (pdf-links-minor-mode)
                (pdf-isearch-minor-mode)))
    (add-hook 'pdf-view-mode-hook 'pdf-view-go-to-last-bookmark)
    (add-hook 'pdf-view-mode-hook 'pdf-annot-minor-mode)
    (add-hook 'pdf-view-mode-hook 'pdf-outline-minor-mode)
    (add-hook 'pdf-view-mode-hook
              (lambda ()
                (xah-fly-insert-mode-activate)))

    (defun pdf-view-go-to-last-bookmark ()
      (interactive)
      (when (member (concat "last-" (buffer-name)) (mapcar #'first bookmark-alist))
        (bookmark-jump (concat "last-" (buffer-name)))))

    (defun pdf-view-save-bookmark ()
      (interactive)
      (when (eq major-mode 'pdf-view-mode)
        (bookmark-set (concat "last-" (buffer-name)))))

    (defun pdf-view-center-pdf ()
      (interactive)
      (image-forward-hscroll 10))

    (advice-add #'pdf-view-enlarge :after (lambda (&rest args) (pdf-view-center-pdf)))
    (advice-add #'interleave--sync-pdf-page-current :after (lambda (&rest args) (pdf-view-center-pdf)))

    (use-package interleave :ensure t)

    (setq auto-revert-interval 0.5)
    (auto-revert-set-timer)

    (define-key pdf-view-mode-map "j" 'pdf-view-next-line-or-next-page)
    (define-key pdf-view-mode-map "k" 'pdf-view-previous-line-or-previous-page)

    (bind-key "i"
              #'modi/imenu-list-display-toggle pdf-view-mode-map)

    (bind-key "O"
              #'ace-window pdf-view-mode-map)

    (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward-regexp)

    (define-key pdf-outline-buffer-mode-map (kbd "k") 'previous-line)
    (define-key pdf-outline-buffer-mode-map (kbd "j") 'next-line)
    (define-key pdf-outline-buffer-mode-map (kbd "i") 'outline-toggle-children)
    (define-key pdf-outline-buffer-mode-map (kbd "l") 'pdf-outline-follow-link)
    (define-key pdf-outline-buffer-mode-map (kbd "h") 'pdf-outline-up-heading)

    (define-key pdf-annot-list-mode-map (kbd "k") 'previous-line)
    (define-key pdf-annot-list-mode-map (kbd "j") 'next-line)
    (define-key pdf-annot-list-mode-map (kbd "l") 'pdf-annot-list-display-annotation-from-id)

    (defhydra hydra-pdftools (:color blue :hint nil)
      "
       Move  History   Scale/Fit     Annotations  Search/Link    Do   │ PDF Tools │
   ╭──────────────────────────────────────────────────────────────────┴───────────╯
         ^^_g_^^      _B_    ^↧^    _+_    ^ ^     [_al_] list    [_s_] search    [_u_] revert buffer
         ^^^↑^^^      ^↑^    _H_    ^↑^  ↦ _W_ ↤   [_am_] markup  [_o_] outline   [_i_] info
         ^^_k_^^      ^ ^    ^↥^    _0_    ^ ^     [_at_] text    [_F_] link      [_d_] dark mode
         ^^^↑^^^      ^↓^  ╭─^─^─┐  ^↓^  ╭─^ ^─┐   [_ad_] delete  [_f_] search link [_m_] bookmark
    _h_ ←pag_e_→ _l_  _N_  │ _P_ │  _-_    _b_     [_aa_] dired
         ^^^↓^^^      ^ ^  ╰─^─^─╯  ^ ^  ╰─^ ^─╯   [_y_]  yank
         ^^_j_^^      ^ ^  _r_eset slice box
         ^^^↓^^^
         ^^_G_^^
   --------------------------------------------------------------------------------
        "
      ("\\" nil "back")
      ("q" nil "quit")
      ("al" pdf-annot-list-annotations)
      ("ad" pdf-annot-delete)
      ("aa" pdf-annot-attachment-dired)
      ("am" pdf-annot-add-markup-annotation)
      ("at" pdf-annot-add-text-annotation)
      ("y" pdf-view-kill-ring-save)
      ("+" pdf-view-enlarge :color red)
      ("-" pdf-view-shrink :color red)
      ("0" pdf-view-scale-reset)
      ("H" pdf-view-fit-height-to-window)
      ("W" pdf-view-fit-width-to-window)
      ("P" pdf-view-fit-page-to-window)
      ("j" pdf-view-next-page-command :color red)
      ("k" pdf-view-previous-page-command :color red)
      ("d" pdf-view-dark-minor-mode)
      ("b" pdf-view-set-slice-from-bounding-box)
      ("m" pdf-view-save-bookmark)
      ("r" pdf-view-reset-slice)
      ("g" pdf-view-first-page)
      ("G" pdf-view-last-page)
      ("e" pdf-view-goto-page)
      ("o" pdf-outline)
      ("s" pdf-occur)
      ("i" pdf-misc-display-metadata)
      ("u" pdf-view-revert-buffer)
      ("F" pdf-links-action-perform)
      ("f" pdf-links-isearch-link)
      ("B" pdf-history-backward :color red)
      ("N" pdf-history-forward :color red)
      ("l" image-forward-hscroll :color red)
      ("h" image-backward-hscroll :color red))))

(use-package org-pdfview
  :if (eq system-type 'darwin)
  :ensure t
  :after pdf-tools)

(provide 'init-pdfview)
