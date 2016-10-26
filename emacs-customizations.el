;;; emacs-customizations.el --- emacs customizations  -*- lexical-binding: t; -*-
;; Keywords: 
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(show-paren-match ((t (:background "default" :foreground "yellow" :underline t :overline nil))))
 '(show-paren-mismatch ((((class color)) (:background "red" :foreground "white")))))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(compilation-message-face (quote default))
 '(fci-rule-color "#383838")
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#20240E" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#20240E" . 100))))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (col-highlight back-button imenu-list esup helm-hunks command-log-mode define-word chess boxquote lorem-ipsum org-pdfview pdf-tools ztree zop-to-char zenburn-theme yaml-mode xah-fly-keys wrap-region workgroups2 worf window-numbering whitespace-cleanup-mode which-key wgrep-ag web-mode volatile-highlights vlf visual-fill-column visible-mark validate use-package typit typed-clojure-mode transpose-frame tiny tidy symon super-save sunshine sqlup-mode sql-indent spacemacs-theme spaceline smooth-scrolling smex smartscan shrink-whitespace sequential-command scratch-message scratch restclient-helm region-bindings-mode recentf-ext ranger rainbow-mode rainbow-delimiters quickrun popwin popup-switcher popup-imenu peep-dired pbcopy paren-face paradox page-break-lines ox-reveal outshine osx-trash osx-browse origami org-pomodoro org-journal org-cliplink org-bullets neotree mwim mu4e-maildirs-extension mu4e-alert move-text monokai-theme markdown-mode magit logview log4j-mode lispy link-hint kurecolor know-your-http-well keyfreq key-chord js2-mode java-imports ivy-hydra imenu-anywhere ido-vertical-mode hungry-delete hl-line+ highlight-parentheses highlight-numbers helm-swoop helm-projectile helm-org-rifle helm-mu helm-gtags helm-google helm-descbinds helm-company helm-cider helm-backup helm-ag grizzl graphviz-dot-mode golden-ratio-scroll-screen golden-ratio gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-gutter-fringe+ fullframe forecast focus flyspell-popup flyspell-correct-ivy flycheck-clojure find-file-in-project fancy-narrow fancy-battery eyebrowse exec-path-from-shell evil-nerd-commenter evil-matchit evil-leader engine-mode embrace elisp-slime-nav easy-kill-extras drag-stuff dockerfile-mode docker discover-my-major dired-subtree dired-rainbow dired-quick-sort dired-narrow dired-launch dired+ diff-hl ctags-update csv-nav csv-mode counsel-projectile counsel-osx-app company-try-hard company-statistics company-quickhelp company-emoji company-emacs-eclim color-identifiers-mode clojure-snippets clojure-cheatsheet cljsbuild-mode clj-refactor cider-eval-sexp-fu cask-mode calfw-gcal calfw bug-hunter buffer-move bm better-defaults beacon anzu aggressive-indent ag ace-jump-mode 4clojure)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(weechat-color-list
   (unspecified "#272822" "#20240E" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
