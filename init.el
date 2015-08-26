(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)

;; This sets up the load path so that we can override it
(package-initialize nil)

;; Override the packages with the git version of Org and other packages
(add-to-list 'load-path "~/.emacs.d/elisp/org-mode/lisp")
(setq org-ditaa-jar-path "~/.emacs.d/elisp/org-mode/contrib/scripts/ditaa.jar")

;; Load the rest of the packages
(package-initialize t)
(setq package-enable-at-startup nil)
(defface org-block-background
       '((t (:background "#474747")))
       "Face used for the source block background.")

(require 'org)
;;(require 'org-plus-contrib)
(require 'ob)
(require 'ob-tangle)

(require 'ob-clojure)
(require 'cl)
(require 'ob-ditaa)

(setq my-emacs-directory (file-name-directory (file-truename load-file-name)))
(org-babel-load-file (expand-file-name "my-emacs.org" my-emacs-directory))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((sh         . t)
   (js         . t)
   (emacs-lisp . t)
   (perl       . t)
   (scala      . t)
   (clojure    . t)
   (python     . t)
   (dot        . t)
   (css        . t)
   (plantuml   . t)
   (ditaa . t)))

(setq org-confirm-babel-evaluate nil)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(custom-safe-themes
   (quote
    ("9dae95cdbed1505d45322ef8b5aa90ccb6cb59e0ff26fef0b8f411dfc416c552" default)))
 '(fci-rule-color "#383838")
 '(org-bullets-bullet-list (quote ("●" "○" "►" "✿")))
 '(package-selected-packages
   (quote
    (org-plus-contrib zoom-frm zenburn-theme workgroups2 workgroups use-package undo-tree smex smartparens smart-mode-line rainbow-delimiters plantuml-mode page-break-lines org-journal org-bullets org multiple-cursors magit latest-clojure-libraries jump-char json-mode ipretty inf-mongo iedit ido-vertical-mode htmlize helm-swoop helm-projectile helm-orgcard helm-gtags helm-descbinds guide-key grizzl gitignore-mode gitconfig-mode git-messenger git-gutter-fringe git-blame flx-ido expand-region drag-stuff direx deft company clojure-snippets clojure-cheatsheet buffer-move bookmark+ ace-window ace-jump-buffer ac-cider 4clojure)))
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
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
