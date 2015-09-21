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

'(org-bullets-bullet-list (quote ("◉" "○" "►" "✿")))
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


