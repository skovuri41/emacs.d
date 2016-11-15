(package-initialize)
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))
(setq default-directory (getenv "HOME"))
(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))
(require 'init-platform)
(require 'use-package)
(require 'init-defaults)
(require 'init-defuns)
(require 'init-ediff)
(require 'init-diminish)
(require 'init-helm)
(require 'init-whichkey)
(require 'init-yasnippet)
(require 'init-projectile)
(require 'init-dired)
(require 'init-ido)
(require 'init-magit)
(require 'init-fancy-narrow)
(require 'init-org-2)
(require 'init-org-bullets)
(require 'init-clojure)
(require 'init-programming)
(require 'init-expand-region)
(require 'init-undotree)
(require 'init-hl-line)
(require 'init-drag-stuff)
(require 'init-hydra)
(require 'init-nxml)
(require 'init-company)
(require 'init-easy-kill)
(require 'init-imenu-anywhere)
(require 'init-zop-to-char)
(require 'init-iedit)
(require 'init-elisp-slime-nav)
(require 'init-aggressive-indent)
(require 'init-ag)
(require 'init-anzu)
(require 'init-smooth-scrolling)
(require 'init-beacon)
(require 'init-super-save)
(require 'init-link-hint)
(require 'init-flycheck)
(require 'init-git)
(require 'init-highlight-parentheses)
(require 'init-weather)
(require 'init-eval-sexp-fu)
(require 'init-ace-window)
(require 'init-avy)
(require 'init-kurecolor)
(require 'init-transpose)
(require 'init-rainbow-mode)
(require 'init-help)
(require 'init-sql)
(require 'init-golden-ratio)
(require 'init-neotree)
(require 'init-popwin)
(require 'init-perspective)
(require 'init-popup)
(require 'init-keyfreq)
(require 'init-color-identifiers)
(require 'init-restclient)
(require 'init-fancy-battery)
(require 'init-minor-modes)
(require 'init-window-numbering)
(require 'init-cal)
(require 'init-htmlize)
(require 'init-eclim)
(require 'init-engine)
(require 'init-pdfview)
(require 'init-spaceline)
(require 'init-docker)
(require 'init-local)
(require 'init-ivy)
(require 'init-utils)
(require 'init-xah-fly-keys)
(require 'init-composable)
(require 'init-keybindings)
(require 'startscreen)

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (select-frame frame)
                (setq ns-use-srgb-colorspace nil)
                (load-theme 'spacemacs-dark t)))
  (load-theme 'spacemacs-dark t))
(setup-startscreen-hook)

(require 'server)
(or (server-running-p) (server-start))
