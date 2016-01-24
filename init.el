;;;; Initialize ;;;;

(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)

(require 'use-package)
;; Add /usr/local/bin to path
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))
(setq default-directory (getenv "HOME"))

;;(server-start)

(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))
(require 'init-defaults)
(use-package better-defaults)
(require 'init-powerline)
(require 'init-evil)
;;(require 'init-guidekey)
(require 'init-whichkey)
(require 'init-yasnippet)
(require 'init-projectile)
(require 'init-ido)
(require 'init-magit)
(require 'init-fancy-narrow)
(require 'init-org)
(require 'init-clojure)
(require 'init-zenburn)
;;(require 'init-minor-modes)
(require 'init-programming)
(require 'init-helm)
(require 'init-expand-region)
(require 'init-undotree)
(require 'init-smartparens)
(require 'init-hl-line)
(require 'init-drag-stuff)
(require 'init-defuns)
(require 'init-company)
(require 'init-avy)

;; No slow flyspell. 
(eval-after-load "flyspell"
  '(defun flyspell-mode (&optional arg)))

;;;; Modes ;;;;
(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)
(add-hook 'clojure-mode-hook 'prettify-symbols-mode)
;;(add-hook 'prog-mode-hook 'relative-line-numbers-mode t)
;;(add-hook 'prog-mode-hook 'line-number-mode t)
;;(add-hook 'prog-mode-hook 'column-number-mode t)
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
(add-to-list 'magic-mode-alist '(";;; " . emacs-lisp-mode))

(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-r") 'helm-recentf)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
; Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-M-%") 'query-replace)

;;switch prev nex user buffers
(global-set-key (kbd "<f11>") 'xah-previous-user-buffer)
(global-set-key (kbd "<f12>") 'xah-next-user-buffer)

(global-set-key (kbd "<S-f11>") 'xah-previous-emacs-buffer)
(global-set-key (kbd "<S-f12>") 'xah-next-emacs-buffer)

;;xah-search-current-word
(global-set-key (kbd "<f8>") 'xah-search-current-word)
(global-set-key (kbd "<prior>") 'xah-backward-block) ; page up key
(global-set-key (kbd "<next>") 'xah-forward-block) ; page down key

(global-set-key (kbd "M-n") 'xah-new-empty-buffer) ; new empty buffer

(global-set-key (kbd "<f2>") 'xah-cut-line-or-region) ; cut
(global-set-key (kbd "<f3>") 'xah-copy-line-or-region) ; copy
(global-set-key (kbd "<f4>") 'yank) ; paste
