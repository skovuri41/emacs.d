;;;; Initialize ;;;;
(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)

(require 'use-package)
;; Add /usr/local/bin to path
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))
(setq default-directory (getenv "HOME"))
(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))
(require 'init-defaults)
(use-package better-defaults)
(require 'init-powerline)
(require 'init-evil)
(require 'init-whichkey)
(require 'init-yasnippet)
(require 'init-projectile)
(require 'init-ido)
(require 'init-magit)
(require 'init-fancy-narrow)
(require 'init-org)
(require 'init-org-bullets)
(require 'init-clojure)
(require 'init-zenburn)
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
(require 'init-easy-kill)
(require 'init-imenu-anywhere)
(require 'init-dired)
(require 'init-zop-to-char)
(require 'init-iedit)
(require 'init-elisp-slime-nav)
(require 'init-aggressive-indent)
(require 'init-ag)
(require 'init-anzu)
(require 'init-ivy)
(require 'init-smooth-scrolling)
(require 'init-beacon)
(require 'init-super-save)
(require 'init-link-hint)
(require 'init-flycheck)
(require 'init-git)
(require 'init-neotree)
(require 'init-weather)

;; Platform specific settings
(defvar *is-a-mac*)
(defvar *is-carbon-emacs*)
(defvar *is-cocoa-emacs*)
(defvar *is-gnu-linux*)
(setq
 *is-a-mac* (eq system-type 'darwin)
 *is-carbon-emacs* (and *is-a-mac* (eq window-system 'mac))
 *is-cocoa-emacs* (and *is-a-mac* (eq window-system 'ns))
 *is-gnu-linux* (eq system-type 'gnu/linux))
(when *is-a-mac*
  (setq
   ;; for multilingual environments
   default-input-method "MacOSX"
   ;; font
   default-frame-alist '((font . "Monaco-13")
                         (width . 120)  ;character
                         (height . 52)) ; lines
   ;; Work around a bug on OS X where system-name is FQDN
   system-name (car (split-string system-name "\\."))
   ;; make emacs open in existing frames
   ;;ns-pop-up-frames nil
   interprogram-cut-function 'paste-to-osx
   interprogram-paste-function 'copy-from-osx
   mac-command-modifier nil))
(when *is-gnu-linux*
  (setq
   ;; font
   default-frame-alist '((font . "Monospace-12"))
   ;; make emacs use the clipboard
   x-select-enable-clipboard t))
(when (executable-find "hunspell")
  (setq-default ispell-program-name "hunspell")
  (setq ispell-really-hunspell t))

;;;; Modes ;;;;
(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)
(add-hook 'clojure-mode-hook 'prettify-symbols-mode)
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
;;(add-to-list 'magic-mode-alist '(";;; " . emacs-lisp-mode))

(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-r") 'helm-recentf)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
;;(global-set-key (kbd "C-s") 'isearch-forward-regexp)
;;(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
;;(global-set-key (kbd "M-%") 'query-replace-regexp)
;;(global-set-key (kbd "C-M-%") 'query-replace)
;;switch prev nex user buffers
(global-set-key (kbd "<f11>") 'xah-previous-user-buffer)
(global-set-key (kbd "<f12>") 'xah-next-user-buffer)
(global-set-key (kbd "<S-f11>") 'xah-previous-emacs-buffer)
(global-set-key (kbd "<S-f12>") 'xah-next-emacs-buffer)
(global-set-key (kbd "<home>") 'xah-backward-left-bracket)
(global-set-key (kbd "<end>") 'xah-forward-right-bracket)
(global-set-key (kbd "<prior>") 'xah-backward-block) ; page up key
(global-set-key (kbd "<next>") 'xah-forward-block) ; page down key
(global-set-key (kbd "<f8>") 'xah-search-current-word)
;;(global-set-key (kbd "M-n") 'xah-new-empty-buffer) ; new empty buffer
(global-set-key (kbd "M-n") 'new-scratch-buffer) ; new empty buffer
(global-set-key (kbd "<f2>") 'xah-cut-line-or-region) ; cut
(global-set-key (kbd "<f3>") 'xah-copy-line-or-region) ; copy
(global-set-key (kbd "<f4>") 'yank) ; paste
(global-set-key '[(f1)]          'call-last-kbd-macro)
(global-set-key '[(shift f1)]    'toggle-kbd-macro-recording-on)
(define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)
(global-set-key (kbd "M-`") 'helm-all-mark-rings)
(global-set-key (kbd "C-`") 'push-mark-no-activate)
(global-set-key (kbd "C-[ [ a a") 'push-mark-no-activate)
(global-set-key [remap mark-sexp] 'easy-mark)
(global-set-key (kbd "<f7>") 'repeat-complex-command)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key (kbd "C-c I") 'find-user-init-file)
(global-set-key (kbd "C-c E")  'erase-buffer)

(setq ispell-program-name "hunspell")
;; No flyspell. 
(eval-after-load "flyspell"
  '(defun flyspell-mode (&optional arg)))

