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
   default-frame-alist '((font . "Menlo-13"))
   ;; Work around a bug on OS X where system-name is FQDN
   system-name (car (split-string system-name "\\."))
   ;; make emacs open in existing frames
   ;;ns-pop-up-frames nil
   ;; brew install hunspell, https://joelkuiper.eu/spellcheck_emacs
   ispell-program-name "hunspell"
   ;; modifier keys meta and cmd swapped in system preferences
   ;; but could do it here with
   ;;mac-command-modifier 'meta
   ;;mac-option-modifier 'super
   ;; hitting cmd still gets me in trouble in emacs though
   mac-command-modifier nil))
(when *is-gnu-linux*
  (setq
   ;; font
   default-frame-alist '((font . "Monospace-12"))
   ;; make emacs use the clipboard
   x-select-enable-clipboard t
   ;; use hunspell
   ispell-program-name "hunspell"))

;; No slow flyspell. 
(eval-after-load "flyspell"
  '(defun flyspell-mode (&optional arg)))

;;;; Modes ;;;;
(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)
(add-hook 'clojure-mode-hook 'prettify-symbols-mode)
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
(global-set-key (kbd "<home>") 'xah-backward-left-bracket)
(global-set-key (kbd "<end>") 'xah-forward-right-bracket)
(global-set-key (kbd "<prior>") 'xah-backward-block) ; page up key
(global-set-key (kbd "<next>") 'xah-forward-block) ; page down key
;;xah-search-current-word
(global-set-key (kbd "<f8>") 'xah-search-current-word)
(global-set-key (kbd "<prior>") 'xah-backward-block) ; page up key
(global-set-key (kbd "<next>") 'xah-forward-block) ; page down key
(global-set-key (kbd "M-n") 'xah-new-empty-buffer) ; new empty buffer
(global-set-key (kbd "<f2>") 'xah-cut-line-or-region) ; cut
(global-set-key (kbd "<f3>") 'xah-copy-line-or-region) ; copy
(global-set-key (kbd "<f4>") 'yank) ; paste
(global-set-key '[(f1)]          'call-last-kbd-macro)
(global-set-key '[(shift f1)]    'toggle-kbd-macro-recording-on)
(define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)
(global-set-key (kbd "M-`") 'helm-all-mark-rings)
