(use-package flyspell
;;  :ensure-system-package hunspell
  :init
  (setq flyspell-issue-welcome-flag nil)
  :config
  (progn
    (setq ispell-program-name (executable-find "hunspell")
          ispell-dictionary "en_US"
          ispell-local-dictionary-alist '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US,en_US-med") nil utf-8)))
    (setq ispell-extra-args '("-d en_US"))
    (setq ispell-really-hunspell t)
    (add-hook 'prog-mode-hook #'flyspell-prog-mode)
    (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))
    (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
    (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
    (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))))

(use-package flyspell-correct
  :ensure t
  :commands (flyspell-correct-word-generic
             flyspell-correct-previous-word-generic)
  :init
  (setq flyspell-correct-interface 'flyspell-correct-ivy))

(use-package flyspell-correct-ivy
  :ensure t
  :after flyspell-correct
  :config
  (progn
    (bind-key [remap ispell-word] 'flyspell-correct-word-generic)))


;; No flyspell.
;; (eval-after-load "flyspell"
;;   '(defun flyspell-mode (&optional arg)))

;; Autocomplete with a dictionary and hippie-expand
;; The actual expansion function
(defun try-expand-by-dict (old)
  ;; old is true if we have already attempted an expansion
  (unless (bound-and-true-p ispell-minor-mode)
    (ispell-minor-mode 1))

  ;; english-words.txt is the fallback dicitonary
  (if (not ispell-alternate-dictionary)
      (setq ispell-alternate-dictionary (file-truename "~/.emacs.d/misc/english-words.txt")))
  (let ((lookup-func (if (fboundp 'ispell-lookup-words)
                         'ispell-lookup-words
                       'lookup-words)))
    (unless old
      (he-init-string (he-lisp-symbol-beg) (point))
      (if (not (he-string-member he-search-string he-tried-table))
          (setq he-tried-table (cons he-search-string he-tried-table)))
      (setq he-expand-list
            (and (not (equal he-search-string ""))
                 (funcall lookup-func (concat (buffer-substring-no-properties (he-lisp-symbol-beg) (point)) "*")))))
    (if (null he-expand-list)
        (if old (he-reset-string))
      (he-substitute-string (car he-expand-list))
      (setq he-expand-list (cdr he-expand-list))
      t)))



(provide 'init-spell-check)
