(use-package org
  :ensure t
  :defer t
  :commands (org-capture)
  :config
  (setq org-replace-disputed-keys t)
  (setq org-return-follows-link t)
  (add-to-list 'auto-mode-alist '("\\.txt$" . org-mode))
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  (setq org-directory "~/notes/")
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-agenda-include-all-todo t)
  (setq org-agenda-include-diary t)
  ;(setq org-agenda-ndays 7)
  (setq org-agenda-show-all-dates t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-start-on-weekday nil)
  (setq org-startup-indented t)
  (setq org-hide-leading-stars t)
  (setq org-odd-levels-only nil)
  ;; alphabetical lists
  (setq org-alphabetical-lists t)
  (setq org-todo-keywords 
        '((sequence "TODO" 
                    "IN-PROGRESS"
                    "PENDING"
                    "CANCELLED"
                    "DONE")))
  (setq org-capture-templates 
	'(
          ;; capture bookmarks   
	  ("b" "Bookmark" plain (file "~/notes/bookmarks.org" "Bookmarks"))
	  ;; capture Tasks
          ("t" "Todo" entry (file+headline "~/notes/gtd.org" "Tasks")
            "* TODO %?\n  %i\n  %c")
          ))
  (setq org-tags-column 0)
  (setq org-agenda-text-search-extra-files '(agenda-archives))
  ;;(setq org-blank-before-new-entry '((heading . t)
  ;;                                   (plain-list-item . t)))
  (setq org-enforce-todo-dependencies t)
  (setq org-log-done (quote time))
  (setq org-log-redeadline (quote time))
  (setq org-log-reschedule (quote time))
  (add-hook 'org-capture-mode-hook
            (lambda ()
              (evil-insert-state)))
  (add-hook 'org-mode-hook
            (lambda ()
              (setq-local my-timer
                          (run-with-idle-timer 1 t
                                               (lambda ()
                                                 (when (and (eq major-mode 'org-mode)
                                                            (and evil-state
                                                                 (not (eq evil-state 'insert)))
                                                            (buffer-file-name)
                                                            (buffer-modified-p))
                                                   (save-buffer)))))
              (evil-define-key 'normal org-mode-map (kbd "TAB") 'org-cycle)
              (auto-fill-mode)
              (flyspell-mode)
              (org-indent-mode)))

  (defun set-org-mode-app-defaults ()
    (setq org-file-apps
	'(((auto-mode . emacs)
           ("\\.mm\\'" . default)
           ("\\.x?html?\\'" . system)
           ("\\.pdf\\'" . system)))))
  (add-hook 'org-mode-hook 'set-org-mode-app-defaults)
  (setq org-html-postamble nil)

  ;; Let's have pretty source code blocks
  (setq org-edit-src-content-indentation 0
        org-src-tab-acts-natively t
        org-src-fontify-natively t
        org-confirm-babel-evaluate nil)
  (defun org-text-bold () "Wraps the region with asterisks."
         (interactive)
         (surround-text "*"))
  (defun org-text-italics () "Wraps the region with slashes."
         (interactive)
         (surround-text "/"))
  (defun org-text-code () "Wraps the region with equal signs."
         (interactive)
         (surround-text "="))
  
  )

(use-package org-bullets
  :ensure t
  :disabled t
  :init
  (setq org-bullets-bullet-list
      '("▶" "◉" "★" "○" "◇"  "◉" "○" "►" ))
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package ob-clojure
   :config
   (setq org-babel-clojure-backend 'cider)
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
      (plantuml   . t))
    
    ))

(provide 'init-org)
