(use-package org-roam
  :ensure nil
  :hook
  (after-init . org-roam-mode)
  :commands (org-roam-build-cache)
  :straight (:host github :repo "jethrokuan/org-roam" :branch "master")
  :config
  (progn
    (setq org-roam-directory "~/org/notes")
    (setq org-roam-completion-system 'ivy))
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-show-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert)))
  :custom-face
  (org-roam-link ((t (:inherit org-link :foreground "#C991E1")))))

(use-package company-org-roam
  :straight (:host github :repo "jethrokuan/company-org-roam")
  :config
  (push 'company-org-roam company-backends))


(use-package deft
  :ensure t
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/org/notes"))

(use-package org-journal
  :bind
  ("C-c n j" . org-journal-new-entry)
  :custom
  (org-journal-dir "~/org/notes")
  (org-journal-date-prefix "#+TITLE: ")
  (org-journal-file-format "%Y-%m-%d.org"))
