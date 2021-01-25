(use-package org-roam
  :ensure nil
  :diminish org-roam-mode
  :hook
  (after-init . org-roam-mode)
  :commands (org-roam-build-cache)
  :straight (:host github :repo "jethrokuan/org-roam" :branch "master")
  :config
  (progn
    (require 'org-roam-protocol)
    (setq org-roam-directory "~/Documents/org/notes")
    (setq org-roam-graph-extra-config '(("overlap" . "prism")
                                        ("color" . "skyblue")))
    (setq org-roam-capture-templates
          '(("d"
             "default"
             plain
             #'org-roam-capture--get-point
             "%?"
             :file-name "%<%Y-%m-%d_%H:%M>-${slug}"
             :head "#+title: ${title}\n* Tasks\n"
             :unnarrowed t)))

    (defhydra hydra-org-roam (:exit t :idle 0.8)
      "Launcher for `org-roam'."
      ("r" org-roam "roam")
      ("i" org-roam-insert "insert")
      ("f" org-roam-find-file "find-file")
      ("r" org-roam-random-note "random")
      ("v" org-roam-buffer-activate "view backlinks")
      ("b" org-roam-find-backlink "find backlink")
      ("t" org-roam-today "todo")
      ("g" org-roam-show-graph "show graph"))
    (setq org-roam-buffer-position 'bottom))
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
  (deft-directory "~/Documents/org/notes"))

(use-package org-journal
  :bind
  ("C-c n j" . org-journal-new-entry)
  :custom
  (org-journal-dir "~/Documents/org/notes")
  (org-journal-date-prefix "#+TITLE: ")
  (org-journal-file-format "%Y-%m-%d.org"))


(provide 'ora-org-roam)
