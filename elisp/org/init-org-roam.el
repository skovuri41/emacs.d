(use-package org-roam
  :ensure t
  :diminish org-roam-mode
  :hook
  (after-init . org-roam-mode)
  :commands (org-roam-build-cache)
  :straight (:host github :repo "jethrokuan/org-roam" :branch "master")
  :config
  (progn
    (require 'org-roam-protocol)
    (setq org-roam-directory "~/Documents/org/")
    (setq org-roam-file-extensions '("org" "txt"))
    (setq org-roam-index-file "index.org")
    (setq org-roam-graph-extra-config '(("overlap" . "prism")
                                        ("color" . "skyblue"))
          org-roam-graph-exclude-matcher "private")
    (setq org-roam-capture-templates
          '(("d" "default" plain
             #'org-roam-capture--get-point
             :file-name "%<%Y-%m-%d>-${slug}"
             :head "#+title: ${title}\n#+ROAM_TAGS: %^{org-roam-tags}\n#+created: %u\n#+last_modified: %U\n%?"
             :unnarrowed t
             :jump-to-captured t)
            ("p" "private" plain
             #'org-roam-capture--get-point
             :file-name "private/${slug}"
             :head "#+title: ${title}\n#+ROAM_TAGS: %^{org-roam-tags}\n#+created: %u\n#+last_modified: %U\n%?"
             :unnarrowed t
             :jump-to-captured t)))

    "org-roam-switch-to-buffer" "b" #'org-roam-switch-to-buffer
    (defhydra hydra-roam (:color teal
                                 :hint nil)
      "
  _f_:ind file  _i_:nsert  _I_:ndex  _g_:raph
  _c_:apture  _s_:erver    _r_:oam  rando:_m_
  _b_:uffer
  "
      ("q" nil "quit")
      ("r" org-roam)
      ("f" org-roam-find-file)
      ("i" org-roam-insert)
      ("I" org-roam-jump-to-index)
      ("g" org-roam-graph)
      ("c" org-roam-capture)
      ("m" org-roam-random-note)
      ("s" org-roam-server-mode)
      ("b" org-roam-switch-to-buffer))
    (setq org-roam-buffer-position 'right))

  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           #'org-roam-capture--get-point
           "* %?"
           :file-name "daily/%<%Y-%m-%d>"
           :head "#+title: %<%Y-%m-%d>\n\n")))

  :custom-face
  (org-roam-link ((t (:inherit org-link :foreground "#C991E1")))))

(use-package org-roam-server
  :ensure t
  :after org-roam
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8070
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files nil
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20)
  ;; (require 'simple-httpd)
  ;; (setq httpd-root "/var/www")
  ;;(httpd-start)
  (defun org-roam-server-open ()
    "Ensure the server is active, then open the roam graph."
    (interactive)
    (org-roam-server-mode 1)
    (browse-url-xdg-open (format "http://localhost:%d" org-roam-server-port))))

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
  (deft-directory "~/Documents/org/"))

(use-package org-journal
  :bind
  ("C-c n j" . org-journal-new-entry)
  :custom
  (org-journal-dir "~/Documents/org/")
  (org-journal-date-prefix "#+TITLE: ")
  (org-journal-file-format "%Y-%m-%d.org"))


(provide 'init-org-roam)
