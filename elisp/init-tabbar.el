(use-package centaur-tabs
  :ensure t
  :config
  (progn
    (setq centaur-tabs-style "bar")
    (setq centaur-tabs-height 24)
    (setq centaur-tabs-set-icons t)
    ;; (setq centaur-tabs-set-bar 'left)
    (setq centaur-tabs-set-bar 'under)
    ;; (setq centaur-tabs-set-bar 'over)
    (setq centaur-tabs-set-modified-marker t)
    (centaur-tabs-enable-buffer-reordering)
    (setq centaur-tabs-adjust-buffer-order 'right)
    (centaur-tabs-mode t)
    (centaur-tabs-headline-match)))


(use-package tabbar
  ;; :ensure t
  :disabled t
  :init
  ;; (setq tabbar-background-color nil)
  (setq tabbar-use-images t)
  (setq tabbar-cycle-scope 'groups)
  (setq tabbar-separator (quote (1.0)))
  (setq tabbar-auto-scroll-flag t
        table-time-before-update 0.1)
  :config
  (progn
    (defun my-tabbar-buffer-groups-by-project ()
      (list
       (cond
        ((memq major-mode '(mu4e-view-mode mu4e-main-mode mu4e-headers-mode mu4e-view-raw-mode
                                           twittering-mode weibo-timeline-mode
                                           jabber-roster-mode jabber-chat-mode erc-mode douban-music-mode))
         "Activity")
        ((memq major-mode '(eshell-mode term-mode shell-mode))
         (if (projectile-project-p) (projectile-project-name) "Common"))
        ((string-equal "*" (substring (buffer-name) 0 1))
         "Emacs")
        ((memq major-mode '(fundamental-mode))
         "Emacs")
        ((memq major-mode '(org-mode org-agenda-mode diary-mode))
         "OrgMode")
        (t
         (if (projectile-project-p) (projectile-project-name) "Common")))))

    (defun my-tabbar-buffer-groups-by-all ()
      (list
       (cond
        ((string-equal "*" (substring (buffer-name) 0 1))
         "Emacs")
        (t "All"))))

    (defun string-starts (string prefix)
      "Return t if STRING starts with PREFIX."
      (and
       (string-match (rx-to-string `(: bos ,prefix) t) string) t))

    (defun string-ends (string prefix)
      "Return t if STRING ends with PREFIX."
      (and
       (string-match (rx-to-string `(: prefix ,bos) t) string) t))

    ;; Sort tabs by dir structure
    (defun tabbar-add-tab (tabset object &optional append)
      "Add to TABSET a tab with value OBJECT if there isn't one there yet.
       If the tab is added, it is added at the beginning of the tab list,
       unless the optional argument APPEND is non-nil, in which case it is
       added at the end."
      (let ((tabs (tabbar-tabs tabset)))
        (if (tabbar-get-tab object tabset)
            tabs
          (let ((tab (tabbar-make-tab object tabset)))
            (tabbar-set-template tabset nil)
            (set tabset
                 (if (projectile-project-p)
                     (sort (cons tab tabs) #'tabbar-default-directory<)
                   (if append
                       (append tabs (list tab))
                     (cons tab tabs))))))))

    (defun tabbar-default-directory< (a b)
      "Is the `default-directory' of tab A `string<' than that of B?"
      ;; If not a file buffer, sort by its buffer name"
      (if (and (buffer-local-value 'buffer-file-name (car a)) (buffer-local-value 'buffer-file-name (car b)))
          (string<
           (expand-file-name (buffer-local-value 'buffer-file-name (car a)))
           (expand-file-name (buffer-local-value 'buffer-file-name (car b))))

        (progn
          (let ((aname (buffer-name (car a))) (bname (buffer-name (car b))))
            ;; Keep # buffers later
            (setq aname (if (eql (elt aname 0) ?#) (concat "~" aname) aname))
            (setq bname (if (eql (elt bname 0) ?#) (concat "~" bname) bname))

            (string<
             aname bname)))))

    (defun tabbar-tab-derived-mode-p (mode tab)
      "Is the major mode of TAB derived from MODE?"
      (with-current-buffer (car tab)
        (derived-mode-p mode)))

    ;; group buffers if any *s are left
    (defun my-tabbar-groups-project ()
      "Returns the list of group names the current buffer belongs to."
      (list
       (cond
        ;; ((member (buffer-name)
        ;;    '("*compilation*"))
        ;;   "Compile" ;; this is a group name)
        ;; A *buffer*
        ((string-starts (buffer-name) "*Slack")
         "Slack")
        ((string-equal "*" (substring (buffer-name) 0 1))
         "Emacs")
        ;; Ends with .pdf
        ((string-equal
          ".pdf"
          (substring (buffer-name)
                     (- (length (buffer-name)) 4)
                     (length (buffer-name))))
         "Documents")
        ((string-equal major-mode "erc-mode")
         "ERC")
        (t
         ;; Otherwise, group by projectile project
         (if
             (projectile-project-p)
             (projectile-project-name)
           "General")))))


    ;; hide some buffers
    (setq tabbar-buffer-list-function
          (lambda ()
            (remove-if
             (lambda(buffer)
               (and
                (not
                 (or
                  ;; (string-starts (buffer-name buffer) "*compilation*")
                  (string-starts (buffer-name buffer) "*ansi-term*")
                  (string-starts (buffer-name buffer) "*shell*")
                  (string-starts (buffer-name buffer) "*terminal")
                  (string-starts (buffer-name buffer) "*run")
                  (string-starts (buffer-name buffer) "*gud")
                  (string-starts (buffer-name buffer) "*eshell*")
                  (string-starts (buffer-name buffer) "*Python")
                  (string-starts (buffer-name buffer) "*eww")
                  (string-starts (buffer-name buffer) "*xkcd")
                  (string-starts (buffer-name buffer) "*startscreen")
                  (string-starts (buffer-name buffer) "*scratch*")
                  (string-starts (buffer-name buffer) "*Messages")
                  (string-starts (buffer-name buffer) "*Slack")
                  (string-starts (buffer-name buffer) "*slime-repl")))
                (or
                 (string-equal (buffer-name buffer) "TAGS")
                 (find (aref (buffer-name buffer) 0) " *")
                 )))
             (buffer-list))))

    ;; Cache The tabbar groups function to speed it up!
    (defun my-cached (func)
      "Turn a function into a cache dict."
      (lexical-let ((table (make-hash-table :test 'equal))
                    (f func))
        (lambda (key)
          (let ((value (gethash key table)))
            (if value
                value
              (puthash key (funcall f) table))))))

    (defun clear-projectile-cache ()
      "Clears my custom projectile cache"
      (interactive)
      (setq cached-ppn (my-cached 'my-tabbar-groups-project)))

    ;; (clear-projectile-cache)
    ;; Clear projectile cache when we open a new buffer (seems to be good enough)
    (add-hook 'change-major-mode-hook #'clear-projectile-cache)

    (defun my-tabbar-groups-by-project ()
      (funcall cached-ppn (buffer-name)))

    (setq tabbar-buffer-groups-function 'my-tabbar-groups-by-project)

    ;; (tabbar-mode 1)

    ))

(provide 'init-tabbar)
