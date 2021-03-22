(define-key global-map "\C-cl" #'org-store-link)
(define-key global-map "\C-ca" #'org-agenda)
(define-key global-map "\C-cb" #'org-iswitchb)
(define-key global-map "\C-cc" #'org-capture)
(define-key global-map "\C-cp" #'org-pomodoro)
(define-key org-mode-map (kbd "C-c C-j") #'avy-goto-word-1)
(define-key org-mode-map (kbd "C-c M-m") #'org-contacts-view-send-email)
(define-key org-agenda-mode-map (kbd "C-c C-j") #'avy-goto-word-1)
(define-key org-mode-map (kbd "M-i") #'my-org-imenu)
(define-key org-mode-map (kbd "<M-S-return>") 'my-org-insert-todo-heading)
(define-key org-mode-map [remap org-return] (lambda () (interactive)
                                                (if (org-in-src-block-p)
                                                    (org-return)
                                                  (org-return-indent))))

;;calendar tool on C-c .
(define-key org-read-date-minibuffer-local-map (kbd "M-h")
  (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-day 1))))
(define-key org-read-date-minibuffer-local-map (kbd "M-l")
  (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-day 1))))
(define-key org-read-date-minibuffer-local-map (kbd "M-k")
  (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-week 1))))
(define-key org-read-date-minibuffer-local-map (kbd "M-j")
  (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-week 1))))
(define-key org-read-date-minibuffer-local-map (kbd "M-H")
  (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-month 1))))
(define-key org-read-date-minibuffer-local-map (kbd "M-L")
  (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-month 1))))
(define-key org-read-date-minibuffer-local-map (kbd "M-K")
  (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-year 1))))
(define-key org-read-date-minibuffer-local-map (kbd "M-J")
  (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-year 1))))

(bind-keys :map org-mode-map
           ((kbd "M-H") . org-shiftleft)
           ((kbd "M-h") . org-metaleft)
           ((kbd "M-J") . org-shiftdown)
           ((kbd "M-j") . org-metadown)
           ((kbd "M-K") . org-shiftup)
           ((kbd "M-k") . org-metaup)
           ((kbd "M-L") . org-shiftright)
           ((kbd "M-l") . org-metaright)
           ((kbd "M-o") . (lambda () (interactive)
                            (org-eol-call
                             '(lambda()
                                (org-insert-heading)
                                (org-metaright)))))
           ((kbd "M-t") . (lambda () (interactive)
                            (org-eol-call
                             '(lambda()
                                (org-insert-todo-heading nil)
                                (org-metaright))))))


;; (bind-keys :map orgstruct-mode-map
;;            ((kbd "M-H") . org-metaleft)
;;            ((kbd "M-j") . org-shiftdown)
;;            ((kbd "M-J") . org-metadown)
;;            ((kbd "M-k") . org-shiftright)
;;            ((kbd "M-K") . org-metaup)
;;            ((kbd "M-L") . org-metaright)
;;            ((kbd "M-o") . (lambda () (interactive)
;;                             (org-eol-call
;;                              '(lambda()
;;                                 (org-insert-heading)
;;                                 (org-metaright)))))
;;            ((kbd "M-t") .     ((lambda () (interactive)
;;                                  (org-eol-call
;;                                   '(lambda()
;;                                      (org-insert-todo-heading nil)
;;                                      (org-metaright)))))))

(bind-keys :map org-agenda-mode-map
           ("j" . org-agenda-next-line)
           ("k" . org-agenda-previous-line)
           ((kbd "M-j") . org-agenda-next-item)
           ((kbd "M-k") . org-agenda-previous-item)
           ((kbd "M-h") . org-agenda-earlier)
           ((kbd "M-l") . org-agenda-later)
           ("l" 'org-agenda-later)
           ("h" 'org-agenda-earlier)
           ;; ((kbd "RET") 'org-agenda-switch-to)
           ([escape] 'org-agenda-quit)
           ;; ("q" 'org-agenda-quit)
           ("s" 'org-agenda-save-all-org-buffers)
           ("t" 'org-agenda-todo)
           ((kbd "SPC") 'org-agenda-show-and-scroll-up)
           ("i" . org-agenda-clock-in)
           ("o" . org-agenda-clock-out)
           ("0" . ora-org-schedule-today)
           ("1" . ora-org-schedule-tomorrow)
           ("v" . sk/hydra-org-agenda-view)
           ("T" . worf-clock-in-and-out))

(which-key-add-key-based-replacements
  "SPC o <" "org date from calendar"
  "SPC o >" "org goto calendar"
  "SPC o /" "org file helm-swoop"
  "SPC o A" "org archive"
  "SPC o B" "org table blank field"
  "SPC o C" "org clocking"
  "SPC o D" "org deadlines"
  "SPC o E" "org set effort"
  "SPC o F" "org attach"
  "SPC o H" "org heading respect content"
  "SPC o I" "org insert"
  "SPC o I" "org toggle inline images"
  "SPC o K" "org bibtex add keyword"
  "SPC o L" "org toggle link display"
  "SPC o N" "org note"
  "SPC o P" "org properties"
  "SPC o R" "org refile"
  "SPC o S" "org make subtree"
  "SPC o V" "org reveal"
  "SPC o W" "org widen"
  "SPC o N" "org narrow to subtree"
  "SPC o a" "org agenda"
  "SPC o b" "org check box"
  "SPC o c" "org capture"
  "SPC o d" "org todo"
  "SPC o e" "org export"
  "SPC o f" "org filter"
  "SPC o g" "org goto"
  "SPC o h" "org toggle heading"
  "SPC o i" "org insert link"
  "SPC o j" "org jump"
  "SPC o k" "org kill subtree"
  "SPC o l" "org latex toggle"
  "SPC o m" "org manipulate table"
  "SPC o n" "org interleave"
  "SPC o o" "org organize"
  "SPC o p" "org practice"
  "SPC o q" "org quit special buffer"
  "SPC o r" "org roam"
  "SPC o s" "org store link"
  "SPC o t" "org tags command"
  "SPC o v" "org agenda view"
  "SPC o w" "org edit in special buffer"
  "SPC o x" "org encrypt entry"
  "SPC o X" "org decrypt entry"
  "SPC o y" "org copy subtree"
  "SPC o z" "org clone indirect buffer"
  "SPC o" "org prefix")


(bind-keys :prefix-map my-org-prefix-map
           :prefix "C-c o"
           ("." . org-edit-special)
           (":" . org-set-tags)
           ("*" . org-ctrl-c-star)
           ("RET" . org-ctrl-c-ret)
           ("-" . org-ctrl-c-minus)
           ("^" . org-sort)
           ("/" . org-sparse-tree)
           ("a" . org-agenda)
           ("A" . org-archive-subtree)
           ("b" . sk/hydra-org-checkbox/body)
           ("B" . org-table-blank-field)
           ("c" . org-capture)
           ;; ("C" . helm-org-capture-templates)
           ("C" . sk/hydra-org-clock/body)
           ("d" . org-todo)
           ("D" . sk/hydra-org-todo/body)
           ("e" . org-export-dispatch)
           ("E" . org-set-effort)
           ("g" . org-goto)
           ("h" . org-toggle-heading)
           ("H" . worf-back-to-heading)
           ("il" . org-insert-link)
           ("iu" . org-cliplink)
           ("if" . org-footnote-new)
           ("iH" . org-insert-heading-after-current)
           ("ih" . org-insert-heading-respect-content)
           ;; ("ih" . org-insert-heading)
           ("I" . org-toggle-inline-images)
           ("j" . sk/hydra-org-jump/body)
           ("k" . org-cut-subtree)
           ("l" . org-open-at-point)
           ;; ("l"   . org-toggle-latex-fragment)
           ("L" . org-toggle-link-display)
           ("m" . sk/hydra-org-tables/body)
           ("n" . org-add-note)
           ("N" . org-narrow-to-subtree)
           ("o" . hydra-org-organize/body)
           ("O" . org-footnote)
           ("p" . org-pomodoro)
           ("P" . sk/hydra-org-property/body)
           ("q" . org-edit-src-exit)
           ;; ("r" . sk/hydra-org-drill/body)
           ("r" . hydra-roam/body)
           ("R" . #'my-org-refile-hydra/body)
           ("s" . org-store-link)
           ("S" . org-list-make-subtree)
           ("y" . org-copy-subtree)
           ("t" . org-set-tags-command)
           ("u" . org-update-dblock)
           ("U" . org-update-all-dblocks)
           ("v" . sk/hydra-org-agenda-view/body)
           ("V" . org-reveal)
           ("w" . org-edit-special)
           ("W" . widen)
           ("x" . org-encrypt-entry)
           ("X" . org-decrypt-entry)
           ("z" . clone-indirect-buffer-other-window)
           ("<" . org-date-from-calendar)
           (">" . org-goto-calendar)

           ;; ("a s"   . org-mark-subtree)
           ;; ("RET" . org-open-at-point)
           ;; ("b" . org-tree-to-indirect-buffer)
           ;; tables
           ;; ("tc" . org-table-convert)
           ;; ("tE" . org-table-export)
           ;; ("tiH" . org-table-hline-and-move)
           ;; ("tI" . org-table-import)
           ;; ("tj" . org-table-next-row)
           ;; ("tn" . org-table-create)
           ;; ("tN" . org-table-create-with-table)
           ;; ("tr" . org-table-recalculate)
           ;; ("ts" . org-table-sort-lines)
           ;; ("ttf" . org-table-toggle-formula-debugger)
           ;; ("tto" . org-table-toggle-coordinate-overlays)
           ;; ("tw" . org-table-wrap-region)
           )
(provide 'org-keybindings)
