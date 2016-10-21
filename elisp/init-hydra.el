(use-package hydra
  :ensure t
  :config
  (progn

    (defhydra hydra-calendar
      (:color pink
              :hint nil
              :columns 4
              :body-pre (calendar)
              )
      "
 ^Calendar^     ^Journal^   ^ ^           ^Quit^
  _._: today    _n_ext      _e_: edit     _b_: journal
  _?_: date     _p_revious  _C-e_: view   _q_: quit
  ^ ^           _N_ew
"
      ("." calendar-goto-today)
      ("?" calendar-goto-date)
      ("n" org-journal-next-entry)
      ("p" org-journal-previous-entry)
      ("e" org-journal-read-entry :color blue)
      ("C-e" org-journal-display-entry)
      ("N" org-journal-new-date-entry)
      ("b" hydra-journal/body :color blue :exit-function (calendar-exit))
      ("q" nil "quit" :color blue :exit-function (calendar-exit)))


    (defhydra hydra-journal (:color pink :hint nil :columns 4)
      "
Journal
"
      ("n" org-journal-new-entry "new" :color blue)
      ("/" org-journal-search-forever "search" :color blue)
      ("c" hydra-calendar/body "calendar" :color blue)
      ("q" nil "quit" :color blue))

    ;; hydra zoom
    (defhydra hydra-zoom (global-map "<f5>")
      "zoom"
      ("+" text-scale-increase "in")
      ("-" text-scale-decrease "out"))

    ;; hydra rectangle
    (defun my-ex-point-mark ()
      (interactive)
      (if rectangle-mark-mode
          (exchange-point-and-mark)
        (let ((mk (mark)))
          (rectangle-mark-mode 1)
          (goto-char mk))))

    (defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                                         :color pink
                                         :post (deactivate-mark))
      "
  ^_k_^     _d_elete    _s_tring
_h_   _l_   _q_uit        _y_ank
  ^_j_^     _n_ew-copy  _r_eset
^^^^        _e_xchange  _u_ndo
^^^^        ^ ^         _p_aste
"
      ("h" backward-char nil)
      ("l" forward-char nil)
      ("k" previous-line nil)
      ("j" next-line nil)
      ("e" my-ex-point-mark nil)
      ("n" copy-rectangle-as-kill nil)
      ("d" delete-rectangle nil)
      ("r" (if (region-active-p)
               (deactivate-mark)
             (rectangle-mark-mode 1)) nil)
      ("y" yank-rectangle nil)
      ("u" undo nil)
      ("s" string-rectangle nil)
      ("p" kill-rectangle nil)
      ("q" nil nil))
    (global-set-key (kbd "C-x SPC") 'hydra-rectangle/body)

    (defhydra hydra-toggle-map nil
      "
     Toggle^
     ^^^^^^^--------------------
     d_: debug-on-error
     D_: debug-on-quit
     f_: auto-fill-mode
     l_: toggle-truncate-lines
     h_: hl-line-mode
     r_: read-only-mode
     v_: viewing-mode
     n_: narrow-or-widen-dwim
     g_: golden-ratio-mode
     q_: quit
    "
      ("d" toggle-debug-on-error :exit t)
      ("D" toggle-debug-on-quit :exit t)
      ("g" golden-ratio-mode :exit t)
      ("f" auto-fill-mode :exit t)
      ("l" toggle-truncate-lines :exit t)
      ("r" read-only-mode :exit t)
      ("h" hl-line-mode :exit t)
      ("v" my/turn-on-viewing-mode :exit t)
      ("n" my/narrow-or-widen-dwim :exit t)
      ("q" nil :exit t))

    (global-set-key (kbd "C-x t") 'hydra-toggle-map/body)

    (defun hydra/smart-copy (f)
      "Invoke exprand region function F before call `smart-copy-add`"
      (funcall
       `(lambda () (interactive)
          (save-excursion
            (,f)
            (kill-new (buffer-substring (region-beginning) (region-end)))
            (keyboard-quit)))))

    (defhydra hydra-smart-copy (:color pink :hint nil)
      "
^Mark^                    | ^Files^        | ^Pairs^            | ^Quotes^
------------------------+--------------^+^------------------^+^-----------------------
_w_: word    _._: sentence  | _d_: file dir  | _p_: Inside Pairs  | _o_:
Inside Quotes
_s_: symbol  _h_: paragraph | _f_: file name | _P_: Outside Pairs | _O_:
Outside Quotes
_d_: defun   _u_: url       | _F_: full path
                        | _b_: buffer
"
      ("w" (hydra/smart-copy 'er/mark-word) :exit t :color teal)
      ("s" (hydra/smart-copy 'er/mark-symbol) :exit t :color teal)
      ("d" (hydra/smart-copy 'er/mark-defun) :exit t :color teal)
      ("p" (hydra/smart-copy 'er/mark-inside-pairs) :exit t :color teal)
      ("P" (hydra/smart-copy 'er/mark-outside-pairs) :exit t :color teal)
      ("o" (hydra/smart-copy 'er/mark-inside-quotes) :exit t :color teal)
      ("O" (hydra/smart-copy 'er/mark-outside-quotes) :exit t :color teal)
      ("." (hydra/smart-copy 'er/mark-text-sentence) :exit t :color teal)
      ("h" (hydra/smart-copy 'er/mark-text-paragraph) :exit t :color teal)
      ("b" (kill-new (buffer-name)) :exit t :color teal)
      ("F" (kill-new (buffer-file-name)) :exit t :color teal)
      ("f" (kill-new (file-name-nondirectory (buffer-file-name))) :exit t :color
       teal)
      ("d" (kill-new (file-name-directory (buffer-file-name))) :exit t :color
       teal)
      ("u" (hydra/smart-copy 'er/mark-url) :exit t :color teal)
      ("m" avy-goto-char "move")
      ("q" nil "quit"))


    ;; YASnippet
    (defhydra hydra-yasnippet (:color blue :hint nil)
      "
              ^YASnippets^
--------------------------------------------
  Modes:    Load/Visit:    Actions:
 _g_lobal  _d_irectory    _i_nsert
 _m_inor   _f_ile         _t_ryout
 _e_xtra   _l_ist         _n_ew
         _a_ll
"
      ("d" yas-load-directory)
      ("e" yas-activate-extra-mode)
      ("i" yas-insert-snippet)
      ("f" yas-visit-snippet-file :color blue)
      ("n" yas-new-snippet)
      ("t" yas-tryout-snippet)
      ("l" yas-describe-tables)
      ("g" yas/global-mode)
      ("m" yas/minor-mode)
      ("a" yas-reload-all))
    (global-set-key (kbd "C-c C-y") 'hydra-yasnippet/body)

    (defhydra git-timemachine-hydra (:post (if (bound-and-true-p git-timemachine-mode) (git-timemachine-quit))
                                           :hint nil :color pink)
      "
Git Timemachine
    _e_: enter
    _k_: previous     _j_: next    _g_: go to nth
    _w_: copy abbr    _W_: copy full
_q_: Quit
"
      ("e" git-timemachine)
      ("k" git-timemachine-show-previous-revision)
      ("j" git-timemachine-show-next-revision)
      ("g" git-timemachine-show-nth-revision)
      ("w" git-timemachine-kill-abbreviated-revision)
      ("W" git-timemachine-kill-revision)
      ("q" nil))

    (defhydra hydra-vi (:pre (set-cursor-color "#e52b50")
                             :post (set-cursor-color "#ffffff")
                             :color pink)
      "vi"
      ;; movement
      ("w" forward-word)
      ("b" backward-word)
      ;; scrolling
      ("C-v" scroll-up-command nil)
      ("M-v" scroll-down-command nil)
      ("v" recenter-top-bottom)
      ;; arrows
      ("h" backward-char)
      ("j" next-line)
      ("k" previous-line)
      ("l" forward-char)
      ;; delete
      ("x" delete-char)
      ("d" hydra-vi-del/body "del" :exit t)
      ("u" undo)
      ;; should be generic "open"
      ("r" push-button "open")
      ("." hydra-repeat)
      ;; bad
      ("m" set-mark-command "mark")
      ("a" move-beginning-of-line "beg")
      ("e" move-end-of-line "end")
      ("y" kill-ring-save "yank" :exit t)
      ;; exit points
      ("q" nil "ins")
      ("C-n" (forward-line 1) nil :exit t)
      ("C-p" (forward-line -1) nil :exit t))

    ;; (global-set-key (kbd "C-v") 'hydra-vi/body)

    (defhydra hydra-buffer-menu (:color pink
                                        :hint nil)
      "
^Mark^             ^Unmark^           ^Actions^          ^Search
^^^^^^^^-----------------------------------------------------------------                        (__)
_m_: mark          _u_: unmark        _x_: execute       _R_: re-isearch                         (oo)
_s_: save          _U_: unmark up     _b_: bury          _I_: isearch                      /------\\/
_d_: delete        ^ ^                _g_: refresh       _O_: multi-occur                 / |    ||
_D_: delete up     ^ ^                _T_: files only: % -28`Buffer-menu-files-only^^    *  /\\---/\\
_~_: modified      ^ ^                ^ ^                ^^                                 ~~   ~~
"
      ("m" Buffer-menu-mark)
      ("u" Buffer-menu-unmark)
      ("U" Buffer-menu-backup-unmark)
      ("d" Buffer-menu-delete)
      ("D" Buffer-menu-delete-backwards)
      ("s" Buffer-menu-save)
      ("~" Buffer-menu-not-modified)
      ("x" Buffer-menu-execute)
      ("b" Buffer-menu-bury)
      ("g" revert-buffer)
      ("T" Buffer-menu-toggle-files-only)
      ("O" Buffer-menu-multi-occur :color blue)
      ("I" Buffer-menu-isearch-buffers :color blue)
      ("R" Buffer-menu-isearch-buffers-regexp :color blue)
      ("c" nil "cancel")
      ("v" Buffer-menu-select "select" :color blue)
      ("o" Buffer-menu-other-window "other-window" :color blue)
      ("q" quit-window "quit" :color blue))

    (defhydra hydra-ibuffer-main (:color pink :hint nil)
      "
 ^Navigation^ | ^Mark^        | ^Actions^        | ^View^
-^----------^-+-^----^--------+-^-------^--------+-^----^-------
  _k_:    ʌ   | _m_: mark     | _D_: delete      | _g_: refresh
  _l_: visit  | _u_: unmark   | _S_: save        | _s_: sort
  _j_:    v   | _*_: specific | _a_: all actions | _/_: filter
-^----------^-+-^----^--------+-^-------^--------+-^----^-------
"
      ("j" ibuffer-forward-line)
      ("l" (lambda () (interactive) (progn
                                 (ibuffer-visit-buffer)
                                 (xah-fly-command-mode-activate)))  :color blue)
      ("k" ibuffer-backward-line)

      ("m" ibuffer-mark-forward)
      ("u" ibuffer-unmark-forward)
      ("*" hydra-ibuffer-mark/body :color blue)

      ("D" ibuffer-do-delete)
      ("S" ibuffer-do-save)
      ("a" hydra-ibuffer-action/body :color blue)

      ("g" ibuffer-update)
      ("s" hydra-ibuffer-sort/body :color blue)
      ("/" hydra-ibuffer-filter/body :color blue)
      ("o" (lambda () (interactive) (progn
                                 (ibuffer-visit-buffer-other-window)
                                 (xah-fly-command-mode-activate))) "other window" :color blue)
      ;; ("q" ibuffer-quit "quit ibuffer" :color blue)
      ("q" (lambda () (interactive)
             (progn
               (quit-window)
               (xah-fly-command-mode-activate)))  "quit ibuffer" :color blue)
      ("." nil "toggle hydra" :color blue))

    (defhydra hydra-ibuffer-mark (:color teal :columns 5
                                         :after-exit (hydra-ibuffer-main/body))
      "Mark"
      ("*" ibuffer-unmark-all "unmark all")
      ("M" ibuffer-mark-by-mode "mode")
      ("m" ibuffer-mark-modified-buffers "modified")
      ("u" ibuffer-mark-unsaved-buffers "unsaved")
      ("s" ibuffer-mark-special-buffers "special")
      ("r" ibuffer-mark-read-only-buffers "read-only")
      ("/" ibuffer-mark-dired-buffers "dired")
      ("e" ibuffer-mark-dissociated-buffers "dissociated")
      ("h" ibuffer-mark-help-buffers "help")
      ("z" ibuffer-mark-compressed-file-buffers "compressed")
      ("b" hydra-ibuffer-main/body "back" :color blue))

    (defhydra hydra-ibuffer-action (:color teal :columns 4
                                           :after-exit
                                           (if (eq major-mode 'ibuffer-mode)
                                               (hydra-ibuffer-main/body)))
      "Action"
      ("A" ibuffer-do-view "view")
      ("E" ibuffer-do-eval "eval")
      ("F" ibuffer-do-shell-command-file "shell-command-file")
      ("I" ibuffer-do-query-replace-regexp "query-replace-regexp")
      ("H" ibuffer-do-view-other-frame "view-other-frame")
      ("N" ibuffer-do-shell-command-pipe-replace "shell-cmd-pipe-replace")
      ("M" ibuffer-do-toggle-modified "toggle-modified")
      ("O" ibuffer-do-occur "occur")
      ("P" ibuffer-do-print "print")
      ("Q" ibuffer-do-query-replace "query-replace")
      ("R" ibuffer-do-rename-uniquely "rename-uniquely")
      ("T" ibuffer-do-toggle-read-only "toggle-read-only")
      ("U" ibuffer-do-replace-regexp "replace-regexp")
      ("V" ibuffer-do-revert "revert")
      ("W" ibuffer-do-view-and-eval "view-and-eval")
      ("X" ibuffer-do-shell-command-pipe "shell-command-pipe")
      ("b" nil "back"))

    (defhydra hydra-ibuffer-sort (:color amaranth :columns 3)
      "Sort"
      ("i" ibuffer-invert-sorting "invert")
      ("a" ibuffer-do-sort-by-alphabetic "alphabetic")
      ("v" ibuffer-do-sort-by-recency "recently used")
      ("s" ibuffer-do-sort-by-size "size")
      ("f" ibuffer-do-sort-by-filename/process "filename")
      ("m" ibuffer-do-sort-by-major-mode "mode")
      ("b" hydra-ibuffer-main/body "back" :color blue))

    (defhydra hydra-ibuffer-filter (:color amaranth :columns 4)
      "Filter"
      ("m" ibuffer-filter-by-used-mode "mode")
      ("M" ibuffer-filter-by-derived-mode "derived mode")
      ("n" ibuffer-filter-by-name "name")
      ("c" ibuffer-filter-by-content "content")
      ("e" ibuffer-filter-by-predicate "predicate")
      ("f" ibuffer-filter-by-filename "filename")
      (">" ibuffer-filter-by-size-gt "size")
      ("<" ibuffer-filter-by-size-lt "size")
      ("/" ibuffer-filter-disable "disable")
      ("b" hydra-ibuffer-main/body "back" :color blue))


    (defhydra hydra-projectile (:color blue :hint nil :idle 0.4)
      "
                                                                                                                          ╭────────────┐                   ╭────────┐
   Files             Search          Buffer             Do                Other Window      Run             Cache         │ Projectile │                   │ Fixmee │
 ╭────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┴────────────╯  ╭────────────────┴────────╯
   [_f_] file          [_a_] ag          [_b_] switch (Helm)  [_g_] magit         [_F_] file          [_U_] test        [_kc_] clear         [_x_] TODO & FIXME
   [_l_] file dwim     [_A_] grep        [_v_] show all       [_P_] commander     [_L_] dwim          [_m_] compile     [_kk_] add current   [_X_] toggle
   [_r_] recent file   [_s_] occur       [_V_] ibuffer        [_i_] info          [_D_] dir           [_c_] shell       [_ks_] cleanup
   [_d_] dir           [_S_] replace     [_K_] kill all        ^ ^                [_O_] other         [_C_] command     [_kd_] remove
    ^ ^                 ^ ^               ^ ^                  ^ ^                [_B_] buffer
   [_p_] Switch Project
  --------------------------------------------------------------------------------
        "
      ("<tab>" hydra-master/body "back")
      ("q" nil "quit")
      ("a"   projectile-ag)
      ("A"   projectile-grep)
      ("B"   projectile-switch-to-buffer)
      ("b"   projectile-switch-to-buffer)
      ("c"   projectile-run-async-shell-command-in-root)
      ("C"   projectile-run-command-in-root)
      ("d"   projectile-find-dir)
      ("D"   projectile-find-dir-other-window)
      ("f"   counsel-projectile-find-file)
      ("F"   projectile-find-file)
      ("g"   projectile-vc)
      ("h"   projectile-dired)
      ("i"   projectile-project-info)
      ("kc"  projectile-invalidate-cache)
      ("kd"  projectile-remove-known-project)
      ("kk"  projectile-cache-current-file)
      ("p"   projectile-switch-project)
      ("P"   projectile-switch-project)
      ("K"   projectile-kill-buffers)
      ("ks"  projectile-cleanup-known-projects)
      ("l"   projectile-find-file-dwim)
      ("L"   projectile-find-file-dwim-other-window)
      ("m"   projectile-compile-project)
      ("o"   projectile-find-other-file)
      ("O"   projectile-find-other-file-other-window)
      ("r"   projectile-recentf)
      ("R"   projectile-recentf)
      ("s"   projectile-multi-occur)
      ("S"   projectile-replace)
      ("t"   projectile-find-tag)
      ("T"   projectile-regenerate-tags)
      ("u"   projectile-find-test-file)
      ("U"   projectile-test-project)
      ("v"   projectile-display-buffer)
      ("V"   projectile-ibuffer)
      ("X"   fixmee-mode)
      ("x"   fixmee-view-listing))

    (defhydra hydra-bm (:hint nil :color teal)
      ("j" bm-next "next" :exit nil)
      ("k" bm-previous "prev" :exit nil)
      ("s" bm-show-all "show all")
      ("b" bm-show "show")
      ("l" helm-bookmarks "list")
      ("a" bm-bookmark-annotate "annotate" :exit nil :color blue)
      ;; ("n" bm-common-next)
      ;; ("N" bm-lifo-next)
      ;; ("p" bm-common-previous)
      ;; ("P" bm-lifo-previous)
      ("p" bm-toggle-buffer-persistence "toggle persistence" :exit nil)
      ("x" bm-remove-all-current-buffer "remove current buffer" :exit nil :color blue)
      ("X" bm-remove-all-all-buffers "remove all buffers" :exit nil :color blue)
      ("m" bm-toggle "toggle" :exit nil)
      ("q" nil "quit" :exit t))

    (defhydra hydra-smartscan (:hint nil :color teal)
      ("j" smartscan-symbol-go-forward "next" :exit nil)
      ("k" smartscan-symbol-go-backward "prev" :exit nil)
      ("r" smartscan-symbol-replace "replace" :exit nil)
      ("q" nil "quit" :exit t))

    (defhydra hydra-winner (:color red :columns 2)
      "Winner"
      ("j" winner-undo "undo")
      ("k" winner-redo "redo")
      ("q" nil "quit" :exit t))

    (defhydra hydra-eval (:color blue :columns 8)
      "Eval"
      ("e" eval-expression "expression")
      ("d" eval-defun "defun")
      ("b" eval-buffer "buffer")
      ("l" eval-last-sexp "last sexp")
      ("1" async-shell-command "shell-command"))

    (defhydra hydra-hl (:color blue :columns 8)
      "Highlight"
      ("s" highlight-symbol-at-point "symbol at point")
      ("r" highlight-regexp "regexp")
      ("i" highlight-indentation-mode "indentation")
      ("h" hi-lock-mode "toggle hi-lock-mode"))

    (defhydra hydra-view-buffer (:color red :columns 2)
      "Switch Buffers"
      ("j" xah-previous-user-buffer "previous user buffer")
      ("k" xah-next-user-buffer "next user buffer")
      ("h" xah-previous-emacs-buffer "previous emacs buffer")
      ("l" xah-next-emacs-buffer "next emacs buffer")
      ("b" (lambda () (interactive)
             (progn
               (call-interactively 'projectile-ibuffer)
               (xah-fly-insert-mode-activate)
               (hydra-ibuffer-main/body))) "buffer list" :exit t)
      ("q" nil "quit" :exit t))

    (defhydra hydra-perspective (:color red :columns 2)
      "Switch Window Config"
      ("j" eyebrowse-next-window-config "next view")
      ("k" eyebrowse-prev-window-config "prev view")
      ("h" eyebrowse-switch-to-window-config-0 "first view")
      ("l" eyebrowse-last-window-config "last view")
      ("w" eyebrowse-switch-to-window-config "switch view" :exit t)
      ("o" ivy-switch-project-with-eyebrowse "open view" :exit t)
      ("c" eyebrowse-create-window-config "create view" :exit t)
      ("," eyebrowse-rename-window-config "rename view" :exit t)
      ("rb" my-eyebrowse-rename-1 "rename(buffer-name)" :exit t)
      ("rp" my-eyebrowse-rename-2 "rename(project-name)" :exit t)
      ("x" eyebrowse-close-window-config "close view" :exit t)
      ("q" nil "quit" :exit t))

    (defhydra hydra-next-error (:color red :columns 2)
      "next-error"
      ("j" next-error "next" :bind nil)
      ("k" previous-error "previous" :bind nil)
      ("l" flycheck-list-errors "list-errors" :exit t))

    (defhydra hydra-outline
      (:hint nil :body-pre (outline-minor-mode 1))
      "
Outline

   ^Navigate^     ^Show/Hide^                            ^Manipulate^
_c_: up      _C-c_: hide subtree  _C-S-c_: hide all   _M-r_: demote
_t_: next    _C-t_: show entry    _C-S-t_: show child _M-c_: promote
_s_: prev    _C-s_: hide entry    _C-S-s_: hide child _M-t_: move down
_r_: next    _C-r_: show subtree  _C-S-r_: show all   _M-s_: move up
_b_: bwd
_f_: fwd
"
      ("c" outline-up-heading)
      ("t" outline-next-visible-heading)
      ("s" outline-previous-visible-heading)
      ("r" outline-next-heading)
      ("b" outline-backward-same-level)
      ("f" outline-forward-same-level)
      ("C-c" outline-hide-subtree)
      ("C-t" outline-show-entry)
      ("C-s" outline-hide-entry)
      ("C-r" outline-show-subtree)
      ("C-S-c" outline-hide-body)
      ("C-S-t" outline-show-children)
      ("C-S-s" outline-hide-sublevels)
      ("C-S-r" outline-show-all)
      ("M-r" outline-demote)
      ("M-c" outline-promote)
      ("M-t" outline-move-subtree-down)
      ("M-s" outline-move-subtree-up)
      ("i" outline-insert-heading "insert heading" :color blue)
      ("q" nil "quit" :color blue))

    ))


(provide 'init-hydra)
