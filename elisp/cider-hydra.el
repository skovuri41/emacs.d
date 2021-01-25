;;; Code:

(pretty-hydra-define clj-hydra-repl
  (:color teal :quit-key "q")
  ("REPL"
   (("j" cider-jack-in "jack in clj")
    ("J" cider-jack-in-clojurescript "jack in cljs")
    ("c" cider-connect "connect")
    ("b" cider-switch-to-repl-buffer "switch to repl")
    ("l" cider-repl-clear-output "clear repl output")
    ("q" cider-quit "quit repl")
    ("n" cider-repl-set-ns "set-repl-ns")
    ("r" cider-restart "restart")
    ("i" cider-interrupt "interrupt"))))

(pretty-hydra-define clj-hydra-eval
  (:color teal :quit-key "q")
  ("Eval"
   (("b" cider-eval-buffer "eval buffer")
    ("e" cider-eval-last-sexp "eval last sexp")
    ("." cider-eval-last-sexp-to-repl "eval last to repl")
    ("v" cider-eval-last-sexp-and-replace "eval last sexp and replace")
    ("d" cider-eval-defun-at-point "eval defun at point")
    ("f" cider-load-file "file")
    ("r" cider-eval-region "eval region")
    ("n" cider-eval-ns-form "eval ns form")
    ("h" cider-ns-refresh "ns-reload")
    ("m" cider-macroexpand-1 "macroexpand-1")
    ("M" cider-macroexpand-all "macroexpand all")
    ("i" cider-inspect-last-result "inspect-last-result"))))

(pretty-hydra-define clj-hydra-docs
  (:color teal :quit-key "q")
  ("Documentation"
   (("a" cider-apropos "apropos")
    ("h" cider-doc "doc")
    ("j" cider-javadoc "javadoc")
    ("n" cider-browse-ns "browse namespace")
    ("N" cider-browse-ns-all "browse all namespaces"))))

(pretty-hydra-define clj-hydra-jump
  (:color teal :quit-key "q")
  ("Jump"
   (("g" (lambda (p) (interactive "P") (cider-find-var (if p nil (list 4)))) "find var")
    ("b" cider-pop-back "pop back")
    ("n" cider-find-ns "ns"))))

(pretty-hydra-define clj-hydra-refactor
  (:color teal :quit-key "q")
  ("Refactor"
   (("s" cljstyle-region "cljstyle region")
    ("S" cljstyle-current-file "cljstyle current file"))))

(pretty-hydra-define clj-hydra-test
  (:color teal :quit-key "q")
  ("Test"
   (("a" cider-test-run-ns-tests "run ns tests")
    ("r" cider-test-show-report "show test report")
    ("l" cider-test-run-loaded-tests "loaded")
    ("A" cider-auto-test-mode "toggle auto test mode")
    ("f" cider-test-rerun-failed-tests "rerun failed tests"))))

(pretty-hydra-define clj-hydra-namespace
  (:color teal :quit-key "q")
  ("Namespace"
   (("n" clojure-insert-ns-form "insert ns form")
    ("h" clojure-insert-ns-form-at-point "insert ns form here")
    ("p" clojure-update-ns "update ns form")
    ("s" clojure-sort-ns "sort ns"))))

(pretty-hydra-define clj-hydra-collections
  (:color teal :quit-key "q")
  ("Collections"
   (("l" clojure-convert-collection-to-list "to list")
    ("q" clojure-convert-collection-to-quoted-list "to quoted list")
    ("m" clojure-convert-collection-to-map "to map")
    ("v" clojure-convert-collection-to-vector "to vector")
    ("s" clojure-convert-collection-to-set "to set"))))

(major-mode-hydra-define (clojure-mode clojurescript-mode clojurec-mode cider-repl-mode) (:title "Clojure" :quit-key "q")
  ("Menu"
   (("c" clj-hydra-repl/body "Repl")
    ("e" clj-hydra-eval/body "Eval")
    ("d" clj-hydra-docs/body "Documentation")
    ("j" clj-hydra-jump/body "Jump")
    ("r" clj-hydra-refactor/body "Refactor")
    ("n" clj-hydra-namespace/body "Namespace")
    ("k" clj-hydra-collections/body "Collections")
    ("t" clj-hydra-test/body "Test"))))


;;; *** main
(defhydra hydra-cider-main (:hint nil :color teal)
  "
 CIDER Main Menu:
 _b_: buffers         _i_: inspection
 _c_: connections     _._: insert
 _g_: debug           _k_: refactor
 _d_: doc             _p_: popup
 _v_: environment     _r_: repl 1
 _e_: eval            _l_: repl 2
 _f_: format          _s_: stacktrace
 _h_: help            _t_: test
 _m_: miscellanea     _a_: tracing
 _q_: quit
"
  ("b" hydra-cider-buffers/body)
  ("c" hydra-cider-connections/body)
  ("g" hydra-cider-debug/body)
  ("d" hydra-cider-doc/body)
  ("v" hydra-cider-environment/body)
  ("e" hydra-cider-eval/body)
  ("f" hydra-cider-format/body)
  ("h" hydra-cider-help/body)
  ("i" hydra-cider-inspection/body)
  ("." hydra-cider-insert/body)
  ("k" hydra-cljr-help-menu/body)
  ("p" hydra-cider-popup/body)
  ("r" hydra-cider-repl-menu-1/body)
  ("l" hydra-cider-repl-menu-2/body)
  ("s" hydra-cider-stacktrace/body)
  ("t" hydra-cider-test/body)
  ("a" hydra-cider-tracing/body)
  ("m" hydra-cider-miscellanea/body)
  ("q" nil :exit t))

;;; *** buffers
(defhydra hydra-cider-buffers (:hint nil :color teal)
  "
CIDER Buffers Menu
 _q_:  cider-main
 _a_:  cider-change-buffers-designation              _ls_: cider-load-buffer-and-switch-to-repl-buffer
 _x_:  cider-clear-compilation-highlights            _p_:  cider-pop-back
 _c_:  cider-close-ancillary-buffers                 _f_:  cider-refresh-dynamic-font-lock
 _d_:  cider-disable-on-existing-clojure-buffers     _s_:  cider-scratch
 _i_:  cider-enable-on-existing-clojure-buffers      _t_:  cider-selector
 _g_:  cider-enlighten-mode                          _b_:  cider-switch-buffer/repl
 _lb_: cider-load-buffer                             _e_:  cider-visit-error-buffer
 _o_:  cider-turn-on-eldoc-mode
"
  ("q" hydra-cider-main/body)
  ("a" cider-change-buffers-designation)
  ("x" cider-clear-compilation-highlights)
  ("c" cider-close-ancillary-buffers)
  ("d" cider-disable-on-existing-clojure-buffers)
  ("i" cider-enable-on-existing-clojure-buffers)
  ("g" cider-enlighten-mode)
  ("lb" cider-load-buffer)
  ("ls" cider-load-buffer-and-switch-to-repl-buffer)
  ("p" cider-pop-back)
  ("f" cider-refresh-dynamic-font-lock)
  ("s" cider-scratch)
  ("t" cider-selector)
  ;; ("b" cider-switch-to-last-clojure-buffer)
  ("b" (if (memq major-mode '(cider-repl-mode
                              cider-stacktrace-mode))
           (cider-switch-to-last-clojure-buffer)
         (cider-switch-to-repl-buffer)))
  ;; ("r" cider-switch-to-repl-buffer)
  ("o" cider-turn-on-eldoc-mode)
  ("e" cider-visit-error-buffer)
  )

;;; *** connections
(defhydra hydra-cider-connections (:hint nil :color teal)
  "
CIDER Connections Menu
 _q_:  cider-main
 _b_:  cider-assoc-buffer-with-connection       _D_:  cider-connections-make-default
 _P_:  cider-assoc-project-with-connection      _S_:  cider-create-sibling-cljs-repl
 _l_:  cider-clear-buffer-local-connection      _N_:  cider-describe-nrepl-session
 _x_:  cider-close-nrepl-session                _i_:  cider-display-connection-info
 _c_:  cider-connect                            _j_:  cider
 _w_:  cider-connection-browser                 _J_:  cider-jack-in-clojurescript
 _g_:  cider-connections-buffer-mode            _d_:  cider-make-connection-default
 _h_:  cider-connections-close-connection       _p_:  cider-ping
 _o_:  cider-connections-goto-connection        _Q_:  cider-quit
 _n_:  cider-replicate-connection
 _r_:  cider-restart
 _u_:  cider-rotate-default-connection
"
  ("q" hydra-cider-main/body)
  ("b" cider-assoc-buffer-with-connection)
  ("P" cider-assoc-project-with-connection)
  ("l" cider-clear-buffer-local-connection)
  ("x" cider-close-nrepl-session)
  ("c" cider-connect)
  ("w" cider-connection-browser)
  ("g" cider-connections-buffer-mode)
  ("h" cider-connections-close-connection)
  ("o" cider-connections-goto-connection)
  ("D" cider-connections-make-default)
  ("S" cider-create-sibling-cljs-repl)
  ("N" cider-describe-nrepl-session)
  ("i" cider-display-connection-info)
  ("j" cider-jack-in)
  ("J" cider-jack-in-clojurescript)
  ("d" cider-make-connection-default)
  ("p" cider-ping)
  ("Q" cider-quit)
  ("n" cider-replicate-connection)
  ("r" cider-restart)
  ("u" cider-rotate-default-connection)
  )

;;; *** debug
(defhydra hydra-cider-debug (:hint nil :color teal)
  "
CIDER Debug Menu
 _q_:  cider-main
 _a_:  cider-debug-defun-at-point
 _b_:  cider-debug-mode-menu
 _c_:  cider-debug-mode-send-reply
 _d_:  cider-debug-move-here
 _e_:  cider-debug-toggle-locals
"
  ("q" hydra-cider-main/body)
  ("a" cider-debug-defun-at-point)
  ("b" cider-debug-mode-menu)
  ("c" cider-debug-mode-send-reply)
  ("d" cider-debug-move-here)
  ("e" cider-debug-toggle-locals)
  )

;;; *** doc
(defhydra hydra-cider-doc (:hint nil :color teal)
  "
CIDER: Documentation Commands Menu
 _c_:  cheet-sheet
 _d_:  cider-doc
 _s_:  cider-docview-source
 _g_:  cider-grimoire
 _w_:  cider-grimoire-web
 _j_:  cider-javadoc
"
  ("q" hydra-cider-main/body)
  ("d"  cider-doc)
  ("s"  cider-docview-source)
  ("g"  cider-grimoire)
  ("w"  cider-grimoire-web)
  ("j"  cider-javadoc)
  ("c"  clojure-cheatsheet)
  )

;;; *** environment
(defhydra hydra-cider-environment (:hint nil :color teal)
  "
CIDER Environment Menu
 _q_:  cider-main
 _a_:  cider-apropos                      _b_:   cider-browse-ns-mode
 _d_:  cider-apropos-documentation        _no_:  cider-browse-ns-operate-at-point
 _i_:  cider-browse-instrumented-defs     _c_:   cider-classpath
 _ns_: cider-browse-ns                    _fl_:  cider-find-and-clear-repl-output
 _na_: cider-browse-ns-all                _ff_:  cider-find-dwim
 _np_: cider-browse-ns-doc-at-point       _fw_:  cider-find-dwim-other-window
 _nf_: cider-browse-ns-find-at-point      _fn_:  cider-find-ns
 _nh_: cider-browse-ns-handle-mouse       _fr_:  cider-find-resource
 _fv_: cider-find-var
 _o_:  cider-open-classpath-entry
"
  ("q" hydra-cider-main/body)
  ("a" cider-apropos)
  ("d" cider-apropos-documentation)
  ("i" cider-browse-instrumented-defs)
  ("ns" cider-browse-ns)
  ("na" cider-browse-ns-all)
  ("c" cider-classpath)
  ("np" cider-browse-ns-doc-at-point)
  ("nf" cider-browse-ns-find-at-point)
  ("nh" cider-browse-ns-handle-mouse)
  ("b" cider-browse-ns-mode)
  ("no" cider-browse-ns-operate-at-point)
  ("fl" cider-find-and-clear-repl-output)
  ("ff" cider-find-dwim)
  ("fw" cider-find-dwim-other-window)
  ("fn" cider-find-ns)
  ("fr" cider-find-resource)
  ("fv" cider-find-var)
  ("o" cider-open-classpath-entry)
  )

;;; *** eval
(defhydra hydra-cider-eval (:hint nil :color teal)
  "
CIDER: Evaluation Commands Menu
 _q_:   cider-main
 _b_:   cider-eval-buffer                    _n_:   cider-eval-ns-form
 _d_:   cider-eval-defun-at-point            _m_:   cider-eval-region
 _c_:   cider-eval-defun-to-comment          _u_:   cider-undef
 _f_:   cider-eval-file                      _pd_:  cider-pprint-eval-defun-at-point
 _e_:   cider-eval-last-sexp                 _pe_:  cider-pprint-eval-last-sexp
 _y_:   cider-eval-last-sexp-and-replace     _v_:   cider-read-and-eval
 _s_:   cider-eval-last-sexp-to-repl         _ln_:  cider-load-all-project-ns
 _i_:   cider-interrupt                      _r_:   cider-refresh
 _xo_:  cider-macroexpand-1                  _xa_:  cider-macroexpand-all
 _lf_:  cider-load-file                      _R_:   cider-run
 _t_:   cider-eval-sexp-at-point             _k_:   cider-benchmark-defun-at-point
 _pr_:  cider-eval-print-last-sexp
"
  ("q" hydra-cider-main/body)
  ("b" cider-eval-buffer)
  ("d" cider-eval-defun-at-point)
  ("c" cider-eval-defun-to-comment)
  ("f" cider-eval-file)
  ("t" cider-eval-sexp-at-point)
  ("e" cider-eval-last-sexp)
  ("y" cider-eval-last-sexp-and-replace)
  ("s" cider-eval-last-sexp-to-repl)
  ("n" cider-eval-ns-form)
  ("m" cider-eval-defun-or-region)
  ("pd" cider-pprint-eval-defun-at-point)
  ("pe" cider-pprint-eval-last-sexp)
  ("v" cider-read-and-eval)
  ("R" cider-run)
  ("lf" cider-load-file)
  ("u" cider-undef)
  ("ln" cider-load-all-project-ns)
  ("i" cider-interrupt)
  ("r" cider-refresh)
  ("xo" cider-macroexpand-1)
  ("xa" cider-macroexpand-all)
  ("k" cider-benchmark-defun-at-point)
  ("pr" cider-eval-print-last-sexp))

;;; *** format
(defhydra hydra-cider-format (:hint nil :color teal)
  "
CIDER Format Menu
 _q_:    cider-main
 _a_:    cider-format-buffer
 _b_:    cider-format-defun
 _c_:    cider-format-edn-buffer
 _d_:    cider-format-edn-region
 _e_:    cider-format-region
"
  ("q" hydra-cider-main/body)
  ("a" cider-format-buffer)
  ("b" cider-format-defun)
  ("c" cider-format-edn-buffer)
  ("d" cider-format-edn-region)
  ("e" cider-format-region)
  )

;;; *** help
(defhydra hydra-cider-help (:hint nil :color teal)
  "
CIDER Help Menu
 _q_:     hydra-cider-main
 _a_:     cider-drink-a-sip
 _b_:     cider-report-bug
 _c_:     cider-version
 _d_:     cider-view-manual
 _e_:     cider-view-refcard
 _s_:     clojure-cheatsheet
"
  ("q" hydra-cider-main/body)
  ("a" cider-drink-a-sip)
  ("b" cider-report-bug)
  ("c" cider-version)
  ("d" cider-view-manual)
  ("e" cider-view-refcard)
  ("s" clojure-cheatsheet)
  )

;;; *** inspection
(defhydra hydra-cider-inspection (:hint nil :color teal)
  "
CIDER Inspection Menu
 _q_:   cider-main
 _i_:   cider-inspect                               _n_:   cider-inspector-next-page
 _d_:   cider-inspect-defun-at-point                _h_:   cider-inspector-operate-on-click
 _e_:   cider-inspect-last-sexp                     _t_:   cider-inspector-operate-on-point
 _x_:   cider-inspect-expr                          _o_:   cider-inspector-pop
 _p_:   cider-inspector-prev-page                   _k_:   cider-inspector-previous-inspectable-object
 _j_:   cider-inspector-next-inspectable-object     _r_:   cider-inspector-refresh
 _z_:   cider-inspector-set-page-size
"
  ("q" hydra-cider-main/body)
  ("i" cider-inspect)
  ("d" cider-inspect-defun-at-point)
  ("e" cider-inspect-last-sexp)
  ("x" cider-inspect-expr)
  ("j" cider-inspector-next-inspectable-object)
  ("n" cider-inspector-next-page)
  ("h" cider-inspector-operate-on-click)
  ("t" cider-inspector-operate-on-point)
  ("o" cider-inspector-pop)
  ("p" cider-inspector-prev-page)
  ("k" cider-inspector-previous-inspectable-object)
  ("r" cider-inspector-refresh)
  ("z" cider-inspector-set-page-size)
  )

;;; *** insert
(defhydra hydra-cider-insert (:hint nil :color teal)
  "
CIDER Insert Menu
 _q_:     cider-main
 _d_:     cider-insert-defun-in-repl
 _e_:     cider-insert-last-sexp-in-repl
 _n_:     cider-insert-ns-form-in-repl
 _m_:     cider-insert-region-in-repl
 _s_:     cider-send-and-evaluate-sexp
"
  ("q" hydra-cider-main/body)
  ("d" cider-insert-defun-in-repl)
  ("e" cider-insert-last-sexp-in-repl)
  ("n" cider-insert-ns-form-in-repl)
  ("m" cider-insert-region-in-repl)
  ("s" cider-send-and-evaluate-sexp)
  )

;;; *** popup buffer
(defhydra hydra-cider-popup (:hint nil :color teal)
  "
 Key^^    Command
 _q_:  cider-main
 _a_:  cider-popup-buffer-mode
 _b_:  cider-popup-buffer-quit
 _c_:  cider-popup-buffer-quit-function
"
  ("q" hydra-cider-main/body)
  ("a" cider-popup-buffer-mode)
  ("b" cider-popup-buffer-quit)
  ("c" cider-popup-buffer-quit-function)
  )

;;; *** repl-1
(defhydra hydra-cider-repl-menu-1 (:hint nil :color teal)
  "
 Key^^    Command
 _q_:   cider-main
 _>_:   cider-repl-menu-2
 _b_:   cider-repl-backward-input           _m_:   cider-repl-closing-return
 _a_:   cider-repl-beginning-of-defun       _e_:   cider-repl-end-of-defun
 _c_:   cider-repl-bol-mark                 _f_:   cider-repl-forward-input
 _d_:   cider-repl-clear-banners            _k_:   cider-repl-handle-shortcut
 _l_:   cider-repl-clear-buffer             _hl_:  cider-repl-history-load
 _n_:   cider-repl-clear-help-banner        _hs_:  cider-repl-history-save
 _o_:   cider-repl-clear-output             _i_:   cider-repl-indent-and-complete-symbol
 _x_:   cider-repl-kill-input               _t_:   toggle-repl-buffer
"
  ("q" hydra-cider-main/body)
  (">" hydra-cider-repl-menu-2/body)
  ("b" cider-repl-backward-input)
  ("a" cider-repl-beginning-of-defun)
  ("c" cider-repl-bol-mark)
  ("d" cider-repl-clear-banners)
  ("l" (if (memq major-mode '(cider-repl-mode))
           (cider-repl-clear-buffer)
         (cider-find-and-clear-repl-output '(4))))
  ("n" cider-repl-clear-help-banner)
  ("o" (if (memq major-mode '(cider-repl-mode))
           (cider-repl-clear-output)
         (progn
           (cider-switch-to-repl-buffer)
           (cider-repl-clear-output)
           (cider-switch-to-last-clojure-buffer))))
  ("m" (if (memq major-mode '(cider-repl-mode))
           (cider-repl-closing-return)
         (progn
           (cider-switch-to-repl-buffer)
           (cider-repl-closing-return)
           (cider-switch-to-last-clojure-buffer))))
  ("e" cider-repl-end-of-defun)
  ("f" cider-repl-forward-input)
  ("k" cider-repl-handle-shortcut)
  ("hl" cider-repl-history-load)
  ("hs" cider-repl-history-save)
  ("i" cider-repl-indent-and-complete-symbol)
  ("x" (if (memq major-mode '(cider-repl-mode))
           (cider-repl-kill-input)
         (progn
           (cider-switch-to-repl-buffer)
           (cider-repl-kill-input)
           (cider-switch-to-last-clojure-buffer))))
  ("t" toggle-nrepl-buffer)
  ;; ("x" (cider-find-and-clear-repl-output '(4)))
  )

;;; *** repl-2
(defhydra hydra-cider-repl-menu-2 (:hint nil :color teal)
  "
CIDER REPL Menu 2
 _q_:   cider-main
 _<_:   cider-repl-menu-1
 _a_:   cider-repl-mode                     _h_:   cider-repl-previous-matching-input
 _b_:   cider-repl-mode-menu                _i_:   cider-repl-previous-prompt
 _c_:   cider-repl-newline-and-indent       _j_:   cider-repl-require-repl-utils
 _d_:   cider-repl-next-input               _k_:   cider-repl-return
 _e_:   cider-repl-next-matching-input      _l_:   cider-repl-set-ns
 _f_:   cider-repl-next-prompt              _m_:   cider-repl-shortcuts-help
 _g_:   cider-repl-previous-input           _n_:   cider-repl-switch-to-other
 _o_:   cider-repl-tab
 _p_:   cider-repl-toggle-pretty-printing
"
  ("q" hydra-cider-main/body)
  ("<" hydra-cider-repl-menu-1/body)
  ("a" cider-repl-mode)
  ("b" cider-repl-mode-menu)
  ("c" cider-repl-newline-and-indent)
  ("d" cider-repl-next-input)
  ("e" cider-repl-next-matching-input)
  ("f" cider-repl-next-prompt)
  ("g" cider-repl-previous-input)
  ("h" cider-repl-previous-matching-input)
  ("i" cider-repl-previous-prompt)
  ("j" cider-repl-require-repl-utils)
  ("k" cider-repl-return)
  ("l" cider-repl-set-ns)
  ("m" cider-repl-shortcuts-help)
  ("n" cider-repl-switch-to-other)
  ("o" cider-repl-tab)
  ("p" cider-repl-toggle-pretty-printing)
  )

;;; *** stacktrace
(defhydra hydra-cider-stacktrace (:hint nil :color teal)
  "
CIDER Stacktrace Menu
 _q_:  cider-main
 _a_:  cider-stacktrace-cycle-all-causes             _i_:  cider-stacktrace-mode
 _b_:  cider-stacktrace-cycle-cause-1                _z_:  cider-stacktrace-mode-menu
 _c_:  cider-stacktrace-cycle-cause-2                _j_:  cider-stacktrace-next-cause
 _d_:  cider-stacktrace-cycle-cause-3                _k_:  cider-stacktrace-previous-cause
 _e_:  cider-stacktrace-cycle-cause-4                _m_:  cider-stacktrace-toggle-all
 _f_:  cider-stacktrace-cycle-cause-5                _n_:  cider-stacktrace-toggle-clj
 _g_:  cider-stacktrace-cycle-current-cause          _o_:  cider-stacktrace-toggle-duplicates
 _h_:  cider-stacktrace-jump                         _p_:  cider-stacktrace-toggle-java
 _t_:  cider-stacktrace-toggle-repl
 _r_:  cider-stacktrace-toggle-tooling
"
  ("q" hydra-cider-main/body)

  ("a" cider-stacktrace-cycle-all-causes)
  ("b" cider-stacktrace-cycle-cause-1)
  ("c" cider-stacktrace-cycle-cause-2)
  ("d" cider-stacktrace-cycle-cause-3)
  ("e" cider-stacktrace-cycle-cause-4)
  ("f" cider-stacktrace-cycle-cause-5)
  ("g" cider-stacktrace-cycle-current-cause)
  ("h" cider-stacktrace-jump)
  ("i" cider-stacktrace-mode)
  ("z" cider-stacktrace-mode-menu)
  ("j" cider-stacktrace-next-cause)
  ("k" cider-stacktrace-previous-cause)
  ("m" cider-stacktrace-toggle-all)
  ("n" cider-stacktrace-toggle-clj)
  ("o" cider-stacktrace-toggle-duplicates)
  ("p" cider-stacktrace-toggle-java)
  ("t" cider-stacktrace-toggle-repl)
  ("r" cider-stacktrace-toggle-tooling)
  )

;;; *** test
(defhydra hydra-cider-test (:hint nil :color teal)
  "
CIDER Test Menu
 _q_:   cider-main
 _l_:   cider-test-clear-highlights   _s_:   cider-test-stacktrace
 _e_:   cider-test-ediff              _o_:   cider-test-run-loaded-tests
 _d_:   cider-test-ediff-cleanup      _n_:   cider-test-run-ns-tests
 _m_:   cider-test-jump               _p_:   cider-test-run-project-tests
 _j_:   cider-test-next-result        _f_:   cider-test-rerun-failed-tests
 _k_:   cider-test-previous-result    _a_:   cider-test-show-report
 _t_:   cider-test-run-test           _r_:   cider-test-rerun-tests
"
  ("q" hydra-cider-main/body)
  ("l" cider-test-clear-highlights)
  ("e" cider-test-ediff)
  ("d" cider-test-ediff-cleanup)
  ("m" cider-test-jump)
  ("j" cider-test-next-result)
  ("k" cider-test-previous-result)
  ("r" cider-test-rerun-tests)
  ("o" cider-test-run-loaded-tests)
  ("n" cider-test-run-ns-tests)
  ("p" cider-test-run-project-tests)
  ("t" cider-test-run-test)
  ("a" cider-test-show-report)
  ("s" cider-test-stacktrace)
  ("f" cider-test-rerun-failed-tests)
  )

;;; *** tracing
(defhydra hydra-cider-tracing (:hint nil :color teal)
  "
CIDER Tracing Menu
 _q_: cider-main
 _a_: cider-toggle-trace-ns
 _b_: cider-toggle-trace-var
"
  ("q" hydra-cider-main/body)
  ("a" cider-toggle-trace-ns)
  ("b" cider-toggle-trace-var)
  )

;;; *** miscellanea
(defhydra hydra-cider-miscellanea (:hint nil :color teal)
  "
CIDER Miscellanea Menu
 _q_: cider-main
 _d_: cider-jump-to-compilation-error
 _e_: cider-mode
"
  ("q" hydra-cider-main/body)
  ("d" cider-jump-to-compilation-error)
  ("e" cider-mode)
  ("i" cider-pop-back)
  )

(provide 'cider-hydra)
