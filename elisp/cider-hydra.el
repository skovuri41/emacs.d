;;; Code:

;;; *** main
(defhydra hydra-cider-main (:hint nil :color teal)
  "
 CIDER  Main Menu:
 _b_:     buffers         _i_:     inspection
 _c_:     connections     _._:     insert
 _g_:     debug           _k_:     refactor
 _d_:     doc             _p_:     popup
 _v_:     environment     _r_:     repl 1
 _e_:     eval            _l_:     repl 2
 _f_:     format          _s_:     stacktrace
 _h_:     help            _t_:     test
 _m_:     miscellanea     _a_:     tracing
 _q_:     quit
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
CIDER  Buffers Menu
 Key^^    Command
 _q_:     cider-main
 _a_:     cider-change-buffers-designation              _h_:     cider-load-buffer-and-switch-to-repl-buffer
 _x_:     cider-clear-compilation-highlights            _p_:     cider-pop-back
 _c_:     cider-close-ancillary-buffers                 _j_:     cider-refresh-dynamic-font-lock
 _d_:     cider-disable-on-existing-clojure-buffers     _s_:     cider-scratch
 _i_:     cider-enable-on-existing-clojure-buffers      _l_:     cider-selector
 _f_:     cider-enlighten-mode                          _b_:     cider-switch-to-last-clojure-buffer
 _l_:     cider-load-buffer                             _r_:     cider-switch-to-repl-buffer
 _o_:     cider-turn-on-eldoc-mode                      _e_:     cider-visit-error-buffer
"
  ("q" hydra-cider-main/body)
  ("a" cider-change-buffers-designation)
  ("x" cider-clear-compilation-highlights)
  ("c" cider-close-ancillary-buffers)
  ("d" cider-disable-on-existing-clojure-buffers)
  ("i" cider-enable-on-existing-clojure-buffers)
  ("f" cider-enlighten-mode)
  ("l" cider-load-buffer)
  ("h" cider-load-buffer-and-switch-to-repl-buffer)
  ("p" cider-pop-back)
  ("j" cider-refresh-dynamic-font-lock)
  ("s" cider-scratch)
  ("l" cider-selector)
  ("b" cider-switch-to-last-clojure-buffer)
  ("r" cider-switch-to-repl-buffer)
  ("o" cider-turn-on-eldoc-mode)
  ("e" cider-visit-error-buffer)
  )

;;; *** connections
(defhydra hydra-cider-connections (:hint nil :color teal)
  "
CIDER  Connections Menu
 Key^^    Command
 _q_:     cider-main
 _b_:     cider-assoc-buffer-with-connection       _D_:     cider-connections-make-default
 _p_:     cider-assoc-project-with-connection      _S_:     cider-create-sibling-cljs-repl
 _l_:     cider-clear-buffer-local-connection      _N_:     cider-describe-nrepl-session
 _x_:     cider-close-nrepl-session                _i_:     cider-display-connection-info
 _c_:     cider-connect                            _j_:     cider-jack-in
 _f_:     cider-connection-browser                 _J_:     cider-jack-in-clojurescript
 _g_:     cider-connections-buffer-mode            _p_:     cider-make-connection-default
 _h_:     cider-connections-close-connection       _P_:     cider-ping
 _o_:     cider-connections-goto-connection        _Q_:     cider-quit
 _n_:     cider-replicate-connection
 _r_:     cider-restart
 _u_:     cider-rotate-default-connection
"

  ("q" hydra-cider-main/body)
  ("b" cider-assoc-buffer-with-connection)
  ("p" cider-assoc-project-with-connection)
  ("l" cider-clear-buffer-local-connection)
  ("x" cider-close-nrepl-session)
  ("c" cider-connect)
  ("f" cider-connection-browser)
  ("g" cider-connections-buffer-mode)
  ("h" cider-connections-close-connection)
  ("o" cider-connections-goto-connection)
  ("D" cider-connections-make-default)
  ("S" cider-create-sibling-cljs-repl)
  ("N" cider-describe-nrepl-session)
  ("i" cider-display-connection-info)
  ("j" cider-jack-in)
  ("J" cider-jack-in-clojurescript)
  ("p" cider-make-connection-default)
  ("P" cider-ping)
  ("Q" cider-quit)
  ("n" cider-replicate-connection)
  ("r" cider-restart)
  ("u" cider-rotate-default-connection)
  )

;;; *** debug
(defhydra hydra-cider-debug (:hint nil :color teal)
  "
CIDER  Debug Menu
 Key^^    Command
 _q_:     cider-main
 _a_:     cider-debug-defun-at-point
 _b_:     cider-debug-mode-menu
 _c_:     cider-debug-mode-send-reply
 _d_:     cider-debug-move-here
 _e_:     cider-debug-toggle-locals
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
CIDER : Documentation Commands Menu
 Key^^    Command
 _q_:     cider-main
 _a_:     cider-doc              _s_:  cheet-sheet
 _b_:     cider-doc-map          _k_:  cider-code
 _c_:     cider-docview-source
 _d_:     cider-grimoire
 _e_:     cider-grimoire-web
 _f_:     cider-javadoc
"
  ("q" hydra-cider-main/body)
  ("a"  cider-doc)
  ("b"  cider-doc-map)
  ("c"  cider-docview-source)
  ("d"  cider-grimoire)
  ("e"  cider-grimoire-web)
  ("f"  cider-javadoc)
  ("k"  cider-code)
  ("s"  clojure-cheatsheet)
  )

;;; *** environment
(defhydra hydra-cider-environment (:hint nil :color teal)
  "
CIDER  Environment Menu
 Key^^    Command
 _q_:     cider-main
 _a_:     cider-apropos                      _i_:     cider-browse-ns-mode
 _b_:     cider-apropos-documentation        _j_:     cider-browse-ns-operate-at-point
 _c_:     cider-browse-instrumented-defs     _k_:     cider-classpath
 _d_:     cider-browse-ns                    _l_:     cider-find-and-clear-repl-output
 _e_:     cider-browse-ns-all                _m_:     cider-find-dwim
 _f_:     cider-browse-ns-doc-at-point       _n_:     cider-find-dwim-other-window
 _g_:     cider-browse-ns-find-at-point      _o_:     cider-find-ns
 _h_:     cider-browse-ns-handle-mouse       _p_:     cider-find-resource
 _v_:     cider-find-var
 _r_:     cider-open-classpath-entry
"
  ("q" hydra-cider-main/body)
  ("a" cider-apropos)
  ("b" cider-apropos-documentation)
  ("c" cider-browse-instrumented-defs)
  ("d" cider-browse-ns)
  ("e" cider-browse-ns-all)
  ("f" cider-classpath)
  ("g" cider-browse-ns-doc-at-point)
  ("h" cider-browse-ns-find-at-point)
  ("i" cider-browse-ns-handle-mouse)
  ("j" cider-browse-ns-mode)
  ("k" cider-browse-ns-operate-at-point)
  ("l" cider-find-and-clear-repl-output)
  ("m" cider-find-dwim)
  ("n" cider-find-dwim-other-window)
  ("o" cider-find-ns)
  ("p" cider-find-resource)
  ("v" cider-find-var)
  ("r" cider-open-classpath-entry)
  )

;;; *** eval
(defhydra hydra-cider-eval (:hint nil :color teal)
  "
CIDER : Evaluation Commands Menu
 Key^^    Command
 _q_:     cider-main
 _b_:     cider-eval-buffer                    _n_:     cider-eval-ns-form
 _d_:     cider-eval-defun-at-point            _m_:     cider-eval-region
 _c_:     cider-eval-defun-to-comment          _x_:     cider-interrupt-eval
 _f_:     cider-eval-file                      _k_:     cider-pprint-eval-defun-at-point
 _e_:     cider-eval-last-sexp                 _l_:     cider-pprint-eval-last-sexp
 _f_:     cider-eval-last-sexp-and-replace     _v_:     cider-read-and-eval
 _g_:     cider-eval-last-sexp-to-repl         _p_:     cider-load-all-project-ns
 _i_:     cider-interrupt                      _r_:     cider-refresh
 _s_:     cider-macroexpand-1                  _t_:     cider-macroexpand-all
 _o_:     cider-load-file                      _w_:     cider-run
 _U_:     cider-undef

"
  ("q" hydra-cider-main/body)
  ("b" cider-eval-buffer)
  ("d" cider-eval-defun-at-point)
  ("c" cider-eval-defun-to-comment)
  ("f" cider-eval-file)
  ("e" cider-eval-last-sexp)
  ("f" cider-eval-last-sexp-and-replace)
  ("g" cider-eval-last-sexp-to-repl)
  ("n" cider-eval-ns-form)
  ("m" cider-eval-region)
  ("x" cider-interrupt-eval)
  ("k" cider-pprint-eval-defun-at-point)
  ("l" cider-pprint-eval-last-sexp)
  ("v" cider-read-and-eval)
  ("w" cider-run)
  ("o" cider-load-file)
  ("U" cider-undef)
  ("p" cider-load-all-project-ns)
  ("i" cider-interrupt)
  ("r" cider-refresh)
  ("s" cider-macroexpand-1)
  ("t" cider-macroexpand-all)
  )

;;; *** format
(defhydra hydra-cider-format (:hint nil :color teal)
  "
CIDER  Format Menu
 Key^^    Command
 _q_:     cider-main
 _a_:     cider-format-buffer
 _b_:     cider-format-defun
 _c_:     cider-format-edn-buffer
 _d_:     cider-format-edn-region
 _e_:     cider-format-region
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
CIDER  Help Menu
 Key^^    Command
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
CIDER  Inspection Menu
 Key^^    Command
 _q_:     cider-main
 _a_:     cider-inspect                               _g_:     cider-inspector-next-page
 _b_:     cider-inspect-defun-at-point                _h_:     cider-inspector-operate-on-click
 _c_:     cider-inspect-last-sexp                     _i_:     cider-inspector-operate-on-point
 _d_:     cider-inspect-read-and-inspect              _j_:     cider-inspector-pop
 _e_:     cider-inspector-mode                        _k_:     cider-inspector-prev-page
 _f_:     cider-inspector-next-inspectable-object     _l_:     cider-inspector-previous-inspectable-object
 _m_:     cider-inspector-refresh
 _n_:     cider-inspector-set-page-size
"
  ("q" hydra-cider-main/body)
  ("a" cider-inspect)
  ("b" cider-inspect-defun-at-point)
  ("c" cider-inspect-last-sexp)
  ("d" cider-inspect-read-and-inspect)
  ("e" cider-inspector-mode)
  ("f" cider-inspector-next-inspectable-object)
  ("g" cider-inspector-next-page)
  ("h" cider-inspector-operate-on-click)
  ("i" cider-inspector-operate-on-point)
  ("j" cider-inspector-pop)
  ("k" cider-inspector-prev-page)
  ("l" cider-inspector-previous-inspectable-object)
  ("m" cider-inspector-refresh)
  ("n" cider-inspector-set-page-size)
  )

;;; *** insert
(defhydra hydra-cider-insert (:hint nil :color teal)
  "
CIDER  Insert Menu
 key^^    command
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
CIDER  Popup Menu
 Key^^    Command
 _q_:     cider-main
 _a_:     cider-popup-buffer-mode
 _b_:     cider-popup-buffer-quit
 _c_:     cider-popup-buffer-quit-function
"
  ("q" hydra-cider-main/body)
  ("a" cider-popup-buffer-mode)
  ("b" cider-popup-buffer-quit)
  ("c" cider-popup-buffer-quit-function)
  )

;;; *** repl-1
(defhydra hydra-cider-repl-menu-1 (:hint nil :color teal)
  "
CIDER  Repl Menu
 Key^^    Command
 _q_:     cider-main
 _>_:     cider-repl-menu-2
 _b_:     cider-repl-backward-input           _m_:     cider-repl-closing-return
 _a_:     cider-repl-beginning-of-defun       _e_:     cider-repl-end-of-defun
 _c_:     cider-repl-bol-mark                 _f_:     cider-repl-forward-input
 _d_:     cider-repl-clear-banners            _k_:     cider-repl-handle-shortcut
 _l_:     cider-repl-clear-buffer             _hl_:    cider-repl-history-load
 _n_:     cider-repl-clear-help-banner        _hs_:    cider-repl-history-save
 _o_:     cider-repl-clear-output             _i_:     cider-repl-indent-and-complete-symbol
 _y_:     cider-repl-kill-input               _t_:     toggle-repl-buffer
 _x_:     cider-find-and-clear-repl-output
"
  ("q" hydra-cider-main/body)
  (">" hydra-cider-repl-menu-2/body)
  ("b" cider-repl-backward-input)
  ("a" cider-repl-beginning-of-defun)
  ("c" cider-repl-bol-mark)
  ("d" cider-repl-clear-banners)
  ("l" cider-repl-clear-buffer)
  ("n" cider-repl-clear-help-banner)
  ("o" cider-repl-clear-output)
  ("m" cider-repl-closing-return)
  ("e" cider-repl-end-of-defun)
  ("f" cider-repl-forward-input)
  ("k" cider-repl-handle-shortcut)
  ("hl" cider-repl-history-load)
  ("hs" cider-repl-history-save)
  ("i" cider-repl-indent-and-complete-symbol)
  ("y" cider-repl-kill-input)
  ("t" toggle-nrepl-buffer)
  ("x" (cider-find-and-clear-repl-output '(4)))
  )

;;; *** repl-2
(defhydra hydra-cider-repl-menu-2 (:hint nil :color teal)
  "
CIDER  REPL Menu 2
 Key^^    Command
 _q_:     cider-main
 _<_:     cider-repl-menu-1
 _a_:     cider-repl-mode                     _h_:     cider-repl-previous-matching-input
 _b_:     cider-repl-mode-menu                _i_:     cider-repl-previous-prompt
 _c_:     cider-repl-newline-and-indent       _j_:     cider-repl-require-repl-utils
 _d_:     cider-repl-next-input               _k_:     cider-repl-return
 _e_:     cider-repl-next-matching-input      _l_:     cider-repl-set-ns
 _f_:     cider-repl-next-prompt              _m_:     cider-repl-shortcuts-help
 _g_:     cider-repl-previous-input           _n_:     cider-repl-switch-to-other
 _o_:     cider-repl-tab
 _p_:     cider-repl-toggle-pretty-printing
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
CIDER  Stacktrace Menu
 Key^^    Command
 _q_:     cider-main
 _a_:     cider-stacktrace-cycle-all-causes             _i_:     cider-stacktrace-mode
 _b_:     cider-stacktrace-cycle-cause-1                _z_:     cider-stacktrace-mode-menu
 _c_:     cider-stacktrace-cycle-cause-2                _j_:     cider-stacktrace-next-cause
 _d_:     cider-stacktrace-cycle-cause-3                _k_:     cider-stacktrace-previous-cause
 _e_:     cider-stacktrace-cycle-cause-4                _m_:     cider-stacktrace-toggle-all
 _f_:     cider-stacktrace-cycle-cause-5                _n_:     cider-stacktrace-toggle-clj
 _g_:     cider-stacktrace-cycle-current-cause          _o_:     cider-stacktrace-toggle-duplicates
 _h_:     cider-stacktrace-jump                         _p_:     cider-stacktrace-toggle-java
 _t_:     cider-stacktrace-toggle-repl
 _r_:     cider-stacktrace-toggle-tooling
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
CIDER  Test Menu
 Key^^    Command
 _q_:     cider-main
 _a_:     cider-test-clear-highlights   _h_:     cider-test-report-mode
 _b_:     cider-test-commands-map       _i_:     cider-test-report-mode-menu
 _c_:     cider-test-ediff              _j_:     cider-test-rerun-tests
 _d_:     cider-test-ediff-cleanup      _k_:     cider-test-run-loaded-tests
 _e_:     cider-test-jump               _l_:     cider-test-run-ns-tests
 _f_:     cider-test-next-result        _m_:     cider-test-run-project-tests
 _g_:     cider-test-previous-result    _s_:     cider-test-rerun-failed-tests
 _n_:     cider-test-run-test           _r_:     cider-test-run-loaded-tests
 _o_:     cider-test-show-report        _t_:     cider-test-show-report
 _p_:     cider-test-stacktrace
"
  ("q" hydra-cider-main/body)
  ("a" cider-test-clear-highlights)
  ("b" cider-test-commands-map)
  ("c" cider-test-ediff)
  ("d" cider-test-ediff-cleanup)
  ("e" cider-test-jump)
  ("f" cider-test-next-result)
  ("g" cider-test-previous-result)
  ("h" cider-test-report-mode)
  ("i" cider-test-report-mode-menu)
  ("j" cider-test-rerun-tests)
  ("k" cider-test-run-loaded-tests)
  ("l" cider-test-run-ns-tests)
  ("m" cider-test-run-project-tests)
  ("n" cider-test-run-test)
  ("o" cider-test-show-report)
  ("p" cider-test-stacktrace)
  ("r" cider-test-run-loaded-tests)
  ("s" cider-test-rerun-failed-tests)
  ("t" cider-test-show-report)
  )

;;; *** tracing
(defhydra hydra-cider-tracing (:hint nil :color teal)
  "
CIDER  Tracing Menu
 Key^^    Command
 _q_:     cider-main
 _a_:     cider-toggle-trace-ns
 _b_:     cider-toggle-trace-var
"
  ("q" hydra-cider-main/body)
  ("a" cider-toggle-trace-ns)
  ("b" cider-toggle-trace-var)
  )

;;; *** miscellanea
(defhydra hydra-cider-miscellanea (:hint nil :color teal)
  "
CIDER  Miscellanea Menu
 Key^^    Command
 _q_:     cider-main
 _d_:     cider-jump-to-compilation-error
 _e_:     cider-mode
 _f_:     cider-mode-menu
"
  ("q" hydra-cider-main/body)
  ("d" cider-jump-to-compilation-error)
  ("e" cider-mode)
  ("f" cider-mode-menu)
  ("i" cider-pop-back)
  )

(provide 'cider-hydra)
