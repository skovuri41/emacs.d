;;; cider-buddy.el --- Helpers utilities for the CIDER package
;;
;; Author: Sean Irby
;; Copyright © , Sean Irby
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; This file is not a part of GNU Emacs
;;
;;; Commentary:
;;
;;; Code:

(with-eval-after-load 'cider

  (defun cider-buddy-shortcut-str (command)
    (prin1-to-string (key-description (where-is-internal command cider-mode-map t)) t))

  (with-eval-after-load 'hydra

;;; *** keybindings
    (define-key cider-mode-map (kbd "C-c b m") 'hydra-cider-buddy-main/body)

    (define-key cider-mode-map (kbd "C-c b b") 'hydra-cider-buddy-buffers/body)
    (define-key cider-mode-map (kbd "C-c b c") 'hydra-cider-buddy-connections/body)
    (define-key cider-mode-map (kbd "C-c b d") 'hydra-cider-buddy-debug/body)
    (define-key cider-mode-map (kbd "C-c b D") 'hydra-cider-buddy-doc/body)
    (define-key cider-mode-map (kbd "C-c b e") 'hydra-cider-buddy-eval/body)
    (define-key cider-mode-map (kbd "C-c b E") 'hydra-cider-buddy-environment/body)
    (define-key cider-mode-map (kbd "C-c b f") 'hydra-cider-buddy-find/body)
    (define-key cider-mode-map (kbd "C-c b F") 'hydra-cider-buddy-format/body)
    (define-key cider-mode-map (kbd "C-c b h") 'hydra-cider-buddy-help/body)
    (define-key cider-mode-map (kbd "C-c b i") 'hydra-cider-buddy-inspection/body)
    (define-key cider-mode-map (kbd "C-c b I") 'hydra-cider-buddy-insert/body)
    (define-key cider-mode-map (kbd "C-c b p") 'hydra-cider-buddy-popup/body)
    (define-key cider-mode-map (kbd "C-c b r") 'hydra-cider-buddy-repl-menu-1/body)
    (define-key cider-mode-map (kbd "C-c b R") 'hydra-cider-buddy-repl-menu-2/body)
    (define-key cider-mode-map (kbd "C-c b s") 'hydra-cider-buddy-stacktrace/body)
    (define-key cider-mode-map (kbd "C-c b t") 'hydra-cider-buddy-test/body)
    (define-key cider-mode-map (kbd "C-c b T") 'hydra-cider-buddy-tracing/body)

    (define-key cider-mode-map (kbd "C-c b M") 'hydra-cider-buddy-miscellanea/body)

;;; *** main
    (defhydra hydra-cider-buddy-main (:hint nil :color blue)
      "
CIDER Buddy Main Menu

 Key^^    Menu         Shortcut

 _b_:     buffers      %(cider-buddy-shortcut-str 'hydra-cider-buddy-buffers/body)
 _c_:     connections  %(cider-buddy-shortcut-str 'hydra-cider-buddy-connections/body)
 _d_:     debug        %(cider-buddy-shortcut-str 'hydra-cider-buddy-debug/body)
 _D_:     doc          %(cider-buddy-shortcut-str 'hydra-cider-buddy-doc/body)
 _e_:     environment  %(cider-buddy-shortcut-str 'hydra-cider-buddy-eval/body)
 _E_:     eval         %(cider-buddy-shortcut-str 'hydra-cider-buddy-eval/body)
 _f_:     format       %(cider-buddy-shortcut-str 'hydra-cider-buddy-format/body)
 _h_:     help         %(cider-buddy-shortcut-str 'hydra-cider-buddy-help/body)
 _i_:     inspection   %(cider-buddy-shortcut-str 'hydra-cider-buddy-inspection/body)
 _I_:     insert       %(cider-buddy-shortcut-str 'hydra-cider-buddy-insert/body)
 _k_:     refactor
 _p_:     popup        %(cider-buddy-shortcut-str 'hydra-cider-buddy-popup/body)
 _r_:     repl 1       %(cider-buddy-shortcut-str 'hydra-cider-buddy-repl-menu-1/body)
 _R_:     repl 2       %(cider-buddy-shortcut-str 'hydra-cider-buddy-repl-menu-2/body)
 _s_:     stacktrace   %(cider-buddy-shortcut-str 'hydra-cider-buddy-stacktrace/body)
 _t_:     test         %(cider-buddy-shortcut-str 'hydra-cider-buddy-test/body)
 _T_:     tracing      %(cider-buddy-shortcut-str 'hydra-cider-buddy-tracing/body)
 _M_:     miscellanea  %(cider-buddy-shortcut-str 'hydra-cider-buddy-miscellanea/body)
 _q_:     quit
"
      ("b" hydra-cider-buddy-buffers/body)
      ("c" hydra-cider-buddy-connections/body)
      ("d" hydra-cider-buddy-debug/body)
      ("D" hydra-cider-buddy-doc/body)
      ("e" hydra-cider-buddy-environment/body)
      ("E" hydra-cider-buddy-eval/body)
      ("f" hydra-cider-buddy-format/body)
      ("h" hydra-cider-buddy-help/body)
      ("i" hydra-cider-buddy-inspection/body)
      ("I" hydra-cider-buddy-insert/body)
      ("k" hydra-cljr-help-menu/body)
      ("p" hydra-cider-buddy-popup/body)
      ("r" hydra-cider-buddy-repl-menu-1/body)
      ("R" hydra-cider-buddy-repl-menu-2/body)
      ("s" hydra-cider-buddy-stacktrace/body)
      ("t" hydra-cider-buddy-test/body)
      ("T" hydra-cider-buddy-tracing/body)
      ("M" hydra-cider-buddy-miscellanea/body)
      ("q" nil :exit t))

;;; *** buffers
    (defhydra hydra-cider-buddy-buffers (:hint nil :color blue)
      "
CIDER Buddy Buffers Menu

 Key^^    Command                                      Shortcut

 _/_:     cider-buddy-main                             %(cider-buddy-shortcut-str 'hydra-cider-buddy-main/body)

 _a_:     cider-change-buffers-designation             %(cider-buddy-shortcut-str 'cider-change-buffers-designation)
 _b_:     cider-clear-compilation-highlights           %(cider-buddy-shortcut-str 'cider-clear-compilation-highlights)
 _c_:     cider-close-ancillary-buffers                %(cider-buddy-shortcut-str 'cider-close-ancillary-buffers)
 _d_:     cider-disable-on-existing-clojure-buffers    %(cider-buddy-shortcut-str 'cider-disable-on-existing-clojure-buffers)
 _e_:     cider-enable-on-existing-clojure-buffers     %(cider-buddy-shortcut-str 'cider-enable-on-existing-clojure-buffers)
 _f_:     cider-enlighten-mode                         %(cider-buddy-shortcut-str 'cider-enlighten-mode)
 _g_:     cider-load-buffer                            %(cider-buddy-shortcut-str 'cider-load-buffer)
 _h_:     cider-load-buffer-and-switch-to-repl-buffer  %(cider-buddy-shortcut-str 'cider-load-buffer-and-switch-to-repl-buffer)
 _i_:     cider-pop-back                               %(cider-buddy-shortcut-str 'cider-pop-back)
 _j_:     cider-refresh-dynamic-font-lock              %(cider-buddy-shortcut-str 'cider-refresh-dynamic-font-lock)
 _k_:     cider-scratch                                %(cider-buddy-shortcut-str 'cider-scratch)
 _l_:     cider-selector                               %(cider-buddy-shortcut-str 'cider-selector)
 _m_:     cider-switch-to-last-clojure-buffer          %(cider-buddy-shortcut-str 'cider-switch-to-last-clojure-buffer)
 _n_:     cider-switch-to-repl-buffer                  %(cider-buddy-shortcut-str 'cider-switch-to-repl-buffer)
 _o_:     cider-turn-on-eldoc-mode                     %(cider-buddy-shortcut-str 'cider-turn-on-eldoc-mode)
 _p_:     cider-visit-error-buffer                     %(cider-buddy-shortcut-str 'cider-visit-error-buffer)
"
      ("/" hydra-cider-buddy-main/body)
      ("a" cider-change-buffers-designation)
      ("b" cider-clear-compilation-highlights)
      ("c" cider-close-ancillary-buffers)
      ("d" cider-disable-on-existing-clojure-buffers)
      ("e" cider-enable-on-existing-clojure-buffers)
      ("f" cider-enlighten-mode)
      ("g" cider-load-buffer)
      ("h" cider-load-buffer-and-switch-to-repl-buffer)
      ("i" cider-pop-back)
      ("j" cider-refresh-dynamic-font-lock)
      ("k" cider-scratch)
      ("l" cider-selector)
      ("m" cider-switch-to-last-clojure-buffer)
      ("n" cider-switch-to-repl-buffer)
      ("o" cider-turn-on-eldoc-mode)
      ("p" cider-visit-error-buffer)
      )

;;; *** connections
    (defhydra hydra-cider-buddy-connections (:hint nil :color blue)
      "
CIDER Buddy Connections Menu

 Key^^    Command                              Shortcut

 _/_:     cider-buddy-main                     %(cider-buddy-shortcut-str 'hydra-cider-buddy-main/body)

 _a_:     cider-assoc-buffer-with-connection   %(cider-buddy-shortcut-str 'cider-assoc-buffer-with-connection)
 _b_:     cider-assoc-project-with-connection  %(cider-buddy-shortcut-str 'cider-assoc-project-with-connection)
 _c_:     cider-clear-buffer-local-connection  %(cider-buddy-shortcut-str 'cider-clear-buffer-local-connection)
 _d_:     cider-close-nrepl-session            %(cider-buddy-shortcut-str 'cider-close-nrepl-session)
 _e_:     cider-connect                        %(cider-buddy-shortcut-str 'cider-connect)
 _f_:     cider-connection-browser             %(cider-buddy-shortcut-str 'cider-connection-browser)
 _g_:     cider-connections-buffer-mode        %(cider-buddy-shortcut-str 'cider-connections-buffer-mode)
 _h_:     cider-connections-close-connection   %(cider-buddy-shortcut-str 'cider-connections-close-connection)
 _i_:     cider-connections-goto-connection    %(cider-buddy-shortcut-str 'cider-connections-goto-connection)
 _j_:     cider-connections-make-default       %(cider-buddy-shortcut-str 'cider-connections-make-default)
 _k_:     cider-create-sibling-cljs-repl       %(cider-buddy-shortcut-str 'cider-create-sibling-cljs-repl)
 _l_:     cider-describe-nrepl-session         %(cider-buddy-shortcut-str 'cider-describe-nrepl-session)
 _m_:     cider-display-connection-info        %(cider-buddy-shortcut-str 'cider-display-connection-info)
 _n_:     cider-jack-in                        %(cider-buddy-shortcut-str 'cider-jack-in)
 _o_:     cider-jack-in-clojurescript          %(cider-buddy-shortcut-str 'cider-jack-in-clojurescript)
 _p_:     cider-make-connection-default        %(cider-buddy-shortcut-str 'cider-make-connection-default)
 _q_:     cider-ping                           %(cider-buddy-shortcut-str 'cider-ping)
 _r_:     cider-quit                           %(cider-buddy-shortcut-str 'cider-quit)
 _s_:     cider-replicate-connection           %(cider-buddy-shortcut-str 'cider-replicate-connection)
 _t_:     cider-restart                        %(cider-buddy-shortcut-str 'cider-restart)
 _u_:     cider-rotate-default-connection      %(cider-buddy-shortcut-str 'cider-rotate-default-connection)
"

      ("/" hydra-cider-buddy-main/body)
      ("a" cider-assoc-buffer-with-connection)
      ("b" cider-assoc-project-with-connection)
      ("c" cider-clear-buffer-local-connection)
      ("d" cider-close-nrepl-session)
      ("e" cider-connect)
      ("f" cider-connection-browser)
      ("g" cider-connections-buffer-mode)
      ("h" cider-connections-close-connection)
      ("i" cider-connections-goto-connection)
      ("j" cider-connections-make-default)
      ("k" cider-create-sibling-cljs-repl)
      ("l" cider-describe-nrepl-session)
      ("m" cider-display-connection-info)
      ("n" cider-jack-in)
      ("o" cider-jack-in-clojurescript)
      ("p" cider-make-connection-default)
      ("q" cider-ping)
      ("r" cider-quit)
      ("s" cider-replicate-connection)
      ("t" cider-restart)
      ("u" cider-rotate-default-connection)
      )

;;; *** debug
    (defhydra hydra-cider-buddy-debug (:hint nil :color blue)
      "
CIDER Buddy Debug Menu

 Key^^    Command                      Shortcut

 _/_:     cider-buddy-main             %(cider-buddy-shortcut-str 'hydra-cider-buddy-main/body)

 _a_:     cider-debug-defun-at-point   %(cider-buddy-shortcut-str 'cider-debug-defun-at-point)
 _b_:     cider-debug-mode-menu        %(cider-buddy-shortcut-str 'cider-debug-mode-menu)
 _c_:     cider-debug-mode-send-reply  %(cider-buddy-shortcut-str 'cider-debug-mode-send-reply)
 _d_:     cider-debug-move-here        %(cider-buddy-shortcut-str 'cider-debug-move-here)
 _e_:     cider-debug-toggle-locals    %(cider-buddy-shortcut-str 'cider-debug-toggle-locals)
"
      ("/" hydra-cider-buddy-main/body)
      ("a" cider-debug-defun-at-point)
      ("b" cider-debug-mode-menu)
      ("c" cider-debug-mode-send-reply)
      ("d" cider-debug-move-here)
      ("e" cider-debug-toggle-locals)
      )

;;; *** doc
    (defhydra hydra-cider-buddy-doc (:hint nil :color blue)
      "
CIDER Buddy: Documentation Commands Menu

 Key^^    Command                      Shortcut

 _/_:     cider-buddy-main             %(cider-buddy-shortcut-str 'hydra-cider-buddy-main/body)

 _a_:     cider-doc                    %(cider-buddy-shortcut-str 'cider-doc)
 _b_:     cider-doc-map                %(cider-buddy-shortcut-str 'cider-doc-map)
 _c_:     cider-docview-source         %(cider-buddy-shortcut-str 'cider-docview-source)
 _d_:     cider-grimoire               %(cider-buddy-shortcut-str 'cider-grimoire)
 _e_:     cider-grimoire-web           %(cider-buddy-shortcut-str 'cider-grimoire-web)
 _f_:     cider-javadoc                %(cider-buddy-shortcut-str 'cider-javadoc)
"
      ("/" hydra-cider-buddy-main/body)
      ("a"  cider-doc)
      ("b"  cider-doc-map)
      ("c"  cider-docview-source)
      ("d"  cider-grimoire)
      ("e"  cider-grimoire-web)
      ("f"  cider-javadoc)
      )

;;; *** environment
    (defhydra hydra-cider-buddy-environment (:hint nil :color blue)
      "
CIDER Buddy Environment Menu

 Key^^    Command                           Shortcut

 _/_:     cider-buddy-main                  %(cider-buddy-shortcut-str 'hydra-cider-buddy-main)

 _a_:     cider-apropos                     %(cider-buddy-shortcut-str 'cider-apropos)
 _b_:     cider-apropos-documentation       %(cider-buddy-shortcut-str 'cider-apropos-documentation)
 _c_:     cider-browse-instrumented-defs    %(cider-buddy-shortcut-str 'cider-browse-instrumented-defs)
 _d_:     cider-browse-ns                   %(cider-buddy-shortcut-str 'cider-browse-ns)
 _e_:     cider-browse-ns-all               %(cider-buddy-shortcut-str 'cider-browse-ns-all)
 _f_:     cider-browse-ns-doc-at-point      %(cider-buddy-shortcut-str 'cider-browse-ns-doc-at-point)
 _g_:     cider-browse-ns-find-at-point     %(cider-buddy-shortcut-str 'cider-browse-ns-find-at-point)
 _h_:     cider-browse-ns-handle-mouse      %(cider-buddy-shortcut-str 'cider-browse-ns-handle-mouse)
 _i_:     cider-browse-ns-mode              %(cider-buddy-shortcut-str 'cider-browse-ns-mode)
 _j_:     cider-browse-ns-operate-at-point  %(cider-buddy-shortcut-str 'cider-browse-ns-operate-at-point)
 _k_:     cider-classpath                   %(cider-buddy-shortcut-str 'cider-classpath)
 _l_:     cider-find-and-clear-repl-output  %(cider-buddy-shortcut-str 'cider-find-and-clear-repl-output)
 _m_:     cider-find-dwim                   %(cider-buddy-shortcut-str 'cider-find-dwim)
 _n_:     cider-find-dwim-other-window      %(cider-buddy-shortcut-str 'cider-find-dwim-other-window)
 _o_:     cider-find-ns                     %(cider-buddy-shortcut-str 'cider-find-ns)
 _p_:     cider-find-resource               %(cider-buddy-shortcut-str 'cider-find-resource)
 _q_:     cider-find-var                    %(cider-buddy-shortcut-str 'cider-find-var)
 _r_:     cider-open-classpath-entry        %(cider-buddy-shortcut-str 'cider-open-classpath-entry)
"
      ("/" hydra-cider-buddy-main/body)
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
      ("q" cider-find-var)
      ("r" cider-open-classpath-entry)
      )

;;; *** eval
    (defhydra hydra-cider-buddy-eval (:hint nil :color blue)
      "
CIDER Buddy: Evaluation Commands Menu

 Key^^    Command                           Shortcut

 _/_:     cider-buddy-main                  %(cider-buddy-shortcut-str 'hydra-cider-buddy-main/body)

 _a_:     cider-eval-buffer                 %(cider-buddy-shortcut-str 'cider-eval-buffer)
 _b_:     cider-eval-defun-at-point         %(cider-buddy-shortcut-str 'cider-eval-defun-at-point)
 _c_:     cider-eval-defun-to-comment       %(cider-buddy-shortcut-str 'cider-eval-defun-to-comment)
 _d_:     cider-eval-file                   %(cider-buddy-shortcut-str 'cider-eval-file)
 _e_:     cider-eval-last-sexp              %(cider-buddy-shortcut-str 'cider-eval-last-sexp)
 _f_:     cider-eval-last-sexp-and-replace  %(cider-buddy-shortcut-str 'cider-eval-last-sexp-and-replace)
 _g_:     cider-eval-last-sexp-to-repl      %(cider-buddy-shortcut-str 'cider-eval-last-sexp-to-repl)
 _h_:     cider-eval-ns-form                %(cider-buddy-shortcut-str 'cider-eval-ns-form)
 _i_:     cider-eval-region                 %(cider-buddy-shortcut-str 'cider-eval-region)
 _j_:     cider-interrupt-eval              %(cider-buddy-shortcut-str 'cider-interrupt-eval)
 _k_:     cider-pprint-eval-defun-at-point  %(cider-buddy-shortcut-str 'cider-pprint-eval-defun-at-point)
 _l_:     cider-pprint-eval-last-sexp       %(cider-buddy-shortcut-str 'cider-pprint-eval-last-sexp)
 _m_:     cider-read-and-eval               %(cider-buddy-shortcut-str 'cider-read-and-eval)
"
      ("/" hydra-cider-buddy-main/body)
      ("a" cider-eval-buffer)
      ("b" cider-eval-defun-at-point)
      ("c" cider-eval-defun-to-comment)
      ("d" cider-eval-file)
      ("e" cider-eval-last-sexp)
      ("f" cider-eval-last-sexp-and-replace)
      ("g" cider-eval-last-sexp-to-repl)
      ("h" cider-eval-ns-form)
      ("i" cider-eval-region)
      ("j" cider-interrupt-eval)
      ("k" cider-pprint-eval-defun-at-point)
      ("l" cider-pprint-eval-last-sexp)
      ("m" cider-read-and-eval)
      ("n" cider-run)
      ("o" cider-load-file)
      ("p" cider-undef)
      ("q" cider-interrupt)
      ("r" cider-refresh)
      ("s" cider-macroexpand-1)
      ("t" cider-macroexpand-all)
      )

;;; *** format
    (defhydra hydra-cider-buddy-format (:hint nil :color blue)
      "
CIDER Buddy Format Menu

 Key^^    Command                  Shortcut

 _/_:     cider-buddy-main         %(cider-buddy-shortcut-str 'hydra-cider-buddy-main/body)

 _a_:     cider-format-buffer      %(cider-buddy-shortcut-str 'cider-format-buffer)
 _b_:     cider-format-defun       %(cider-buddy-shortcut-str 'cider-format-defun)
 _c_:     cider-format-edn-buffer  %(cider-buddy-shortcut-str 'cider-format-edn-buffer)
 _d_:     cider-format-edn-region  %(cider-buddy-shortcut-str 'cider-format-edn-region)
 _e_:     cider-format-region      %(cider-buddy-shortcut-str 'cider-format-region)
"
      ("/" hydra-cider-buddy-main/body)
      ("a" cider-format-buffer)
      ("b" cider-format-defun)
      ("c" cider-format-edn-buffer)
      ("d" cider-format-edn-region)
      ("e" cider-format-region)
      )

;;; *** help
    (defhydra hydra-cider-buddy-help (:hint nil :color blue)
      "
CIDER Buddy Help Menu

 Key^^    Command                 Shortcut

 _/_:     hydra-cider-buddy-main  %(cider-buddy-shortcut-str 'cider-browse-instrumented-defs)

 _a_:     cider-drink-a-sip       %(cider-buddy-shortcut-str 'cider-drink-a-sip)
 _b_:     cider-report-bug        %(cider-buddy-shortcut-str 'cider-report-bug)
 _c_:     cider-version           %(cider-buddy-shortcut-str 'cider-version)
 _d_:     cider-view-manual       %(cider-buddy-shortcut-str 'cider-view-manual)
 _e_:     cider-view-refcard      %(cider-buddy-shortcut-str 'cider-view-refcard)
"
      ("/" hydra-cider-buddy-main/body)
      ("a" cider-drink-a-sip)
      ("b" cider-report-bug)
      ("c" cider-version)
      ("d" cider-view-manual)
      ("e" cider-view-refcard)
      )

;;; *** inspection
    (defhydra hydra-cider-buddy-inspection (:hint nil :color blue)
      "
CIDER Buddy Inspection Menu

 Key^^    Command                                      Shortcut

 _/_:     cider-buddy-main                             %(cider-buddy-shortcut-str 'hydra-cider-buddy-main/body)

 _a_:     cider-inspect                                %(cider-buddy-shortcut-str 'cider-inspect)
 _b_:     cider-inspect-defun-at-point                 %(cider-buddy-shortcut-str 'cider-inspect-defun-at-point)
 _c_:     cider-inspect-last-sexp                      %(cider-buddy-shortcut-str 'cider-inspect-last-sexp)
 _d_:     cider-inspect-read-and-inspect               %(cider-buddy-shortcut-str 'cider-inspect-read-and-inspect)
 _e_:     cider-inspector-mode                         %(cider-buddy-shortcut-str 'cider-inspector-mode)
 _f_:     cider-inspector-next-inspectable-object      %(cider-buddy-shortcut-str 'cider-inspector-next-inspectable-object)
 _g_:     cider-inspector-next-page                    %(cider-buddy-shortcut-str 'cider-inspector-next-page)
 _h_:     cider-inspector-operate-on-click             %(cider-buddy-shortcut-str 'cider-inspector-operate-on-click)
 _i_:     cider-inspector-operate-on-point             %(cider-buddy-shortcut-str 'cider-inspector-operate-on-point)
 _j_:     cider-inspector-pop                          %(cider-buddy-shortcut-str 'cider-inspector-pop)
 _k_:     cider-inspector-prev-page                    %(cider-buddy-shortcut-str 'cider-inspector-prev-page)
 _l_:     cider-inspector-previous-inspectable-object  %(cider-buddy-shortcut-str 'cider-inspector-previous-inspectable-object)
 _m_:     cider-inspector-refresh                      %(cider-buddy-shortcut-str 'cider-inspector-refresh)
 _n_:     cider-inspector-set-page-size                %(cider-buddy-shortcut-str 'cider-inspector-set-page-size)
"
      ("/" hydra-cider-buddy-main/body)
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
    (defhydra hydra-cider-buddy-insert (:hint nil :color blue)
      "
CIDER Buddy Insert Menu

 key^^    command                          shortcut

 _/_:     cider-buddy-main                 %(cider-buddy-shortcut-str 'hydra-cider-buddy-main/body)

 _a_:     cider-insert-defun-in-repl       %(cider-buddy-shortcut-str 'cider-insert-defun-in-repl)
 _b_:     cider-insert-last-sexp-in-repl   %(cider-buddy-shortcut-str 'cider-insert-last-sexp-in-repl)
 _c_:     cider-insert-ns-form-in-repl     %(cider-buddy-shortcut-str 'cider-insert-ns-form-in-repl)
 _d_:     cider-insert-region-in-repl      %(cider-buddy-shortcut-str 'cider-insert-region-in-repl)
"
      ("/" hydra-cider-buddy-main/body)
      ("a" cider-insert-defun-in-repl)
      ("b" cider-insert-last-sexp-in-repl)
      ("c" cider-insert-ns-form-in-repl)
      ("d" cider-insert-region-in-repl)
      )

;;; *** popup buffer
    (defhydra hydra-cider-buddy-popup (:hint nil :color blue)
      "
CIDER Buddy Popup Menu

 Key^^    Command                           Shortcut

 _/_:     cider-buddy-main                  %(cider-buddy-shortcut-str 'hydra-cider-buddy-main/body)

 _a_:     cider-popup-buffer-mode           %(cider-buddy-shortcut-str 'cider-popup-buffer-mode)
 _b_:     cider-popup-buffer-quit           %(cider-buddy-shortcut-str 'cider-popup-buffer-quit)
 _c_:     cider-popup-buffer-quit-function  %(cider-buddy-shortcut-str 'cider-popup-buffer-quit-function)
"
      ("/" hydra-cider-buddy-main/body)
      ("a" cider-popup-buffer-mode)
      ("b" cider-popup-buffer-quit)
      ("c" cider-popup-buffer-quit-function)
      )

;;; *** repl
    (defhydra hydra-cider-buddy-repl-menu-1 (:hint nil :color blue)
      "
CIDER Buddy Repl Menu

 Key^^    Command                                Shortcut

 _/_:     cider-buddy-main                       %(cider-buddy-shortcut-str 'hydra-cider-buddy-main/body)
 _>_:     cider-buddy-repl-menu-2                %(cider-buddy-shortcut-str 'hydra-cider-buddy-repl-menu-2/body)

 _a_:     cider-repl-backward-input              %(cider-buddy-shortcut-str 'cider-repl-backward-input)
 _b_:     cider-repl-beginning-of-defun          %(cider-buddy-shortcut-str 'cider-repl-beginning-of-defun)
 _c_:     cider-repl-bol-mark                    %(cider-buddy-shortcut-str 'cider-repl-bol-mark)
 _d_:     cider-repl-clear-banners               %(cider-buddy-shortcut-str 'cider-repl-clear-banners)
 _e_:     cider-repl-clear-buffer                %(cider-buddy-shortcut-str 'cider-repl-clear-buffer)
 _f_:     cider-repl-clear-help-banner           %(cider-buddy-shortcut-str 'cider-repl-clear-help-banner)
 _g_:     cider-repl-clear-output                %(cider-buddy-shortcut-str 'cider-repl-clear-output)
 _h_:     cider-repl-closing-return              %(cider-buddy-shortcut-str 'cider-repl-closing-return)
 _i_:     cider-repl-end-of-defun                %(cider-buddy-shortcut-str 'cider-repl-end-of-defun)
 _j_:     cider-repl-forward-input               %(cider-buddy-shortcut-str 'cider-repl-forward-input)
 _k_:     cider-repl-handle-shortcut             %(cider-buddy-shortcut-str 'cider-repl-handle-shortcut)
 _l_:     cider-repl-history-load                %(cider-buddy-shortcut-str 'cider-repl-history-load)
 _m_:     cider-repl-history-save                %(cider-buddy-shortcut-str 'cider-repl-history-save)
 _n_:     cider-repl-indent-and-complete-symbol  %(cider-buddy-shortcut-str 'cider-repl-indent-and-complete-symbol)
 _o_:     cider-repl-kill-input                  %(cider-buddy-shortcut-str 'cider-repl-kill-input)
"
      ("/" hydra-cider-buddy-main/body)
      (">" hydra-cider-buddy-repl-menu-2/body)
      ("a" cider-repl-backward-input)
      ("b" cider-repl-beginning-of-defun)
      ("c" cider-repl-bol-mark)
      ("d" cider-repl-clear-banners)
      ("e" cider-repl-clear-buffer)
      ("f" cider-repl-clear-help-banner)
      ("g" cider-repl-clear-output)
      ("h" cider-repl-closing-return)
      ("i" cider-repl-end-of-defun)
      ("j" cider-repl-forward-input)
      ("k" cider-repl-handle-shortcut)
      ("l" cider-repl-history-load)
      ("m" cider-repl-history-save)
      ("n" cider-repl-indent-and-complete-symbol)
      ("o" cider-repl-kill-input)
      )


    (defhydra hydra-cider-buddy-repl-menu-2 (:hint nil :color blue)

      "
CIDER Buddy REPL Menu 2

 Key^^    Command                                Shortcut

 _/_:     cider-buddy-main                       %(cider-buddy-shortcut-str 'hydra-cider-buddy-main/body)
 _<_:     cider-buddy-repl-menu-1                %(cider-buddy-shortcut-str 'hydra-cider-buddy-repl-menu-1/body)

 _a_:     cider-repl-mode                        %(cider-buddy-shortcut-str 'cider-repl-mode)
 _b_:     cider-repl-mode-menu                   %(cider-buddy-shortcut-str 'cider-repl-mode-menu)
 _c_:     cider-repl-newline-and-indent          %(cider-buddy-shortcut-str 'cider-repl-newline-and-indent)
 _d_:     cider-repl-next-input                  %(cider-buddy-shortcut-str 'cider-repl-next-input)
 _e_:     cider-repl-next-matching-input         %(cider-buddy-shortcut-str 'cider-repl-next-matching-input)
 _f_:     cider-repl-next-prompt                 %(cider-buddy-shortcut-str 'cider-repl-next-prompt)
 _g_:     cider-repl-previous-input              %(cider-buddy-shortcut-str 'cider-repl-previous-input)
 _h_:     cider-repl-previous-matching-input     %(cider-buddy-shortcut-str 'cider-repl-previous-matching-input)
 _i_:     cider-repl-previous-prompt             %(cider-buddy-shortcut-str 'cider-repl-previous-prompt)
 _j_:     cider-repl-require-repl-utils          %(cider-buddy-shortcut-str 'cider-repl-require-repl-utils)
 _k_:     cider-repl-return                      %(cider-buddy-shortcut-str 'cider-repl-return)
 _l_:     cider-repl-set-ns                      %(cider-buddy-shortcut-str 'cider-repl-set-ns)
 _m_:     cider-repl-shortcuts-help              %(cider-buddy-shortcut-str 'cider-repl-shortcuts-help)
 _n_:     cider-repl-switch-to-other             %(cider-buddy-shortcut-str 'cider-repl-switch-to-other)
 _o_:     cider-repl-tab                         %(cider-buddy-shortcut-str 'cider-repl-tab)
 _p_:     cider-repl-toggle-pretty-printing      %(cider-buddy-shortcut-str 'cider-repl-toggle-pretty-printing)
"
      ("/" hydra-cider-buddy-main/body)
      ("<" hydra-cider-buddy-repl-menu-1/body)
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
    (defhydra hydra-cider-buddy-stacktrace (:hint nil :color blue)
      "
CIDER Buddy Stacktrace Menu

 Key^^    Command                               Shortcut

 _/_:     cider-buddy-main                      %(cider-buddy-shortcut-str 'hydra-cider-buddy-main/body)

 _a_:     cider-stacktrace-cycle-all-causes     %(cider-buddy-shortcut-str 'cider-stacktrace-cycle-all-causes)
 _b_:     cider-stacktrace-cycle-cause-1        %(cider-buddy-shortcut-str 'cider-stacktrace-cycle-cause-1)
 _c_:     cider-stacktrace-cycle-cause-2        %(cider-buddy-shortcut-str 'cider-stacktrace-cycle-cause-2)
 _d_:     cider-stacktrace-cycle-cause-3        %(cider-buddy-shortcut-str 'cider-stacktrace-cycle-cause-3)
 _e_:     cider-stacktrace-cycle-cause-4        %(cider-buddy-shortcut-str 'cider-stacktrace-cycle-cause-4)
 _f_:     cider-stacktrace-cycle-cause-5        %(cider-buddy-shortcut-str 'cider-stacktrace-cycle-cause-5)
 _g_:     cider-stacktrace-cycle-current-cause  %(cider-buddy-shortcut-str 'cider-stacktrace-cycle-current-cause)
 _h_:     cider-stacktrace-jump                 %(cider-buddy-shortcut-str 'cider-stacktrace-jump)
 _i_:     cider-stacktrace-mode                 %(cider-buddy-shortcut-str 'cider-stacktrace-mode)
 _j_:     cider-stacktrace-mode-menu            %(cider-buddy-shortcut-str 'cider-stacktrace-mode-menu)
 _k_:     cider-stacktrace-next-cause           %(cider-buddy-shortcut-str 'cider-stacktrace-next-cause)
 _l_:     cider-stacktrace-previous-cause       %(cider-buddy-shortcut-str 'cider-stacktrace-previous-cause)
 _m_:     cider-stacktrace-toggle-all           %(cider-buddy-shortcut-str 'cider-stacktrace-toggle-all)
 _n_:     cider-stacktrace-toggle-clj           %(cider-buddy-shortcut-str 'cider-stacktrace-toggle-clj)
 _o_:     cider-stacktrace-toggle-duplicates    %(cider-buddy-shortcut-str 'cider-stacktrace-toggle-duplicates)
 _p_:     cider-stacktrace-toggle-java          %(cider-buddy-shortcut-str 'cider-stacktrace-toggle-java)
 _q_:     cider-stacktrace-toggle-repl          %(cider-buddy-shortcut-str 'cider-stacktrace-toggle-repl)
 _r_:     cider-stacktrace-toggle-tooling       %(cider-buddy-shortcut-str 'cider-stacktrace-toggle-tooling)
"
      ("/" hydra-cider-buddy-main/body)

      ("a" cider-stacktrace-cycle-all-causes)
      ("b" cider-stacktrace-cycle-cause-1)
      ("c" cider-stacktrace-cycle-cause-2)
      ("d" cider-stacktrace-cycle-cause-3)
      ("e" cider-stacktrace-cycle-cause-4)
      ("f" cider-stacktrace-cycle-cause-5)
      ("g" cider-stacktrace-cycle-current-cause)
      ("h" cider-stacktrace-jump)
      ("i" cider-stacktrace-mode)
      ("j" cider-stacktrace-mode-menu)
      ("k" cider-stacktrace-next-cause)
      ("l" cider-stacktrace-previous-cause)
      ("m" cider-stacktrace-toggle-all)
      ("n" cider-stacktrace-toggle-clj)
      ("o" cider-stacktrace-toggle-duplicates)
      ("p" cider-stacktrace-toggle-java)
      ("q" cider-stacktrace-toggle-repl)
      ("r" cider-stacktrace-toggle-tooling)
      )

;;; *** test
    (defhydra hydra-cider-buddy-test (:hint nil :color blue)
      "
CIDER Buddy Test Menu

 Key^^    Command                       Shortcut

 _/_:     cider-buddy-main              %(cider-buddy-shortcut-str 'hydra-cider-buddy-main/body)

 _a_:     cider-test-clear-highlights   %(cider-buddy-shortcut-str 'cider-test-clear-highlights)
 _b_:     cider-test-commands-map       %(cider-buddy-shortcut-str 'cider-test-commands-map)
 _c_:     cider-test-ediff              %(cider-buddy-shortcut-str 'cider-test-ediff)
 _d_:     cider-test-ediff-cleanup      %(cider-buddy-shortcut-str 'cider-test-ediff-cleanup)
 _e_:     cider-test-jump               %(cider-buddy-shortcut-str 'cider-test-jump)
 _f_:     cider-test-next-result        %(cider-buddy-shortcut-str 'cider-test-next-result)
 _g_:     cider-test-previous-result    %(cider-buddy-shortcut-str 'cider-test-previous-result)
 _h_:     cider-test-report-mode        %(cider-buddy-shortcut-str 'cider-test-report-mode)
 _i_:     cider-test-report-mode-menu   %(cider-buddy-shortcut-str 'cider-test-report-mode-menu)
 _j_:     cider-test-rerun-tests        %(cider-buddy-shortcut-str 'cider-test-rerun-tests)
 _k_:     cider-test-run-loaded-tests   %(cider-buddy-shortcut-str 'cider-test-run-loaded-tests)
 _l_:     cider-test-run-ns-tests       %(cider-buddy-shortcut-str 'cider-test-run-ns-tests)
 _m_:     cider-test-run-project-tests  %(cider-buddy-shortcut-str 'cider-test-run-project-tests)
 _n_:     cider-test-run-test           %(cider-buddy-shortcut-str 'cider-test-run-test)
 _o_:     cider-test-show-report        %(cider-buddy-shortcut-str 'cider-test-show-report)
 _p_:     cider-test-stacktrace         %(cider-buddy-shortcut-str 'cider-test-stacktrace)
"
      ("/" hydra-cider-buddy-main/body)
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
      )

;;; *** tracing
    (defhydra hydra-cider-buddy-tracing (:hint nil :color blue)
      "
CIDER Buddy Tracing Menu

 Key^^    Command                 Shortcut

 _/_:     cider-buddy-main        %(cider-buddy-shortcut-str 'hydra-cider-buddy-main/body)

 _a_:     cider-toggle-trace-ns   %(cider-buddy-shortcut-str 'cider-toggle-trace-ns)
 _b_:     cider-toggle-trace-var  %(cider-buddy-shortcut-str 'cider-toggle-trace-var)
"
      ("/" hydra-cider-buddy-main/body)
      ("a" cider-toggle-trace-ns)
      ("b" cider-toggle-trace-var)
      )

;;; *** miscellanea
    (defhydra hydra-cider-buddy-miscellanea (:hint nil :color blue)
      "
CIDER Buddy Miscellanea Menu

 Key^^    Command                          Shortcut

 _/_:     cider-buddy-main                 %(cider-buddy-shortcut-str 'hydra-cider-buddy-main/body)

 _d_:     cider-jump-to-compilation-error  %(cider-buddy-shortcut-str 'cider-jump-to-compilation-error)
 _e_:     cider-mode                       %(cider-buddy-shortcut-str 'cider-mode)
 _f_:     cider-mode-menu                  %(cider-buddy-shortcut-str 'cider-mode-menu)
"
      ("/" hydra-cider-buddy-main/body)
      ("d" cider-jump-to-compilation-error)
      ("e" cider-mode)
      ("f" cider-mode-menu)
      ("i" cider-pop-back)
      )

    ))

(provide 'cider-buddy)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cider-buddy.el ends here
