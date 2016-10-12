(defun java-completing-dot ()
  "Insert a period and show company completions."
  (interactive "*")
  (when (s-matches? (rx (+ (not space)))
                    (buffer-substring (line-beginning-position) (point)))
    (delete-horizontal-space t))
  (java-delete-horizontal-space)
  (insert ".")
  (company-emacs-eclim 'interactive))

(defun java-completing-double-colon ()
  "Insert double colon and show company completions."
  (interactive "*")
  (java-delete-horizontal-space)
  (insert ":")
  (let ((curr (point)))
    (when (s-matches? (buffer-substring (- curr 2) (- curr 1)) ":")
      (company-emacs-eclim 'interactive))))

(defun java-delete-horizontal-space ()
  (when (s-matches? (rx (+ (not space)))
                    (buffer-substring (line-beginning-position) (point)))
    (delete-horizontal-space t)))

(defun java-maven-test ()
  (interactive)
  (eclim-maven-run "test"))

(defun java-maven-clean-install ()
  (interactive)
  (eclim-maven-run "clean install"))

(defun java-maven-install ()
  (interactive)
  (eclim-maven-run "install"))

(use-package eclim
  :defer t
  :ensure t
  :diminish eclim-mode
  :functions (eclim--project-dir eclim--project-name)
  :commands (eclim-mode global-eclim-mode)
  :init
  (add-hook 'java-mode-hook 'eclim-mode)
  (setq eclim-eclipse-dirs '("~/eclipse")
        eclim-executable "~/eclipse/eclim")
  :config
  (defhydra hydra-eclim (:color teal
                                :hint nil)
    "
Eclim:
 ╭─────────────────────────────────────────────────────┐
 │ Java                                                │       Problems
╭┴─────────────────────────────────────────────────────┴────────────────────────────────────╯
  _d_: Show Doc             _i_: Implement (Override)          _ea_: Show Problems
  _g_: Make getter/setter  _fd_: Find Declarations             _ec_: Show Corrections
  _o_: Organize Imports    _fr_: Find References               _er_: Buffer Refresh
  _h_: Hierarchy            _R_: Refactor Symbol
  _c_: Java Constructor    _ft_: Find Type
  _F_: Java Format
  _r_: Java Run
Project                            Maven
─────────────────────────────────────────────────────────
_pj_: Jump to proj           _mi_: Mvn Clean Install
_pc_: Create                 _mI_: Mvn Install
_pi_: Import Proj            _mt_: Mvn Test
_pg_: Mode Refresh           _mr_: Mvn Run
_pr_: Proj Refresh
_pm_: Proj Mode
"
    ("d" eclim-java-show-documentation-for-current-element)
    ("g" eclim-java-generate-getter-and-setter)
    ("o" eclim-java-import-organize)
    ("h" eclim-java-call-hierarchy)
    ("i" eclim-java-implement)
    ("r" eclim-java-run-run)
    ("fd" eclim-java-find-declaration)
    ("fr" eclim-java-find-references)
    ("R" eclim-java-refactor-rename-symbol-at-point)
    ("fg" eclim-java-find-generic)
    ("ft" eclim-java-find-type)
    ("c" eclim-java-constructor)
    ("F" eclim-java-format)
    ("I" eclim-java-hierarchy)

    ("mi" java-maven-clean-install)
    ("mI" java-maven-install)
    ("mp" eclim-maven-lifecycle-phases)
    ("mr" eclim-maven-run)
    ("mR" eclim-maven-lifecycle-phase-run)
    ("mt" java-maven-test)

    ("." java-completing-dot)
    (":" java-completing-double-colon)

    ("ea" eclim-problems-show-all)
    ("eb" eclim-problems)
    ("ec" eclim-problems-correct)
    ("ee" eclim-problems-show-errors)
    ("ef" eclim-problems-toggle-filefilter)
    ("en" eclim-problems-next-same-window)
    ("eo" eclim-problems-open)
    ("ep" eclim-problems-previous-same-window)
    ("ew" eclim-problems-show-warnings)
    ("er" eclim-problems-buffer-refresh)
    ("eO" eclim-problems-open-current)

    ("pj" eclim-project-goto)
    ("pc" eclim-project-create)
    ("pi" eclim-project-import)
    ("pb" eclim-project-build)
    ("pd" eclim-project-delete)
    ("pI" eclim-project-info-mode)
    ("pk" eclim-project-close)
    ("po" eclim-project-open)
    ("p1" eclim-project-mode)
    ("pu" eclim-project-update)
    ("pm" eclim-project-mode)
    ("pM" eclim-project-mark-all)
    ("pu" eclim-project-unmark-current)
    ("pU" eclim-project-unmark-all)
    ("pg" eclim-project-mode-refresh)
    ("pR" eclim-project-rename)
    ("pr" eclim-project-refresh)
    ("q" nil "cancel" :color blue)))

(use-package company-emacs-eclim
  :ensure t
  :functions company-emacs-eclim-setup
  :config (company-emacs-eclim-setup))

(use-package eclim-java-run
  :defer t
  :init (require 'eclim-java-run))

(use-package java-imports
  :ensure t
  :config
  ;; Elasticsearch's import style
  (setq java-imports-find-block-function 'java-imports-find-place-sorted-block))

(provide  'init-eclim)
