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

(use-package emacs-eclim
  :ensure t
  :diminish eclim-mode
  :commands (eclim-mode global-eclim-mode)
  :init
  (add-hook 'java-mode-hook (lambda () (eclim-mode 1)))
  (setq eclim-eclipse-dirs '("~/eclipse")
        eclim-executable     "~/eclipse/eclim")
  :config
  (progn
    (setq help-at-pt-display-when-idle t
          help-at-pt-timer-delay 0.1)
    (help-at-pt-set-timer)
    (add-to-list 'minor-mode-alist
                 '(eclim-mode (:eval (eclim-modeline-string))))
    ;; company completion.
    (require 'company-emacs-eclim)
    (company-emacs-eclim-setup)
    (require 'eclimd)
    (with-eval-after-load 'cc-mode 
      (evil-define-key 'insert java-mode-map
        (kbd ".") 'java-completing-dot
        (kbd ":") 'java-completing-double-colon
        (kbd "M-.") 'eclim-java-find-declaration
        (kbd "M-,") 'pop-tag-mark
        (kbd "M-<mouse-3>") 'eclim-java-find-declaration
        (kbd "<mouse-8>") 'pop-tag-mark)

      (evil-define-key 'normal java-mode-map
        (kbd "M-.") 'eclim-java-find-declaration
        (kbd "M-,") 'pop-tag-mark
        (kbd "M-<mouse-3>") 'eclim-java-find-declaration
        (kbd "<mouse-8>") 'pop-tag-mark)

      (evil-define-key 'normal eclim-problems-mode-map
        (kbd "a") 'eclim-problems-show-all
        (kbd "e") 'eclim-problems-show-errors
        (kbd "g") 'eclim-problems-buffer-refresh
        (kbd "q") 'eclim-quit-window
        (kbd "w") 'eclim-problems-show-warnings
        (kbd "f") 'eclim-problems-toggle-filefilter
        (kbd "c") 'eclim-problems-correct
        (kbd "RET") 'eclim-problems-open-current)

      (evil-define-key 'normal eclim-project-mode-map
        (kbd "N") 'eclim-project-create
        (kbd "m") 'eclim-project-mark-current
        (kbd "M") 'eclim-project-mark-all
        (kbd "u") 'eclim-project-unmark-current
        (kbd "U") 'eclim-project-unmark-all
        (kbd "o") 'eclim-project-open
        (kbd "c") 'eclim-project-close
        (kbd "i") 'eclim-project-info-mode
        (kbd "I") 'eclim-project-import
        (kbd "RET") 'eclim-project-goto
        (kbd "D") 'eclim-project-delete
        (kbd "p") 'eclim-project-update
        (kbd "g") 'eclim-project-mode-refresh
        (kbd "R") 'eclim-project-rename
        (kbd "q") 'eclim-quit-window)

      (evil-leader/set-key-for-mode 'java-mode
        "mea" 'eclim-problems-show-all
        "meb" 'eclim-problems
        "mec" 'eclim-problems-correct
        "mee" 'eclim-problems-show-errors
        "mef" 'eclim-problems-toggle-filefilter
        "men" 'eclim-problems-next-same-window
        "meo" 'eclim-problems-open
        "mep" 'eclim-problems-previous-same-window
        "mew" 'eclim-problems-show-warnings

        "mff" 'eclim-java-find-generic

        "mgg" 'eclim-java-find-declaration
        "mgt" 'eclim-java-find-type

        "mrc" 'eclim-java-constructor
        "mrg" 'eclim-java-generate-getter-and-setter
        "mrf" 'eclim-java-format
        "mri" 'eclim-java-import-organize
        "mrj" 'eclim-java-implement
        "mrr" 'eclim-java-refactor-rename-symbol-at-point

        "mhc" 'eclim-java-call-hierarchy
        "mhh" 'eclim-java-show-documentation-for-current-element
        "mhi" 'eclim-java-hierarchy
        "mhu" 'eclim-java-find-references

        "mmi" 'nil
        "mmI" 'nil
        "mmp" 'eclim-maven-lifecycle-phases
        "mmr" 'eclim-maven-run
        "mmR" 'eclim-maven-lifecycle-phase-run
        "mmt" 'nil

        "maa" 'eclim-ant-run
        "mac" 'eclim-ant-clear-cache
        "mar" 'eclim-ant-run
        "mav" 'eclim-ant-validate

        "mpb" 'eclim-project-build
        "mpc" 'eclim-project-create
        "mpd" 'eclim-project-delete
        "mpg" 'eclim-project-goto
        "mpi" 'eclim-project-import
        "mpj" 'eclim-project-info-mode
        "mpk" 'eclim-project-close
        "mpo" 'eclim-project-open
        "mpp" 'eclim-project-mode
        "mpu" 'eclim-project-update

        "mtt" 'eclim-run-junit))))



(provide 'init-eclim)
