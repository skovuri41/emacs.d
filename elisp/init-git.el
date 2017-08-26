(use-package git-timemachine
  :ensure t
  :commands git-timemachine-toggle)

(use-package git-gutter+
  :diminish (git-gutter+-mode . "")
  :commands global-git-gutter+-mode
  :defer 2
  :init
  (progn
    (setq git-gutter+-hide-gutter t)
    (add-hook 'magit-pre-refresh-hook 'git-gutter+-refresh)
    (global-git-gutter+-mode)
    (use-package git-gutter-fringe+
      ;; :init
      ;; (global-git-gutter+-mode)
      :config
      (progn
        (set-face-foreground 'git-gutter-fr+-modified "magenta")
        (set-face-foreground 'git-gutter-fr+-added "green")
        (set-face-foreground 'git-gutter-fr+-deleted "red")
        ;; (setq-default left-fringe-width  40)

        (add-hook 'git-gutter+-mode-hook 'my/set-fringe-bg)

        ;; custom graphics that works nice with half-width fringes
        (fringe-helper-define 'git-gutter-fr+-added nil
          "..X...."
          "..X...."
          "XXXXX.."
          "..X...."
          "..X....")
        (fringe-helper-define 'git-gutter-fr+-deleted nil
          "......."
          "......."
          "XXXXX.."
          "......."
          ".......")
        (fringe-helper-define 'git-gutter-fr+-modified nil
          "..X...."
          ".XXX..."
          "XX.XX.."
          ".XXX..."
          "..X....")
        
        ;; hydra git-gutter
        (defhydra hydra-git-gutter (:body-pre (global-git-gutter+-mode 1)
                                              :hint nil)
          "
      Git gutter:
      _j_: next hunk        _s_tage hunk     _q_uit
      _k_: previous hunk    _r_evert hunk   
      _m_: git-gutter+-mode _c_ommit 
      _b_: stage & commit   _B_: stage & commit whole buffer
      _h_: show hunk inline at point
"
          ("j" git-gutter+-next-hunk)
          ("k" git-gutter+-previous-hunk)
          ("s" git-gutter+-stage-hunks)
          ("r" git-gutter+-revert-hunk)
          ("m" global-git-gutter+-mode)
          ("c" (lambda () (interactive)
                 (git-gutter+-commit)
                 (switch-to-buffer-other-window "*Commit Message*")) :exit t)
          ("b" (lambda () (interactive)
                 (git-gutter+-stage-and-commit)
                 (switch-to-buffer-other-window "*Commit Message*")) :exit t)
          ("B" (lambda () (interactive)
                 (git-gutter+-stage-and-commit-whole-buffer)
                 (switch-to-buffer-other-window "*Commit Message*")))
          ("h" git-gutter+-show-hunk-inline-at-point)
          ("q" nil :color blue))))))


(use-package gitconfig-mode
  :ensure t
  :mode ("/\\.git/?config\\'"
         "/\\.gitconfig\\.local\\'"
         "/\\.gitmodules\\'"))

(use-package gitignore-mode
  :ensure t
  :mode ("/\\.gitignore\\'"
         "/\\.gitignore\\.global\\'"))
;;
(use-package gitattributes-mode
  :ensure t
  :mode "/\\.gitattributes\\'")

(use-package git-link
  :ensure t
  ;; :defer t
  :init
  (progn
    (defun my/git-link-copy-url-only ()
      "Only copy the generated link to the kill ring."
      (interactive)
      (let (git-link-open-in-browser)
        (call-interactively 'git-link)))
    (defun my/git-link-commit-copy-url-only ()
      "Only copy the generated link to the kill ring."
      (interactive)
      (let (git-link-open-in-browser)
        (call-interactively 'git-link-commit)))
    ;; default is to open the generated link
    (setq git-link-open-in-browser t)))

(defun my/git-pull-repo-at-path (path)
  "Pull repository at PATH."
  (my/file-with-current-file path (magit-pull)))

(defun my/git-unpushed-changes-p ()
  "Check if unpushed changes are present in git master."
  (not (string-empty-p (shell-command-to-string "git log --oneline origin/master..master"))))

(defun my/git-pending-repo-at-path-p (path)
  "Check if pending changes in repository at PATH."
  (or (my/file-with-current-file path (magit-anything-modified-p))
      (my/git-unpushed-changes-p)))

(defun my/git-check-frequent-repos-pending ()
  "Check for for uncommited changes in frequent repos."
  (interactive)
  (cond
   ((my/git-pending-repo-at-path-p "~/stuff/active/dots")
    (magit-status "~/stuff/active/dots"))
   ((my/git-pending-repo-at-path-p "~/stuff/active/blog")
    (magit-status "~/stuff/active/blog"))
   ((my/git-pending-repo-at-path-p "~/stuff/active/non-public")
    (magit-status "~/stuff/active/non-public"))
   (t (message "Life is good!"))))

(defun my/git-pull-frequent-repos ()
  "Pull all frequent repositories."
  (interactive)
  (my/git-pull-repo-at-path "~/Projects/gitrepos/memory-hole"))

(defun my/git-current-branch ()
  "Get the current git branch."
  (let ((branch-name
         (nth 0 (split-string (shell-command-to-string
                               (format "cd %s && git branch | sed -n -e 's/^\\* \\(.*\\)/\\1/p'"
                                       (file-name-directory (buffer-file-name))))))))
    (assert (length branch-name) nil "Branch not found")
    branch-name))

(provide 'init-git)
