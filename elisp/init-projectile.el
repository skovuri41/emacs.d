(use-package projectile
  :commands (projectile-ack
             projectile-ag
             projectile-compile-project
             projectile-dired
             projectile-find-dir
             projectile-find-file
             projectile-find-tag
             projectile-test-project
             projectile-grep
             projectile-invalidate-cache
             projectile-kill-buffers
             projectile-multi-occur
             projectile-project-p
             projectile-project-root
             projectile-recentf
             projectile-regenerate-tags
             projectile-replace
             projectile-replace-regexp
             projectile-run-async-shell-command-in-root
             projectile-run-shell-command-in-root
             projectile-switch-project
             projectile-switch-to-buffer
             projectile-vc)
  :init
  (progn
    (setq projectile-enable-caching t)
    (setq projectile-known-projects-file
          (expand-file-name "cache/projectile-bookmarks.eld" user-emacs-directory))
    (setq projectile-cache-file
          (expand-file-name "cache/projectile.cache" user-emacs-directory)))
  :config
  (progn

    ;; Use faster search tools: ripgrep or the silver search
    (let ((command
           (cond
            ((executable-find "rg")
             "rg -0 --files --color=never --hidden --sort-files")
            ((executable-find "ag")
             (concat "ag -0 -l --nocolor --hidden"
                     (mapconcat #'identity
                                (cons "" projectile-globally-ignored-directories)
                                " --ignore-dir="))))))
      (setq projectile-generic-command command))

    (defun projectile-custom-mode-line ()
      (if (projectile-project-p)
          (let* ((project-name (projectile-project-name))
                 (project-name-mode-line (if (> (length project-name) 12)
                                             (substring project-name 0 8)
                                           project-name)))
            (format " P[%s] " project-name-mode-line))
        ""))
    ;; (setq-default projectile-mode-line '(:eval (projectile-custom-mode-line)))
    (setq-default projectile-mode-line nil)
    (setq projectile-completion-system 'ivy)
    (setq projectile-indexing-method 'alien)
    (setq projectile-switch-project-action 'projectile-dired)
    (defadvice projectile-dired (after xah-wrapper-dired-commands activate)
      (xah-wrapper-dired-commands))

    (def-projectile-commander-method ?s
      "Open a *shell* buffer for the project."
      (shell (get-buffer-create
              (format "*shell %s*"
                      (projectile-project-name)))))

    (def-projectile-commander-method ?c
      "Run `compile' in the project."
      (call-interactively #'compile))

    (def-projectile-commander-method ?p
      "Go back to project selection."
      (projectile-switch-project))

    (def-projectile-commander-method ?d
      "Open project root in dired."
      (projectile-dired))

    (def-projectile-commander-method ?F
      "Git fetch."
      ;; (magit-status)
      (call-interactively #'magit-status)
      ;; (call-interactively #'magit-fetch-popup)
      )

    (def-projectile-commander-method ?j
      "Jack-in."
      (let* ((opts (projectile-current-project-files))
             (file (ido-completing-read
                    "Find file: "
                    opts
                    nil nil nil nil
                    (car (cl-member-if
                          (lambda (f)
                            (string-match "core\\.clj\\'" f))
                          opts)))))
        (find-file (expand-file-name
                    file (projectile-project-root)))
        (run-hooks 'projectile-find-file-hook)
        (cider-jack-in)))

    (setq projectile-globally-ignored-directories
          '(".idea"
            ".eunit"
            ".git"
            ".hg"
            ".fslckout"
            ".bzr"
            "_darcs"
            ".tox"
            ".svn"
            "build"))

    (add-to-list 'projectile-globally-ignored-files ".DS_Store")
    (add-to-list 'projectile-globally-ignored-files "*.pyc")
    (add-to-list 'projectile-globally-ignored-files "*.python-version")
    (add-to-list 'projectile-globally-ignored-files "*.egg-info")
    (add-to-list 'projectile-globally-ignored-files "*.class")
    (add-to-list 'projectile-globally-ignored-files "*.jar")
    (add-to-list 'projectile-globally-ignored-files "*.tar")
    (add-to-list 'projectile-globally-ignored-files "*.tar.gz")
    (add-to-list 'projectile-globally-ignored-files "*.zip")
    (add-to-list 'projectile-globally-ignored-files "*.el~")
    (add-to-list 'projectile-globally-ignored-files "*.swp")
    (setq projectile-globally-ignored-files (append '(".ensime"
                                                      ".gitignore"
                                                      ".bintray"
                                                      ".travis.yml"
                                                      ".mode"
                                                      ".cask")
                                                    projectile-globally-ignored-files))

    (add-to-list 'projectile-globally-ignored-directories "__pycache__")
    (add-to-list 'projectile-globally-ignored-directories ".env")
    (add-to-list 'projectile-globally-ignored-directories ".venv")
    (add-to-list 'projectile-globally-ignored-directories ".cask")
    (add-to-list 'projectile-globally-ignored-directories ".cache")
    (add-to-list 'projectile-globally-ignored-directories "elpa")
    (add-to-list 'projectile-globally-ignored-directories ".node_modules")
    (add-to-list 'projectile-globally-ignored-directories ".m2")
    (setq projectile-sort-order 'recently-active)

    (projectile-mode)
    ))

(provide 'init-projectile)
