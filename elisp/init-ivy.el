(unless (executable-find "ag")
  (message "%s" "executable: ag not found!, counsel-ag will not work."))

(use-package swiper
  :ensure t
  :diminish ivy-mode
  :config
  (progn
    (defun swiper-the-thing ()
      (interactive)
      (swiper (if (region-active-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (thing-at-point 'symbol))))))

(use-package ivy
  :ensure swiper
  :diminish ivy-mode
  ;; :init
  :config
  (setq ivy-initial-inputs-alist nil)
  (progn
    (setq ivy-display-function nil)
    (setq ivy-use-virtual-buffers t
          ivy-display-style 'fancy)
    (setq ivy-count-format "(%d/%d) ")
    (setq ivy-initial-inputs-alist nil)
    (validate-setq ivy-re-builders-alist
                   '((read-file-name-internal . ivy--regex-fuzzy)
                     (counsel-find-file . ivy--regex-fuzzy)
                     (counsel-projectile-find-file . ivy--regex-fuzzy)
                     (counsel-recentf . ivy--regex-fuzzy)
                     (nameframe-switch-frame . ivy--regex-fuzzy)
                     (ivy-switch-buffer . ivy--regex-fuzzy)
                     (counsel-imenu . ivy--regex-fuzzy)
                     (counsel-M-x . ivy--regex-fuzzy)
                     (t . ivy--regex-plus)))
    (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-insert-current)
    (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-backward-kill-word)
    (define-key ivy-minibuffer-map [escape] (kbd "C-g"))
    (key-chord-define ivy-minibuffer-map "kj" (kbd "C-g"))
    ;; partial complete without exiting
    (define-key ivy-minibuffer-map (kbd "TAB") #'ivy-partial)
    ;;advise swiper to recenter on exit
    (defun bjm-swiper-recenter (&rest args)
      "recenter display after swiper"
      (recenter))
    (advice-add 'swiper :after #'bjm-swiper-recenter)
    ;; fancier colors
    (setq ivy-display-style 'fancy)
    ;;ivy-wrap
    (setq ivy-wrap t)

    (ivy-set-actions
     t
     '(("i" (lambda (x) (with-ivy-window
                     (insert x))) "insert candidate")
       (" " (lambda (x) (ivy-resume)) "resume")
       ("?" (lambda (x)
              (interactive)
              (describe-keymap ivy-minibuffer-map)) "Describe keys")))

    ;; open recent directory, requires ivy (part of swiper)
    ;; borrows from http://stackoverflow.com/questions/23328037/in-emacs-how-to-maintain-a-list-of-recent-directories
    (defun bjm/ivy-dired-recent-dirs ()
      "Present a list of recently used directories and open the selected one in dired"
      (interactive)
      (let ((recent-dirs
             (delete-dups
              (mapcar (lambda (file)
                        (if (file-directory-p file) file (file-name-directory file)))
                      recentf-list))))

        (let ((dir (ivy-read "Directory: "
                             recent-dirs
                             :re-builder #'ivy--regex-fuzzy
                             ;;ivy--regex
                             :sort nil
                             :initial-input nil)))
          (dired dir))))

    (defun counsel-goto-recent-directory ()
      "Open recent directory with dired"
      (interactive)
      (unless recentf-mode (recentf-mode 1))
      (let ((collection
             (delete-dups
              (append (mapcar 'file-name-directory recentf-list)
                      ;; fasd history
                      (if (executable-find "fasd")
                          (split-string (shell-command-to-string "fasd -ld") "\n" t))))))
        (ivy-read "directories:" collection :action 'dired)))


    (defun ivy-dired ()
      (interactive)
      (if ivy--directory
          (ivy-quit-and-run
           (dired ivy--directory)
           (when (re-search-forward
                  (regexp-quote
                   (substring ivy--current 0 -1)) nil t)
             (goto-char (match-beginning 0))))
        (user-error
         "Not completing files currently")))
    (define-key ivy-minibuffer-map (kbd "C-:") 'ivy-dired)

    (defun ivy-switch-project ()
      (interactive)
      (ivy-read
       "Switch to project: "
       (if (projectile-project-p)
           (cons (abbreviate-file-name (projectile-project-root))
                 (projectile-relevant-known-projects))
         projectile-known-projects)
       :action #'projectile-switch-project-by-name))

    (ivy-set-actions
     'ivy-switch-project
     '(("d" dired "Open Dired in project's directory")
       ("v" counsel-projectile "Open project root in vc-dir or magit")
       ("c" projectile-compile-project "Compile project")
       ("r" projectile-remove-known-project "Remove project(s)")))


    (define-key ivy-minibuffer-map "\C-o"
      (defhydra soo-ivy (:hint nil :color pink)
        "
 Move     ^^^^^^^^^^ | Call         ^^^^ | Cancel^^ | Options^^ | Action _w_/_s_/_a_: %s(ivy-action-name)
----------^^^^^^^^^^-+--------------^^^^-+-------^^-+--------^^-+---------------------------------
 _g_ ^ ^ _k_ ^ ^ _u_ | _f_orward _o_ccur | _i_nsert | _c_alling: %-7s(if ivy-calling \"on\" \"off\") _C_ase-fold: %-10`ivy-case-fold-search
 ^↨^ _h_ ^+^ _l_ ^↕^ | _RET_ done     ^^ | _q_uit   | _m_atcher: %-7s(ivy--matcher-desc) _t_runcate: %-11`truncate-lines
 _G_ ^ ^ _j_ ^ ^ _d_ | _TAB_ alt-done ^^ | ^ ^      | _<_/_>_: shrink/grow
"
        ;; arrows
        ("j" ivy-next-line)
        ("k" ivy-previous-line)
        ("l" ivy-alt-done)
        ("h" ivy-backward-delete-char)
        ("g" ivy-beginning-of-buffer)
        ("G" ivy-end-of-buffer)
        ("d" ivy-scroll-up-command)
        ("u" ivy-scroll-down-command)
        ("e" ivy-scroll-down-command)
        ;; actions
        ("q" keyboard-escape-quit :exit t)
        ("C-g" keyboard-escape-quit :exit t)
        ("<escape>" keyboard-escape-quit :exit t)
        ("C-o" nil)
        ("i" nil)
        ("TAB" ivy-alt-done :exit nil)
        ("C-j" ivy-alt-done :exit nil)
        ;; ("d" ivy-done :exit t)
        ("RET" ivy-done :exit t)
        ("C-m" ivy-done :exit t)
        ("f" ivy-call)
        ("c" ivy-toggle-calling)
        ("m" ivy-toggle-fuzzy)
        (">" ivy-minibuffer-grow)
        ("<" ivy-minibuffer-shrink)
        ("w" ivy-prev-action)
        ("s" ivy-next-action)
        ("a" ivy-read-action)
        ("t" (setq truncate-lines (not truncate-lines)))
        ("C" ivy-toggle-case-fold)
        ("o" ivy-occur :exit t)))

    ;; Use faster search tools: ripgrep or the silver search
    (let ((command
           (cond
            ((executable-find "rg")
             "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
            ((executable-find "ag")
             "ag -i --noheading --nocolor --nofilename --numbers '%s' %s"))))
      (setq counsel-grep-base-command command)))
  :init
  (progn
    (ivy-mode 1)))

(use-package counsel
  :ensure t
  :config
  (progn
    (setq counsel-yank-pop-separator "\n-------\n")
    (ivy-add-actions
     'counsel-find-file
     '(("c" (lambda (x) (kill-new (f-relative x))) "Copy relative path")
       ("4" (lambda (x) (find-file-other-window x)) "Open in new window")
       ("5" (lambda (x) (find-file-other-frame x)) "Open in new frame")
       ("C" (lambda (x) (kill-new x)) "Copy absolute path")
       ("d" (lambda (x) (dired x)) "Open in dired")
       ("D" (lambda (x) (delete-file x)) "Delete file")
       ("e" (lambda (x) (shell-command (format "open %s" x)))
        "Open in external program")
       ("f" (lambda (x)
              "Open X in another frame."
              (find-file-other-frame x))
        "Open in new frame")
       ("p" (lambda (path)
              (with-ivy-window
                (insert (f-relative path))))
        "Insert relative path")
       ("P" (lambda (path)
              (with-ivy-window
                (insert path)))
        "Insert absolute path")
       ("l" (lambda (path)
              "Insert org-link with relative path"
              (with-ivy-window
                (insert (format "[[file:%s]]" (f-relative path)))))
        "Insert org-link (rel. path)")
       ("L" (lambda (path)
              "Insert org-link with absolute path"
              (with-ivy-window
                (insert (format "[[file:%s]]" path))))
        "Insert org-link (abs. path)")
       ("r" (lambda (path)
              (rename-file path (read-string "New name: ")))
        "Rename")))

    (defun counsel-ag-projectile ()
      "Counsel version of projectile-ag."
      (interactive)
      (counsel-ag "" (projectile-project-root)))

    (defun counsel-ag-project-symbol ()
      (interactive)
      (counsel-ag (if (region-active-p)
                      (buffer-substring-no-properties (region-beginning) (region-end))
                    (thing-at-point 'symbol))
                  (projectile-project-root)))))

(use-package counsel-projectile
  :after projectile
  :ensure t)

(use-package ivy-hydra
  :ensure t)

(use-package counsel-osx-app
  :ensure t)

(use-package tiny
  :commands tiny-expand)

(use-package ivy-rich
  :ensure t
  :disabled t
  :after swiper
  :commands ivy-switch-buffer
  :config (progn
            (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer)
            ;; Do not align the virtual buffers, breaks ivy-rich
            (setq ivy-rich-switch-buffer-align-virtual-buffer nil)))

(use-package flyspell-correct-ivy
  :ensure t
  :after flyspell
  :config
  (progn
    (bind-key [remap ispell-word] 'flyspell-correct-word-generic)))

(provide 'init-ivy)
