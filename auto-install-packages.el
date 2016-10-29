(require 'cl)
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(ac-cider . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(clojure-mode . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(clj-refactor . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(cider-eval-sexp-fu. "melpa-stable") t)
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(defvar dotfiles-packages-list nil)
(setq dotfiles-packages-list
      '(company
        company-quickhelp
        direx
        drag-stuff
        expand-region
        evil-leader
        evil-nerd-commenter
        evil-matchit
        evil-visualstar
        flx-ido
        flyspell
        flycheck
        fancy-narrow
        git-blame
        gitconfig-mode
        gitignore-mode
        grizzl
        guide-key
        helm
        helm-descbinds
        helm-gtags
        helm-projectile
        helm-swoop
        helm-ag
        helm-backup
        ido-vertical-mode
        json-mode
        magit
        multiple-cursors
        org
        org-bullets
        org-pomodoro
        projectile
        smart-mode-line
        smartparens
        smex
        undo-tree
        use-package
        which-key
        yasnippet
        zenburn-theme
        ace-jump-buffer
        ace-window
        projectile
        easy-kill
        easy-kill-extras
        zop-to-char
        web-mode
        csv-mode
        csv-nav
        rainbow-delimiters
        imenu-anywhere
        aggressive-indent
        iedit
        tidy
        drag-stuff
        elisp-slime-nav
        better-defaults
        color-identifiers-mode
        whitespace-cleanup-mode
        powerline
        anzu
        thingatpt
        keyfreq
        ag
        swiper
        counsel
        super-save
        smooth-scrolling
        link-hint
        highlight-parentheses
        neotree
        quickrun
        kurecolor
        java-imports
        origami
        rainbow-mode
        golden-ratio
        golden-ratio-scroll-screen
        popwin
        git-timemachine
        git-gutter-fringe+
        git-gutter+
        hydra
        spaceline
        spacemacs-theme
        restclient
        restclient-helm
        htmlize
        window-numbering
        highlight-numbers
        hungry-delete
        pbcopy
        ctags-update
        workgroups2
        smartparens
        clojure-mode
        clojure-snippets
        cider
        clojure-cheatsheet
        cider-eval-sexp-fu
        4clojure
        paren-face
        clj-refactor
        typed-clojure-mode
        transpose-frame
        auto-compile
        sqlup-mode
        sql-indent
        peep-dired
        ranger
        xah-fly-keys
        embrace
        key-chord
        mwim
        worf
        tiny
        outshine
        outorg
        gitconfig-mode
        gitignore-mode
        gitattributes-mode
        flyspell-correct-ivy
        dired-launch
        cljsbuild-mode
        lispy
        recentf-ext
        diff-hl
        fullframe
        engine-mode
        dired-subtree))

(defun dotfiles-auto-install-packages ()
  (package-refresh-contents)
  (mapc #'(lambda (package)
            (unless (package-installed-p package)
              (package-install package)))
        dotfiles-packages-list)
  (save-buffers-kill-emacs))

(dotfiles-auto-install-packages)
