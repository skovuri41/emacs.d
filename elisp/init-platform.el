;; Platform specific settings
(defvar *is-a-mac*)
(defvar *is-carbon-emacs*)
(defvar *is-cocoa-emacs*)
(defvar *is-gnu-linux*)
(setq
 *is-a-mac* (eq system-type 'darwin)
 *is-carbon-emacs* (and *is-a-mac* (eq window-system 'mac))
 *is-cocoa-emacs* (and *is-a-mac* (eq window-system 'ns))
 *cygwin* (eq system-type 'cygwin)
 *is-gnu-linux* (eq system-type 'gnu/linux))

(when *is-gnu-linux*
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
  (setq exec-path (append exec-path '("/usr/local/bin")))
  (setq default-directory (getenv "HOME"))
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives
               '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))
  (add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
  (add-to-list 'package-pinned-packages '(ac-cider . "melpa-stable") t)
  (add-to-list 'package-pinned-packages '(clojure-mode . "melpa-stable") t)
  (add-to-list 'package-pinned-packages '(clj-refactor . "melpa-stable") t)
  (add-to-list 'package-pinned-packages '(cider-eval-sexp-fu. "melpa-stable") t)

  ;; keep the installed packages in .emacs.d
  (setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
  ;; Install all packages required
  (load-file (expand-file-name "elisp/init-elpa-list.el" user-emacs-directory))
  (package-initialize)
  ;; Fetch packages the first time
  (unless (file-exists-p package-user-dir)
    (package-refresh-contents))
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(when *is-a-mac*
  (setq
   ;; for multilingual environments
   default-input-method "MacOSX"
   ;; font
   default-frame-alist '((font . "Monaco-13")
                         (width . 120)  ;character
                         (height . 52)) ; lines
   ;; Work around a bug on OS X where system-name is FQDN
   system-name (car (split-string system-name "\\."))
   ;; make emacs open in existing frames
   ;;ns-pop-up-frames nil
   interprogram-cut-function 'paste-to-osx
   interprogram-paste-function 'copy-from-osx
   mac-command-modifier nil))

(when *is-a-mac*
  (require 'init-cask)
  )

(defun setup-firacode ()
  (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
                 (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
                 (36 . ".\\(?:>\\)")
                 (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
                 (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
                 (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
                 (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
                 (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
                 (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
                 (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
                 (48 . ".\\(?:x[a-zA-Z]\\)")
                 (58 . ".\\(?:::\\|[:=]\\)")
                 (59 . ".\\(?:;;\\|;\\)")
                 (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
                 (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
                 (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
                 (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
                 (91 . ".\\(?:]\\)")
                 (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
                 (94 . ".\\(?:=\\)")
                 (119 . ".\\(?:ww\\)")
                 (123 . ".\\(?:-\\)")
                 (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
                 (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
                 )
               ))
    (dolist (char-regexp alist)
      (set-char-table-range composition-function-table (car char-regexp)
                            `([,(cdr char-regexp) 0 font-shape-gstring]))))
  )

(when *is-gnu-linux*
  (setq
   ;; font
   ;; default-frame-alist '((font . "Monaco-10"))
   ;; default-frame-alist '((font . "Fira Code-11"))
   default-frame-alist '((font . "Fira Code-11")
                         (width . 120)  ;character
                         (height . 50)) ; lines
   ;; make emacs use the clipboard
   x-select-enable-clipboard t)
  (setup-firacode)
  )


(provide 'init-platform)
