(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
   Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
  This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))

(defun save-macro (name)
  "Save a macro. Take a NAME as argument and save the last defined macro under this name at the end of your .emacs."
  (interactive "SName of the macro :")  ; ask for the name of the macro
  (kmacro-name-last-macro name)         ; use this name for the macro
  (find-file user-init-file)            ; open ~/.emacs or other user init file
  (goto-char (point-max))               ; go to the end of the .emacs
  (newline)                             ; insert a newline
  (insert-kbd-macro name)               ; copy the macro
  (newline)                             ; insert a newline
  (switch-to-buffer nil))               ; return to the initial buffer

(defun user--save-macro (name)
  "Save a macro. Take a NAME as an argument and save the last defined macro under this name."
  (interactive "SName of the macro :")
  (kmacro-name-last-macro name)
  (find-file "~/.emacs.d/macros.el")
  (goto-char (point-max))
  (newline)
  (insert-kbd-macro name)
  (newline)
  (save-buffer)
  (switch-to-buffer nil))

(defun reindent-whole-buffer ()
  "Reindent the whole buffer."
  (interactive)
  (indent-region (point-min)
                 (point-max)))

;;; To facilitate easier keyboard macro usage (from http://www.emacswiki.org/emacs/KeyboardMacros)
(defun toggle-kbd-macro-recording-on ()
  "One-key keyboard macros: turn recording on."
  (interactive)
  (define-key global-map (this-command-keys)
    'toggle-kbd-macro-recording-off)
  (start-kbd-macro nil))

(defun toggle-kbd-macro-recording-off ()
  "One-key keyboard macros: turn recording off."
  (interactive)
  (define-key global-map (this-command-keys)
    'toggle-kbd-macro-recording-on)
  (end-kbd-macro))

(defun toggle-full-window()
  "Toggle the full view of selected window"
  (interactive)
  ;; @see http://www.gnu.org/software/emacs/manual/html_node/elisp/Splitting-Windows.html
  (if (window-parent)
      (delete-other-windows)
    (winner-undo)
    ))

;;; Open Lines
(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))


(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input."
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

;;; Join Lines
(defun join-lines ()
  "If at the end of the line, will join the following line to the
end of this one...unless it is blank, in which case, it will keep
joining lines until the next line with text is connected."
  (interactive)
  ;; Move to the the beginning of the white space before attempting
  ;; this process. This allows us to join lines even if we are in the
  ;; middle of some empty lines.
  (re-search-backward "[^[:space:]\\r\\n]")
  (forward-char)
  ;; Just in case we have some trailing whitespace we can't see, let's
  ;; just get rid of it. Won't do anything if in the middle of a line,
  ;; or if there is not trailing whitespace.
  (delete-trailing-whitespace (point) (point-at-eol))
  ;; While we are at the end of the line, join a line, remove the
  ;; whitespace, and keep on going until we're through...
  (while (eq (point-at-eol) (point))
    (delete-char 1)
    (delete-trailing-whitespace (point) (point-at-eol))))

;;; copy-line with variable arugments
(defun copy-line (&optional arg)
  "Copy lines (as many as prefix argument) in the kill ring."
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line(s) copied" arg ))

(defun backward-kill-line (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg)))

(defun kill-to-beginning-of-line ()
  (interactive)
  (kill-region (save-excursion (beginning-of-line) (point))
               (point)))

(defun nxml-pretty-print-buffer ()
  "pretty print the XML in a buffer."
  (interactive)
  (pretty-print-xml-region (point-min) (point-max)))

;;; XML pretty print
(defun pretty-print-xml-region (begin end)
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n"))
    (indent-region begin end))
  (message "Ah, much better!"))

(defun nxml-kill-tag-contents ()
  "Copy the contents between two tags"
;  (interactive "*p\ncCopy tag contents: ") ; this expects arguments input
  (interactive)
  (nxml-backward-up-element)
  (kill-region
    (progn (search-forward ">")
      (point))
    (progn (nxml-backward-up-element)
      (nxml-forward-element)
      (search-backward "</")
      (point))))

(defun nxml-copy-tag-contents ()
  "Copy the contents between two tags"
                                        ;  (interactive "*p\ncCopy tag contents: ") ; this expects arguments input
  (interactive)
  (nxml-backward-up-element)
  (copy-region-as-kill
   (progn (search-forward ">") (point))
   (progn (nxml-backward-up-element)
     (nxml-forward-element)
     (search-backward "</")
     (point))))

(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))

(defun save-buffer-always ()
  "Save the buffer even if it is not modified.
Useful if something is watching file modification times."
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer))

(defun insert-date-string ()
  "Insert a nicely formated date string."
  (interactive)
  (insert (format-time-string "%a %b %d %H:%M:%S %Y")))

(defun my/mkdir ()
  "Create directory."
  (interactive)
  (if (equal major-mode 'dired-mode)
      (call-interactively 'dired-create-directory)
    (if (equal major-mode 'neotree-mode)
        (call-interactively 'neotree-create-node)
      (call-interactively 'make-directory))))

(defun delete-this-file ()
  "Delete the current file, and kill the buffer. The `delete-file'
function does not kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory
                              buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
      (progn
        (rename-file name new-name 1)
        (rename-buffer new-name)
        (set-visited-file-name new-name)
        (set-buffer-modified-p nil)))))

(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (browse-url (concat "file://" (buffer-file-name))))

(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(use-package mwim
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line))
  )

(defun dos2unix ()
  "Not exactly but it's easier to remember"
  (interactive)
  (set-buffer-file-coding-system 'unix 't))

(defun open-terminal-at (directory)
  "Open a terminal at the `directory' passed in using a name derived
from the directory.  If a terminal by that name already exists, just
switch to it instead of creating a new one.
If the directory does not exist, an error message is displayed."
  (interactive)
  (if (not (file-exists-p directory))
      (message (concat
                "Terminal not created, Directory does not exist: "
                directory))
    (let* ((name (concat "term-" directory))
           (term-name (concat "*" name "*"))
           (default-directory directory))
      (if (get-buffer term-name)
          (switch-to-buffer term-name)
        (progn
          (ansi-term "/bin/bash" name) ;; wraps name with *'s
          (process-send-string
           (get-process term-name)
           (concat "cd " directory "\nclear\n")))))))

(defun open-eshell-at (directory)
  "Open an eshell at the `directory' passed in using a name derived
from the directory.  If an eashll by that name already exists, just
switch to it instead of creating a new one.
If the directory does not exit, an error message is displayed."
  (interactive)
  (if (not (file-exists-p directory))
      (message (concat
                "Eshell not created, Directory does not exist: "
                directory))
    (let* ((name (concat "*eshell-" directory "*"))
           (default-directory directory))
      (if (not (get-buffer name))
          (progn
            (setq eshell-buffer-name name)
            (eshell)
            (setq eshell-buffer-name "*eshell*")))
      (switch-to-buffer name))))

(defun current-location ()
    "Function that works in buffers, dired, term and eshell to return
current location."
    (interactive)
    (or (if (buffer-file-name) (file-name-directory (buffer-file-name)))
        (if (fboundp 'eshell/pwd) (eshell/pwd))
        (if (fboundp 'default-directory) (default-directory))
        (getenv "HOME")))

(defun eshell-here ()
  "Open an eshell at the `current-location'"
  (interactive)
  (open-eshell-at (current-location)))
(defalias 'ee 'eshell-here)

(defun terminal-here ()
  "Open a terminal at the `current-location'"
  (interactive)
  (open-terminal-at (current-location)))

(defalias 'tt 'terminal-here)

(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

;;; Faster pop-to-mark command
(defun modi/multi-pop-to-mark (orig-fun &rest args)
  "Call ORIG-FUN until the cursor moves.
Try the repeated popping up to 10 times."
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point))
        (apply orig-fun args)))))

(advice-add 'pop-to-mark-command :around
            #'modi/multi-pop-to-mark)

(defun xah-html-decode-percent-encoded-url ()
  "Decode percent encoded URI of URI under cursor or selection.

Example:
    http://en.wikipedia.org/wiki/Saint_Jerome_in_His_Study_%28D%C3%BCrer%29
becomes
    http://en.wikipedia.org/wiki/Saint_Jerome_in_His_Study_(Dürer)

Example:
    http://zh.wikipedia.org/wiki/%E6%96%87%E6%9C%AC%E7%BC%96%E8%BE%91%E5%99%A8
becomes
    http://zh.wikipedia.org/wiki/文本编辑器

For string version, see `xah-html-url-percent-decode-string'.
To encode, see `xah-html-encode-percent-encoded-url'.
URL `http://ergoemacs.org/emacs/elisp_decode_uri_percent_encoding.html'
Version 2015-09-14."
  (interactive)
  (let (ξboundaries ξp1 ξp2 ξinput-str)
    (if (use-region-p)
        (progn
          (setq ξp1 (region-beginning))
          (setq ξp2 (region-end)))
      (progn
        (setq ξboundaries (bounds-of-thing-at-point 'url))
        (setq ξp1 (car ξboundaries))
        (setq ξp2 (cdr ξboundaries))))
    (setq ξinput-str (buffer-substring-no-properties ξp1 ξp2))
    (require 'url-util)
    (delete-region ξp1 ξp2)
    (insert (decode-coding-string (url-unhex-string ξinput-str) 'utf-8))))

(defun xah-html-encode-percent-encoded-url ()
  "Percent encode URL under cursor or selection.

Example:
    http://en.wikipedia.org/wiki/Saint_Jerome_in_His_Study_(Dürer)
becomes
    http://en.wikipedia.org/wiki/Saint_Jerome_in_His_Study_(D%C3%BCrer)

Example:
    http://zh.wikipedia.org/wiki/文本编辑器
becomes
    http://zh.wikipedia.org/wiki/%E6%96%87%E6%9C%AC%E7%BC%96%E8%BE%91%E5%99%A8

URL `http://ergoemacs.org/emacs/elisp_decode_uri_percent_encoding.html'
Version 2015-09-14."
  (interactive)
  (let (ξboundaries ξp1 ξp2 ξinput-str)
    (if (use-region-p)
        (progn
          (setq ξp1 (region-beginning))
          (setq ξp2 (region-end)))
      (progn
        (setq ξboundaries (bounds-of-thing-at-point 'url))
        (setq ξp1 (car ξboundaries))
        (setq ξp2 (cdr ξboundaries))))
    (setq ξinput-str (buffer-substring-no-properties ξp1 ξp2))
    (require 'url-util)
    (delete-region ξp1 ξp2)
    (insert (url-encode-url ξinput-str))))

(defun beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
                             "python -mjson.tool" (current-buffer) t)))

(defun jq-format (beg end)
  (interactive "r")
  (shell-command-on-region beg end "jq ." nil t))

(defun add-full-stop ()
  "Terminate each line with a full stop."
  (interactive "*")
  (while (re-search-forward "$")
    (insert ".")
    (forward-char )))

(defun go-to-column (column)
  (interactive "nColumn: ")
  (move-to-column column t))

(defun paste-from-x-clipboard ()
  (interactive)
  (shell-command
   (cond
    (*cygwin* "getclip")
    (*is-a-mac* "pbpaste")
    (t "xsel -ob"))
   1))

(defun my/paste-in-minibuffer ()
  (local-set-key (kbd "M-y") 'paste-from-x-clipboard)
  )

(add-hook 'minibuffer-setup-hook 'my/paste-in-minibuffer)

(defun number-rectangle (start end format-string from)
  "Delete (don't save) text in the region-rectangle, then number it."
  (interactive
   (list (region-beginning) (region-end)
         (read-string "Number rectangle: "
                      (if (looking-back "^ *") "%d. " "%d"))
         (read-number "From: " 1)))
  (save-excursion
    (goto-char start)
    (setq start (point-marker))
    (goto-char end)
    (setq end (point-marker))
    (delete-rectangle start end)
    (goto-char start)
    (loop with column = (current-column)
          while (and (<= (point) end) (not (eobp)))
          for i from from   do
          (move-to-column column t)
          (insert (format format-string i))
          (forward-line 1)))
  (goto-char start))

(defun get-buffer-mode (&optional buffer-or-name)
  "Returns the major mode associated with a buffer.
If buffer-or-name is nil return current buffer's mode."
  (buffer-local-value 'major-mode
                      (if buffer-or-name (get-buffer buffer-or-name) (current-buffer))))


(defun endless/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'endless/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(global-set-key [remap fill-paragraph]
                #'endless/fill-or-unfill)

(defun symbol-boundary (backward)
  "Returns position of either left or right boundary of current symbol."
  (save-excursion
    (let ((skip (if backward 'skip-syntax-backward 'skip-syntax-forward))
 	  (syntax "w_"))
      (funcall skip syntax)
      (point))))

(defun kill-symbol ()
  "Kill current symbol."
  (interactive)
  (kill-region (symbol-boundary t) (symbol-boundary nil)))

(defun delete-symbol ()
  "Delete current symbol."
  (interactive)
  (delete-region (symbol-boundary t) (symbol-boundary nil)))

(defun my/forward-to-sentence-end ()
  "Move point to just before the end of the current sentence."
  (forward-sentence)
  (backward-char)
  (unless (looking-back "[[:alnum:]]")
    (backward-char)))

(defun my/beginning-of-sentence-p ()
  "Return  t if point is at the beginning of a sentence."
  (let ((start (point))
        (beg (save-excursion (forward-sentence) (forward-sentence -1))))
    (eq start beg)))

(defun my/kill-sentence-dwim ()
  "Kill the current sentence up to and possibly including the punctuation.
When point is at the beginning of a sentence, kill the entire
sentence. Otherwise kill forward but preserve any punctuation at
the sentence end."
  (interactive)
  (expand-abbrev)
  (if (my/beginning-of-sentence-p)
      (progn
        (kill-sentence)
        (just-one-space)
        (when (looking-back "^[[:space:]]+") (delete-horizontal-space)))
    (kill-region (point) (progn (my/forward-to-sentence-end) (point)))
    (just-one-space 0)))


(require 'eshell)

;;;###autoload
(defun eshell-this-dir ()
  "Open or move eshell in `default-directory'."
  (interactive)
  (unless (get-buffer eshell-buffer-name)
    (eshell))
  (switch-to-buffer eshell-buffer-name)
  (goto-char (point-max))
  (eshell-kill-input)
  (insert (format "cd %s" default-directory))
  (eshell-send-input)
  (goto-char (point-max)))

;; ;;* Customize
(defmacro csetq (variable value)
  `(funcall (or (get ',variable 'custom-set) 'set-default) ',variable ,value))

;;  http://zck.me/emacs-repeat-emacs-repeat
(defun insert-here ()
  (interactive)
  (insert "here")
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "h") #'insert-here)
     (define-key map (kbd "k") #'insert-here)
     map)))

(defun my-pop-to-mark-command ()
  (interactive)
  (pop-to-mark-command)
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "k") #'my-pop-to-mark-command)
     map)))

(require 'ansi-color)
(defun endless/colorize-compilation ()
  "Colorize from `compilation-filter-start' to `point'."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region
     compilation-filter-start (point))))

(add-hook 'compilation-filter-hook
          #'endless/colorize-compilation)

(defun tidy ()
  "Ident, untabify and unwhitespacify current buffer, or region if active."
  (interactive)
  (let ((beg (if (region-active-p) (region-beginning) (point-min)))
        (end (if (region-active-p) (region-end) (point-max))))
    (indent-region beg end)
    (whitespace-cleanup)
    (untabify beg (if (< end (point-max)) end (point-max)))))

(defun duplicate-thing (comment)
  "Duplicates the current line, or the region if active. If an argument is
given, the duplicated region will be commented out."
  (interactive "P")
  (save-excursion
    (let ((start (if (region-active-p) (region-beginning) (point-at-bol)))
          (end   (if (region-active-p) (region-end) (point-at-eol))))
      (goto-char end)
      (unless (region-active-p)
        (newline))
      (insert (buffer-substring start end))
      (when comment (comment-region start end)))))

(defun my/package-upgrade-packages (&optional no-fetch)
  "Upgrade all packages.  No questions asked.
This function is equivalent to `list-packages', followed by a
`package-menu-mark-upgrades' and a `package-menu-execute'.  Except
the user isn't asked to confirm deletion of packages.
The NO-FETCH prefix argument is passed to `list-packages'.  It
prevents re-download of information about new versions.  It does
not prevent downloading the actual packages (obviously)."
  (interactive "P")
  (let ((package-menu-async nil)) ; This variable was introduced in emacs 25.0
    (save-window-excursion
      (package-list-packages no-fetch)
      (package-menu-mark-upgrades)
      (package-menu-execute 'noquery))))

(defun iterm-goto-filedir-or-home ()
  "Go to present working dir and focus iterm"
  (interactive)
  (do-applescript
   (concat
    " tell application \"iTerm2\"\n"
    "   tell the current session of current window\n"
    (format "     write text \"cd %s\" \n"
            ;; string escaping madness for applescript
            (replace-regexp-in-string "\\\\" "\\\\\\\\"
                                      (shell-quote-argument (or default-directory "~"))))
    "   end tell\n"
    " end tell\n"
    " do shell script \"open -a iTerm\"\n")))

(defun iterm-focus ()
  (interactive)
  (do-applescript
   " do shell script \"open -a iTerm\"\n"
   ))

(defun toggle-shell ()
  "Jumps to eshell or back."
  (interactive)
  (if (string= (buffer-name) "*shell*")
      (switch-to-prev-buffer)
    (shell)))

(defun clear-comint ()
  "Runs `comint-truncate-buffer' with the
`comint-buffer-maximum-size' set to zero."
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(require 'f)

;;;; files related to buffers
(defun path-for-current-buffer ()
  "Find a directory path associated with the current buffer, if
possible. No trailing slash. Returns nil otherwise."
  (let ((file-name (buffer-file-name)))
    (if file-name (directory-file-name (file-name-directory file-name))
      default-directory)))

;;;; path searching
(defun find-path-parent-directory (path file-name)
  "Search PATH and all parent directories for file FILE-NAME,
returning the path where FILE-NAME can be found."
  (let ((directory-path (locate-dominating-file path file-name)))
    (when directory-path
      (f-join directory-path file-name))))


(add-hook 'comint-mode-hook (lambda () (local-set-key (kbd "C-l") 'clear-comint)))

;;;###autoload
(defun ora-region-str-or-symbol ()
  "Return the contents of region or current symbol."
  (if (region-active-p)
      (buffer-substring-no-properties
       (region-beginning)
       (region-end))
    (let ((sym (thing-at-point 'symbol)))
      (when (stringp sym)
        (regexp-quote sym)))))

;;;###autoload
(defun ora-occur ()
  "Call `occur' with a sane default."
  (interactive)
  (push (ora-region-str-or-symbol) regexp-history)
  (call-interactively 'occur))

(defun ensure-packages-installed (packages)
  (unless package-archive-contents
    (package-refresh-contents))
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         package
       (progn (message (format "Installing package %s." package))
              (package-install package))))
   packages))

(defun ora-move-key (key-from key-to keymap)
  "Move the command bound to KEY-FROM to KEY-TO in KEYMAP."
  (if (null key-to)
      (define-key keymap (kbd key-from) nil)
    (let* ((key-from (kbd key-from))
           (key-to (kbd key-to))
           (cmd (lookup-key keymap key-from)))
      (when cmd
        (define-key keymap key-to cmd)
        (define-key keymap key-from nil)))))

(defmacro shut-up! (&rest body)
  "Silence message output from code."
  (declare (indent defun))
  `(let (message-log-max) ,@body (message "")))

(defun diff-current-buffer-with-file ()
  (interactive)
  "Diff the current buffer with the content saved in the file."
  (diff-buffer-with-file (current-buffer)))

(defun get-buffers-matching-mode (mode)
  "Returns a list of buffers where their major-mode is equal to MODE"
  (let ((buffer-mode-matches '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (if (eq mode major-mode)
            (add-to-list 'buffer-mode-matches buf))))
    buffer-mode-matches))

(defun my-make-temp-file (name)
  "Creates a temporary file in the system temp directory, for various purposes."
  (interactive "sFile name:")
  (generate-new-buffer name)
  (switch-to-buffer name)
  (write-file (concat temporary-file-directory name)))

;;;###autoload
(defun mark-buffer-after-point (reversep)
  "Select the part of the buffer after point.
With a prefix argument, select the part before point."
  (interactive "P")
  (push-mark (if reversep (point-min) (point-max)) nil t)
  (setq deactivate-mark  nil))

;;;###autoload
(defun mark-buffer-before-point (reversep)
  "Select the part of the buffer before point.
With a prefix argument, select the part after point."
  (interactive "P")
  (mark-buffer-after-point t))

(provide 'init-defuns)
