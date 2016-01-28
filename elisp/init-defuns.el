;; masteringemacs.org fixing mark commands tmm
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
   "save a macro. Take a name as argument
    and save the last defined macro under 
    this name at the end of your .emacs"
    (interactive "SName of the macro :")  ; ask for the name of the macro    
    (kmacro-name-last-macro name)         ; use this name for the macro    
    (find-file user-init-file)            ; open ~/.emacs or other user init file 
    (goto-char (point-max))               ; go to the end of the .emacs
    (newline)                             ; insert a newline
    (insert-kbd-macro name)               ; copy the macro 
    (newline)                             ; insert a newline
    (switch-to-buffer nil))               ; return to the initial buffer

(defun reindent-whole-buffer ()
  "Reindent the whole buffer."
  (interactive)
  (indent-region (point-min)
                 (point-max)))

;; To facilitate easier keyboard macro usage (from http://www.emacswiki.org/emacs/KeyboardMacros)
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


;;; Open Lines

(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

;;;;Join Lines
(defun join-lines ()
   "If at the end of the line, will join the following line to the
   end of this one...unless it is blank, in which case, it will
   keep joining lines until the next line with text is
   connected."
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

;;;; copy-line with variable arugments
(defun copy-line (&optional arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line(s) copied" arg ))


(defun nxml-pretty-print-buffer ()
  "pretty print the XML in a buffer."
  (interactive)
  (nxml-pretty-print-region (point-min) (point-max)))

;; XML pretty print
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


(defun xah-cut-line-or-region ()
  "Cut current line, or text selection.
When `universal-argument' is called first, cut whole buffer (respects `narrow-to-region').

URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'
Version 2015-06-10"
  (interactive)
  (if current-prefix-arg
      (progn ; not using kill-region because we don't want to include previous kill
        (kill-new (buffer-string))
        (delete-region (point-min) (point-max)))
    (progn (if (use-region-p)
               (kill-region (region-beginning) (region-end) t)
             (kill-region (line-beginning-position) (line-beginning-position 2))))))

(defun xah-copy-line-or-region ()
  "Copy current line, or text selection.
When called repeatedly, append copy subsequent lines.
When `universal-argument' is called first, copy whole buffer (respects `narrow-to-region').

URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'
Version 2015-09-18"
  (interactive)
  (let (ξp1 ξp2)
    (if current-prefix-arg
        (progn (setq ξp1 (point-min))
               (setq ξp2 (point-max)))
      (progn
        (if (use-region-p)
            (progn (setq ξp1 (region-beginning))
                   (setq ξp2 (region-end)))
          (progn (setq ξp1 (line-beginning-position))
                 (setq ξp2 (line-end-position))))))
    (if (eq last-command this-command)
        (progn
          (kill-append "\n" nil)
          (forward-line 1)
          (end-of-line)
          (kill-append (buffer-substring-no-properties (line-beginning-position) (line-end-position)) nil)
          (message "Line copy appended"))
      (progn
        (kill-ring-save ξp1 ξp2)
        (if current-prefix-arg
            (message "Buffer text copied")
          (message "Text copied"))))))

(defun xah-copy-to-register-1 ()
  "Copy current line or text selection to register 1.
See also: `xah-paste-from-register-1', `copy-to-register'."
  (interactive)
  (let (p1 p2)
    (if (region-active-p)
        (progn (setq p1 (region-beginning))
               (setq p2 (region-end)))
      (progn (setq p1 (line-beginning-position))
             (setq p2 (line-end-position))))
    (copy-to-register ?1 p1 p2)
    (message "copied to register 1: %s." (buffer-substring-no-properties p1 p2))))

(defun xah-paste-from-register-1 ()
  "Paste text from register 1.
See also: `xah-copy-to-register-1', `insert-register'."
  (interactive)
  (when (use-region-p)
    (delete-region (region-beginning) (region-end) )
    )
  (insert-register ?1 t))


(defvar xah-switch-buffer-ignore-dired t "If t, ignore dired buffer when calling `xah-next-user-buffer' or `xah-previous-user-buffer'")
(setq xah-switch-buffer-ignore-dired t)

(defun xah-next-user-buffer ()
  "Switch to the next user buffer.
 “user buffer” is a buffer whose name does not start with “*”.
If `xah-switch-buffer-ignore-dired' is true, also skip directory buffer.
2015-01-05 URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'"
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (or
           (string-equal "*" (substring (buffer-name) 0 1))
           (if (string-equal major-mode "dired-mode")
               xah-switch-buffer-ignore-dired
             nil
             ))
          (progn (next-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun xah-previous-user-buffer ()
  "Switch to the previous user buffer.
 “user buffer” is a buffer whose name does not start with “*”.
If `xah-switch-buffer-ignore-dired' is true, also skip directory buffer.
2015-01-05 URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'"
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (or
           (string-equal "*" (substring (buffer-name) 0 1))
           (if (string-equal major-mode "dired-mode")
               xah-switch-buffer-ignore-dired
             nil
             ))
          (progn (previous-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun xah-next-emacs-buffer ()
  "Switch to the next emacs buffer.
 (buffer name that starts with “*”)"
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (not (string-equal "*" (substring (buffer-name) 0 1))) (< i 20))
      (setq i (1+ i)) (next-buffer))))

(defun xah-previous-emacs-buffer ()
  "Switch to the previous emacs buffer.
 (buffer name that starts with “*”)"
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (not (string-equal "*" (substring (buffer-name) 0 1))) (< i 20))
      (setq i (1+ i)) (previous-buffer))))


(defvar xah-left-brackets nil "List of open bracket chars.")
(setq xah-left-brackets '("(" "{" "[" "<" "〔" "【" "〖" "〈" "《" "「" "『" "“" "‘" "‹" "«" "（"))

(defvar xah-right-brackets nil "list of close bracket chars.")
(setq xah-right-brackets '(")" "]" "}" ">" "〕" "】" "〗" "〉" "》" "」" "』" "”" "’" "›" "»" "）"))

(defun xah-backward-left-bracket ()
  "Move cursor to the previous occurrence of left bracket.
The list of brackets to jump to is defined by `xah-left-brackets'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-03-24"
  (interactive)
  (search-backward-regexp (eval-when-compile (regexp-opt xah-left-brackets)) nil t))

(defun xah-forward-right-bracket ()
  "Move cursor to the next occurrence of right bracket.
The list of brackets to jump to is defined by `xah-right-brackets'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-03-24"
  (interactive)
  (search-forward-regexp (eval-when-compile (regexp-opt xah-right-brackets)) nil t))

(defun xah-forward-block (&optional φn)
  "Move cursor forward to the beginning of next text block.
A text block is separated by blank lines.
In most major modes, this is similar to `forward-paragraph', but this command's behavior is the same regardless of syntax table."
  (interactive "p")
  (let ((φn (if (null φn) 1 φn)))
    (search-forward-regexp "\n[\t\n ]*\n+" nil "NOERROR" φn)))

(defun xah-backward-block (&optional φn)
  "Move cursor backward to previous text block.
See: `xah-forward-block'"
  (interactive "p")
  (let ((φn (if (null φn) 1 φn))
        (ξi 1))
    (while (<= ξi φn)
      (if (search-backward-regexp "\n[\t\n ]*\n+" nil "NOERROR")
          (progn (skip-chars-backward "\n\t "))
        (progn (goto-char (point-min))
               (setq ξi φn)))
      (setq ξi (1+ ξi)))))


(defun xah-make-backup ()
  "Make a backup copy of current file or dired marked files.
If in dired, backup current file or marked files.
The backup file name is
 ‹name›~‹timestamp›~
example:
 file.html~20150721T014457~
in the same dir. If such a file already exist, it's overwritten.
If the current buffer is not associated with a file, nothing's done.
URL `http://ergoemacs.org/emacs/elisp_make-backup.html'
Version 2015-08-17"
  (interactive)
  (let (
        (ξfile-list
         (if (string-equal major-mode "dired-mode")
             (dired-get-marked-files)
           (list (buffer-file-name)))))
    (mapc (lambda (ξfname)
            (let ((ξbackup-name
                   (concat ξfname "~" (format-time-string "%Y%m%dT%H%M%S") "~")))
              (copy-file ξfname ξbackup-name t)
              (message (concat "Backup saved at: " ξbackup-name))))
          ξfile-list)))
;;xah search word at cursor

(defun xah-search-current-word ()
  "Call `isearch' on current word or text selection.
“word” here is A to Z, a to z, and hyphen 「-」 and underline 「_」, independent of syntax table.
URL `http://ergoemacs.org/emacs/modernization_isearch.html'
Version 2015-04-09"
  (interactive)
  (let ( ξp1 ξp2 )
    (if (use-region-p)
        (progn
          (setq ξp1 (region-beginning))
          (setq ξp2 (region-end)))
      (save-excursion
        (skip-chars-backward "-_A-Za-z0-9")
        (setq ξp1 (point))
        (right-char)
        (skip-chars-forward "-_A-Za-z0-9")
        (setq ξp2 (point))))
    (setq mark-active nil)
    (when (< ξp1 (point))
      (goto-char ξp1))
    (isearch-mode t)
    (isearch-yank-string (buffer-substring-no-properties ξp1 ξp2))))

(defun xah-new-empty-buffer ()
  "Open a new empty buffer.
URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2015-06-12"
  (interactive)
  (let ((ξbuf (generate-new-buffer "untitled")))
    (switch-to-buffer ξbuf)
    (funcall (and initial-major-mode))
    (setq buffer-offer-save t)))

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

(defun browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (browse-url (concat "file://" (buffer-file-name))))

(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(defun smarter-move-beginning-of-line (arg)
  "Toggle between beginning-of-line and back-to-indentation"
  (interactive "^p")
  (setq arg (or arg 1))
  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)
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

(provide 'init-defuns)
