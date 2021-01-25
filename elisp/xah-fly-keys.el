;;; xah-fly-keys.el --- A efficient modal keybinding set minor mode based on ergonomics.

;; Copyright © 2013-2015, by Xah Lee

;; Author: Xah Lee ( http://xahlee.org/ )
;; Version: 5.1.0
;; Package-Version: 20160731.44
;; Created: 10 Sep 2013
;; Keywords: convenience, emulations, vim, ergoemacs
;; Homepage: http://ergoemacs.org/misc/ergoemacs_vi_mode.html

;; This file is not part of GNU Emacs.

;;; License:
;; You can redistribute this program and/or modify it under the terms of the GNU General Public License version 2.


;;; Code:

(require 'dired) ; in emacs
(require 'dired-x) ; in emacs
(require 'ido) ; in emacs
(require 'lispy)

(defvar xah-fly-command-mode-activate-hook nil "Hook for `xah-fly-command-mode-activate'")
(defvar xah-fly-insert-mode-activate-hook nil "Hook for `xah-fly-insert-mode-activate'")

(defvar xah-fly-use-control-key nil "if true, define standard keys for open, close, paste, etc.")
(setq xah-fly-use-control-key t)

;; cursor movement

(defun xah-pop-local-mark-ring ()
  "Move cursor to last mark position of current buffer.
Call this repeatedly will cycle all positions in `mark-ring'.
URL `http://ergoemacs.org/emacs/emacs_jump_to_previous_position.html'
version 2016-04-04"
  (interactive)
  (set-mark-command t))

(defun xah-forward-block (&optional n)
  "Move cursor beginning of next text block.
A text block is separated by blank lines.
This command similar to `forward-paragraph', but this command's behavior is the same regardless of syntax table.
URL `http://ergoemacs.org/emacs/emacs_move_by_paragraph.html'
Version 2016-06-15"
  (interactive "p")
  (let ((n (if (null n) 1 n)))
    (search-forward-regexp "\n[\t\n ]*\n+" nil "NOERROR" n)))

(defun xah-backward-block (&optional n)
  "Move cursor to previous text block.
See: `xah-forward-block'
URL `http://ergoemacs.org/emacs/emacs_move_by_paragraph.html'
Version 2016-06-15"
  (interactive "p")
  (let ((n (if (null n) 1 n))
        (-i 1))
    (while (<= -i n)
      (if (search-backward-regexp "\n[\t\n ]*\n+" nil "NOERROR")
          (progn (skip-chars-backward "\n\t "))
        (progn (goto-char (point-min))
               (setq -i n)))
      (setq -i (1+ -i)))))

(defun xah-beginning-of-line-or-block (&optional n)
  "Move cursor to beginning of line, or beginning of current or previous text block.
 (a text block is separated by blank lines)
URL `http://ergoemacs.org/emacs/emacs_keybinding_design_beginning-of-line-or-block.html'
version 2016-06-15"
  (interactive "p")
  (let ((n (if (null n) 1 n)))
    (if (equal n 1)
        (if (or (equal (point) (line-beginning-position))
                (equal last-command this-command)
                ;; (equal last-command 'xah-end-of-line-or-block )
                )
            (xah-backward-block n)
          ;; (beginning-of-line)
          (mwim-beginning-of-code-or-line)
          )
      (xah-backward-block n))))

(defun xah-end-of-line-or-block (&optional n)
  "Move cursor to end of line, or end of current or next text block.
 (a text block is separated by blank lines)
URL `http://ergoemacs.org/emacs/emacs_keybinding_design_beginning-of-line-or-block.html'
version 2016-06-15"
  (interactive "p")
  (let ((n (if (null n) 1 n)))
    (if (equal n 1)
        (if (or (equal (point) (line-end-position))
                (equal last-command this-command)
                ;; (equal last-command 'xah-beginning-of-line-or-block )
                )
            (xah-forward-block)
          ;;(end-of-line)
          (mwim-end-of-code-or-line))
      (progn (xah-forward-block n)))))

(defvar xah-brackets nil "string of left/right brackets pairs.")
(setq xah-brackets "()[]{}<>（）［］｛｝⦅⦆〚〛⦃⦄“”‘’‹›«»「」〈〉
《》【】〔〕⦗⦘『』〖〗〘〙｢｣
⟦⟧⟨⟩⟪⟫⟮⟯⟬⟭⌈⌉⌊⌋⦇⦈⦉⦊❛❜❝❞❨❩❪❫❴❵❬❭❮❯❰❱❲❳〈〉⦑⦒⧼⧽﹙﹚﹛﹜﹝﹞⁽⁾₍₎⦋⦌⦍⦎⦏⦐⁅⁆⸢⸣⸤⸥⟅⟆⦓⦔⦕⦖⸦⸧⸨⸩
｟｠
⧘⧙⧚⧛⸜⸝⸌⸍⸂⸃⸄⸅⸉⸊᚛᚜༺༻༼༽⏜⏝⎴⎵⏞⏟⏠⏡﹁﹂﹃﹄︹︺︻︼︗︘︿﹀︽︾﹇﹈︷︸")

(defvar xah-left-brackets '("(" "{" "[" "<" "〔" "【" "〖" "〈" "《" "「" "『" "“" "‘" "‹" "«" )
  "List of left bracket chars.")

(progn
  ;; make xah-left-brackets based on xah-brackets
  (setq xah-left-brackets '())
  (dotimes (-x (- (length xah-brackets) 1))
    (when (= (% -x 2) 0)
      (push (char-to-string (elt xah-brackets -x))
            xah-left-brackets)))
  (setq xah-left-brackets (reverse xah-left-brackets)))

(defvar xah-right-brackets '(")" "]" "}" ">" "〕" "】" "〗" "〉" "》" "」" "』" "”" "’" "›" "»")
  "list of right bracket chars.")
(progn
  (setq xah-right-brackets '())
  (dotimes (-x (- (length xah-brackets) 1))
    (when (= (% -x 2) 1)
      (push (char-to-string (elt xah-brackets -x))
            xah-right-brackets)))
  (setq xah-right-brackets (reverse xah-right-brackets)))

(defvar xah-punctuation-regex nil "a regex string for the purpose of jumping to punctuations in programing modes.")
(setq xah-punctuation-regex "[\\!\?\"'#$%&*+,/:;<=>@^`|~]+")

(defun xah-forward-punct (&optional n)
  "Move cursor to the next occurrence of punctuation.
The list of punctuations to jump to is defined by `xah-punctuation-regex'"
  (interactive "p")
  (search-forward-regexp xah-punctuation-regex nil t n))

(defun xah-backward-punct (&optional n)
  "Move cursor to the previous occurrence of punctuation.
See `xah-forward-punct'"
  (interactive "p")
  (search-backward-regexp xah-punctuation-regex nil t n))

(defun xah-backward-left-bracket ()
  "Move cursor to the previous occurrence of left bracket.
The list of brackets to jump to is defined by `xah-left-brackets'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-10-01"
  (interactive)
  (search-backward-regexp (regexp-opt xah-left-brackets) nil t))

(defun xah-forward-right-bracket ()
  "Move cursor to the next occurrence of right bracket.
The list of brackets to jump to is defined by `xah-right-brackets'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-10-01"
  (interactive)
  (search-forward-regexp (regexp-opt xah-right-brackets) nil t))

(defun xah-forward-equal-quote ()
  "Move cursor to the next occurrence of 「='」 or 「=\"」, with or without space.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-05-05"
  (interactive)
  (search-forward-regexp "=[ \n]*\\('+\\|\\\"+\\)" nil t))

(defun xah-forward-equal-sign ()
  "Move cursor to the next occurrence of equal sign 「=」.
URL `http://ergoemacs.org/emacs/emacs_jump_to_punctuations.html'
Version 2015-06-15"
  (interactive)
  (search-forward-regexp "=+" nil t))

(defun xah-backward-equal-sign ()
  "Move cursor to previous occurrence of equal sign 「=」.
URL `http://ergoemacs.org/emacs/emacs_jump_to_punctuations.html'
Version 2015-06-15"
  (interactive)
  (when (search-backward-regexp "=+" nil t)
    (while (search-backward "=" (- (point) 1) t)
      (left-char))))

(defun xah-forward-comma-sign ()
  "Move cursor to the next occurrence of comma 「,」.
Version 2016-01-19"
  (interactive)
  (search-forward-regexp ",+" nil t))

(defun xah-backward-comma-sign ()
  "Move cursor to previous occurrence of comma sign 「,」.
Version 2016-01-19"
  (interactive)
  (when (search-backward-regexp ",+" nil t)
    (while (search-backward "," (- (point) 1) t)
      (left-char))))

(defun xah-forward-quote ()
  "Move cursor to the next occurrence of \".
If there are consecutive quotes of the same char, keep moving until none.
Returns `t' if found, else `nil'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2016-07-23"
  (interactive)
  (if (search-forward-regexp "\\\"+" nil t)
      t
    (progn
      (message "No more quotes after cursor..")
      nil)))

(defun xah-forward-quote-twice ()
  "Call `xah-forward-quote' twice.
Returns `t' if found, else `nil'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2016-07-23"
  (interactive)
  (when (xah-forward-quote)
    (xah-forward-quote)))

(defun xah-backward-quote ()
  "Move cursor to the previous occurrence of \".
If there are consecutive quotes of the same char, keep moving until none.
Returns `t' if found, else `nil'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2016-07-23"
  (interactive)
  (if (search-backward-regexp "\\\"+" nil t)
      (when (char-before) ; isn't nil, at beginning of buffer
        (while (char-equal (char-before) (char-after))
          (left-char)
          t))
    (progn
      (message "No more quotes before cursor.")
      nil)))

(defun xah-forward-dot-comma ()
  "Move cursor to the next occurrence of 「.」 「,」 「;」.
URL `http://ergoemacs.org/emacs/emacs_jump_to_punctuations.html'
Version 2015-03-24"
  (interactive)
  (search-forward-regexp "\\.+\\|,+\\|;+" nil t))

(defun xah-backward-dot-comma ()
  "Move cursor to the previous occurrence of 「.」 「,」 「;」
URL `http://ergoemacs.org/emacs/emacs_jump_to_punctuations.html'
Version 2015-03-24"
  (interactive)
  (search-backward-regexp "\\.+\\|,+\\|;+" nil t))


;; text selection

(defun xah-delete-current-line ()
  "Delete current line."
  (interactive)
  (delete-region (line-beginning-position) (line-end-position))
  (when (looking-at "\n")
    (delete-char 1)))

(defun xah-copy-line-or-region ()
  "Copy current line, or text selection.
When called repeatedly, append copy subsequent lines.
When `universal-argument' is called first, copy whole buffer (respects `narrow-to-region').

URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'
Version 2016-06-18"
  (interactive)
  (let (-p1 -p2)
    (if current-prefix-arg
        (setq -p1 (point-min) -p2 (point-max))
      (if (use-region-p)
          (setq -p1 (region-beginning) -p2 (region-end))
        (setq -p1 (line-beginning-position) -p2 (line-end-position))))
    (if (eq last-command this-command)
        (progn
          (progn ; hack. exit if there's no more next line
            (end-of-line)
            (forward-char)
            (backward-char))
          ;; (push-mark (point) "NOMSG" "ACTIVATE")
          (kill-append "\n" nil)
          (kill-append (buffer-substring-no-properties (line-beginning-position) (line-end-position)) nil)
          (message "Line copy appended"))
      (progn
        (kill-ring-save -p1 -p2)
        (if current-prefix-arg
            (message "Buffer text copied")
          (message "Text copied"))))
    (end-of-line)
    (forward-char)
    ))

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

(defun xah-copy-all-or-region ()
  "Put the whole buffer content to `kill-ring', or text selection if there's one.
Respects `narrow-to-region'.
URL `http://ergoemacs.org/emacs/emacs_copy_cut_all_or_region.html'
Version 2015-08-22"
  (interactive)
  (if (use-region-p)
      (progn
        (kill-new (buffer-substring (region-beginning) (region-end)))
        (message "Text selection copied."))
    (progn
      (kill-new (buffer-string))
      (message "Buffer content copied."))))

(defun xah-cut-all-or-region ()
  "Cut the whole buffer content to `kill-ring', or text selection if there's one.
Respects `narrow-to-region'.
URL `http://ergoemacs.org/emacs/emacs_copy_cut_all_or_region.html'
Version 2015-08-22"
  (interactive)
  (if (use-region-p)
      (progn
        (kill-new (buffer-substring (region-beginning) (region-end)))
        (delete-region (region-beginning) (region-end)))
    (progn
      (kill-new (buffer-string))
      (delete-region (point-min) (point-max)))))

(defun xah-copy-all ()
  "Put the whole buffer content into the `kill-ring'.
(respects `narrow-to-region')
URL `http://ergoemacs.org/emacs/elisp_cut_copy_yank_kill-ring.html'
Version 2015-05-06"
  (interactive)
  (kill-new (buffer-string))
  (message "Buffer content copied."))

(defun xah-cut-all ()
  "Cut the whole buffer content into the `kill-ring'.
Respects `narrow-to-region'."
  (interactive)
  (kill-new (buffer-string))
  (delete-region (point-min) (point-max)))


;; editing commands

(defun xah-toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Always cycle in this order: Init Caps, ALL CAPS, all lower.

URL `http://ergoemacs.org/emacs/modernization_upcase-word.html'
Version 2016-01-08"
  (interactive)
  (let (
        (deactivate-mark nil)
        -p1 -p2)
    (if (use-region-p)
        (setq -p1 (region-beginning)
              -p2 (region-end))
      (save-excursion
        (skip-chars-backward "[:alnum:]")
        (setq -p1 (point))
        (skip-chars-forward "[:alnum:]")
        (setq -p2 (point))))
    (when (not (eq last-command this-command))
      (put this-command 'state 0))
    (cond
     ((equal 0 (get this-command 'state))
      (upcase-initials-region -p1 -p2)
      (put this-command 'state 1))
     ((equal 1  (get this-command 'state))
      (upcase-region -p1 -p2)
      (put this-command 'state 2))
     ((equal 2 (get this-command 'state))
      (downcase-region -p1 -p2)
      (put this-command 'state 0)))))

;; test case
;; test_case some
;; test-case
;; tes▮t-case

(defun xah-toggle-previous-letter-case ()
  "Toggle the letter case of the letter to the left of cursor.
URL `http://ergoemacs.org/emacs/modernization_upcase-word.html'
Version 2015-12-22"
  (interactive)
  (let ((case-fold-search nil))
    (left-char 1)
    (cond
     ((looking-at "[[:lower:]]") (upcase-region (point) (1+ (point))))
     ((looking-at "[[:upper:]]") (downcase-region (point) (1+ (point)))))
    (right-char)))

(defun xah-shrink-whitespaces ()
  "Remove whitespaces around cursor to just one or none.
Call this command again to shrink more. 3 calls will remove all whitespaces.
URL `http://ergoemacs.org/emacs/emacs_shrink_whitespace.html'
Version 2015-11-04"
  (interactive)
  (let ((pos0 (point))
        -line-has-char-p ; current line contains non-white space chars
        -has-space-tab-neighbor-p
        -whitespace-begin -whitespace-end
        -space-or-tab-begin -space-or-tab-end
        )
    (save-excursion
      (setq -has-space-tab-neighbor-p
            (if (or
                 (looking-at " \\|\t")
                 (looking-back " \\|\t" 1))
                t
              nil))
      (beginning-of-line)
      (setq -line-has-char-p (search-forward-regexp "[[:graph:]]" (line-end-position) t))

      (goto-char pos0)
      (skip-chars-backward "\t ")
      (setq -space-or-tab-begin (point))

      (skip-chars-backward "\t \n")
      (setq -whitespace-begin (point))

      (goto-char pos0)
      (skip-chars-forward "\t ")
      (setq -space-or-tab-end (point))
      (skip-chars-forward "\t \n")
      (setq -whitespace-end (point)))

    (if -line-has-char-p
        (if -has-space-tab-neighbor-p
            (let (-deleted-text)
              ;; remove all whitespaces in the range
              (setq -deleted-text
                    (delete-and-extract-region -space-or-tab-begin -space-or-tab-end))
              ;; insert a whitespace only if we have removed something different than a simple whitespace
              (when (not (string= -deleted-text " "))
                (insert " ")))

          (progn
            (when (equal (char-before) 10) (delete-char -1))
            (when (equal (char-after) 10) (delete-char 1))))
      (progn (delete-blank-lines)))))

(defun xah-fill-or-unfill ()
  "Reformat current paragraph or region to `fill-column', like `fill-paragraph' or “unfill”.
When there is a text selection, act on the the selection, else, act on a text block separated by blank lines.
URL `http://ergoemacs.org/emacs/modernization_fill-paragraph.html'
Version 2016-07-13"
  (interactive)
  ;; This command symbol has a property “'compact-p”, the possible
  ;; values are t and nil. This property is used to easily determine
  ;; whether to compact or uncompact, when this command is called
  ;; again
  (let ( (-compact-p
          (if (eq last-command this-command)
              (get this-command 'compact-p)
            (> (- (line-end-position) (line-beginning-position)) fill-column)))
         (deactivate-mark nil)
         (-blanks-regex "\n[ \t]*\n")
         -p1 -p2
         )
    (if (use-region-p)
        (progn (setq -p1 (region-beginning))
               (setq -p2 (region-end)))
      (save-excursion
        (if (re-search-backward -blanks-regex nil "NOERROR")
            (progn (re-search-forward -blanks-regex)
                   (setq -p1 (point)))
          (setq -p1 (point)))
        (if (re-search-forward -blanks-regex nil "NOERROR")
            (progn (re-search-backward -blanks-regex)
                   (setq -p2 (point)))
          (setq -p2 (point)))))
    (if -compact-p
        (fill-region -p1 -p2)
      (let ((fill-column most-positive-fixnum ))
        (fill-region -p1 -p2)))
    (put this-command 'compact-p (not -compact-p))))

(defun xah-reformat-lines ()
  "Reformat current text block into 1 long line or multiple short lines.
When there is a text selection, act on the the selection, else, act on a text block separated by blank lines.

When the command is called for the first time, it checks the
current line's length to decide to go into 1 line or multiple
lines. If current line is short, it'll reformat to 1 long lines.
And vice versa.

Repeated call toggles between formatting to 1 long line and multiple lines.
URL `http://ergoemacs.org/emacs/emacs_reformat_lines.html'
Version 2016-07-13"
  (interactive)
  ;; This command symbol has a property “'compact-p”, the possible
  ;; values are t and nil. This property is used to easily determine
  ;; whether to compact or uncompact, when this command is called
  ;; again
  (let (
        (-compact-p
         (if (eq last-command this-command)
             (get this-command 'compact-p)
           (> (- (line-end-position) (line-beginning-position)) fill-column)))
        (deactivate-mark nil)
        (-blanks-regex "\n[ \t]*\n")
        -p1 -p2
        )
    (if (use-region-p)
        (progn (setq -p1 (region-beginning))
               (setq -p2 (region-end)))
      (save-excursion
        (if (re-search-backward -blanks-regex nil "NOERROR")
            (progn (re-search-forward -blanks-regex)
                   (setq -p1 (point)))
          (setq -p1 (point)))
        (if (re-search-forward -blanks-regex nil "NOERROR")
            (progn (re-search-backward -blanks-regex)
                   (setq -p2 (point)))
          (setq -p2 (point)))))
    (save-excursion
      (if -compact-p
          (xah-reformat-to-multi-lines-region -p1 -p2)
        (xah-reformat-to-single-line-region -p1 -p2))
      (put this-command 'compact-p (not -compact-p)))))

(defun xah-reformat-to-single-line-region (*begin *end)
  "Replace whitespaces at end of each line by one space.
URL `http://ergoemacs.org/emacs/emacs_reformat_lines.html'
Version 2016-07-12"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region *begin *end)
      (goto-char (point-min))
      (while
          (search-forward "\n" nil 'NOERROR)
        (replace-match " ")
        (while (looking-back " \\|\t" 1) (delete-char -1))
        (insert " ")
        (while (looking-at " \\|\t") (delete-char 1))))))

(defun xah-reformat-to-multi-lines-region (*begin *end)
  "replace space by a newline char at places so lines are not long.
URL `http://ergoemacs.org/emacs/emacs_reformat_lines.html'
Version 2016-07-12"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region *begin *end)
      (goto-char (point-min))
      (while
          (search-forward " " nil 'NOERROR)
        (when (> (- (point) (line-beginning-position)) fill-column)
          (replace-match "\n" ))))))

(defun xah-unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces.
This command does the inverse of `fill-paragraph'.

URL `http://ergoemacs.org/emacs/emacs_unfill-paragraph.html'
Version 2016-07-13"
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph)))

(defun xah-unfill-region (*begin *end)
  "Replace newline chars in region by single spaces.
This command does the inverse of `fill-region'.

URL `http://ergoemacs.org/emacs/emacs_unfill-paragraph.html'
Version 2016-07-13"
  (interactive "r")
  (let ((fill-column most-positive-fixnum))
    (fill-region *begin *end)))

(defun xah-cycle-hyphen-underscore-space ()
  "Cycle {underscore, space, hypen} chars of current word or text selection.
When called repeatedly, this command cycles the {“_”, “-”, “ ”} characters, in that order.

URL `http://ergoemacs.org/emacs/elisp_change_space-hyphen_underscore.html'
Version 2016-07-17"
  (interactive)
  ;; this function sets a property 「'state」. Possible values are 0 to length of -charArray.
  (let (-p1 -p2)
    (if (use-region-p)
        (progn
          (setq -p1 (region-beginning))
          (setq -p2 (region-end)))
      (save-excursion
        ;; 2016-01-14 not use (bounds-of-thing-at-point 'symbol),
        ;; because if at end of buffer, it returns nil. also, it's
        ;; syntax table dependent
        (skip-chars-backward "-_[:alnum:]")
        (setq -p1 (point))
        (skip-chars-forward "-_[:alnum:]")
        (setq -p2 (point))))
    (let* ((-inputText (buffer-substring-no-properties -p1 -p2))
           (-charArray ["_" "-" " "])
           (-length (length -charArray))
           (-regionWasActive-p (region-active-p))
           (-nowState
            (if (equal last-command this-command )
                (get 'xah-cycle-hyphen-underscore-space 'state)
              0 ))
           (-changeTo (elt -charArray -nowState)))
      (save-excursion
        (save-restriction
          (narrow-to-region -p1 -p2)
          (goto-char (point-min))
          (while
              (search-forward-regexp
               (elt -charArray (% (+ -nowState 2) -length))
               ;; (concat
               ;;  (elt -charArray (% (+ -nowState 1) -length))
               ;;  "\\|"
               ;;  (elt -charArray (% (+ -nowState 2) -length)))
               (point-max)
               'NOERROR)
            (replace-match -changeTo 'FIXEDCASE 'LITERAL))))
      (when (or (string= -changeTo " ") -regionWasActive-p)
        (goto-char -p2)
        (set-mark -p1)
        (setq deactivate-mark nil))
      (put 'xah-cycle-hyphen-underscore-space 'state (% (+ -nowState 1) -length)))))

(defun xah-underscore-to-space-region (*begin *end)
  "Change underscore char to space.
URL `http://ergoemacs.org/emacs/elisp_change_space-hyphen_underscore.html'
Version 2015-08-18"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region *begin *end)
      (goto-char (point-min))
      (while
          (search-forward-regexp "_" (point-max) 'NOERROR)
        (replace-match " " 'FIXEDCASE 'LITERAL)))))

(defun xah-copy-file-path (&optional *dir-path-only-p)
  "Copy the current buffer's file path or dired path to `kill-ring'.
Result is full path.
If `universal-argument' is called first, copy only the dir path.
URL `http://ergoemacs.org/emacs/emacs_copy_file_path.html'
Version 2016-07-17"
  (interactive "P")
  (let ((-fpath
         (if (equal major-mode 'dired-mode)
             (expand-file-name default-directory)
           (if (null (buffer-file-name))
               (user-error "Current buffer is not associated with a file.")
             (buffer-file-name)))))
    (kill-new
     (if (null *dir-path-only-p)
         (progn
           (message "File path copied: 「%s」" -fpath)
           -fpath
           )
       (progn
         (message "Directory path copied: 「%s」" (file-name-directory -fpath))
         (file-name-directory -fpath))))))

(defun xah-delete-text-block ()
  "Delete the current or next text block and also copy to `kill-ring'.
URL `http://ergoemacs.org/emacs/emacs_delete_block.html'
Version 2016-07-22"
  (interactive)
  (let (-p1 -p2)
    (progn
      (beginning-of-line)
      (if (search-forward-regexp "[[:graph:]]" (line-end-position) 'NOERROR )
          (xah-delete-current-text-block)
        (when (search-forward-regexp "[[:graph:]]" )
          (xah-delete-current-text-block))))))

(defun xah-delete-current-text-block ()
  "Delete the current text block and also copy to `kill-ring'.
URL `http://ergoemacs.org/emacs/emacs_delete_block.html'
Version 2016-07-22"
  (interactive)
  (let (-p1 -p2)
    (progn
      (if (re-search-backward "\n[ \t]*\n" nil "NOERROR")
          (progn (re-search-forward "\n[ \t]*\n")
                 (setq -p1 (point)))
        (setq -p1 (point)))
      (re-search-forward "\n[ \t]*\n" nil "NOERROR")
      (setq -p2 (point)))
    (kill-region -p1 -p2)))

(defun xah-copy-to-register-1 ()
  "Copy current line or text selection to register 1.
See also: `xah-paste-from-register-1', `copy-to-register'.

URL `http://ergoemacs.org/emacs/elisp_copy-paste_register_1.html'
Version 2015-12-08"
  (interactive)
  (let (-p1 -p2)
    (if (region-active-p)
        (progn (setq -p1 (region-beginning))
               (setq -p2 (region-end)))
      (progn (setq -p1 (line-beginning-position))
             (setq -p2 (line-end-position))))
    (copy-to-register ?1 -p1 -p2)
    (message "copied to register 1: 「%s」." (buffer-substring-no-properties -p1 -p2))))

(defun xah-paste-from-register-1 ()
  "Paste text from register 1.
See also: `xah-copy-to-register-1', `insert-register'.
URL `http://ergoemacs.org/emacs/elisp_copy-paste_register_1.html'
Version 2015-12-08"
  (interactive)
  (when (use-region-p)
    (delete-region (region-beginning) (region-end)))
  (insert-register ?1 t))

(defun xah-copy-rectangle-to-kill-ring (*begin *end)
  "Copy region as column (rectangle region) to `kill-ring'
See also: `kill-rectangle', `copy-to-register'.
URL `http://ergoemacs.org/emacs/emacs_copy_rectangle_text_to_clipboard.html'
version 2016-07-17"
  ;; extract-rectangle suggested by YoungFrog, 2012-07-25
  (interactive "r")
  (require 'rect)
  (kill-new (mapconcat 'identity (extract-rectangle *begin *end) "\n")))

(defun xah-upcase-sentence ()
  "Upcase sentence.
TODO 2014-09-30 command incomplete
"
  (interactive)
  (let (-p1 -p2)

    (if (region-active-p)
        (progn
          (setq -p1 (region-beginning))
          (setq -p2 (region-end)))
      (progn
        (save-excursion
          (progn
            (if (re-search-backward "\n[ \t]*\n" nil "move")
                (progn (re-search-forward "\n[ \t]*\n")
                       (setq -p1 (point)))
              (setq -p1 (point)))
            (if (re-search-forward "\n[ \t]*\n" nil "move")
                (progn (re-search-backward "\n[ \t]*\n")
                       (setq -p2 (point)))
              (setq -p2 (point)))))))

    (save-excursion
      (save-restriction
        (narrow-to-region -p1 -p2)

        (goto-char (point-min))
        (while (search-forward "\. \{1,2\}\\([a-z]\\)" nil t)
          nil
          ;; (replace-match "myReplaceStr2")

          )))))

(defun xah-escape-quotes (*begin *end)
  "Replace 「\"」 by 「\\\"」 in current line or text selection.
See also: `xah-unescape-quotes'
URL `http://ergoemacs.org/emacs/elisp_escape_quotes.html'
Version 2016-07-17"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (save-excursion
    (save-restriction
      (narrow-to-region *begin *end)
      (goto-char (point-min))
      (while (search-forward "\"" nil t)
        (replace-match "\\\"" 'FIXEDCASE 'LITERAL)))))

(defun xah-unescape-quotes (*begin *end)
  "Replace  「\\\"」 by 「\"」 in current line or text selection.
See also: `xah-escape-quotes'
URL `http://ergoemacs.org/emacs/elisp_escape_quotes.html'
Version 2016-07-17"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (save-excursion
    (save-restriction
      (narrow-to-region *begin *end)
      (goto-char (point-min))
      (while (search-forward "\\\"" nil t)
        (replace-match "\"" 'FIXEDCASE 'LITERAL)))))

(defun xah-title-case-region-or-line (*begin *end)
  "Title case text between nearest brackets, or current line, or text selection.
Capitalize first letter of each word, except words like {to, of,
the, a, in, or, and, …}. If a word already contains cap letters
such as HTTP, URL, they are left as is.

When called in a elisp program, *begin *end are region boundaries.
URL `http://ergoemacs.org/emacs/elisp_title_case_text.html'
Version 2015-05-07"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (let (
           -p1
           -p2
           (-skipChars "^\"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕"))
       (progn
         (skip-chars-backward -skipChars (line-beginning-position))
         (setq -p1 (point))
         (skip-chars-forward -skipChars (line-end-position))
         (setq -p2 (point)))
       (list -p1 -p2))))
  (let* (
         (-strPairs [
                     [" A " " a "]
                     [" And " " and "]
                     [" At " " at "]
                     [" As " " as "]
                     [" By " " by "]
                     [" Be " " be "]
                     [" Into " " into "]
                     [" In " " in "]
                     [" Is " " is "]
                     [" It " " it "]
                     [" For " " for "]
                     [" Of " " of "]
                     [" Or " " or "]
                     [" On " " on "]
                     [" Via " " via "]
                     [" The " " the "]
                     [" That " " that "]
                     [" To " " to "]
                     [" Vs " " vs "]
                     [" With " " with "]
                     [" From " " from "]
                     ["'S " "'s "]
                     ]))
    (save-excursion
      (save-restriction
        (narrow-to-region *begin *end)
        (upcase-initials-region (point-min) (point-max))
        (let ((case-fold-search nil))
          (mapc
           (lambda (-x)
             (goto-char (point-min))
             (while
                 (search-forward (aref -x 0) nil t)
               (replace-match (aref -x 1) 'FIXEDCASE 'LITERAL)))
           -strPairs))))))


;; insertion commands

(defun xah-insert-date (&optional add-time-stamp-p)
  "Insert current date and or time.

• In this format yyyy-mm-dd.
• When called with `universal-argument', insert date and time, e.g. 2012-05-28T07:06:23-07:00
• Replaces text selection.

See also `xah-current-date-time-string'.
version 2016-04-12"
  (interactive "P")
  (when (use-region-p) (delete-region (region-beginning) (region-end)))
  (insert
   (if add-time-stamp-p
       (xah-current-date-time-string)
     (format-time-string "%Y-%m-%d"))))

(defun xah-current-date-time-string ()
  "Returns current date-time string in full ISO 8601 format.
Example: 「2012-04-05T21:08:24-07:00」.

Note, for the time zone offset, both the formats 「hhmm」 and
「hh:mm」 are valid ISO 8601. However, Atom Webfeed spec seems to
require 「hh:mm」."
  (concat
   (format-time-string "%Y-%m-%dT%T")
   ((lambda (-x) (format "%s:%s" (substring -x 0 3) (substring -x 3 5))) (format-time-string "%z"))))

(defun xah-insert-bracket-pair (*left-bracket *right-bracket)
  "Wrap or Insert a matching bracket and place cursor in between.

If there's a text selection, wrap brackets around it. Else,
smartly decide wrap or insert. (basically, if there's no char
after cursor, just insert bracket pair.)

*left-bracket ＆ *right-bracket are strings.

URL `http://ergoemacs.org/emacs/elisp_insert_brackets_by_pair.html'
Version 2015-04-19"
  (if (use-region-p)
      (progn
        (let (
              (-p1 (region-beginning))
              (-p2 (region-end)))
          (goto-char -p2)
          (insert *right-bracket)
          (goto-char -p1)
          (insert *left-bracket)
          (goto-char (+ -p2 2))))
    (progn ; no text selection
      (if
          (or
           (looking-at "[^-_[:alnum:]]")
           (eq (point) (point-max)))
          (progn
            (insert *left-bracket *right-bracket)
            (search-backward *right-bracket ))
        (progn
          (let (-p1 -p2)
            ;; basically, want all alphanumeric, plus hyphen and underscore, but don't want space or punctuations. Also want chinese.
            ;; 我有一帘幽梦，不知与谁能共。多少秘密在其中，欲诉无人能懂。
            (skip-chars-backward "-_[:alnum:]")
            (setq -p1 (point))
            (skip-chars-forward "-_[:alnum:]")
            (setq -p2 (point))
            (goto-char -p2)
            (insert *right-bracket)
            (goto-char -p1)
            (insert *left-bracket)
            (goto-char (+ -p2 (length *left-bracket)))))))))

;; (insert-parentheses)

(defun xah-insert-paren () (interactive) (xah-insert-bracket-pair "(" ")") )
(defun xah-insert-square-bracket () (interactive) (xah-insert-bracket-pair "[" "]") )
(defun xah-insert-brace () (interactive) (xah-insert-bracket-pair "{" "}") )
(defun xah-insert-greater-less () (interactive) (xah-insert-bracket-pair "<" ">") )

(defun xah-insert-double-curly-quote“” () (interactive) (xah-insert-bracket-pair "“" "”") )
(defun xah-insert-curly-single-quote‘’ () (interactive) (xah-insert-bracket-pair "‘" "’") )
(defun xah-insert-single-angle-quote‹› () (interactive) (xah-insert-bracket-pair "‹" "›") )
(defun xah-insert-double-angle-quote«» () (interactive) (xah-insert-bracket-pair "«" "»") )
(defun xah-insert-ascii-double-quote () (interactive) (xah-insert-bracket-pair "\"" "\"") )
(defun xah-insert-ascii-single-quote () (interactive) (xah-insert-bracket-pair "'" "'") )
(defun xah-insert-emacs-quote () (interactive) (xah-insert-bracket-pair "`" "'") )
(defun xah-insert-corne-bracket「」 () (interactive) (xah-insert-bracket-pair "「" "」") )
(defun xah-insert-white-corner-bracket『』 () (interactive) (xah-insert-bracket-pair "『" "』") )
(defun xah-insert-angle-bracket〈〉 () (interactive) (xah-insert-bracket-pair "〈" "〉") )
(defun xah-insert-double-angle-bracket《》 () (interactive) (xah-insert-bracket-pair "《" "》") )
(defun xah-insert-white-lenticular-bracket〖〗 () (interactive) (xah-insert-bracket-pair "〖" "〗") )
(defun xah-insert-black-lenticular-bracket【】 () (interactive) (xah-insert-bracket-pair "【" "】") )
(defun xah-insert-tortoise-shell-bracket〔〕 () (interactive) (xah-insert-bracket-pair "〔" "〕") )

(defun xah-insert-string-assignment ()
  "Insert space before cursor"
  (interactive)
  (progn (insert "=\"\"")
         (left-char)))

(defun xah-insert-space-before ()
  "Insert space before cursor"
  (interactive)
  (insert " "))

(defun xah-insert-space-after ()
  "Insert space after cursor"
  (interactive)
  (insert " ")
  (left-char))

(defun xah-insert-form-feed ()
  "insert a form feed char (ASCII 12)"
  (interactive)
  (insert ""))

(defun xah-insert-column-counter (*n)
  "Insert a sequence of numbers vertically.

 (this command is similar to emacs 24.x's `rectangle-number-lines'.)

For example, if your text is:

a b
c d
e f

and your cursor is after “a”, then calling this function with argument
3 will change it to become:

a1 b
c2 d
e3 f

If there are not enough existing lines after the cursor
when this function is called, it aborts at the last line.

This command is conveniently used together with `kill-rectangle' and `string-rectangle'."
  (interactive "nEnter the max integer: ")
  (let ((-i 1) -colpos )
    (setq -colpos (- (point) (line-beginning-position)))
    (while (<= -i *n)
      (insert (number-to-string -i))
      (forward-line) (beginning-of-line) (forward-char -colpos)
      (setq -i (1+ -i)))))

(defun xah-insert-alphabets-az (&optional *use-uppercase-p)
  "Insert letters a to z vertically.
If `universal-argument' is called first, use CAPITAL letters.

URL `http://ergoemacs.org/emacs/emacs_insert-alphabets.html'
Version 2015-11-06"
  (interactive "P")
  (let ((-startChar (if *use-uppercase-p 65 97 )))
    (dotimes (-i 26)
      (insert (format "%c\n" (+ -startChar -i))))))

(defvar xah-unicode-list nil "Associative list of Unicode
symbols. First element is a Unicode character, second element is
a string used as key shortcut in `ido-completing-read'")
(setq xah-unicode-list
      '(
        ("_" . "underscore" )
        ("•" . ".bullet" )
        ("→" . "tn")
        ("◇" . "3" )
        ("◆" . "4" )
        ("¤" . "2" )
        ("…" . "...ellipsis" )
        (" " . "nbsp" )
        ("、" . "," )
        ("⭑" . "9" )
        ("🎶" . "5" )
        ("—" . "-emdash" )
        ("＆" . "7" )
        ("↓" . "tt")
        ("←" . "th")
        ("↑" . "tc")
        ("👍" . "tu")
        ) )

(defun xah-insert-unicode ()
  "Insert a unicode"
  (interactive)
  (let (gotThis)
    (setq gotThis
          (ido-completing-read "insert:" (mapcar (lambda (x) (concat (car x) (cdr x))) xah-unicode-list)))
    (insert (car (assoc (substring gotThis 0 1) xah-unicode-list)))))


;; text selection

(defun xah-select-current-block ()
  "Select the current block of text between blank lines.

URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2016-07-22"
  (interactive)
  (let (-p1)
    (progn
      (if (re-search-backward "\n[ \t]*\n" nil "move")
          (progn (re-search-forward "\n[ \t]*\n")
                 (setq -p1 (point)))
        (setq -p1 (point)))
      (re-search-forward "\n[ \t]*\n" nil "move"))
    (set-mark -p1)))

(defun xah-select-block ()
  "Select the current/next block of text between blank lines.
If region is active, extend selection downward by block.

URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2016-07-22"
  (interactive)
  (if (region-active-p)
      (re-search-forward "\n[ \t]*\n" nil "move")
    (xah-select-current-block)))

(defun xah-select-current-line ()
  "Select current line.
URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2016-07-22"
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position)))

(defun xah-select-line ()
  "Select current line. If region is active, extend selection downward by line.
URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2016-07-22"
  (interactive)
  (if (region-active-p)
      (progn
        (forward-line 1)
        (end-of-line))
    (xah-select-current-line)))

(defun xah-semnav-up (arg)
  "Called by `xah-extend-selection'.

URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2015-11-13.
Written by Nikolaj Schumacher, 2008-10-20. Released under GPL 2"
  (interactive "p")
  (when (nth 3 (syntax-ppss))
    (if (> arg 0)
        (progn
          (skip-syntax-forward "^\"")
          (goto-char (1+ (point)))
          (setq arg (1- arg) ))
      (skip-syntax-backward "^\"")
      (goto-char (1- (point)))
      (setq arg (1+ arg) )))
  (up-list arg))

(defun xah-extend-selection (arg &optional incremental-p)
  "Select the current word.
Subsequent calls expands the selection to larger semantic unit.

This command works mostly in lisp syntax.
URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2015-11-13.
Written by Nikolaj Schumacher, 2008-10-20. Released under GPL 2."
  (interactive
   (list (prefix-numeric-value current-prefix-arg)
         (or (use-region-p)
             (eq last-command this-command))))
  (if incremental-p
      (progn
        (xah-semnav-up (- arg))
        (forward-sexp)
        (mark-sexp -1))
    (if (> arg 1)
        (xah-extend-selection (1- arg) t)
      (if (looking-at "\\=\\(\\s_\\|\\sw\\)*\\_>")
          (goto-char (match-end 0))
        (unless (memq (char-before) '(?\) ?\"))
          (forward-sexp)))
      (mark-sexp -1))))

(defun xah-select-text-in-quote ()
  "Select text between the nearest left and right delimiters.
Delimiters here includes the following chars: \"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕（）
This command does not properly deal with nested brackets.
URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2015-05-16"
  (interactive)
  (let (
        (-skipChars
         (if (boundp 'xah-brackets)
             (concat "^\"" xah-brackets)
           "^\"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕（）"))
        -p1
        -p2
        )
    (skip-chars-backward -skipChars)
    (setq -p1 (point))
    (skip-chars-forward -skipChars)
    (setq -p2 (point))
    (set-mark -p1)))


;; misc

(eval-when-compile
  (require 'cl))

(defun count-buffers (&optional display-anyway)
  "Display or return the number of buffers."
  (interactive)
  (let ((buf-count (length (buffer-list))))
    (if (or (interactive-p) display-anyway)
        (message "%d buffers in this Emacs" buf-count)) buf-count))

(defun get-buffers-matching-mode (mode)
  "Returns a list of buffers where their major-mode is equal to MODE"
  (let ((buffer-mode-matches '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (if (eq mode major-mode)
            (add-to-list 'buffer-mode-matches buf))))
    buffer-mode-matches))

(defun count-user-buffers ()
  "Returns a list of buffers where their major-mode is equal to MODE"
  (interactive)
  (let ((i 0))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (if (xah-user-buffer-q)
            (setq i (1+ i)))))
    (message "Count User Buffers %d" i)))

(defun xah-user-buffer-q ()
  "Return t if current buffer is a user buffer, else nil.
Typically, if buffer name starts with *, it's not considered a user buffer.
This function is used by buffer switching command and close buffer command, so that next buffer shown is a user buffer.
You can override this function to get your idea of “user buffer”.
version 2016-06-18"
  (interactive)
  (if (string-equal "*" (substring (buffer-name) 0 1))
      nil
    (if (string-equal major-mode "dired-mode")
        nil
      t
      )))

(defun xah-next-user-buffer ()
  "Switch to the next user buffer.
“user buffer” is determined by `xah-user-buffer-q'.
URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (xah-user-buffer-q))
          (progn (next-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun xah-previous-user-buffer ()
  "Switch to the previous user buffer.
“user buffer” is determined by `xah-user-buffer-q'.
URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (xah-user-buffer-q))
          (progn (previous-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun xah-next-emacs-buffer ()
  "Switch to the next emacs buffer.
“emacs buffer” here is buffer whose name starts with *.
URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (not (string-equal "*" (substring (buffer-name) 0 1))) (< i 20))
      (setq i (1+ i)) (next-buffer))))

(defun xah-previous-emacs-buffer ()
  "Switch to the previous emacs buffer.
“emacs buffer” here is buffer whose name starts with *.
URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (not (string-equal "*" (substring (buffer-name) 0 1))) (< i 20))
      (setq i (1+ i)) (previous-buffer))))

(defvar xah-recently-closed-buffers nil "alist of recently closed
buffers. Each element is (buffer name, file path). The max number
to track is controlled by the variable
`xah-recently-closed-buffers-max'.")

(defvar xah-recently-closed-buffers-max 40 "The maximum length for `xah-recently-closed-buffers'.")

(defun xah-close-current-buffer ()
  "Close the current buffer.

Similar to `kill-buffer', with the following addition:

• Prompt user to save if the buffer has been modified even if the buffer is not associated with a file.
• If the buffer is editing a source file in an org-mode file, prompt the user to save before closing.
• If the buffer is a file, add the path to the list `xah-recently-closed-buffers'.
• If it is the minibuffer, exit the minibuffer

URL `http://ergoemacs.org/emacs/elisp_close_buffer_open_last_closed.html'
Version 2016-06-19"
  (interactive)
  (let (-emacs-buff-p
        (-org-p (string-match "^*Org Src" (buffer-name))))

    (setq -emacs-buff-p (if (string-match "^*" (buffer-name)) t nil))

    (if (string= major-mode "minibuffer-inactive-mode")
        (minibuffer-keyboard-quit) ; if the buffer is minibuffer
      (progn
        ;; offer to save buffers that are non-empty and modified, even for non-file visiting buffer. (because kill-buffer does not offer to save buffers that are not associated with files)
        (when (and (buffer-modified-p)
                   (not -emacs-buff-p)
                   (not (string-equal major-mode "dired-mode"))
                   (if (equal (buffer-file-name) nil)
                       (if (string-equal "" (save-restriction (widen) (buffer-string))) nil t)
                     t))
          (if (y-or-n-p (format "Buffer %s modified; Do you want to save? " (buffer-name)))
              (save-buffer)
            (set-buffer-modified-p nil)))
        (when (and (buffer-modified-p)
                   -org-p)
          (if (y-or-n-p (format "Buffer %s modified; Do you want to save? " (buffer-name)))
              (org-edit-src-save)
            (set-buffer-modified-p nil)))

        ;; save to a list of closed buffer
        (when (buffer-file-name)
          (setq xah-recently-closed-buffers
                (cons (cons (buffer-name) (buffer-file-name)) xah-recently-closed-buffers))
          (when (> (length xah-recently-closed-buffers) xah-recently-closed-buffers-max)
            (setq xah-recently-closed-buffers (butlast xah-recently-closed-buffers 1))))

        ;; close
        (kill-buffer (current-buffer))))))

(defun xah-open-last-closed ()
  "Open the last closed file.
URL `http://ergoemacs.org/emacs/elisp_close_buffer_open_last_closed.html'
Version 2016-06-19"
  (interactive)
  (if (> (length xah-recently-closed-buffers) 0)
      (find-file (cdr (pop xah-recently-closed-buffers)))
    (progn (message "No recently close buffer in this session."))))

(defun xah-open-recently-closed ()
  "Open recently closed file.
Prompt for a choice.
URL `http://ergoemacs.org/emacs/elisp_close_buffer_open_last_closed.html'
Version 2016-06-19"
  (interactive)
  (find-file (ido-completing-read "open:" (mapcar (lambda (f) (cdr f)) xah-recently-closed-buffers))))

(defun xah-list-recently-closed ()
  "List recently closed file.
URL `http://ergoemacs.org/emacs/elisp_close_buffer_open_last_closed.html'
Version 2016-06-19"
  (interactive)
  (let ((-buf (generate-new-buffer "*recently closed*")))
    (switch-to-buffer -buf)
    (mapc (lambda (-f) (insert (cdr -f) "\n"))
          xah-recently-closed-buffers)))

(defun xah-new-empty-buffer ()
  "Open a new empty buffer.
URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2015-06-12"
  (interactive)
  (let ((-buf (generate-new-buffer "untitled")))
    (switch-to-buffer -buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)))

(defun new-same-mode-buffer ()
  "Open a new empty buffer."
  (interactive)
  (scratch)
  (delete-other-windows))

;; note: emacs won't offer to save a buffer that's
;; not associated with a file,
;; even if buffer-modified-p is true.
;; One work around is to define your own my-kill-buffer function
;; that wraps around kill-buffer, and check on the buffer modification
;; status to offer save
;; This custome kill buffer is close-current-buffer.



(defun xah-run-current-file ()
  "Execute the current file.
For example, if the current buffer is the file x.py, then it'll call 「python x.py」 in a shell.
The file can be Emacs Lisp, PHP, Perl, Python, Ruby, JavaScript, Bash, Ocaml, Visual Basic, TeX, Java, Clojure.
File suffix is used to determine what program to run.

If the file is modified or not saved, save it automatically before run.

URL `http://ergoemacs.org/emacs/elisp_run_current_file.html'
version 2016-01-28"
  (interactive)
  (let (
        (-suffix-map
         ;; (‹extension› . ‹shell program name›)
         `(
           ("php" . "php")
           ("pl" . "perl")
           ("py" . "python")
           ("py3" . ,(if (string-equal system-type "windows-nt") "c:/Python32/python.exe" "python3"))
           ("rb" . "ruby")
           ("go" . "go run")
           ("js" . "node") ; node.js
           ("sh" . "bash")
           ("clj" . "java -cp /home/xah/apps/clojure-1.6.0/clojure-1.6.0.jar clojure.main")
           ("rkt" . "racket")
           ("ml" . "ocaml")
           ("vbs" . "cscript")
           ("tex" . "pdflatex")
           ("latex" . "pdflatex")
           ("java" . "javac")
           ;; ("pov" . "/usr/local/bin/povray +R2 +A0.1 +J1.2 +Am2 +Q9 +H480 +W640")
           ))

        -fname
        -fSuffix
        -prog-name
        -cmd-str)

    (when (null (buffer-file-name)) (save-buffer))
    (when (buffer-modified-p) (save-buffer))

    (setq -fname (buffer-file-name))
    (setq -fSuffix (file-name-extension -fname))
    (setq -prog-name (cdr (assoc -fSuffix -suffix-map)))
    (setq -cmd-str (concat -prog-name " \""   -fname "\""))

    (cond
     ((string-equal -fSuffix "el") (load -fname))
     ((string-equal -fSuffix "java")
      (progn
        (shell-command -cmd-str "*xah-run-current-file output*" )
        (shell-command
         (format "java %s" (file-name-sans-extension (file-name-nondirectory -fname))))))
     (t (if -prog-name
            (progn
              (message "Running…")
              (shell-command -cmd-str "*xah-run-current-file output*" ))
          (message "No recognized program file suffix for this file."))))))

(defun xah-clean-whitespace (*begin *end)
  "Delete trailing whitespace, and replace repeated blank lines into just 2.
Only space and tab is considered whitespace here.
Works on whole buffer or text selection, respects `narrow-to-region'.
Saves the file if it is a file.

URL `http://ergoemacs.org/emacs/elisp_compact_empty_lines.html'
Version 2016-07-30"
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (save-excursion
    (save-restriction
      (narrow-to-region *begin *end)
      (progn
        (goto-char (point-min))
        (while (search-forward-regexp "[ \t]+\n" nil "noerror")
          (replace-match "\n")))
      (progn
        (goto-char (point-min))
        (while (search-forward-regexp "\n\n\n+" nil "noerror")
          (replace-match "\n\n")))
      (progn
        (goto-char (point-max))
        (while (equal (char-before) 32)
          (delete-char -1))))))

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
Version 2015-10-14"
  (interactive)
  (let ((-fname (buffer-file-name)))
    (if -fname
        (let ((-backup-name
               (concat -fname "~" (format-time-string "%Y%m%dT%H%M%S") "~")))
          (copy-file -fname -backup-name t)
          (message (concat "Backup saved at: " -backup-name)))
      (if (string-equal major-mode "dired-mode")
          (progn
            (mapc (lambda (-x)
                    (let ((-backup-name
                           (concat -x "~" (format-time-string "%Y%m%dT%H%M%S") "~")))
                      (copy-file -x -backup-name t)))
                  (dired-get-marked-files))
            (message "marked files backed up"))
        (user-error "buffer not file nor dired")))))

(defun xah-make-backup-and-save ()
  "Backup of current file and save, or backup dired marked files.
For detail, see `xah-make-backup'.
If the current buffer is not associated with a file nor dired, nothing's done.
URL `http://ergoemacs.org/emacs/elisp_make-backup.html'
Version 2015-10-14"
  (interactive)
  (if (buffer-file-name)
      (progn
        (xah-make-backup)
        (when (buffer-modified-p)
          (save-buffer)))
    (progn
      (xah-make-backup))))

(defun xah-delete-current-file-make-backup (&optional *no-backup-p)
  "Delete current file, makes a backup~, closes the buffer.

Backup filename is “‹name›~‹date time stamp›~”. Existing file of the same name is overwritten. If the file is not associated with buffer, the backup file name starts with “xx_”.

When `universal-argument' is called first, don't create backup.

URL `http://ergoemacs.org/emacs/elisp_delete-current-file.html'
Version 2016-07-20"
  (interactive "P")
  (let* (
         (-fname (buffer-file-name))
         (-buffer-is-file-p -fname)
         (-backup-suffix (concat "~" (format-time-string "%Y%m%dT%H%M%S") "~")))
    (if -buffer-is-file-p
        (progn
          (save-buffer -fname)
          (when (not *no-backup-p)
            (copy-file
             -fname
             (concat -fname -backup-suffix)
             t))
          (delete-file -fname)
          (message "Deleted. Backup created at 「%s」." (concat -fname -backup-suffix)))
      (when (not *no-backup-p)
        (widen)
        (write-region (point-min) (point-max) (concat "xx" -backup-suffix))
        (message "Backup created at 「%s」." (concat "xx" -backup-suffix))))
    (kill-buffer (current-buffer))))

(defun xah-delete-current-file-copy-to-kill-ring ()
  "Delete current buffer/file and close the buffer, push content to `kill-ring'.
URL `http://ergoemacs.org/emacs/elisp_delete-current-file.html'
Version 2016-07-20"
  (interactive)
  (progn
    (kill-new (buffer-string))
    (message "Buffer content copied to kill-ring.")
    (when (buffer-file-name)
      (when (file-exists-p (buffer-file-name))
        (progn
          (delete-file (buffer-file-name))
          (message "Deleted file: 「%s」." (buffer-file-name)))))
    (let ((buffer-offer-save nil))
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer)))))

(defun xah-delete-current-file (&optional *no-backup-p)
  "Delete current buffer/file.
If buffer is a file, makes a backup~, else, push file content to `kill-ring'.

This commands calls `xah-delete-current-file-make-backup' or
 `xah-delete-current-file-copy-to-kill-ring'.

If next buffer is dired, refresh it.

URL `http://ergoemacs.org/emacs/elisp_delete-current-file.html'
Version 2016-07-20"
  (interactive "P")
  (if (buffer-file-name)
      (xah-delete-current-file-make-backup *no-backup-p)
    (xah-delete-current-file-copy-to-kill-ring))
  (when (eq major-mode 'dired-mode)
    (revert-buffer)))



(defun xah-search-current-word ()
  "Call `isearch' on current word or text selection.
“word” here is A to Z, a to z, and hyphen 「-」 and underline 「_」, independent of syntax table.
URL `http://ergoemacs.org/emacs/modernization_isearch.html'
Version 2015-04-09"
  (interactive)
  (let (-p1 -p2)
    (if (use-region-p)
        (progn
          (setq -p1 (region-beginning))
          (setq -p2 (region-end)))
      (save-excursion
        (skip-chars-backward "-_A-Za-z0-9")
        (setq -p1 (point))
        (right-char)
        (skip-chars-forward "-_A-Za-z0-9")
        (setq -p2 (point))))
    (setq mark-active nil)
    (when (< -p1 (point))
      (goto-char -p1))
    (isearch-mode t)
    (isearch-yank-string (buffer-substring-no-properties -p1 -p2))))

(defun xah-open-in-desktop ()
  "Show current file in desktop (OS's file manager).
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2015-11-30"
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
    (w32-shell-execute "explore" (replace-regexp-in-string "/" "\\" default-directory t t)))
   ((string-equal system-type "darwin") (shell-command "open ."))
   ((string-equal system-type "gnu/linux")
    (let (
          (process-connection-type nil)
          (openFileProgram (if (file-exists-p "/usr/bin/gvfs-open")
                               "/usr/bin/gvfs-open"
                             "/usr/bin/xdg-open")))
      (start-process "" nil openFileProgram "."))
    ;; (shell-command "xdg-open .") ;; 2013-02-10 this sometimes froze emacs till the folder is closed. ⁖ with nautilus
    )))

(defun xah-open-in-external-app ()
  "Open the current file or dired marked files in external app.
The app is chosen from your OS's preference.
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2015-01-26"
  (interactive)
  (let* (
         (-file-list
          (if (string-equal major-mode "dired-mode")
              (dired-get-marked-files)
            (list (buffer-file-name))))
         (-do-it-p (if (<= (length -file-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))
    (when -do-it-p
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda (-fpath)
           (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" -fpath t t))) -file-list))
       ((string-equal system-type "darwin")
        (mapc
         (lambda (-fpath) (shell-command (format "open \"%s\"" -fpath)))  -file-list))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda (-fpath) (let ((process-connection-type nil))
                       (start-process "" nil "xdg-open" -fpath))) -file-list))))))

(defun xah-open-in-terminal ()
  "Open the current dir in a new terminal window.
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2015-12-10"
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
    (message "Microsoft Windows not supported. File a bug report or pull request."))
   ((string-equal system-type "darwin")
    (message "Mac not supported. File a bug report or pull request."))
   ((string-equal system-type "gnu/linux")
    (let ((process-connection-type nil))
      (start-process "" nil "x-terminal-emulator"
                     (concat "--working-directory=" default-directory) )))))

(defun xah-next-window-or-frame ()
  "Switch to next window or frame.
If current frame has only one window, switch to next frame.
If `universal-argument' is called first, do switch frame."
  (interactive)
  (if (null current-prefix-arg)
      (if (one-window-p)
          (other-frame 1)
        (other-window 1))
    (other-frame 1)))

(defun xah-describe-major-mode ()
  "Display inline doc for current `major-mode'."
  (interactive)
  (describe-function major-mode))



(defun xah-fly-map-keys (kmap-name key-cmd-alist)
  "similar to `define-key' but map over a alist."
  (interactive)
  (mapc
   (lambda (pair)
     (define-key kmap-name (kbd (car pair)) (cdr pair)))
   key-cmd-alist))
;;;;; keymaps

(defvar xah-fly-key-map nil "Keybinding for `xah-fly-keys' minor mode.")
(progn
  (setq xah-fly-key-map (make-sparse-keymap))
  ;; (define-key xah-fly-key-map (kbd "'") 'self-insert-command)
  )

;; commands in search-map and facemenu-keymap
(xah-fly-map-keys
 (define-prefix-command 'xah-highlight-keymap)
 '(("." . isearch-forward-symbol-at-point)
   ("b" . facemenu-set-bold)
   ("f" . font-lock-fontify-block)
   ("c" . center-line)
   ("d" . facemenu-set-default)
   ("h ." . highlight-symbol-at-point)
   ("h f" . hi-lock-find-patterns)
   ("h l" . highlight-lines-matching-regexp)
   ("h p" . highlight-phrase)
   ("h r" . highlight-regexp)
   ("h u" . unhighlight-regexp)
   ("h w" . hi-lock-write-interactive-patterns)
   ("i" . facemenu-set-italic)
   ("l" . facemenu-set-bold-italic)
   ("o" . facemenu-set-face)
   ("p" . center-paragraph)
   ("s" . isearch-forward-symbol)
   ("u" . facemenu-set-underline)
   ("w" . isearch-forward-word)))

(xah-fly-map-keys
 (define-prefix-command 'xah-leader-tab-keymap)
 '(
   ("TAB" . indent-for-tab-command)
   ;; ("TAB" . 'indent-or-complete)

   ("i" . complete-symbol)
   ("g" . indent-rigidly)
   ("r" . indent-region)
   ("s" . indent-sexp)

   ("e '" . abbrev-prefix-mark)
   ("e e" . edit-abbrevs)
   ("e p" . expand-abbrev)
   ("e r" . expand-region-abbrevs)
   ("e u" . unexpand-abbrev)
   ("e g" . add-global-abbrev)
   ("e a" . add-mode-abbrev)
   ("e v" . inverse-add-global-abbrev)
   ("e l" . inverse-add-mode-abbrev)
   ("e n" . expand-jump-to-next-slot)
   ("e p" . expand-jump-to-previous-slot)))



(xah-fly-map-keys
 (define-prefix-command 'xah-help-keymap)
 '(
   (";" . Info-goto-emacs-command-node)
   ("." . find-function-on-key)
   ("a" . apropos-command)
   ("b" . counsel-descbinds)
   ("B" . describe-buffer)
   ("c" . describe-char)
   ("d" . apropos-documentation)
   ("D" . discover-my-major)
   ("e" . view-echo-area-messages)
   ("f" . counsel-describe-function)
   ("F" . describe-file)
   ("g" . info-lookup-symbol)
   ;; ("h" . describe-face)
   ("h" . hydra-helpful/body)
   ("i" . info)
   ("j" . man)
   ("k" . helpful-key)
   ;; ("k" . describe-key)
   ("K" . Info-goto-emacs-key-command-node)
   ("l" . view-lossage)
   ("m" . discover-my-mode)
   ("M" . describe-keymap)
   ("n" . apropos-value)
   ("o" . describe-language-environment)
   ("p" . finder-by-keyword)
   ("r" . apropos-variable)
   ("s" . describe-syntax)
   ("t" . describe-input-thod)
   ("u" . elisp-index-search)
   ("v" . counsel-describe-variable)
   ("z" . describe-coding-system)))

(xah-fly-map-keys
 (define-prefix-command 'xah-leader-i-keymap) ; commands in goto-map
 '(
   ("TAB" . move-to-column)
   ("l" . goto-line-with-feedback)
   ("c" . go-to-column)
   ("j" . next-error)
   ("k" . previous-error
    )))

(xah-fly-map-keys
 ;; commands here are harmless (safe). They don't modify text.
 ;; they turn on minor/major mode, change display, prompt, start shell, etc.
 (define-prefix-command 'xah-harmless-keymap)
 '(
   ("SPC" . whitespace-mode)
   ("'" . frame-configuration-to-register)
   (";" . window-configuration-to-register)
   ("1" . set-input-method)
   ("2" . global-hl-line-mode)
   ("4" . linum-mode)
   ("5" . visual-line-mode)
   ("6" . calendar)
   ("7" . calc)
   ("8" . shell)
   ("9" . shell-command)
   ("0" . shell-command-on-region)
   ("a" . text-scale-adjust)
   ("b" . toggle-debug-on-error)
   ("c" . toggle-case-fold-search)
   ("d" . narrow-to-page)
   ("e" . eshell)
   ("g" . toggle-frame-fullscreen)
   ("h" . widen)
   ("i" . make-frame-command)
   ("k" . menu-bar-open)
   ("l" . toggle-word-wrap)
   ("m" . global-linum-mode)
   ("n" . narrow-to-region)
   ("p" . read-only-mode) ; toggle-read-only
   ("q n" . set-file-name-coding-system)
   ("q s" . set-next-selection-coding-system)
   ("q c" . universal-coding-system-argument)
   ("q f" . set-buffer-file-coding-system)
   ("q k" . set-keyboard-coding-system)
   ("q l" . set-language-environment)
   ("q p" . set-buffer-process-coding-system)
   ("q r" . revert-buffer-with-coding-system)
   ("q t" . set-terminal-coding-system)
   ("q x" . set-selection-coding-system)
   ("s" . flyspell-buffer)
   ("t" . narrow-to-defun)
   ("v" . variable-pitch-mode)
   ("w" . eww)
   ("x" . save-some-buffers)
   ("z" . abort-recursive-edit)))

(xah-fly-map-keys
 ;; kinda replacement related
 (define-prefix-command 'xah-edit-cmds-keymap)
 '(
   ("SPC" . rectangle-mark-mode)
   ("9" . delete-non-matching-lines)
   ("0" . delete-duplicate-lines)
   ("," . apply-macro-to-region-lines)
   ("." . kmacro-start-macro)
   ("p" . kmacro-end-macro)
   ("e" . call-last-kbd-macro)
   ("c" . replace-rectangle)
   ("d" . delete-rectangle)
   ("g" . kill-rectangle)
   ("h" . list-matching-lines)
   ("l" . clear-rectangle)
   ("n" . rectangle-number-lines)
   ("o" . open-rectangle)
   ("r" . yank-rectangle)
   ("t" . delete-matching-lines)
   ("y" . delete-whitespace-rectangle)))

(xah-fly-map-keys
 (define-prefix-command 'xah-leader-t-keymap)
 '(
   ("SPC" . xah-clean-whitespace)
   ("." . sort-lines)
   ("," . sort-numeric-fields)
   ("'" . reverse-region)
   ("p" . point-to-register)
   ("j" . jump-to-register)
   ("y" . copy-to-register)
   ("i" . insert-register)
   ("l" . increment-register)
   ("r" . copy-rectangle-to-register)
   ("n" . number-to-register)
   ("m" . xah-make-backup-and-save)
   ("w" . xah-next-window-or-frame)))

(xah-fly-map-keys
 (define-prefix-command 'xah-danger-keymap)
 '(
   ("." . eval-buffer)
   ("e" . eval-defun)
   ("m" . eval-last-sexp)
   ("p" . eval-expression)
   ("u" . eval-region)
   ("q" . save-buffers-kill-terminal)
   ("d" . delete-frame)
   ("j" . xah-run-current-file)))

(xah-fly-map-keys
 (define-prefix-command 'xah-comment-keymap)
 '(
   (";" . evilnc-comment-or-uncomment-lines)
   ("l" . evilnc-quick-comment-or-uncomment-to-the-line)
   ("y" . evilnc-copy-and-comment-lines)
   ("p" . evilnc-comment-or-uncomment-paragraphs)
   ("r" . comment-or-uncomment-region)
   ("v" . evilnc-toggle-invert-comment-line-by-line)))

(xah-fly-map-keys
 (define-prefix-command 'xah-insertion-keymap)
 '(
   ("RET" . insert-char)
   ("SPC" . xah-insert-unicode)
   ;; xah-insert-date
   ("b" . xah-insert-black-lenticular-bracket【】)
   ("c" . xah-insert-ascii-single-quote)
   ("d" . xah-insert-double-curly-quote“”)
   ("f" . xah-insert-emacs-quote)
   ("g" . xah-insert-ascii-double-quote)
   ("h" . xah-insert-brace) ; {}
   ("i" . xah-insert-curly-single-quote‘’)
   ("m" . xah-insert-corner-bracket「」)
   ("n" . xah-insert-square-bracket) ; []
   ("p" . xah-insert-single-angle-quote‹›)
   ("r" . xah-insert-tortoise-shell-bracket〔〕)
   ("s" . xah-insert-string-assignment)
   ("t" . xah-insert-paren)
   ("u" . xah-insert-greater-less)
   ("w" . xah-insert-angle-bracket〈〉)
   ("W" . xah-insert-double-angle-bracket《》)
   ("y" . xah-insert-double-angle-quote«»)))

(xah-fly-map-keys
 (define-prefix-command 'xah-projectile-keymap)
 '(
   ("d" . projectile-dired)
   ("f" . counsel-projectile-find-file)
   ("g" . counsel-ag-projectile)
   ("o" . projectile-find-other-file)
   ("h" . hydra-projectile/body)
   ("p" . (lambda () (interactive)
            (progn
              (projectile-switch-project)
              (delete-other-windows))))
   ("l" . magit-list-repositories)
   ("r" . projectile-run-shell-command-in-root)))

(xah-fly-map-keys
 (define-prefix-command 'search-replace-keymap)
 '(
   ("ad" . counsel-ag)
   ("ag" . counsel-git-grep)
   ("ap" . counsel-ag-projectile)
   ("as" . counsel-ag-project-symbol)
   ("qr" . anzu-query-replace)
   ("qx" . anzu-query-replace-regexp)
   ("qt" . anzu-query-replace-at-cursor-thing)
   ("qc" . anzu-replace-at-cursor-thing)
   ("rg" . counsel-projectile-rg)
   ("e" . hydra-engine/body)))

(progn
  (define-prefix-command 'xah-fly-leader-key-map)
  (define-key xah-fly-leader-key-map (kbd "SPC") 'which-key-abort)
  (define-key xah-fly-leader-key-map (kbd "RET") 'counsel-M-x)
  (define-key xah-fly-leader-key-map (kbd "TAB") xah-leader-tab-keymap)
  (define-key xah-fly-leader-key-map (kbd ".") xah-highlight-keymap)
  (define-key xah-fly-leader-key-map (kbd ";") 'xah-comment-keymap)
  (define-key xah-fly-leader-key-map (kbd "c") 'xah-leader-c-keymap)
  (define-key xah-fly-leader-key-map (kbd "h") 'xah-help-keymap)
  (define-key xah-fly-leader-key-map (kbd "i") 'xah-leader-i-keymap)
  (define-key xah-fly-leader-key-map (kbd "p") 'xah-projectile-keymap)
  ;; (define-key xah-fly-leader-key-map (kbd "n") 'xah-harmless-keymap)
  ;; (define-key xah-fly-leader-key-map (kbd "nt") 'my/neotree-toggle)
  ;; (define-key xah-fly-leader-key-map (kbd "nf") 'neotree-find)
  (define-key xah-fly-leader-key-map (kbd "ns") 'my/neotree-show)
  (define-key xah-fly-leader-key-map (kbd "o") 'my-org-prefix-map)
  (define-key xah-fly-leader-key-map (kbd "r") 'xah-edit-cmds-keymap)
  (define-key xah-fly-leader-key-map (kbd "s") 'search-replace-keymap)
  (define-key xah-fly-leader-key-map (kbd "t") 'xah-leader-t-keymap)
  (define-key xah-fly-leader-key-map (kbd "w") 'xah-danger-keymap))

;;;;; setting keys

(progn
  (when xah-fly-use-control-key
    (progn
      (define-key xah-fly-key-map (kbd "<C-next>") 'xah-next-user-buffer)
      ;; (define-key xah-fly-key-map (kbd "<C-prior>") 'xah-previous-user-buffer)
      ;; (define-key xah-fly-key-map (kbd "<C-tab>") 'xah-next-user-buffer)
      ;; (define-key xah-fly-key-map (kbd "<C-S-iso-lefttab>") 'xah-previous-user-buffer)

      ;; (define-key xah-fly-key-map (kbd "C-v") 'yank)
      (define-key xah-fly-key-map (kbd "C-z") 'undo)
      ;; (define-key xah-fly-key-map (kbd "C-o") 'find-file)
      ;; (define-key xah-fly-key-map (kbd "C-s") 'isearch-forward)
      (define-key xah-fly-key-map (kbd "C-s") 'swiper)
      (define-key xah-fly-key-map (kbd "C-r") 'swiper-the-thing)

      (define-key xah-fly-key-map (kbd "C-S-s") 'write-file)
      (define-key xah-fly-key-map (kbd "C-S-t") 'xah-open-last-closed)
      (define-key xah-fly-key-map (kbd "C-S-n") 'make-frame-command)

      (define-key xah-fly-key-map (kbd "C-+") 'text-scale-increase)
      (define-key xah-fly-key-map (kbd "C--") 'text-scale-decrease)
      (define-key xah-fly-key-map (kbd "C-0") (lambda () (interactive) (text-scale-set 0)))))

  (define-key xah-fly-key-map (kbd "<f7>") 'xah-pop-local-mark-ring)
  ;; (define-key xah-fly-key-map (kbd "<S-f2>") 'pop-global-mark)
  ;; (define-key xah-fly-key-map (kbd "M-RET") 'xah-cycle-hyphen-underscore-space)
  (define-key xah-fly-key-map (kbd "M-c") 'hydra-change-case/body)
  (define-key xah-fly-key-map (kbd "M-m") 'lispy-mark-symbol)
  (define-key xah-fly-key-map (kbd "M-d") 'lispy-kill-word)
  (define-key xah-fly-key-map (kbd "C-,") 'lispy-kill-at-point)
  ;; (define-key xah-fly-key-map (kbd "C-k") 'lispy-kill)
  (define-key xah-fly-key-map (kbd "C-y") 'lispy-yank)
  ;; (define-key xah-fly-key-map (kbd "C-d") 'lispy-delete)
  (define-key xah-fly-key-map (kbd "C-d") 'lispy-delete-or-splice-or-slurp)
  ;; (define-key xah-fly-key-map (kbd "C-e") 'lispy-move-end-of-line)
  (define-key xah-fly-key-map (kbd "M-SPC") 'major-mode-hydra)
  ;; (define-key xah-fly-key-map (kbd "M-SPC") 'xah-fly-command-mode-activate)
  (define-key xah-fly-key-map (kbd "DEL") 'my-lispy-hungry-delete)
  (define-key xah-fly-key-map (kbd "<home>") 'xah-fly-command-mode-activate)
  (define-key xah-fly-key-map (kbd "<menu>") 'xah-fly-command-mode-activate)
  ;; (define-key xah-fly-key-map (kbd "<f5>") 'my/neotree-toggle)
  (define-key xah-fly-key-map (kbd "<f6>") 'modi/imenu-list-display-toggle)
  (define-key xah-fly-key-map (kbd "<f8>") 'xah-fly-command-mode-activate)
  ;; (define-key xah-fly-key-map (kbd "<f9>") xah-fly-leader-key-map)
  ;; (define-key xah-fly-key-map (kbd "<f9>") 'xah-copy-to-register-1)
  ;; (define-key xah-fly-key-map (kbd "<f10>") 'xah-paste-from-register-1)
  (when *is-gnu-linux*
    (define-key xah-fly-key-map (kbd "<f9>") 'linux-copy)
    (define-key xah-fly-key-map (kbd "<f10>") 'linux-paste))
  ;; (define-key xah-fly-key-map (kbd "<f11>") 'xah-previous-user-buffer)
  ;; (define-key xah-fly-key-map (kbd "<f12>") 'xah-next-user-buffer)
  ;; (define-key xah-fly-key-map (kbd "<f12>") 'nswbuff-switch-to-next-buffer)
  ;; (define-key xah-fly-key-map (kbd "<f11>") 'nswbuff-switch-to-previous-buffer)

  ;; (define-key xah-fly-key-map (kbd "<f12>") 'centaur-tabs-forward)
  ;; (define-key xah-fly-key-map (kbd "<f11>") 'centaur-tabs-backward)
  (define-key xah-fly-key-map (kbd "<C-f12>") 'centaur-tabs-forward)
  (define-key xah-fly-key-map (kbd "<C-f11>") 'centaur-tabs-backward)
  ;; (define-key xah-fly-key-map (kbd "<C-f11>") 'xah-previous-emacs-buffer)
  ;; (define-key xah-fly-key-map (kbd "<C-f12>") 'xah-next-emacs-buffer)
  ;; (define-key xah-fly-key-map (kbd "<C-f11>") 'xah-previous-user-buffer)
  ;; (define-key xah-fly-key-map (kbd "<C-f12>") 'xah-next-user-buffer)
  (define-key xah-fly-key-map (kbd "<f11>") 'xah-previous-user-buffer)
  (define-key xah-fly-key-map (kbd "<f12>") 'xah-next-user-buffer)
  ;; (define-key isearch-mode-map (kbd "<up>") 'isearch-ring-retreat)
  ;; (define-key isearch-mode-map (kbd "<down>") 'isearch-ring-advance)
  (define-key isearch-mode-map (kbd "<left>") 'isearch-repeat-backward)
  (define-key isearch-mode-map (kbd "<right>") 'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "M-i") 'iedit-mode)
  (define-key minibuffer-local-isearch-map (kbd "<left>") 'isearch-reverse-exit-minibuffer)
  (define-key minibuffer-local-isearch-map (kbd "<right>") 'isearch-forward-exit-minibuffer)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
  (define-key isearch-mode-map (kbd "C-p") 'isearch-repeat-backward)
  (define-key isearch-mode-map (kbd "C-n") 'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "<up>") 'isearch-ring-retreat)
  (define-key isearch-mode-map (kbd "<down>") 'isearch-ring-advance)
  (define-key minibuffer-local-isearch-map (kbd "<left>") 'isearch-reverse-exit-minibuffer)
  (define-key minibuffer-local-isearch-map (kbd "<right>") 'isearch-forward-exit-minibuffer)

  (defun isearch-enable-key-chord ()
    (key-chord-mode 1)
    (key-chord-define isearch-mode-map "jk" 'isearch-cancel))
  (add-hook 'isearch-mode-hook 'isearch-enable-key-chord)
  ;;
  )

(defvar xah-fly-insert-state-q t "Boolean value. true means insertion mode is on.")
(setq xah-fly-insert-state-q t)

;;;;; command mode keys

(defun xah-fly-command-mode-init ()
  "set command mode keys"
  (interactive)
  (progn
    (define-key xah-fly-key-map (kbd "'") nil)
    (define-key xah-fly-key-map (kbd ",") nil)
    (define-key xah-fly-key-map (kbd ".") 'backward-kill-word)
    (define-key xah-fly-key-map (kbd ";") 'lispy-comment)
    ;; (define-key xah-fly-key-map (kbd ":") nil)
    ;; (define-key xah-fly-key-map (kbd "/") 'swiper)
    ;; (define-key xah-fly-key-map (kbd "?") 'swiper-the-thing)
    (define-key xah-fly-key-map (kbd "/") 'isearch-forward)
    (define-key xah-fly-key-map (kbd "?") 'isearch-backward)
    (define-key xah-fly-key-map (kbd "\\") nil)
    (define-key xah-fly-key-map (kbd "=") #'hydra-expand-region/body)
    (define-key xah-fly-key-map (kbd "[") 'xah-backward-left-bracket)
    (define-key xah-fly-key-map (kbd "]") 'xah-forward-right-bracket)
    ;; (define-key xah-fly-key-map (kbd "`") 'other-frame)
    (define-key xah-fly-key-map (kbd "`") 'nameframe-switch-frame)
    (define-key xah-fly-key-map (kbd "~") nil)
    (define-key xah-fly-key-map (kbd "SPC") xah-fly-leader-key-map)
    (define-key xah-fly-key-map (kbd "*") 'xah-search-current-word)
    (define-key xah-fly-key-map (kbd "(") 'lispy-parens)
    (define-key xah-fly-key-map (kbd "{") 'lispy-braces)
    (define-key xah-fly-key-map (kbd "}") 'lispy-brackets)
    (define-key xah-fly-key-map (kbd "\"") 'lispy-quotes)
    (define-key xah-fly-key-map (kbd ";") 'lispy-comment)
    (define-key xah-fly-key-map (kbd ")") 'lispy-right-nostring)
    (define-key xah-fly-key-map (kbd ">") 'lispy-slurp)
    (define-key xah-fly-key-map (kbd "<") 'lispy-barf)
    (define-key xah-fly-key-map (kbd "$") 'xah-end-of-line-or-block)
    (define-key xah-fly-key-map (kbd "-") nil)
    (define-key xah-fly-key-map (kbd "|") nil)
    (define-key xah-fly-key-map (kbd "a") 'xah-beginning-of-line-or-block)
    (define-key xah-fly-key-map (kbd "ba") '(lambda () (interactive)
                                              (progn
                                                (call-interactively 'ibuffer)
                                                (xah-fly-insert-mode-activate)
                                                (hydra-ibuffer-main/body))))
    (define-key xah-fly-key-map (kbd "bb") 'ivy-switch-buffer)
    (define-key xah-fly-key-map (kbd "bg") 'centaur-tabs-counsel-switch-group)
    (define-key xah-fly-key-map (kbd "bp") 'counsel-projectile-switch-to-buffer)
    (define-key xah-fly-key-map (kbd "bi") 'modi/imenu-list-display-toggle)
    (define-key xah-fly-key-map (kbd "bk") 'xah-close-current-buffer)
    (define-key xah-fly-key-map (kbd "bn") 'xah-new-empty-buffer)
    (define-key xah-fly-key-map (kbd "bc") 'new-same-mode-buffer)
    (define-key xah-fly-key-map (kbd "bs") 'save-buffer)
    (define-key xah-fly-key-map (kbd "bl") '(lambda () (interactive)
                                              (progn
                                                (call-interactively 'projectile-ibuffer)
                                                (xah-fly-insert-mode-activate)
                                                (hydra-ibuffer-main/body))))
    (define-key xah-fly-key-map (kbd "bm") 'hydra-bm/body)
    (define-key xah-fly-key-map (kbd "bw") 'write-file)
    (define-key xah-fly-key-map (kbd "c") 'smex-major-mode-commands)
    ;; (define-key xah-fly-key-map (kbd "cc") 'hydra-cider-buddy-main/body)
    ;; (define-key xah-fly-key-map (kbd "cj") 'hydra-eclim/body)
    (define-key xah-fly-key-map (kbd "e") 'xah-end-of-line-or-block)
    (define-key xah-fly-key-map (kbd "dd") 'xah-delete-current-line)
    (define-key xah-fly-key-map (kbd "dr") 'xah-cut-all-or-region)
    (define-key xah-fly-key-map (kbd "dw") 'ivy-kill-word)
    (define-key xah-fly-key-map (kbd "de") 'lispy-kill)
    (define-key xah-fly-key-map (kbd "d$") 'lispy-kill-sentence)
    (define-key xah-fly-key-map (kbd "d0") 'lispy-delete-backward)
    (define-key xah-fly-key-map (kbd "da") 'backward-kill-line)
    (define-key xah-fly-key-map (kbd "ds") 'kill-symbol)
    (define-key xah-fly-key-map (kbd "fc") 'nameframe-create-frame)
    (define-key xah-fly-key-map (kbd "fs") 'toggle-frame-fullscreen)
    (define-key xah-fly-key-map (kbd "fx") 'delete-frame)
    (define-key xah-fly-key-map (kbd "fr") 'counsel-recentf)
    (define-key xah-fly-key-map (kbd "fD") 'counsel-goto-recent-directory)
    (define-key xah-fly-key-map (kbd "fd") 'counsel-projectile-find-dir)
    (define-key xah-fly-key-map (kbd "ff") 'counsel-find-file)
    ;; (define-key xah-fly-key-map (kbd "ff") 'counsel-projectile-find-file)
    (define-key xah-fly-key-map (kbd "fp") 'helm-projectile)
    (define-key xah-fly-key-map (kbd "fo") 'counsel-outline)
    (define-key xah-fly-key-map (kbd "fj") '(lambda () (interactive)
                                              (xah-insert-mode-wrapper
                                               'dired-jump)))
    (define-key xah-fly-key-map (kbd "fJ") 'dired-jump-other-window)
    (define-key xah-fly-key-map (kbd "ft") 'treemacs)
    (define-key xah-fly-key-map (kbd "ge") 'hydra-flycheck/body)
    (define-key xah-fly-key-map (kbd "gn") 'hydra-smartscan/body)
    (define-key xah-fly-key-map (kbd "g,") 'goto-last-change-reverse)
    (define-key xah-fly-key-map (kbd "g;") 'goto-last-change)
    (define-key xah-fly-key-map (kbd "g3") 'point-to-register)
    (define-key xah-fly-key-map (kbd "g4") 'jump-to-register)
    (define-key xah-fly-key-map (kbd "gs") 'magit-status)
    (define-key xah-fly-key-map (kbd "gt") 'git-timemachine-hydra/body)
    (define-key xah-fly-key-map (kbd "gh") 'hydra-git-gutter/body)
    (define-key xah-fly-key-map (kbd "gf") 'find-file-at-point)
    (define-key xah-fly-key-map (kbd "g.") 'elisp-slime-nav-find-elisp-thing-at-point)
    (define-key xah-fly-key-map (kbd "G") 'end-of-buffer)
    (define-key xah-fly-key-map (kbd "gg") 'beginning-of-buffer)
    (define-key xah-fly-key-map (kbd "go") 'hydra-avy/body)
    (define-key xah-fly-key-map (kbd "gr") 'hydra-register/body)
    (define-key xah-fly-key-map (kbd "gi") '(lambda () (interactive)
                                              (progn
                                                (imenu-list-goto-entry)
                                                (switch-to-buffer-other-window "*Ilist*")
                                                (xah-fly-insert-mode-activate)
                                                )))
    (define-key xah-fly-key-map (kbd "gd") 'my-jump-to-elisp-docs)
    (define-key xah-fly-key-map (kbd "gx") 'counsel-osx-app)
    (define-key xah-fly-key-map (kbd "gl") 'link-hint-open-link)
    (define-key xah-fly-key-map (kbd "gL") 'link-hint-open-multiple-links)
    (define-key xah-fly-key-map (kbd "gm") 'pop-global-mark)

    (define-key xah-fly-key-map (kbd "j") 'next-line)
    (define-key xah-fly-key-map (kbd "h") 'backward-char)
    (define-key xah-fly-key-map (kbd "i") 'xah-fly-insert-mode-activate)
    (define-key xah-fly-key-map (kbd "k") 'previous-line)
    (define-key xah-fly-key-map (kbd "J") 'outline-next-visible-heading)
    (define-key xah-fly-key-map (kbd "K") 'outline-previous-visible-heading)
    (define-key xah-fly-key-map (kbd "H") 'outline-hide-more)
    (define-key xah-fly-key-map (kbd "I") 'outline-cycle)
    (define-key xah-fly-key-map (kbd "L") 'outline-show-more)
    (define-key xah-fly-key-map (kbd "m") 'set-mark-command)
    (define-key xah-fly-key-map (kbd "M") 'helm-all-mark-rings)
    (define-key xah-fly-key-map (kbd "nd") 'fancy-narrow-to-defun)
    (define-key xah-fly-key-map (kbd "nr") 'fancy-narrow-to-region)
    (define-key xah-fly-key-map (kbd "np") 'fancy-narrow-to-page)
    (define-key xah-fly-key-map (kbd "nb") 'org-fancy-narrow-to-block)
    (define-key xah-fly-key-map (kbd "nw") 'fancy-widen)
    (define-key xah-fly-key-map (kbd "ne") 'org-fancy-narrow-to-element)
    (define-key xah-fly-key-map (kbd "ns") 'org-fancy-narrow-to-subtree)
    (define-key xah-fly-key-map (kbd "nf") 'hydra-vimish-fold/body)
    (define-key xah-fly-key-map (kbd "l") 'forward-char)
    (define-key xah-fly-key-map (kbd "o") 'smart-open-line)
    (define-key xah-fly-key-map (kbd "O") 'open-line-above)
    (define-key xah-fly-key-map (kbd "p") 'yank)
    (define-key xah-fly-key-map (kbd "P") 'counsel-yank-pop)
    (define-key xah-fly-key-map (kbd "q") 'my-quit-dwim)
    (define-key xah-fly-key-map (kbd "r") 'undo-tree-redo)
    (define-key xah-fly-key-map (kbd "s") 'embrace-commander)
    (define-key xah-fly-key-map (kbd "t") 'evilmi-jump-items)
    (define-key xah-fly-key-map (kbd "T") 'hydra-switch-tab/body)
    (define-key xah-fly-key-map (kbd "u") 'undo-tree-undo)
    ;; (if (fboundp 'tabbar-mode)
    ;;     (define-key xah-fly-key-map (kbd "v") 'hydra-switch-tab/body)
    ;;   (define-key xah-fly-key-map (kbd "v") 'hydra-view-buffer/body))

    (define-key xah-fly-key-map (kbd "v") 'hydra-torus/body)
    ;; (define-key xah-fly-key-map (kbd "vac") 'torus-add-circle)
    ;; (define-key xah-fly-key-map (kbd "val") 'torus-add-location)
    ;; (define-key xah-fly-key-map (kbd "vdl") 'torus-delete-location)
    ;; (define-key xah-fly-key-map (kbd "vdc") 'torus-delete-circle)
    ;; (define-key xah-fly-key-map (kbd "vj") 'torus-previous-location)
    ;; (define-key xah-fly-key-map (kbd "vk") 'torus-next-location)
    ;; (define-key xah-fly-key-map (kbd "vh") 'torus-previous-circle)
    ;; (define-key xah-fly-key-map (kbd "vl") 'torus-next-circle)
    ;; (define-key xah-fly-key-map (kbd "vw") 'torus-history-newer)
    ;; (define-key xah-fly-key-map (kbd "vs") 'torus-history-older)
    ;; (define-key xah-fly-key-map (kbd "vvc") 'torus-switch-circle)
    ;; (define-key xah-fly-key-map (kbd "vvl") 'torus-switch-location)
    ;; (define-key xah-fly-key-map (kbd "vvt") 'torus-switch-torus)
    ;; (define-key xah-fly-key-map (kbd "v/") 'torus-search)
    ;; (define-key xah-fly-key-map (kbd "vz") 'torus-alternate)
    ;; (define-key xah-fly-key-map (kbd "v|") 'torus-split-horizontally)
    ;; (define-key xah-fly-key-map (kbd "v-") 'torus-split-vertically)

    (define-key xah-fly-key-map (kbd "ww") 'ace-window)
    (define-key xah-fly-key-map (kbd "wd") 'delete-window)
    (define-key xah-fly-key-map (kbd "wx") 'ace-delete-window)
    (define-key xah-fly-key-map (kbd "wm") 'ace-maximize-window)
    (define-key xah-fly-key-map (kbd "wo") 'delete-other-windows)
    (define-key xah-fly-key-map (kbd "wt") 'hydra-transpose-frame/body)
    (define-key xah-fly-key-map (kbd "wh") 'split-window-horizontally)
    (define-key xah-fly-key-map (kbd "wv") 'split-window-vertically)
    (define-key xah-fly-key-map (kbd "wf") 'aw-flip-window)
    (define-key xah-fly-key-map (kbd "wi") 'hydra-winner/body)
    (define-key xah-fly-key-map (kbd "ws") 'ace-swap-window)
    (define-key xah-fly-key-map (kbd "wp") 'hydra-perspective/body)
    (define-key xah-fly-key-map (kbd "wn") 'eyebrowse-switch-to-window-config)
    (define-key xah-fly-key-map (kbd "W") 'fancy-widen)
    (define-key xah-fly-key-map (kbd "x") 'hungry-delete-forward)
    (define-key xah-fly-key-map (kbd "X") 'hungry-delete-backward)
    ;; (define-key xah-fly-key-map (kbd "y") 'easy-kill)
    (define-key xah-fly-key-map (kbd "y") 'composable-copy-region-as-kill)
    (define-key xah-fly-key-map (kbd "z") 'zop-to-char)
    (define-key xah-fly-key-map (kbd ":r") 'restart-emacs)
    (define-key xah-fly-key-map (kbd ":q") 'save-buffers-kill-terminal)
    (define-key xah-fly-key-map (kbd ":set nu") 'linum-mode)
    (define-key xah-fly-key-map (kbd ":t") 'emacs-init-time)))

;;;;; insert mode keys
(defun xah-fly-insert-mode-init ()
  "Set insertion mode keys"
  (interactive)
  (progn
    (define-key xah-fly-key-map (kbd "'") nil)
    (define-key xah-fly-key-map (kbd ",") nil)
    (define-key xah-fly-key-map (kbd "-") nil)
    (define-key xah-fly-key-map (kbd "|") nil)
    (define-key xah-fly-key-map (kbd ".") nil)
    (define-key xah-fly-key-map (kbd "/") nil)
    (define-key xah-fly-key-map (kbd "?") nil)
    (define-key xah-fly-key-map (kbd "$") nil)
    (define-key xah-fly-key-map (kbd ";") nil)
    (define-key xah-fly-key-map (kbd ":") nil)
    (define-key xah-fly-key-map (kbd "=") nil)
    (define-key xah-fly-key-map (kbd "[") nil)
    (define-key xah-fly-key-map (kbd "\\") nil)
    (define-key xah-fly-key-map (kbd "]") nil)
    (define-key xah-fly-key-map (kbd "`") nil)
    (define-key xah-fly-key-map (kbd "~") nil)
    (define-key xah-fly-key-map (kbd "SPC") nil)
    (define-key xah-fly-key-map (kbd "*") nil)
    ;; (define-key xah-fly-key-map (kbd "(") 'lispy-parens)
    ;; (define-key xah-fly-key-map (kbd "{") 'lispy-braces)
    ;; (define-key xah-fly-key-map (kbd "}") 'lispy-brackets)
    (define-key xah-fly-key-map (kbd "(") 'lispy-parens-auto-wrap)
    (define-key xah-fly-key-map (kbd "{") 'lispy-braces-auto-wrap)
    (define-key xah-fly-key-map (kbd "}") 'lispy-brackets-auto-wrap)
    (define-key xah-fly-key-map (kbd "\"") 'lispy-quotes)
    (define-key xah-fly-key-map (kbd ";") nil)
    (define-key xah-fly-key-map (kbd ")") 'lispy-right-nostring)
    (define-key xah-fly-key-map (kbd ">") nil)
    (define-key xah-fly-key-map (kbd "<") nil)

    (define-key xah-fly-key-map (kbd "1") nil)
    (define-key xah-fly-key-map (kbd "2") nil)
    (define-key xah-fly-key-map (kbd "3") nil)
    (define-key xah-fly-key-map (kbd "4") nil)
    (define-key xah-fly-key-map (kbd "5") nil)
    (define-key xah-fly-key-map (kbd "6") nil)
    (define-key xah-fly-key-map (kbd "7") nil)
    (define-key xah-fly-key-map (kbd "8") nil)
    (define-key xah-fly-key-map (kbd "9") nil)
    (define-key xah-fly-key-map (kbd "0") nil)

    (define-key xah-fly-key-map (kbd "a") nil)
    (define-key xah-fly-key-map (kbd "b") nil)
    (define-key xah-fly-key-map (kbd "c") nil)
    (define-key xah-fly-key-map (kbd "d") nil)
    (define-key xah-fly-key-map (kbd "e") nil)
    (define-key xah-fly-key-map (kbd "F") nil)
    (define-key xah-fly-key-map (kbd "f") nil)
    (define-key xah-fly-key-map (kbd "g") nil)
    (define-key xah-fly-key-map (kbd "G") nil)
    (define-key xah-fly-key-map (kbd "h") nil)
    (define-key xah-fly-key-map (kbd "i") nil)
    (define-key xah-fly-key-map (kbd "j") nil)
    (define-key xah-fly-key-map (kbd "k") nil)
    (define-key xah-fly-key-map (kbd "l") nil)
    (define-key xah-fly-key-map (kbd "H") nil)
    (define-key xah-fly-key-map (kbd "I") nil)
    (define-key xah-fly-key-map (kbd "J") nil)
    (define-key xah-fly-key-map (kbd "K") nil)
    (define-key xah-fly-key-map (kbd "L") nil)
    (define-key xah-fly-key-map (kbd "m") nil)
    (define-key xah-fly-key-map (kbd "M") nil)
    (define-key xah-fly-key-map (kbd "n") nil)
    (define-key xah-fly-key-map (kbd "N") nil)
    (define-key xah-fly-key-map (kbd "o") nil)
    (define-key xah-fly-key-map (kbd "O") nil)
    (define-key xah-fly-key-map (kbd "p") nil)
    (define-key xah-fly-key-map (kbd "P") nil)
    (define-key xah-fly-key-map (kbd "q") nil)
    (define-key xah-fly-key-map (kbd "r") nil)
    (define-key xah-fly-key-map (kbd "s") nil)
    (define-key xah-fly-key-map (kbd "t") nil)
    (define-key xah-fly-key-map (kbd "T") nil)
    (define-key xah-fly-key-map (kbd "u") nil)
    (define-key xah-fly-key-map (kbd "v") nil)
    (define-key xah-fly-key-map (kbd "V") nil)
    (define-key xah-fly-key-map (kbd "w") nil)
    (define-key xah-fly-key-map (kbd "W") nil)
    (define-key xah-fly-key-map (kbd "x") nil)
    (define-key xah-fly-key-map (kbd "X") nil)
    (define-key xah-fly-key-map (kbd "y") nil)
    (define-key xah-fly-key-map (kbd "z") nil)))

(defun xah-fly-mode-toggle ()
  "Switch between {insertion, command} modes."
  (interactive)
  (if xah-fly-insert-state-q
      (xah-fly-command-mode-activate)
    (xah-fly-insert-mode-activate)))

;; automatic save buffer when switching to command mode
(add-hook 'xah-fly-command-mode-activate-hook 'xah-fly-save-buffer-if-file)

(defun xah-fly-save-buffer-if-file ()
  "Save current buffer if it is a file."
  (interactive)
  (when (buffer-file-name)
    (save-buffer)))

(defun xah-quit-window ()
  (interactive)
  (progn
    (quit-window)
    (xah-fly-command-mode-activate)))

(defun xah-fly-command-mode-activate ()
  "Activate command mode."
  (interactive)
  (modify-all-frames-parameters (list (cons 'cursor-type 'box)))
  (setq xah-fly-insert-state-q nil )
  (xah-fly-command-mode-init)
  (run-hooks 'xah-fly-command-mode-activate-hook))

(defun xah-fly-insert-mode-activate ()
  "Activate insertion mode."
  (interactive)
  (modify-all-frames-parameters (list (cons 'cursor-type 'bar)))
  (setq xah-fly-insert-state-q t )
  (xah-fly-insert-mode-init)
  (run-hooks 'xah-fly-insert-mode-activate-hook))

(defun xah-fly-insert-mode-activate-newline ()
  "Activate insertion mode, insert newline below."
  (interactive)
  (xah-fly-insert-mode-activate)
  (open-line 1))

(defun xah-fly-insert-mode-activate-space-before ()
  "Insert a space, then activate insertion mode."
  (interactive)
  (insert " ")
  (xah-fly-insert-mode-activate))

(defun xah-fly-insert-mode-activate-space-after ()
  "Insert a space, then activate insertion mode."
  (interactive)
  (insert " ")
  (xah-fly-insert-mode-activate)
  (left-char))

(defun lispy-mode-activate ()
  "Enable lispy mode for selected major modes only"
  (when (memq major-mode '(emacs-lisp-mode clojure-mode
                                           clojurescript-mode
                                           cider-repl-mode))
    (lispy-mode 1)))

(defun hydra-commands-activate ()
  "Enable command key according to major mode"
  (cond
   ((memq major-mode '(clojure-mode clojurescript-mode
                                    cider-repl-mode
                                    cider-docview-mode
                                    cider-stacktrace-mode
                                    clojurec-mode
                                    clojure-mode
                                    clojurex-mode))
    (define-key xah-fly-key-map (kbd "c") 'hydra-cider-main/body))
   ((memq major-mode '(java-mode))
    (define-key xah-fly-key-map (kbd "c") 'hydra-eclim/body))
   (t (define-key xah-fly-key-map (kbd "c") 'counsel-M-x))))

(defvar hydra-command-activate-buffers
  (make-ring 20))
(ring-insert hydra-command-activate-buffers "dummy")

(defadvice select-window (after hydra-command-activate-mode activate)
  (unless (equal (buffer-name) (ring-ref hydra-command-activate-buffers 0))
    (unless xah-fly-insert-state-q
      (ring-insert hydra-command-activate-buffers (buffer-name))
      (hydra-commands-activate))))

(eval-after-load "lispy"
  `(progn
     ;; replace a global binding with major-mode's default
     ;; (define-key lispy-mode-map (kbd "C-j") nil)
     (define-key lispy-mode-map (kbd "[") nil)
     (define-key lispy-mode-map (kbd "M-n") nil)
     (define-key lispy-mode-map (kbd "M-h") 'lispy-left)

     (setq lispy-safe-copy t
           lispy-safe-delete t
           lispy-safe-paste t)

     ;; replace a local binding
     (lispy-define-key lispy-mode-map "t" 'lispy-different)
     (lispy-define-key lispy-mode-map "d" 'lispy-delete)
     (lispy-define-key lispy-mode-map "p" 'lispy-paste)
     (lispy-define-key lispy-mode-map "y" 'lispy-new-copy)
     (lispy-define-key lispy-mode-map "X" 'lispy-splice)
     (lispy-define-key lispy-mode-map "n" 'lispy-occur)
     (lispy-define-key lispy-mode-map "P" 'lispy-eval-other-window)))

(defun xah-command-mode-wrapper (fun)
  (interactive)
  (progn
    (xah-fly-command-mode-activate)
    (funcall fun)))

(defun xah-insert-mode-wrapper (fun)
  (interactive)
  (progn
    (xah-fly-insert-mode-activate)
    (funcall fun)))

;; when in going into minibuffer, switch to insertion mode.
(add-hook 'minibuffer-setup-hook 'xah-fly-insert-mode-activate)
(add-hook 'minibuffer-exit-hook 'xah-fly-command-mode-activate)

(add-hook 'helm-minibuffer-setup-hook 'xah-fly-insert-mode-activate)
(add-hook 'helm-minibuffer-exit-hook 'xah-fly-command-mode-activate)

;; when in shell mode,switch to insertion mode.
(add-hook 'shell-mode-hook 'xah-fly-insert-mode-activate)
(add-hook 'eshell-mode-hook 'xah-fly-insert-mode-activate)
(add-hook 'calendar-mode-hook 'xah-fly-insert-mode-activate)
(add-hook 'xah-fly-command-mode-activate-hook '(lambda () (lispy-mode 0)))
(add-hook 'xah-fly-command-mode-activate-hook 'hydra-commands-activate)
(add-hook 'xah-fly-insert-mode-activate-hook 'lispy-mode-activate)

(define-minor-mode xah-fly-keys
  "A modal keybinding set, like vim, but based on ergonomic principles, like Dvorak layout."
  t "ξflykeys" xah-fly-key-map
  (xah-fly-command-mode-activate))

(defun xah-fly-keys-off ()
  "Turn off xah-fly-keys minor mode."
  (interactive)
  (xah-fly-keys 0))

(provide 'xah-fly-keys)

;; Variables Local:
;; coding: utf-8
;; End:

;;; xah-fly-keys.el ends here
