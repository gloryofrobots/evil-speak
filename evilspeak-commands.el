(require 'emacspeak)
(require 'evil)

(defun evilspeak-speak-line (&optional count)
  (emacspeak-speak-line count))

(defun evilspeak-speak-char ()
  (emacspeak-speak-char t))

(defun evilspeak-speak-word ()
  (emacspeak-speak-word))

(defun evilspeak-speak-paragraph ()
  (emacspeak-speak-paragraph))

(defun evilspeak-speak-long-jump ()
  (emacspeak-speak-line))

(defun evilspeak-speak-jump ()
  (emacspeak-speak-line))

(defun evilspeak-speak-region (arg)
  (emacspeak-speak-region arg))

(evil-define-motion evilspeak-next-line (count)
  "Move the cursor COUNT lines down."
  :type line
  (evil-next-line count)
  (evilspeak-speak-line))

(evil-define-motion evilspeak-previous-line (count)
  "Move the cursor COUNT lines up."
  :type line
  (evil-previous-line count)
  (evilspeak-speak-line))

;;; Code:
(evil-define-motion evilspeak-forward-char (count &optional crosslines noerror)
  "Move cursor to the right by COUNT characters.
Movement is restricted to the current line unless CROSSLINES is non-nil.
If NOERROR is non-nil, don't signal an error upon reaching the end
of the line or the buffer; just return nil."
  :type exclusive
  (evil-forward-char count crosslines noerror)
  (evilspeak-speak-char))

(evil-define-motion evilspeak-backward-char (count &optional crosslines noerror)
  "Move cursor to the left by COUNT characters.
Movement is restricted to the current line unless CROSSLINES is non-nil.
If NOERROR is non-nil, don't signal an error upon reaching the beginning
of the line or the buffer; just return nil."
  :type exclusive
  (evil-backward-char count crosslines noerror)
  (evilspeak-speak-char))


(evil-define-motion evilspeak-next-visual-line (count)
  "Move the cursor COUNT screen lines down."
  :type exclusive
  (evil-next-visual-line count)
  (evilspeak-speak-line))

(evil-define-motion evilspeak-previous-visual-line (count)
  "Move the cursor COUNT screen lines down."
  :type exclusive
  (evil-previous-visual-line count)
  (evilspeak-speak-line))

;; used for repeated commands like "dd"
(evil-define-motion evilspeak-line (count)
  "Move COUNT - 1 lines down."
  :type line
  (evil-line count)
  (evilspeak-speak-line))

(evil-define-motion evilspeak-beginning-of-line ()
  "Move the cursor to the beginning of the current line."
  :type exclusive
  (evil-beginning-of-line)
  (evilspeak-speak-line))

(evil-define-motion evilspeak-end-of-line (count)
  "Move the cursor to the end of the current line.
If COUNT is given, move COUNT - 1 lines downward first."
  :type inclusive
  (evil-end-of-line)
  (evilspeak-speak-line))

(evil-define-motion evilspeak-beginning-of-visual-line ()
  "Move the cursor to the first character of the current screen line."
  :type exclusive
  (evil-beginning-of-visual-line)
  (evilspeak-speak-line))

(evil-define-motion evilspeak-end-of-visual-line (count)
  "Move the cursor to the last character of the current screen line.
If COUNT is given, move COUNT - 1 screen lines downward first."
  :type inclusive
  (evil-end-of-visual-line count)
  (evilspeak-speak-line))

(evil-define-motion evilspeak-middle-of-visual-line ()
  "Move the cursor to the middle of the current visual line."
  :type exclusive
  (evil-middle-of-visual-line)
  (evilspeak-speak-line))

(evil-define-motion evilspeak-beginning-of-line-or-digit-argument ()
  "Move the cursor to the beginning of the current line.
This function passes its command to `digit-argument' (usually a 0)
if it is not the first event."
  :type exclusive
  (evil-beginning-of-line-or-digit-argument)
  (evilspeak-speak-line))

(evil-define-motion evilspeak-first-non-blank ()
  "Move the cursor to the first non-blank character of the current line."
  :type exclusive
  (evil-first-non-blank)
  (evilspeak-speak-line))

(evil-define-motion evilspeak-last-non-blank (count)
  "Move the cursor to the last non-blank character of the current line.
If COUNT is given, move COUNT - 1 lines downward first."
  :type inclusive
  (evil-last-non-blank count)
  (evilspeak-speak-line))

(evil-define-motion evilspeak-first-non-blank-of-visual-line ()
  "Move the cursor to the first non blank character
of the current screen line."
  :type exclusive
  (evil-first-non-blank-of-visual-line)
  (evilspeak-speak-line))

(evil-define-motion evilspeak-next-line-first-non-blank (count)
  "Move the cursor COUNT lines down on the first non-blank character."
  :type line
  (evil-next-line-first-non-blank count)
  (evilspeak-speak-line))

(evil-define-motion evilspeak-next-line-1-first-non-blank (count)
  "Move the cursor COUNT-1 lines down on the first non-blank character."
  :type line
  (evil-next-line-1-first-non-blank count)
  (evilspeak-speak-line))

(evil-define-motion evilspeak-previous-line-first-non-blank (count)
  "Move the cursor COUNT lines up on the first non-blank character."
  :type line
  (evil-previous-line-first-non-blank count)
  (evilspeak-speak-line))

(evil-define-motion evilspeak-goto-line (count)
  "Go to the first non-blank character of line COUNT.
By default the last line."
  :jump t
  :type line
  (evil-goto-line count)
  (evilspeak-speak-line))

(evil-define-motion evilspeak-goto-first-line (count)
  "Go to the first non-blank character of line COUNT.
By default the first line."
  :jump t
  :type line
  (evil-goto-first-line count)
  (evilspeak-speak-line))

(evil-define-motion evilspeak-forward-word-begin (count &optional bigword)
  :type exclusive
  (evil-forward-word-begin count bigword)
  (evilspeak-speak-word))

(evil-define-motion evilspeak-forward-word-end (count &optional bigword)
  "Move the cursor to the end of the COUNT-th next word.
If BIGWORD is non-nil, move by WORDS."
  :type inclusive
  (evil-forward-word-end count bigword)
  (evilspeak-speak-word))

(evil-define-motion evilspeak-backward-word-begin (count &optional bigword)
  "Move the cursor to the beginning of the COUNT-th previous word.
If BIGWORD is non-nil, move by WORDS."
  :type exclusive
  (evil-backward-word-begin count bigword)
  (evilspeak-speak-word))

(evil-define-motion evilspeak-backward-word-end (count &optional bigword)
  "Move the cursor to the end of the COUNT-th previous word.
If BIGWORD is non-nil, move by WORDS."
  :type inclusive
  (evil-backward-word-end count bigword)
  (evilspeak-speak-word))

(evil-define-motion evilspeak-forward-WORD-begin (count)
  "Move the cursor to the beginning of the COUNT-th next WORD."
  :type exclusive
  (evil-forward-WORD-begin count)
  (evilspeak-speak-word))

(evil-define-motion evilspeak-forward-WORD-end (count)
  "Move the cursor to the end of the COUNT-th next WORD."
  :type inclusive
  (evil-forward-WORD-end count)
  (evilspeak-speak-word))

(evil-define-motion evilspeak-backward-WORD-begin (count)
  "Move the cursor to the beginning of the COUNT-th previous WORD."
  :type exclusive
  (evil-backward-WORD-begin count)
  (evilspeak-speak-word))

(evil-define-motion evilspeak-backward-WORD-end (count)
  "Move the cursor to the end of the COUNT-th previous WORD."
  :type inclusive
  (evil-backward-WORD-end count)
  (evilspeak-speak-word))

;; section movement

(evil-define-motion evilspeak-forward-section-begin (count)
  "Move the cursor to the beginning of the COUNT-th next section."
  :jump t
  :type exclusive
  (evil-forward-section-begin count)
  (evilspeak-speak-line))

(evil-define-motion evilspeak-forward-section-end (count)
  "Move the cursor to the end of the COUNT-th next section."
  :jump t
  :type inclusive
  (evil-forward-section-end count)
  (evilspeak-speak-line))

(evil-define-motion evilspeak-backward-section-begin (count)
  "Move the cursor to the beginning of the COUNT-th previous section."
  :jump t
  :type exclusive
  (evil-backward-section-begin count)
  (evilspeak-speak-line))


(evil-define-motion evilspeak-backward-section-end (count)
  "Move the cursor to the end of the COUNT-th previous section."
  :jump t
  :type inclusive
  (evil-backward-section-end count)
  (evilspeak-speak-line))

(evil-define-motion evilspeak-forward-sentence-begin (count)
  "Move to the next COUNT-th beginning of a sentence or end of a paragraph."
  :jump t
  :type exclusive
  (evil-forward-section-begin count)
  (evilspeak-speak-line))

(evil-define-motion evilspeak-backward-sentence-begin (count)
  "Move to the previous COUNT-th beginning of a sentence or paragraph."
  :jump t
  :type exclusive
  (evil-backward-sentence-begin count)
  (evilspeak-speak-line))

(evil-define-motion evilspeak-forward-paragraph (count)
  "Move to the end of the COUNT-th next paragraph."
  :jump t
  :type exclusive
  (evil-forward-paragraph count)
  (evilspeak-speak-paragraph))

(evil-define-motion evilspeak-backward-paragraph (count)
  "Move to the beginning of the COUNT-th previous paragraph."
  :jump t
  :type exclusive
  (evil-backward-paragraph count)
  (evilspeak-speak-paragraph))

(evil-define-motion evilspeak-jump-item (count)
  "Find the next item in this line after or under the cursor
and jump to the corresponding one."
  :jump t
  :type inclusive
  (evil-jump-item count)
  (evilspeak-speak-word))

(evil-define-motion evilspeak-previous-open-paren (count)
  "Go to [count] previous unmatched '('."
  :type exclusive
  (evil-previous-open-paren count)
  (evilspeak-speak-char))

(evil-define-motion evilspeak-next-close-paren (count)
  "Go to [count] next unmatched ')'."
  :type exclusive
  (evil-next-close-paren count)
  (evilspeak-speak-char))

(evil-define-motion evilspeak-previous-open-brace (count)
  "Go to [count] previous unmatched '{'."
  :type exclusive
  (evil-previous-open-brace count)
  (evilspeak-speak-char))

(evil-define-motion evilspeak-next-close-brace (count)
  "Go to [count] next unmatched '}'."
  :type exclusive
  (evil-next-close-brace count)
  (evilspeak-speak-char))


(evil-define-motion evilspeak-find-char (count char)
  "Move to the next COUNT'th occurrence of CHAR."
  :type inclusive
  (evil-find-char count char)
  (evilspeak-speak-word))


(evil-define-motion evilspeak-find-char-backward (count char)
  "Move to the previous COUNT'th occurrence of CHAR."
  :type exclusive
  (evil-find-char-backward count char)
  (evilspeak-speak-word))

(evil-define-motion evilspeak-find-char-to (count char)
  "Move before the next COUNT'th occurrence of CHAR."
  :type inclusive
  (evil-find-char-to count char)
  (evilspeak-speak-word))

(evil-define-motion evilspeak-find-char-to-backward (count char)
  "Move before the previous COUNT'th occurrence of CHAR."
  :type exclusive
  (evil-find-char-to-backward count char)
  (evilspeak-speak-word))

(evil-define-motion evilspeak-repeat-find-char (count)
  "Repeat the last find COUNT times."
  :type inclusive
  (evil-repeat-find-char count)
  (evilspeak-speak-word))

(evil-define-motion evilspeak-repeat-find-char-reverse (count)
  "Repeat the last find COUNT times in the opposite direction."
  :type inclusive
  (evil-repeat-find-char-reverse count)
  (evilspeak-speak-word))

(evil-define-motion evilspeak-goto-column (count)
  "Go to column COUNT on the current line.
Columns are counted from zero."
  :type exclusive
  (evil-goto-column)
  (evilspeak-speak-line t))

(evil-define-command evilspeak-goto-mark (char &optional noerror)
  "Go to the marker specified by CHAR."
  :keep-visual t
  :repeat nil
  :type exclusive
  (evil-goto-mark char noerror)
  (evilspeak-speak-line t))

(evil-define-command evilspeak-goto-mark-line (char &optional noerror)
  "Go to the line of the marker specified by CHAR."
  :keep-visual t
  :repeat nil
  :type line
  (evil-goto-mark-line char noerror)
  (evilspeak-speak-line t))

(evil-define-motion evilspeak-jump-backward (count)
  "Go to older position in jump list."
  (evil-jump-backward count)
  (evilspeak-speak-line))

(evil-define-motion evilspeak-jump-forward (count)
  "Go to newer position in jump list.
To go the other way, press \
\\<evil-motion-state-map>\\[evil-jump-backward]."
  (evil-jump-forward count)
  (evilspeak-speak-long-jump))

(evil-define-motion evilspeak-jump-backward-swap (count)
  "Go to the previous position in jump list.
The current position is placed in the jump list."
  (evil-jump-backward-swap count)
  (evilspeak-speak-long-jump))

(evil-define-motion evilspeak-jump-to-tag (arg)
  "Jump to tag under point.
If called with a prefix argument, provide a prompt
for specifying the tag."
  :jump t
  (evil-jump-to-tag arg)
  (evilspeak-speak-long-jump))

(evil-define-motion evilspeak-lookup ()
  "Look up the keyword at point.
Calls `evil-lookup-func'."
  (evil-lookup)
  (evilspeak-speak-long-jump))


(evil-define-motion evilspeak-ret (count)
  "Move the cursor COUNT lines down.
If point is on a widget or a button, click on it.
In Insert state, insert a newline."
  :type line
  (evil-ret count)
  (evilspeak-speak-line))

(evil-define-motion evilspeak-ret-and-indent (count)
  "Move the cursor COUNT lines down.
If point is on a widget or a button, click on it.
In Insert state, insert a newline and indent."
  :type line
  (evil-ret-and-indent count)
  (evilspeak-speak-line))

(evil-define-motion evilspeak-window-top (count)
  "Move the cursor to line COUNT from the top of the window
on the first non-blank character."
  :jump t
  :type line
  (evil-window-top count)
  (evilspeak-speak-line))

(evil-define-motion evilspeak-window-middle ()
  "Move the cursor to the middle line in the window
on the first non-blank character."
  :jump t
  :type line
  (evil-window-middle)
  (evilspeak-speak-line))

(evil-define-motion evilspeak-window-bottom (count)
  "Move the cursor to line COUNT from the bottom of the window
on the first non-blank character."
  :jump t
  :type line
  (evil-window-bottom count)
  (evilspeak-speak-line))
;; scroll skipped
;; text-object skipped

;; ;;; Operator commands
;; TODO speak registry


(evil-define-operator evilspeak-delete (beg end type register yank-handler)
  "Delete text from BEG to END with TYPE.
Save in REGISTER or in the kill-ring with YANK-HANDLER."
  (evil-delete beg end type register yank-handler)
  (evilspeak-speak-line t))

(evil-define-operator evilspeak-delete-line
  (beg end type register yank-handler)
  "Delete to end of line."
  :motion nil
  :keep-visual t
  (evil-delete-line beg end type register yank-handler)
  (evilspeak-speak-line t))

(evil-define-operator evilspeak-delete-whole-line
  (beg end type register yank-handler)
  "Delete whole line."
  :motion evil-line
  (evil-delete-whole-line beg end type register yank-handler)
  (evilspeak-speak-line t))

(evil-define-operator evilspeak-delete-char (beg end type register)
  "Delete next character."
  :motion evil-forward-char
  (evil-delete-char beg end type register)
  (evilspeak-speak-char))

(evil-define-operator evilspeak-delete-backward-char (beg end type register)
  "Delete previous character."
  :motion evil-backward-char
  (evil-delete-backward-char beg end type register)
  (evilspeak-speak-char))

(evil-define-command evilspeak-delete-backward-char-and-join (count)
  "Delete previous character and join lines.
If point is at the beginning of a line then the current line will
be joined with the previous line if and only if
`evil-backspace-join-lines'."
  (evil-delete-backward-char-and-join count)
  (evilspeak-speak-word t))

(evil-define-command evilspeak-delete-backward-word ()
  (evil-delete-backward-word)
  (evilspeak-speak-word))


(evil-define-command evilspeak-move (beg end address)
  "Move lines in BEG END below line given by ADDRESS."
  :motion evil-line
  (evil-move beg end address)
  (evilspeak-speak-line))


(evil-define-operator evilspeak-upcase (beg end type)
  "Convert text to upper case."
  (evil-upcase beg end type)
  (evilspeak-speak-region))

(evil-define-operator evilspeak-downcase (beg end type)
  "Convert text to lower case."
  (evil-downcase beg end type)
  (evilspeak-speak-region))


(evil-define-operator evilspeak-join (beg end)
  "Join the selected lines."
  :motion evil-line
  (evil-join beg end)
  (evilspeak-speak-line))

(evil-define-operator evilspeak-join-whitespace (beg end)
  "Join the selected lines without changing whitespace.
\\<evil-normal-state-map>Like \\[evil-join], \
but doesn't insert or remove any spaces."
  :motion evil-line
  (evil-join-whitespace beg end)
  (evilspeak-speak-line))

(evil-define-operator evilspeak-fill (beg end)
  "Fill text."
  :move-point nil
  :type line
  (evil-fill beg end)
  (evilspeak-speak-region))

(evil-define-operator evilspeak-fill-and-move (beg end)
  "Fill text and move point to the end of the filled region."
  :move-point nil
  :type line
  (evil-fill-and-move beg end)
  (evilspeak-speak-region))

(evil-define-operator evilspeak-indent (beg end)
  "Indent text."
  :move-point nil
  :type line
  (evil-indent beg end)
  (evilspeak-speak-region))

(evil-define-operator evilspeak-indent-line (beg end)
  "Indent the line."
  :motion evil-line
  (evil-indent beg end)
  (evilspeak-speak-line))


(evil-define-command evilspeak-paste-before
  (count &optional register yank-handler)
  "Pastes the latest yanked text before the cursor position.
The return value is the yanked text."
  :suppress-operator t
  (evil-paste-before count register yank-handler)
  (evilspeak-speak-line))

(evil-define-command evilspeak-paste-after
  (count &optional register yank-handler)
  "Pastes the latest yanked text behind point.
The return value is the yanked text."
  :suppress-operator t
  (evil-paste-after count register yank-handler)
  (evilspeak-speak-line t))


;; ;;; Visual commands

;; ;;; Search

(evil-define-motion evilspeak-search-forward ()
  (format "Search forward for user-entered text.
Searches for regular expression if `evil-regexp-search' is t.%s"
          (if (and (fboundp 'isearch-forward)
                   (documentation 'isearch-forward))
              (format "\n\nBelow is the documentation string \
for `isearch-forward',\nwhich lists available keys:\n\n%s"
                      (documentation 'isearch-forward)) ""))
  :jump t
  :type exclusive
  :repeat evilspeak-repeat-search
  (evil-search-incrementally t evil-regexp-search)
  (evilspeak-speak-jump))


(evil-define-motion evilspeak-search-backward ()
  (format "Search backward for user-entered text.
Searches for regular expression if `evil-regexp-search' is t.%s"
          (if (and (fboundp 'isearch-forward)
                   (documentation 'isearch-forward))
              (format "\n\nBelow is the documentation string \
for `isearch-forward',\nwhich lists available keys:\n\n%s"
                      (documentation 'isearch-forward)) ""))
  :jump t
  :type exclusive
  :repeat evil-repeat-search
  (evil-search-backward)
  (evilspeak-speak-jump))

(evil-define-motion evilspeak-search-next (count)
  "Repeat the last search."
  :jump t
  :type exclusive
  (evil-search-next count)
  (evilspeak-speak-jump))

(evil-define-motion evilspeak-search-previous (count)
  "Repeat the last search in the opposite direction."
  :jump t
  :type exclusive
  (evil-search-previous count)
  (evilspeak-speak-jump))

(evil-define-motion evilspeak-search-word-backward (count &optional symbol)
  "Search backward for symbol under point."
  :jump t
  :type exclusive
  (evil-search-word-backward count symbol)
  (evilspeak-speak-jump))

(evil-define-motion evilspeak-search-word-forward (count &optional symbol)
  "Search forward for symbol under point."
  :jump t
  :type exclusive
  (evil-search-word-forward count symbol)
  (evilspeak-speak-jump))

(evil-define-motion evilspeak-search-unbounded-word-backward (count &optional symbol)
  "Search backward for symbol under point.
The search is unbounded, i.e., the pattern is not wrapped in
\\<...\\>."
  :jump t
  :type exclusive
  (evil-search-unbounded-word-backward count symbol)
  (evilspeak-speak-jump))

(evil-define-motion evilspeak-search-unbounded-word-forward (count &optional symbol)
  "Search forward for symbol under point.
The search is unbounded, i.e., the pattern is not wrapped in
\\<...\\>."
  :jump t
  :type exclusive
  (evil-search-unbounded-word-forward count symbol)
  (evilspeak-speak-jump))

;; gotos

(evil-define-motion evilspeak-goto-definition ()
  "Go to definition or first occurrence of symbol under point."
  :jump t
  :type exclusive
  (evil-goto-definition)
  (evilspeak-speak-long-jump))


(evil-define-command evilspeak-goto-error (count)
  "Go to error number COUNT.

If no COUNT supplied, move to the current error.

Acts like `first-error' other than when given no counts, goes
to the current error instead of the first, like in Vim's :cc
command."
  :repeat nil
  (evil-goto-error count)
  (evilspeak-speak-long-jump))

(evil-define-command evilspeak-buffer (buffer)
  "Switches to another buffer."
  :repeat nil
  (interactive "<b>")
  (evil-buffer buffer)
  (evilspeak-speak-long-jump))

(evil-define-command evilspeak-next-buffer (&optional count)
  "Goes to the `count'-th next buffer in the buffer list."
  :repeat nil
  (evil-next-buffer count)
  (evilspeak-speak-long-jump))

(evil-define-command evilspeak-prev-buffer (&optional count)
  "Goes to the `count'-th prev buffer in the buffer list."
  :repeat nil
  (evil-prev-buffer count)
  (evilspeak-speak-long-jump))


(evil-define-motion evilspeak-ex-search-next (count)
  "Goes to the next occurrence."
  :jump t
  :type exclusive
  (evil-ex-search-next count)
  (evilspeak-speak-jump))

(evil-define-motion evilspeak-ex-search-previous (count)
  "Goes the the previous occurrence."
  :jump t
  :type exclusive
  (evil-ex-search-previous count)
  (evilspeak-speak-jump))


(evil-define-motion evilspeak-ex-search-forward (count)
  "Starts a forward search."
  :jump t
  :type exclusive
  :repeat evil-repeat-ex-search
  (evil-ex-search-forward count)
  (evilspeak-speak-jump))

(evil-define-motion evilspeak-ex-search-backward (count)
  "Starts a forward search."
  :jump t
  :repeat evil-repeat-ex-search
  (evil-ex-search-backward count)
  (evilspeak-speak-jump))

(evil-define-motion evilspeak-ex-search-word-forward (count &optional symbol)
  "Search for the next occurrence of word under the cursor."
  :jump t
  :type exclusive
  (evil-ex-search-word-forward count symbol)
  (evilspeak-speak-jump))

(evil-define-motion evilspeak-ex-search-word-backward (count &optional symbol)
  "Search for the next occurrence of word under the cursor."
  :jump t
  :type exclusive
  (evil-ex-search-word-backward count symbol)
  (evilspeak-speak-jump))

(evil-define-motion evilspeak-ex-search-unbounded-word-forward (count &optional symbol)
  "Search for the next occurrence of word under the cursor."
  :jump t
  :type exclusive
  (evil-ex-search-unbounded-word-forward count symbol)
  (evilspeak-speak-jump))

(evil-define-motion evilspeak-ex-search-unbounded-word-backward (count &optional symbol)
  "Search for the next occurrence of word under the cursor."
  :jump t
  :type exclusive
  (evil-ex-search-unbounded-word-backward count symbol)
  (evilspeak-speak-jump))


(evil-define-command evilspeak-goto-char (position)
  "Go to POSITION in the buffer.
Default position is the beginning of the buffer."
  (evil-goto-char position)
  (evilspeak-speak-line t))

(evil-define-command evilspeak-force-normal-state ()
  "Switch to normal state without recording current command."
  :repeat abort
  :suppress-operator t
  (evil-normal-state)
  (message "-- NORMAL --"))

(provide 'evilspeak-commands)