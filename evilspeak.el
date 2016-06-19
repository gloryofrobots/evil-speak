;; ;; Movement commands, or motions, are defined with the macro
;; ;; `evil-define-motion'. A motion is a command with an optional
;; ;; argument COUNT (interactively accessed by the code "<c>").
;; ;; It may specify the :type command property (e.g., :type line),
;; ;; which determines how it is handled by an operator command.
;; ;; Furthermore, the command must have the command properties
;; ;; :keep-visual t and :repeat motion; these are automatically
;; ;; set by the `evil-define-motion' macro.
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
  (evilspeak-speak-char)

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

(define-key evil-motion-state-map "j" 'evilspeak-next-visual-line)
(define-key evil-motion-state-map "k" 'evilspeak-previous-visual-line)
(define-key evil-motion-state-map "l" 'evilspeak-forward-char)
(define-key evil-motion-state-map "h" 'evilspeak-backward-char)

;;;;;;;;;;;;;;;MAPS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;; Normal state

;; (define-key evil-normal-state-map "gu" 'evil-downcase)
;; (define-key evil-normal-state-map "gU" 'evil-upcase)
;; (define-key evil-normal-state-map "g~" 'evil-invert-case)
;; (define-key evil-normal-state-map "~" 'evil-invert-char)

(define-key evil-normal-state-map [escape] 'evil-force-normal-state)
;;; Motion state

;; "0" is a special command when called first
(evil-redirect-digit-argument evil-motion-state-map "0" 'evil-beginning-of-line)
(define-key evil-motion-state-map "1" 'digit-argument)
(define-key evil-motion-state-map "2" 'digit-argument)
(define-key evil-motion-state-map "3" 'digit-argument)
(define-key evil-motion-state-map "4" 'digit-argument)
(define-key evil-motion-state-map "5" 'digit-argument)
(define-key evil-motion-state-map "6" 'digit-argument)
(define-key evil-motion-state-map "7" 'digit-argument)
(define-key evil-motion-state-map "8" 'digit-argument)
(define-key evil-motion-state-map "9" 'digit-argument)
(define-key evil-motion-state-map "b" 'evil-backward-word-begin)
(define-key evil-motion-state-map "B" 'evil-backward-WORD-begin)
(define-key evil-motion-state-map "e" 'evil-forward-word-end)
(define-key evil-motion-state-map "E" 'evil-forward-WORD-end)
(define-key evil-motion-state-map "f" 'evil-find-char)
(define-key evil-motion-state-map "F" 'evil-find-char-backward)
(define-key evil-motion-state-map "G" 'evil-goto-line)
(define-key evil-motion-state-map "h" 'evil-backward-char)
(define-key evil-motion-state-map "H" 'evil-window-top)
(define-key evil-motion-state-map "j" 'evil-next-line)
(define-key evil-motion-state-map "k" 'evil-previous-line)
(define-key evil-motion-state-map "l" 'evil-forward-char)
(define-key evil-motion-state-map " " 'evil-forward-char)
(define-key evil-motion-state-map "K" 'evil-lookup)
(define-key evil-motion-state-map "L" 'evil-window-bottom)
(define-key evil-motion-state-map "M" 'evil-window-middle)
(define-key evil-motion-state-map "n" 'evil-search-next)
(define-key evil-motion-state-map "N" 'evil-search-previous)
(define-key evil-motion-state-map "t" 'evil-find-char-to)
(define-key evil-motion-state-map "T" 'evil-find-char-to-backward)
(define-key evil-motion-state-map "w" 'evil-forward-word-begin)
(define-key evil-motion-state-map "W" 'evil-forward-WORD-begin)
(define-key evil-motion-state-map "y" 'evil-yank)
(define-key evil-motion-state-map "Y" 'evil-yank-line)
(define-key evil-motion-state-map "gd" 'evil-goto-definition)
(define-key evil-motion-state-map "ge" 'evil-backward-word-end)
(define-key evil-motion-state-map "gE" 'evil-backward-WORD-end)
(define-key evil-motion-state-map "gg" 'evil-goto-first-line)
(define-key evil-motion-state-map "gj" 'evil-next-visual-line)
(define-key evil-motion-state-map "gk" 'evil-previous-visual-line)
(define-key evil-motion-state-map "g0" 'evil-beginning-of-visual-line)
(define-key evil-motion-state-map "g_" 'evil-last-non-blank)
(define-key evil-motion-state-map "g^" 'evil-first-non-blank-of-visual-line)
(define-key evil-motion-state-map "gm" 'evil-middle-of-visual-line)
(define-key evil-motion-state-map "g$" 'evil-end-of-visual-line)
(define-key evil-motion-state-map "g\C-]" 'find-tag)
(define-key evil-motion-state-map "{" 'evil-backward-paragraph)
(define-key evil-motion-state-map "}" 'evil-forward-paragraph)
(define-key evil-motion-state-map "#" 'evil-search-word-backward)
(define-key evil-motion-state-map "g#" 'evil-search-unbounded-word-backward)
(define-key evil-motion-state-map "$" 'evil-end-of-line)
(define-key evil-motion-state-map "%" 'evil-jump-item)
(define-key evil-motion-state-map "`" 'evil-goto-mark)
(define-key evil-motion-state-map "'" 'evil-goto-mark-line)
(define-key evil-motion-state-map "(" 'evil-backward-sentence-begin)
(define-key evil-motion-state-map ")" 'evil-forward-sentence-begin)
(define-key evil-motion-state-map "]]" 'evil-forward-section-begin)
(define-key evil-motion-state-map "][" 'evil-forward-section-end)
(define-key evil-motion-state-map "[[" 'evil-backward-section-begin)
(define-key evil-motion-state-map "[]" 'evil-backward-section-end)
(define-key evil-motion-state-map "[(" 'evil-previous-open-paren)
(define-key evil-motion-state-map "])" 'evil-next-close-paren)
(define-key evil-motion-state-map "[{" 'evil-previous-open-brace)
(define-key evil-motion-state-map "]}" 'evil-next-close-brace)
(define-key evil-motion-state-map "*" 'evil-search-word-forward)
(define-key evil-motion-state-map "g*" 'evil-search-unbounded-word-forward)
(define-key evil-motion-state-map "," 'evil-repeat-find-char-reverse)
(define-key evil-motion-state-map "/" 'evil-search-forward)
(define-key evil-motion-state-map ";" 'evil-repeat-find-char)
(define-key evil-motion-state-map "?" 'evil-search-backward)
(define-key evil-motion-state-map "|" 'evil-goto-column)
(define-key evil-motion-state-map "^" 'evil-first-non-blank)
(define-key evil-motion-state-map "+" 'evil-next-line-first-non-blank)
(define-key evil-motion-state-map "_" 'evil-next-line-1-first-non-blank)
(define-key evil-motion-state-map "-" 'evil-previous-line-first-non-blank)
(define-key evil-motion-state-map "\C-w" 'evil-window-map)
(define-key evil-motion-state-map (kbd "C-6") 'evil-switch-to-windows-last-buffer)
(define-key evil-motion-state-map "\C-]" 'evil-jump-to-tag)
(define-key evil-motion-state-map (kbd "C-b") 'evil-scroll-page-up)
(define-key evil-motion-state-map (kbd "C-d") 'evil-scroll-down)
(define-key evil-motion-state-map (kbd "C-f") 'evil-scroll-page-down)
(define-key evil-motion-state-map (kbd "RET") 'evil-ret)

(define-key evil-motion-state-map "v" 'evil-visual-char)
(define-key evil-motion-state-map "V" 'evil-visual-line)
(define-key evil-motion-state-map "\C-v" 'evil-visual-block)
(define-key evil-motion-state-map "gv" 'evil-visual-restore)
(define-key evil-motion-state-map (kbd "C-^") 'evil-buffer)
(define-key evil-motion-state-map [left] 'evil-backward-char)
(define-key evil-motion-state-map [right] 'evil-forward-char)
(define-key evil-motion-state-map [up] 'evil-previous-line)
(define-key evil-motion-state-map [down] 'evil-next-line)


(define-key evil-motion-state-map (kbd "C-o") 'evil-jump-backward)
(when evil-want-C-i-jump
  (define-key evil-motion-state-map (kbd "C-i") 'evil-jump-forward))

(when evil-want-C-u-scroll
  (define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up))


(evil-ex-define-cmd "e[dit]" 'evil-edit)
(evil-ex-define-cmd "w[rite]" 'evil-write)
(evil-ex-define-cmd "wa[ll]" 'evil-write-all)
(evil-ex-define-cmd "sav[eas]" 'evil-save)
(evil-ex-define-cmd "r[ead]" 'evil-read)
(evil-ex-define-cmd "b[uffer]" 'evil-buffer)
(evil-ex-define-cmd "bn[ext]" 'evil-next-buffer)
(evil-ex-define-cmd "bp[revious]" 'evil-prev-buffer)
(evil-ex-define-cmd "bN[ext]" "bprevious")
(evil-ex-define-cmd "sb[uffer]" 'evil-split-buffer)
(evil-ex-define-cmd "sbn[ext]" 'evil-split-next-buffer)
(evil-ex-define-cmd "sbp[revious]" 'evil-split-prev-buffer)
(evil-ex-define-cmd "sbN[ext]" "sbprevious")
(evil-ex-define-cmd "buffers" 'buffer-menu)
(evil-ex-define-cmd "files" 'evil-show-files)
(evil-ex-define-cmd "ls" "buffers")

(evil-ex-define-cmd "c[hange]" 'evil-change)
(evil-ex-define-cmd "co[py]" 'evil-copy)
(evil-ex-define-cmd "t" "copy")
(evil-ex-define-cmd "m[ove]" 'evil-move)
(evil-ex-define-cmd "d[elete]" 'evil-delete)
(evil-ex-define-cmd "y[ank]" 'evil-yank)
(evil-ex-define-cmd "go[to]" 'evil-goto-char)
(evil-ex-define-cmd "j[oin]" 'evil-join)
(evil-ex-define-cmd "le[ft]" 'evil-align-left)
(evil-ex-define-cmd "ri[ght]" 'evil-align-right)
(evil-ex-define-cmd "ce[nter]" 'evil-align-center)
(evil-ex-define-cmd "sp[lit]" 'evil-window-split)
(evil-ex-define-cmd "vs[plit]" 'evil-window-vsplit)
(evil-ex-define-cmd "new" 'evil-window-new)
(evil-ex-define-cmd "ene[w]" 'evil-buffer-new)
(evil-ex-define-cmd "vne[w]" 'evil-window-vnew)
(evil-ex-define-cmd "clo[se]" 'evil-window-delete)
(evil-ex-define-cmd "on[ly]" 'delete-other-windows)
(evil-ex-define-cmd "q[uit]" 'evil-quit)
(evil-ex-define-cmd "wq" 'evil-save-and-close)
(evil-ex-define-cmd "quita[ll]" 'evil-quit-all)
(evil-ex-define-cmd "qa[ll]" "quitall")
(evil-ex-define-cmd "cq[uit]" 'evil-quit-all-with-error-code)
(evil-ex-define-cmd "wqa[ll]" 'evil-save-and-quit)
(evil-ex-define-cmd "xa[ll]" "wqall")
(evil-ex-define-cmd "x[it]" 'evil-save-modified-and-close)
(evil-ex-define-cmd "exi[t]" 'evil-save-modified-and-close)
(evil-ex-define-cmd "bd[elete]" 'evil-delete-buffer)
(evil-ex-define-cmd "bw[ipeout]" 'evil-delete-buffer)
(evil-ex-define-cmd "g[lobal]" 'evil-ex-global)
(evil-ex-define-cmd "v[global]" 'evil-ex-global-inverted)
(evil-ex-define-cmd "norm[al]" 'evil-ex-normal)
(evil-ex-define-cmd "s[ubstitute]" 'evil-ex-substitute)
(evil-ex-define-cmd "&" 'evil-ex-repeat-substitute)
(evil-ex-define-cmd "&&" 'evil-ex-repeat-substitute-with-flags)
(evil-ex-define-cmd "~" 'evil-ex-repeat-substitute-with-search)
(evil-ex-define-cmd "~&" 'evil-ex-repeat-substitute-with-search-and-flags)
(evil-ex-define-cmd "registers" 'evil-show-registers)
(evil-ex-define-cmd "marks" 'evil-show-marks)
(evil-ex-define-cmd "delm[arks]" 'evil-delete-marks)
(evil-ex-define-cmd "ju[mps]" 'evil-show-jumps)
(evil-ex-define-cmd "noh[lsearch]" 'evil-ex-nohighlight)
(evil-ex-define-cmd "f[ile]" 'evil-show-file-info)
(evil-ex-define-cmd "<" 'evil-shift-left)
(evil-ex-define-cmd ">" 'evil-shift-right)
(evil-ex-define-cmd "=" 'evil-ex-line-number)
(evil-ex-define-cmd "!" 'evil-shell-command)
(evil-ex-define-cmd "@:" 'evil-ex-repeat)
(evil-ex-define-cmd "mak[e]" 'evil-make)
(evil-ex-define-cmd "cc" 'evil-goto-error)
(evil-ex-define-cmd "cfir[st]" 'first-error)
(evil-ex-define-cmd "cr[ewind]" 'first-error)
(evil-ex-define-cmd "cn[ext]" 'next-error)
(evil-ex-define-cmd "cp[revious]" 'previous-error)
(evil-ex-define-cmd "set-initial-state" 'evil-ex-set-initial-state)
(evil-ex-define-cmd "show-digraphs" 'evil-ex-show-digraphs)
(evil-ex-define-cmd "sor[t]" 'evil-ex-sort)
(evil-ex-define-cmd "res[ize]" 'evil-ex-resize)

;; search command line
(define-key evil-ex-search-keymap "\d" #'evil-ex-delete-backward-char)
(define-key evil-ex-search-keymap "\C-r" 'evil-paste-from-register)
(define-key evil-ex-search-keymap "\C-n" 'next-history-element)
(define-key evil-ex-search-keymap "\C-p" 'previous-history-element)

;; ex command line
(define-key evil-ex-completion-map "\d" #'evil-ex-delete-backward-char)
(define-key evil-ex-completion-map "\t" #'evil-ex-completion)
(define-key evil-ex-completion-map [tab] #'evil-ex-completion)
(define-key evil-ex-completion-map [remap completion-at-point] #'evil-ex-completion)
(define-key evil-ex-completion-map "\C-a" 'evil-ex-completion)
(define-key evil-ex-completion-map "\C-b" 'move-beginning-of-line)
(define-key evil-ex-completion-map "\C-c" 'abort-recursive-edit)
(define-key evil-ex-completion-map "\C-d" 'evil-ex-completion)
(define-key evil-ex-completion-map "\C-g" 'abort-recursive-edit)
(define-key evil-ex-completion-map "\C-k" 'evil-insert-digraph)
(define-key evil-ex-completion-map "\C-l" 'evil-ex-completion)
(define-key evil-ex-completion-map "\C-p" #'previous-complete-history-element)
(define-key evil-ex-completion-map "\C-r" 'evil-paste-from-register)
(define-key evil-ex-completion-map "\C-n" #'next-complete-history-element)
(define-key evil-ex-completion-map "\C-u" 'evil-delete-whole-line)
(define-key evil-ex-completion-map "\C-v" #'quoted-insert)
(define-key evil-ex-completion-map "\C-w" 'backward-kill-word)
(define-key evil-ex-completion-map [escape] 'abort-recursive-edit)
(define-key evil-ex-completion-map [S-left] 'backward-word)
(define-key evil-ex-completion-map [S-right] 'forward-word)
(define-key evil-ex-completion-map [up] 'previous-complete-history-element)
(define-key evil-ex-completion-map [down] 'next-complete-history-element)
(define-key evil-ex-completion-map [prior] 'previous-history-element)
(define-key evil-ex-completion-map [next] 'next-history-element)
(define-key evil-ex-completion-map [return] 'exit-minibuffer)
(define-key evil-ex-completion-map (kbd "RET") 'exit-minibuffer)

;; evil-read-key
(define-key evil-read-key-map (kbd "ESC") #'keyboard-quit)
(define-key evil-read-key-map (kbd "C-]") #'keyboard-quit)
(define-key evil-read-key-map (kbd "C-q") #'evil-read-quoted-char)
(define-key evil-read-key-map (kbd "C-v") #'evil-read-quoted-char)
(define-key evil-read-key-map (kbd "C-k") #'evil-read-digraph-char)
(define-key evil-read-key-map "\r" "\n")

;; command line window
(evil-define-key 'normal
  evil-command-window-mode-map (kbd "RET") 'evil-command-window-execute)
(evil-define-key 'insert
  evil-command-window-mode-map (kbd "RET") 'evil-command-window-execute)

(provide 'evil-maps)


;;(load-file "~/.emacs.d/my/evilspeak/evilspeak-maps.el")

;;(provide 'evilspeak)