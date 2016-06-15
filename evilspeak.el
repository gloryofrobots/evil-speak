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


;; (evil-define-motion evilspeak-search-forward ()
;;   (format "Search forward for user-entered text.
;; Searches for regular expression if `evil-regexp-search' is t.%s"
;;           (if (and (fboundp 'isearch-forward)
;;                    (documentation 'isearch-forward))
;;               (format "\n\nBelow is the documentation string \
;; for `isearch-forward',\nwhich lists available keys:\n\n%s"
;;                       (documentation 'isearch-forward)) ""))
;;   :jump t
;;   :type exclusive
;;   :repeat evil-repeat-search
;;   (evil-search-incrementally t evil-regexp-search))

;; (evil-define-motion evilspeak-search-backward ()
;;   (format "Search backward for user-entered text.
;; Searches for regular expression if `evil-regexp-search' is t.%s"
;;           (if (and (fboundp 'isearch-forward)
;;                    (documentation 'isearch-forward))
;;               (format "\n\nBelow is the documentation string \
;; for `isearch-forward',\nwhich lists available keys:\n\n%s"
;;                       (documentation 'isearch-forward)) ""))
;;   :jump t
;;   :type exclusive
;;   :repeat evil-repeat-search
;;   (evil-search-incrementally nil evil-regexp-search))

;; (evil-define-motion evilspeak-search-next (count)
;;   "Repeat the last search."
;;   :jump t
;;   :type exclusive
;;   (dotimes (var (or count 1))
;;     (evil-search (if evil-regexp-search
;;                      (car-safe regexp-search-ring)
;;                    (car-safe search-ring))
;;                  isearch-forward evil-regexp-search)))

;; (evil-define-motion evilspeak-search-previous (count)
;;   "Repeat the last search in the opposite direction."
;;   :jump t
;;   :type exclusive
;;   (dotimes (var (or count 1))
;;     (evil-search (if evil-regexp-search
;;                      (car-safe regexp-search-ring)
;;                    (car-safe search-ring))
;;                  (not isearch-forward) evil-regexp-search)))

;; (evil-define-motion evilspeak-search-word-backward (count &optional symbol)
;;   "Search backward for symbol under point."
;;   :jump t
;;   :type exclusive
;;   (interactive (list (prefix-numeric-value current-prefix-arg)
;;                      evil-symbol-word-search))
;;   (dotimes (var (or count 1))
;;     (evil-search-word nil nil symbol)))

;; (evil-define-motion evilspeak-search-word-forward (count &optional symbol)
;;   "Search forward for symbol under point."
;;   :jump t
;;   :type exclusive
;;   (interactive (list (prefix-numeric-value current-prefix-arg)
;;                      evil-symbol-word-search))
;;   (dotimes (var (or count 1))
;;     (evil-search-word t nil symbol)))

;; (evil-define-motion evilspeak-search-unbounded-word-backward (count &optional symbol)
;;   "Search backward for symbol under point.
;; The search is unbounded, i.e., the pattern is not wrapped in
;; \\<...\\>."
;;   :jump t
;;   :type exclusive
;;   (interactive (list (prefix-numeric-value current-prefix-arg)
;;                      evil-symbol-word-search))
;;   (dotimes (var (or count 1))
;;     (evil-search-word nil t symbol)))

;; (evil-define-motion evilspeak-search-unbounded-word-forward (count &optional symbol)
;;   "Search forward for symbol under point.
;; The search is unbounded, i.e., the pattern is not wrapped in
;; \\<...\\>."
;;   :jump t
;;   :type exclusive
;;   (interactive (list (prefix-numeric-value current-prefix-arg)
;;                      evil-symbol-word-search))
;;   (dotimes (var (or count 1))
;;     (evil-search-word t t symbol)))

;; (evil-define-motion evilspeak-goto-definition ()
;;   "Go to definition or first occurrence of symbol under point."
;;   :jump t
;;   :type exclusive
;;   (let* ((string (evil-find-symbol t))
;;          (search (format "\\_<%s\\_>" (regexp-quote string)))
;;          ientry ipos)
;;     ;; load imenu if available
;;     (unless (featurep 'imenu)
;;       (condition-case nil
;;           (require 'imenu)
;;         (error nil)))
;;     (if (null string)
;;         (user-error "No symbol under cursor")
;;       (setq isearch-forward t)
;;       ;; if imenu is available, try it
;;       (cond
;;        ((fboundp 'imenu--make-index-alist)
;;         (condition-case nil
;;             (setq ientry (imenu--make-index-alist))
;;           (error nil))
;;         (setq ientry (assoc string ientry))
;;         (setq ipos (cdr ientry))
;;         (when (and (markerp ipos)
;;                    (eq (marker-buffer ipos) (current-buffer)))
;;           (setq ipos (marker-position ipos)))
;;         (cond
;;          ;; imenu found a position, so go there and
;;          ;; highlight the occurrence
;;          ((numberp ipos)
;;           (evil-search search t t ipos))
;;          ;; imenu failed, so just go to first occurrence in buffer
;;          (t
;;           (evil-search search t t (point-min)))))
;;        ;; no imenu, so just go to first occurrence in buffer
;;        (t
;;         (evil-search search t t (point-min)))))))

;; ;;; Folding
;; (defun evil-fold-action (list action)
;;   "Perform fold ACTION for each matching major or minor mode in LIST.

;; ACTION will be performed for the first matching handler in LIST.  For more
;; information on its features and format, see the documentation for
;; `evil-fold-list'.

;; If no matching ACTION is found in LIST, an error will signaled.

;; Handler errors will be demoted, so a problem in one handler will (hopefully)
;; not interfere with another."
;;   (if (null list)
;;       (user-error
;;        "Folding is not supported for any of these major/minor modes")
;;     (let* ((modes (caar list)))
;;       (if (evil--mode-p modes)
;;           (let* ((actions (cdar list))
;;                  (fn      (plist-get actions action)))
;;             (when fn
;;               (with-demoted-errors (funcall fn))))
;;         (evil-fold-action (cdr list) action)))))

;; (defun evil--mode-p (modes)
;;   "Determines whether any symbol in MODES represents the current
;; buffer's major mode or any of its minors."
;;   (unless (eq modes '())
;;     (let ((mode (car modes)))
;;       (or (eq major-mode mode)
;;           (and (boundp mode) (symbol-value mode))
;;           (evil--mode-p (cdr modes))))))

;; (evil-define-command evilspeak-toggle-fold ()
;;   "Open or close a fold under point.
;; See also `evil-open-fold' and `evil-close-fold'."
;;   (evil-fold-action evil-fold-list :toggle))

;; (evil-define-command evilspeak-open-folds ()
;;   "Open all folds.
;; See also `evil-close-folds'."
;;   (evil-fold-action evil-fold-list :open-all))

;; (evil-define-command evilspeak-close-folds ()
;;   "Close all folds.
;; See also `evil-open-folds'."
;;   (evil-fold-action evil-fold-list :close-all))

;; (evil-define-command evilspeak-open-fold ()
;;   "Open fold at point.
;; See also `evil-close-fold'."
;;   (evil-fold-action evil-fold-list :open))

;; (evil-define-command evilspeak-open-fold-rec ()
;;   "Open fold at point recursively.
;; See also `evil-open-fold' and `evil-close-fold'."
;;   (evil-fold-action evil-fold-list :open-rec))

;; (evil-define-command evilspeak-close-fold ()
;;   "Close fold at point.
;; See also `evil-open-fold'."
;;   (evil-fold-action evil-fold-list :close))

;; ;;; Ex

;; (evil-define-operator evilspeak-write (beg end type file-or-append &optional bang)
;;   "Save the current buffer, from BEG to END, to FILE-OR-APPEND.
;; If FILE-OR-APPEND is of the form \">> FILE\", append to FILE
;; instead of overwriting.  The current buffer's filename is not
;; changed unless it has no associated file and no region is
;; specified.  If the file already exists and the BANG argument is
;; non-nil, it is overwritten without confirmation."
;;   :motion nil
;;   :move-point nil
;;   :type line
;;   :repeat nil
;;   (interactive "<R><fsh><!>")
;;   (let* ((append-and-filename (evil-extract-append file-or-append))
;;          (append (car append-and-filename))
;;          (filename (cdr append-and-filename))
;;          (bufname (buffer-file-name (buffer-base-buffer))))
;;     (when (zerop (length filename))
;;       (setq filename bufname))
;;     (cond
;;      ((zerop (length filename))
;;       (user-error "Please specify a file name for the buffer"))
;;      ;; execute command on region
;;      ((eq (aref filename 0) ?!)
;;       (shell-command-on-region beg end (substring filename 1)))
;;      ;; with region or append, always save to file without resetting
;;      ;; modified flag
;;      ((or append (and beg end))
;;       (write-region beg end filename append nil nil (not (or append bang))))
;;      ;; no current file
;;      ((null bufname)
;;       (write-file filename (not bang)))
;;      ;; save current buffer to its file
;;      ((string= filename bufname)
;;       (if (not bang) (save-buffer) (write-file filename)))
;;      ;; save to another file
;;      (t
;;       (write-region nil nil filename
;;                     nil (not bufname) nil
;;                     (not bang))))))

;; (evil-define-command evilspeak-write-all (bang)
;;   "Saves all buffers visiting a file.
;; If BANG is non nil then read-only buffers are saved, too,
;; otherwise they are skipped. "
;;   :repeat nil
;;   :move-point nil
;;   (interactive "<!>")
;;   (if bang
;;       (save-some-buffers t)
;;     ;; save only buffer that are not read-only and
;;     ;; that are visiting a file
;;     (save-some-buffers t
;;                        #'(lambda ()
;;                            (and (not buffer-read-only)
;;                                 (buffer-file-name))))))

;; (evil-define-command evilspeak-save (filename &optional bang)
;;   "Save the current buffer to FILENAME.
;; Changes the file name of the current buffer to FILENAME.  If no
;; FILENAME is given, the current file name is used."
;;   :repeat nil
;;   :move-point nil
;;   (interactive "<f><!>")
;;   (when (zerop (length filename))
;;     (setq filename (buffer-file-name (buffer-base-buffer))))
;;   (write-file filename (not bang)))

;; (evil-define-command evilspeak-edit (file &optional bang)
;;   "Open FILE.
;; If no FILE is specified, reload the current buffer from disk."
;;   :repeat nil
;;   (interactive "<f><!>")
;;   (if file
;;       (find-file file)
;;     (revert-buffer bang (or bang (not (buffer-modified-p))) t)))

;; (evil-define-command evilspeak-read (count file)
;;   "Inserts the contents of FILE below the current line or line COUNT."
;;   :repeat nil
;;   :move-point nil
;;   (interactive "P<fsh>")
;;   (when (and file (not (zerop (length file))))
;;     (when count (goto-char (point-min)))
;;     (when (or (not (zerop (forward-line (or count 1))))
;;               (not (bolp)))
;;       (insert "\n"))
;;     (if (/= (aref file 0) ?!)
;;         (let ((result (insert-file-contents file)))
;;           (save-excursion
;;             (forward-char (cadr result))
;;             (unless (bolp) (insert "\n"))))
;;       (shell-command (substring file 1) t)
;;       (save-excursion
;;         (goto-char (mark))
;;         (unless (bolp) (insert "\n"))))))

;; (evil-define-command evilspeak-show-files ()
;;   "Shows the file-list.
;; The same as `buffer-menu', but shows only buffers visiting
;; files."
;;   :repeat nil
;;   (buffer-menu 1))

;; (evil-define-command evilspeak-goto-error (count)
;;   "Go to error number COUNT.

;; If no COUNT supplied, move to the current error.

;; Acts like `first-error' other than when given no counts, goes
;; to the current error instead of the first, like in Vim's :cc
;; command."
;;   :repeat nil
;;   (interactive "<c>")
;;   (if count
;;       (first-error (if (eql 0 count) 1 count))
;;     (next-error 0)))

;; (evil-define-command evilspeak-buffer (buffer)
;;   "Switches to another buffer."
;;   :repeat nil
;;   (interactive "<b>")
;;   (cond
;;    ;; no buffer given, switch to "other" buffer
;;    ((null buffer) (switch-to-buffer (other-buffer)))
;;    ;; we are given the name of an existing buffer
;;    ((get-buffer buffer) (switch-to-buffer buffer))
;;    ;; try to complete the buffer
;;    ((let ((all-buffers (internal-complete-buffer buffer nil t)))
;;       (when (= (length all-buffers) 1)
;;         (switch-to-buffer (car all-buffers)))))
;;    (t
;;     (when (y-or-n-p
;;            (format "No buffer with name \"%s\" exists. Create new buffer? "
;;                    buffer))
;;       (switch-to-buffer buffer)))))

;; (evil-define-command evilspeak-next-buffer (&optional count)
;;   "Goes to the `count'-th next buffer in the buffer list."
;;   :repeat nil
;;   (interactive "p")
;;   (dotimes (i (or count 1))
;;     (next-buffer)))

;; (evil-define-command evilspeak-prev-buffer (&optional count)
;;   "Goes to the `count'-th prev buffer in the buffer list."
;;   :repeat nil
;;   (interactive "p")
;;   (dotimes (i (or count 1))
;;     (previous-buffer)))

;; (evil-define-command evilspeak-delete-buffer (buffer &optional bang)
;;   "Deletes a buffer.
;; All windows currently showing this buffer will be closed except
;; for the last window in each frame."
;;   (interactive "<b><!>")
;;   (with-current-buffer (or buffer (current-buffer))
;;     (when bang
;;       (set-buffer-modified-p nil)
;;       (dolist (process (process-list))
;;         (when (eq (process-buffer process) (current-buffer))
;;           (set-process-query-on-exit-flag process nil))))
;;     ;; get all windows that show this buffer
;;     (let ((wins (get-buffer-window-list (current-buffer) nil t)))
;;       ;; if the buffer which was initiated by emacsclient,
;;       ;; call `server-edit' from server.el to avoid
;;       ;; "Buffer still has clients" message
;;       (if (and (fboundp 'server-edit)
;;                (boundp 'server-buffer-clients)
;;                server-buffer-clients)
;;           (server-edit)
;;         (kill-buffer nil))
;;       ;; close all windows that showed this buffer
;;       (mapc #'(lambda (w)
;;                 (condition-case nil
;;                     (delete-window w)
;;                   (error nil)))
;;             wins))))

;; (evil-define-command evilspeak-quit (&optional force)
;;   "Closes the current window, current frame, Emacs.
;; If the current frame belongs to some client the client connection
;; is closed."
;;   :repeat nil
;;   (interactive "<!>")
;;   (condition-case nil
;;       (delete-window)
;;     (error
;;      (if (and (boundp 'server-buffer-clients)
;;               (fboundp 'server-edit)
;;               (fboundp 'server-buffer-done)
;;               server-buffer-clients)
;;          (if force
;;              (server-buffer-done (current-buffer))
;;            (server-edit))
;;        (condition-case nil
;;            (delete-frame)
;;          (error
;;           (if force
;;               (kill-emacs)
;;             (save-buffers-kill-emacs))))))))

;; (evil-define-command evilspeak-quit-all (&optional bang)
;;   "Exits Emacs, asking for saving."
;;   :repeat nil
;;   (interactive "<!>")
;;   (if (null bang)
;;       (save-buffers-kill-terminal)
;;     (let ((proc (frame-parameter (selected-frame) 'client)))
;;       (if proc
;;           (with-no-warnings
;;             (server-delete-client proc))
;;         (dolist (process (process-list))
;;           (set-process-query-on-exit-flag process nil))
;;         (kill-emacs)))))

;; (evil-define-command evilspeak-quit-all-with-error-code (&optional force)
;;   "Exits Emacs without saving, returning an non-zero error code.
;; The FORCE argument is only there for compatibility and is ignored.
;; This function fails with an error if Emacs is run in server mode."
;;   :repeat nil
;;   (interactive "<!>")
;;   (if (and (boundp 'server-buffer-clients)
;;            (fboundp 'server-edit)
;;            (fboundp 'server-buffer-done)
;;            server-buffer-clients)
;;       (user-error "Cannot exit client process with error code.")
;;     (kill-emacs 1)))

;; (evil-define-command evilspeak-save-and-quit ()
;;   "Save all buffers and exit Emacs."
;;   (save-buffers-kill-terminal t))

;; (evil-define-command evilspeak-save-and-close (file &optional bang)
;;   "Saves the current buffer and closes the window."
;;   :repeat nil
;;   (interactive "<f><!>")
;;   (evil-write nil nil nil file bang)
;;   (evil-quit))

;; (evil-define-command evilspeak-save-modified-and-close (file &optional bang)
;;   "Saves the current buffer and closes the window."
;;   :repeat nil
;;   (interactive "<f><!>")
;;   (when (buffer-modified-p)
;;     (evil-write nil nil nil file bang))
;;   (evil-quit))

;; (evil-define-operator evilspeak-shell-command
;;   (beg end type command &optional previous)
;;   "Execute a shell command.
;; If BEG, END and TYPE is specified, COMMAND is executed on the region,
;; which is replaced with the command's output. Otherwise, the
;; output is displayed in its own buffer. If PREVIOUS is non-nil,
;; the previous shell command is executed instead."
;;   (interactive "<R><sh><!>")
;;   (if (not (evil-ex-p))
;;       (let ((evil-ex-initial-input
;;              (if (and beg
;;                       (not (evil-visual-state-p))
;;                       (not current-prefix-arg))
;;                  (let ((range (evil-range beg end type)))
;;                    (evil-contract-range range)
;;                    ;; TODO: this is not exactly the same as Vim, which
;;                    ;; uses .,+count as range. However, this is easier
;;                    ;; to achieve with the current implementation and
;;                    ;; the very inconvenient range interface.
;;                    ;;
;;                    ;; TODO: the range interface really needs some
;;                    ;; rework!
;;                    (format
;;                     "%d,%d!"
;;                     (line-number-at-pos (evil-range-beginning range))
;;                     (line-number-at-pos (evil-range-end range))))
;;                "!")))
;;         (call-interactively 'evil-ex))
;;     (when command
;;       (setq command (evil-ex-replace-special-filenames command)))
;;     (if (zerop (length command))
;;         (when previous (setq command evil-previous-shell-command))
;;       (setq evil-previous-shell-command command))
;;     (cond
;;      ((zerop (length command))
;;       (if previous (user-error "No previous shell command")
;;         (user-error "No shell command")))
;;      (evil-ex-range
;;       (if (not evil-display-shell-error-in-message)
;;           (shell-command-on-region beg end command nil t)
;;         (let ((output-buffer (generate-new-buffer " *temp*"))
;;               (error-buffer (generate-new-buffer " *temp*")))
;;           (unwind-protect
;;               (if (zerop (shell-command-on-region beg end
;;                                                   command
;;                                                   output-buffer nil
;;                                                   error-buffer))
;;                   (progn
;;                     (delete-region beg end)
;;                     (insert-buffer-substring output-buffer)
;;                     (goto-char beg)
;;                     (evil-first-non-blank))
;;                 (display-message-or-buffer error-buffer))
;;             (kill-buffer output-buffer)
;;             (kill-buffer error-buffer)))))
;;      (t
;;       (shell-command command)))))

;; (evil-define-command evilspeak-make (arg)
;;   "Call a build command in the current directory.
;; If ARG is nil this function calls `recompile', otherwise it calls
;; `compile' passing ARG as build command."
;;   (interactive "<sh>")
;;   (if (and (fboundp 'recompile)
;;            (not arg))
;;       (recompile)
;;     (compile arg)))

;; ;; TODO: escape special characters (currently only \n) ... perhaps
;; ;; there is some Emacs function doing this?
;; (evil-define-command evilspeak-show-registers ()
;;   "Shows the contents of all registers."
;;   :repeat nil
;;   (evil-with-view-list
;;     :name "evil-registers"
;;     :mode-name "Evil Registers"
;;     :format
;;     [("Register" 10 nil)
;;      ("Value" 1000 nil)]
;;     :entries
;;     (cl-loop for (key . val) in (evil-register-list)
;;              collect `(nil [,(char-to-string key)
;;                             ,(or (and val
;;                                       (stringp val)
;;                                       (replace-regexp-in-string "\n" "^J" val))
;;                                  "")]))))

;; (evil-define-command evilspeak-show-marks (mrks)
;;   "Shows all marks.
;; If MRKS is non-nil it should be a string and only registers
;; corresponding to the characters of this string are shown."
;;   :repeat nil
;;   (interactive "<a>")
;;   ;; To get markers and positions, we can't rely on 'global-mark-ring'
;;   ;; provided by Emacs (although it will be much simpler and faster),
;;   ;; because 'global-mark-ring' does not store mark characters, but
;;   ;; only buffer name and position. Instead, 'evil-markers-alist' is
;;   ;; used; this is list maintained by Evil for each buffer.
;;   (let ((all-markers
;;          ;; get global and local marks
;;          (append (cl-remove-if (lambda (m)
;;                                  (or (evil-global-marker-p (car m))
;;                                      (not (markerp (cdr m)))))
;;                                evil-markers-alist)
;;                  (cl-remove-if (lambda (m)
;;                                  (or (not (evil-global-marker-p (car m)))
;;                                      (not (markerp (cdr m)))))
;;                                (default-value 'evil-markers-alist)))))
;;     (when mrks
;;       (setq mrks (string-to-list mrks))
;;       (setq all-markers (cl-delete-if (lambda (m)
;;                                         (not (member (car m) mrks)))
;;                                       all-markers)))
;;     ;; map marks to list of 4-tuples (char row col file)
;;     (setq all-markers
;;           (mapcar (lambda (m)
;;                     (with-current-buffer (marker-buffer (cdr m))
;;                       (save-excursion
;;                         (goto-char (cdr m))
;;                         (list (car m)
;;                               (line-number-at-pos (point))
;;                               (current-column)
;;                               (buffer-name)))))
;;                   all-markers))
;;     (evil-with-view-list
;;       :name "evil-marks"
;;       :mode-name "Evil Marks"
;;       :format [("Mark" 8 nil)
;;                ("Line" 8 nil)
;;                ("Column" 8 nil)
;;                ("Buffer" 1000 nil)]
;;       :entries (cl-loop for m in (sort all-markers (lambda (a b) (< (car a) (car b))))
;;                         collect `(nil [,(char-to-string (nth 0 m))
;;                                        ,(number-to-string (nth 1 m))
;;                                        ,(number-to-string (nth 2 m))
;;                                        (,(nth 3 m))]))
;;       :select-action #'evil--show-marks-select-action)))

;; (defun evil--show-marks-select-action (entry)
;;   (kill-buffer)
;;   (switch-to-buffer (car (elt entry 3)))
;;   (evil-goto-mark (string-to-char (elt entry 0))))

;; (evil-define-command evilspeak-delete-marks (marks &optional force)
;;   "Delete all marks.
;; MARKS is a string denoting all marks to be deleted. Mark names are
;; either single characters or a range of characters in the form A-Z.

;; If FORCE is non-nil all local marks except 0-9 are removed.
;; "
;;   (interactive "<a><!>")
;;   (cond
;;    ;; delete local marks except 0-9
;;    (force
;;     (setq evil-markers-alist
;;           (cl-delete-if (lambda (m)
;;                           (not (and (>= (car m) ?0) (<= (car m) ?9))))
;;                         evil-markers-alist)))
;;    (t
;;     (let ((i 0)
;;           (n (length marks))
;;           delmarks)
;;       (while (< i n)
;;         (cond
;;          ;; skip spaces
;;          ((= (aref i ?\ )) (cl-incf i))
;;          ;; ranges of marks
;;          ((and (< (+ i 2) n)
;;                (= (aref marks (1+ i)) ?-)
;;                (or (and (>= (aref marks i) ?a)
;;                         (<= (aref marks i) ?z)
;;                         (>= (aref marks (+ 2 i)) ?a)
;;                         (<= (aref marks (+ 2 i)) ?z))
;;                    (and (>= (aref marks i) ?A)
;;                         (<= (aref marks i) ?Z)
;;                         (>= (aref marks (+ 2 i)) ?A)
;;                         (<= (aref marks (+ 2 i)) ?Z))))
;;           (let ((m (aref marks i)))
;;             (while (<= m (aref marks (+ 2 i)))
;;               (push m delmarks)
;;               (cl-incf m)))
;;           (cl-incf i 2))
;;          ;; single marks
;;          (t
;;           (push (aref marks i) delmarks)
;;           (cl-incf i))))
;;       ;; now remove all marks
;;       (setq evil-markers-alist
;;             (cl-delete-if (lambda (m) (member (car m) delmarks))
;;                           evil-markers-alist))
;;       (set-default 'evil-markers-alist
;;                    (cl-delete-if (lambda (m) (member (car m) delmarks))
;;                                  (default-value 'evil-markers-alist)))))))

;; (eval-when-compile (require 'ffap))
;; (evil-define-command evilspeak-find-file-at-point-with-line ()
;;   "Opens the file at point and goes to line-number."
;;   (require 'ffap)
;;   (let ((fname (with-no-warnings (ffap-file-at-point))))
;;     (if fname
;;         (let ((line
;;                (save-excursion
;;                  (goto-char (cadr ffap-string-at-point-region))
;;                  (and (re-search-backward ":\\([0-9]+\\)\\="
;;                                           (line-beginning-position) t)
;;                       (string-to-number (match-string 1))))))
;;           (with-no-warnings (ffap-other-window))
;;           (when line
;;             (goto-char (point-min))
;;             (forward-line (1- line))))
;;       (user-error "File does not exist."))))

;; (evil-ex-define-argument-type state
;;   "Defines an argument type which can take state names."
;;   :collection
;;   (lambda (arg predicate flag)
;;     (let ((completions
;;            (append '("nil")
;;                    (mapcar #'(lambda (state)
;;                                (format "%s" (car state)))
;;                            evil-state-properties))))
;;       (when arg
;;         (cond
;;          ((eq flag nil)
;;           (try-completion arg completions predicate))
;;          ((eq flag t)
;;           (all-completions arg completions predicate))
;;          ((eq flag 'lambda)
;;           (test-completion arg completions predicate))
;;          ((eq (car-safe flag) 'boundaries)
;;           (cons 'boundaries
;;                 (completion-boundaries arg
;;                                        completions
;;                                        predicate
;;                                        (cdr flag)))))))))

;; (evil-define-interactive-code "<state>"
;;   "A valid evil state."
;;   :ex-arg state
;;   (list (when (and (evil-ex-p) evil-ex-argument)
;;           (intern evil-ex-argument))))

;; ;; TODO: should we merge this command with `evil-set-initial-state'?
;; (evil-define-command evilspeak-ex-set-initial-state (state)
;;   "Set the initial state for the current major mode to STATE.
;; This is the state the buffer comes up in. See `evil-set-initial-state'."
;;   :repeat nil
;;   (interactive "<state>")
;;   (if (not (or (assq state evil-state-properties)
;;                (null state)))
;;       (user-error "State %s cannot be set as initial Evil state" state)
;;     (let ((current-initial-state (evil-initial-state major-mode)))
;;       (unless (eq current-initial-state state)
;;         ;; only if we selected a new mode
;;         (when (y-or-n-p (format "Major-mode `%s' has initial mode `%s'. \
;; Change to `%s'? "
;;                                 major-mode
;;                                 (or current-initial-state "DEFAULT")
;;                                 (or state "DEFAULT")))
;;           (evil-set-initial-state major-mode state)
;;           (when (y-or-n-p "Save setting in customization file? ")
;;             (dolist (s (list current-initial-state state))
;;               (when s
;;                 (let ((var (intern (format "evil-%s-state-modes" s))))
;;                   (customize-save-variable var (symbol-value var)))))))))))

;; (evil-define-command evilspeak-force-normal-state ()
;;   "Switch to normal state without recording current command."
;;   :repeat abort
;;   :suppress-operator t
;;   (evil-normal-state))

;; (evil-define-motion evilspeak-ex-search-next (count)
;;   "Goes to the next occurrence."
;;   :jump t
;;   :type exclusive
;;   (evil-ex-search count))

;; (evil-define-motion evilspeak-ex-search-previous (count)
;;   "Goes the the previous occurrence."
;;   :jump t
;;   :type exclusive
;;   (let ((evil-ex-search-direction
;;          (if (eq evil-ex-search-direction 'backward) 'forward 'backward)))
;;     (evil-ex-search count)))

;; (defun evil-repeat-ex-search (flag)
;;   "Called to record a search command.
;; FLAG is either 'pre or 'post if the function is called before
;; resp.  after executing the command."
;;   (cond
;;    ((and (evil-operator-state-p) (eq flag 'pre))
;;     (evil-repeat-record (this-command-keys))
;;     (evil-clear-command-keys))
;;    ((and (evil-operator-state-p) (eq flag 'post))
;;     ;; The value of (this-command-keys) at this point should be the
;;     ;; key-sequence that called the last command that finished the
;;     ;; search, usually RET. Therefore this key-sequence will be
;;     ;; recorded in the post-command of the operator. Alternatively we
;;     ;; could do it here.
;;     (evil-repeat-record (evil-ex-pattern-regex evil-ex-search-pattern)))
;;    (t (evil-repeat-motion flag))))

;; (evil-define-motion evilspeak-ex-search-forward (count)
;;   "Starts a forward search."
;;   :jump t
;;   :type exclusive
;;   :repeat evil-repeat-ex-search
;;   (evil-ex-start-search 'forward count))

;; (evil-define-motion evilspeak-ex-search-backward (count)
;;   "Starts a forward search."
;;   :jump t
;;   :repeat evil-repeat-ex-search
;;   (evil-ex-start-search 'backward count))

;; (evil-define-motion evilspeak-ex-search-word-forward (count &optional symbol)
;;   "Search for the next occurrence of word under the cursor."
;;   :jump t
;;   :type exclusive
;;   (interactive (list (prefix-numeric-value current-prefix-arg)
;;                      evil-symbol-word-search))
;;   (evil-ex-start-word-search nil 'forward count symbol))

;; (evil-define-motion evilspeak-ex-search-word-backward (count &optional symbol)
;;   "Search for the next occurrence of word under the cursor."
;;   :jump t
;;   :type exclusive
;;   (interactive (list (prefix-numeric-value current-prefix-arg)
;;                      evil-symbol-word-search))
;;   (evil-ex-start-word-search nil 'backward count symbol))

;; (evil-define-motion evilspeak-ex-search-unbounded-word-forward (count &optional symbol)
;;   "Search for the next occurrence of word under the cursor."
;;   :jump t
;;   :type exclusive
;;   (interactive (list (prefix-numeric-value current-prefix-arg)
;;                      evil-symbol-word-search))
;;   (evil-ex-start-word-search t 'forward count symbol))

;; (evil-define-motion evilspeak-ex-search-unbounded-word-backward (count &optional symbol)
;;   "Search for the next occurrence of word under the cursor."
;;   :jump t
;;   :type exclusive
;;   (interactive (list (prefix-numeric-value current-prefix-arg)
;;                      evil-symbol-word-search))
;;   (evil-ex-start-word-search t 'backward count symbol))

;; (evil-define-operator evilspeak-ex-substitute
;;   (beg end pattern replacement flags)
;;   "The Ex substitute command.
;; \[BEG,END]substitute/PATTERN/REPLACEMENT/FLAGS"
;;   :repeat nil
;;   :jump t
;;   :move-point nil
;;   :motion evil-line
;;   (interactive "<r><s/>")
;;   (evil-ex-nohighlight)
;;   (unless pattern
;;     (user-error "No pattern given"))
;;   (setq replacement (or replacement ""))
;;   (setq evil-ex-last-was-search nil)
;;   (let* ((flags (append flags nil))
;;          (confirm (memq ?c flags))
;;          (case-fold-search (evil-ex-pattern-ignore-case pattern))
;;          (case-replace case-fold-search)
;;          (evil-ex-substitute-regex (evil-ex-pattern-regex pattern)))
;;     (setq evil-ex-substitute-pattern pattern
;;           evil-ex-substitute-replacement replacement
;;           evil-ex-substitute-flags flags
;;           isearch-string evil-ex-substitute-regex)
;;     (isearch-update-ring evil-ex-substitute-regex t)
;;     (if (evil-ex-pattern-whole-line pattern)
;;         ;; this one is easy, just use the built-in function
;;         (perform-replace evil-ex-substitute-regex
;;                          evil-ex-substitute-replacement
;;                          confirm t nil nil nil
;;                          beg
;;                          (if (and (> end (point-min))
;;                                   (= (char-after (1- end)) ?\n))
;;                              (1- end)
;;                            end))
;;       (let ((evil-ex-substitute-nreplaced 0)
;;             (evil-ex-substitute-last-point (point))
;;             markers
;;             transient-mark-mode)
;;         (save-excursion
;;           (goto-char beg)
;;           (beginning-of-line)
;;           (while (< (point) end)
;;             (push (move-marker (make-marker) (point)) markers)
;;             (forward-line)))
;;         (setq markers (nreverse markers))
;;         (if confirm
;;             (let ((evil-ex-substitute-overlay
;;                    (make-overlay (point) (point)))
;;                   (evil-ex-substitute-hl
;;                    (evil-ex-make-hl 'evil-ex-substitute)))
;;               (evil-ex-hl-change 'evil-ex-substitute pattern)
;;               (unwind-protect
;;                   ;; this one is more difficult: we have to do
;;                   ;; the highlighting and querying on our own
;;                   (progn
;;                     (overlay-put evil-ex-substitute-overlay
;;                                  'face 'isearch)
;;                     (overlay-put evil-ex-substitute-overlay
;;                                  'priority 1001)
;;                     (map-y-or-n-p
;;                      #'(lambda (x)
;;                          (set-match-data x)
;;                          (move-overlay evil-ex-substitute-overlay
;;                                        (match-beginning 0)
;;                                        (match-end 0))
;;                          (format "Query replacing %s with %s: "
;;                                  (match-string 0)
;;                                  (evil-match-substitute-replacement
;;                                   evil-ex-substitute-replacement
;;                                   (not case-replace))))
;;                      #'(lambda (x)
;;                          (set-match-data x)
;;                          (evil-replace-match evil-ex-substitute-replacement
;;                                              (not case-replace))
;;                          (setq evil-ex-substitute-last-point (point))
;;                          (setq evil-ex-substitute-nreplaced
;;                                (1+ evil-ex-substitute-nreplaced))
;;                          (evil-ex-hl-set-region 'evil-ex-substitute
;;                                                 (save-excursion
;;                                                   (forward-line)
;;                                                   (point))
;;                                                 (evil-ex-hl-get-max
;;                                                  'evil-ex-substitute)))
;;                      #'(lambda ()
;;                          (catch 'found
;;                            (while markers
;;                              (let ((m (pop markers)))
;;                                (goto-char m)
;;                                (move-marker m nil))
;;                              (when (re-search-forward evil-ex-substitute-regex
;;                                                       (line-end-position) t nil)
;;                                (goto-char (match-beginning 0))
;;                                (throw 'found (match-data))))))))
;;                 (evil-ex-delete-hl 'evil-ex-substitute)
;;                 (delete-overlay evil-ex-substitute-overlay)))

;;           ;; just replace the first occurrences per line
;;           ;; without highlighting and asking
;;           (while markers
;;             (let ((m (pop markers)))
;;               (goto-char m)
;;               (move-marker m nil))
;;             (when (re-search-forward evil-ex-substitute-regex
;;                                      (line-end-position) t nil)
;;               (setq evil-ex-substitute-nreplaced
;;                     (1+ evil-ex-substitute-nreplaced))
;;               (evil-replace-match evil-ex-substitute-replacement
;;                                   (not case-replace))
;;               (setq evil-ex-substitute-last-point (point)))))

;;         (while markers (move-marker (pop markers) nil))
;;         (goto-char evil-ex-substitute-last-point)

;;         (message "Replaced %d occurrence%s"
;;                  evil-ex-substitute-nreplaced
;;                  (if (/= evil-ex-substitute-nreplaced 1) "s" ""))))
;;     (evil-first-non-blank)))

;; (evil-define-operator evilspeak-ex-repeat-substitute
;;   (beg end flags)
;;   "Repeat last substitute command.
;; This is the same as :s//~/"
;;   :repeat nil
;;   :jump t
;;   :move-point nil
;;   :motion evil-line
;;   (interactive "<r><a>")
;;   (apply #'evil-ex-substitute beg end
;;          (evil-ex-get-substitute-info (concat "//~/" flags))))

;; (evil-define-operator evilspeak-ex-repeat-substitute-with-flags
;;   (beg end flags)
;;   "Repeat last substitute command with last flags.
;; This is the same as :s//~/&"
;;   :repeat nil
;;   :jump t
;;   :move-point nil
;;   :motion evil-line
;;   (interactive "<r><a>")
;;   (apply #'evil-ex-substitute beg end
;;          (evil-ex-get-substitute-info (concat "//~/&" flags))))

;; (evil-define-operator evilspeak-ex-repeat-substitute-with-search
;;   (beg end flags)
;;   "Repeat last substitute command with last search pattern.
;; This is the same as :s//~/r"
;;   :repeat nil
;;   :jump t
;;   :move-point nil
;;   :motion evil-line
;;   (interactive "<r><a>")
;;   (apply #'evil-ex-substitute beg end
;;          (evil-ex-get-substitute-info (concat "//~/r" flags))))

;; (evil-define-operator evilspeak-ex-repeat-substitute-with-search-and-flags
;;   (beg end flags)
;;   "Repeat last substitute command with last search pattern and last flags.
;; This is the same as :s//~/&r"
;;   :repeat nil
;;   :jump t
;;   :move-point nil
;;   :motion evil-line
;;   (interactive "<r><a>")
;;   (apply #'evil-ex-substitute beg end
;;          (evil-ex-get-substitute-info (concat "//~/&r" flags))))

;; (evil-define-operator evilspeak-ex-repeat-global-substitute ()
;;   "Repeat last substitute command on the whole buffer.
;; This is the same as :%s//~/&"
;;   :repeat nil
;;   :jump t
;;   :move-point nil
;;   :motion evil-line
;;   (interactive)
;;   (apply #'evil-ex-substitute (point-min) (point-max)
;;          (evil-ex-get-substitute-info (concat "//~/&"))))

;; (evil-define-operator evilspeak-ex-global
;;   (beg end pattern command &optional invert)
;;   "The Ex global command.
;; \[BEG,END]global[!]/PATTERN/COMMAND"
;;   :motion mark-whole-buffer
;;   :move-point nil
;;   (interactive "<r><g/><!>")
;;   (unless pattern
;;     (user-error "No pattern given"))
;;   (unless command
;;     (user-error "No command given"))
;;   (evil-with-single-undo
;;     (let ((case-fold-search
;;            (eq (evil-ex-regex-case pattern 'smart) 'insensitive))
;;           match markers)
;;       (when (and pattern command)
;;         (setq isearch-string pattern)
;;         (isearch-update-ring pattern t)
;;         (goto-char beg)
;;         (evil-move-beginning-of-line)
;;         (while (< (point) end)
;;           (setq match (re-search-forward pattern (line-end-position) t))
;;           (when (or (and match (not invert))
;;                     (and invert (not match)))
;;             (push (move-marker (make-marker)
;;                                (or (and match (match-beginning 0))
;;                                    (line-beginning-position)))
;;                   markers))
;;           (forward-line))
;;         (setq markers (nreverse markers))
;;         (unwind-protect
;;             (dolist (marker markers)
;;               (goto-char marker)
;;               (evil-ex-eval command))
;;           ;; ensure that all markers are deleted afterwards,
;;           ;; even in the event of failure
;;           (dolist (marker markers)
;;             (set-marker marker nil)))))))

;; (evil-define-operator evilspeak-ex-global-inverted
;;   (beg end pattern command &optional invert)
;;   "The Ex vglobal command.
;; \[BEG,END]vglobal/PATTERN/COMMAND"
;;   :motion mark-whole-buffer
;;   :move-point nil
;;   (interactive "<r><g/><!>")
;;   (evil-ex-global beg end pattern command (not invert)))

;; (evil-define-operator evilspeak-ex-normal (beg end commands)
;;   "The Ex normal command.
;; Execute the argument as normal command on each line in the
;; range. The given argument is passed straight to
;; `execute-kbd-macro'.  The default is the current line."
;;   :motion evil-line
;;   (interactive "<r><a>")
;;   (evil-with-single-undo
;;     (let (markers evil-ex-current-buffer prefix-arg current-prefix-arg)
;;       (goto-char beg)
;;       (while
;;           (and (< (point) end)
;;                (progn
;;                  (push (move-marker (make-marker) (line-beginning-position))
;;                        markers)
;;                  (and (= (forward-line) 0) (bolp)))))
;;       (setq markers (nreverse markers))
;;       (deactivate-mark)
;;       (evil-force-normal-state)
;;       ;; replace ^[ by escape
;;       (setq commands
;;             (vconcat
;;              (mapcar #'(lambda (ch) (if (equal ch ?) 'escape ch))
;;                      (append commands nil))))
;;       (dolist (marker markers)
;;         (goto-char marker)
;;         (condition-case nil
;;             (execute-kbd-macro commands)
;;           (error nil))
;;         (evil-force-normal-state)
;;         (set-marker marker nil)))))

;; (evil-define-command evilspeak-goto-char (position)
;;   "Go to POSITION in the buffer.
;; Default position is the beginning of the buffer."
;;   (interactive "p")
;;   (let ((position (evil-normalize-position
;;                    (or position (point-min)))))
;;     (goto-char position)))

;; (evil-define-operator evilspeak-ex-line-number (beg end)
;;   "Print the last line number."
;;   :motion mark-whole-buffer
;;   :move-point nil
;;   (interactive "<r>")
;;   (message "%d" (count-lines (point-min) end)))

;; (evil-define-command evilspeak-show-file-info ()
;;   "Shows basic file information."
;;   (let* ((nlines   (count-lines (point-min) (point-max)))
;;          (curr     (line-number-at-pos (point)))
;;          (perc     (if (> nlines 0)
;;                        (format "%d%%" (* (/ (float curr) (float nlines)) 100.0))
;;                      "No lines in buffer"))
;;          (file     (buffer-file-name (buffer-base-buffer)))
;;          (writable (and file (file-writable-p file)))
;;          (readonly (if (and file (not writable)) "[readonly] " "")))
;;     (if file
;;         (message "\"%s\" %d %slines --%s--" file nlines readonly perc)
;;       (message "%d lines --%s--" nlines perc))))

;; (evil-define-operator evilspeak-ex-sort (beg end &optional options reverse)
;;   "The Ex sort command.
;; \[BEG,END]sort[!] [i][u]
;; The following additional options are supported:

;;   * i   ignore case
;;   * u   remove duplicate lines

;; The 'bang' argument means to sort in reverse order."
;;   :motion mark-whole-buffer
;;   :move-point nil
;;   (interactive "<r><a><!>")
;;   (let ((beg (copy-marker beg))
;;         (end (copy-marker end))
;;         sort-fold-case uniq)
;;     (dolist (opt (append options nil))
;;       (cond
;;        ((eq opt ?i) (setq sort-fold-case t))
;;        ((eq opt ?u) (setq uniq t))
;;        (t (user-error "Unsupported sort option: %c" opt))))
;;     (sort-lines reverse beg end)
;;     (when uniq
;;       (let (line prev-line)
;;         (goto-char beg)
;;         (while (and (< (point) end) (not (eobp)))
;;           (setq line (buffer-substring-no-properties
;;                       (line-beginning-position)
;;                       (line-end-position)))
;;           (if (and (stringp prev-line)
;;                    (eq t (compare-strings line nil nil
;;                                           prev-line nil nil
;;                                           sort-fold-case)))
;;               (delete-region (progn (forward-line 0) (point))
;;                              (progn (forward-line 1) (point)))
;;             (setq prev-line line)
;;             (forward-line 1)))))
;;     (goto-char beg)
;;     (set-marker beg nil)
;;     (set-marker end nil)))

;; ;;; Window navigation

;; (defun evil-resize-window (new-size &optional horizontal)
;;   "Set the current window's width or height to NEW-SIZE.
;; If HORIZONTAL is non-nil the width of the window is changed,
;; otherwise its height is changed."
;;   (let ((count (- new-size (if horizontal (window-width) (window-height)))))
;;     (if (>= emacs-major-version 24)
;;         (enlarge-window count horizontal)
;;       (let ((wincfg (current-window-configuration))
;;             (nwins (length (window-list)))
;;             (inhibit-redisplay t))
;;         (catch 'done
;;           (save-window-excursion
;;             (while (not (zerop count))
;;               (if (> count 0)
;;                   (progn
;;                     (enlarge-window 1 horizontal)
;;                     (setq count (1- count)))
;;                 (progn
;;                   (shrink-window 1 horizontal)
;;                   (setq count (1+ count))))
;;               (if (= nwins (length (window-list)))
;;                   (setq wincfg (current-window-configuration))
;;                 (throw 'done t)))))
;;         (set-window-configuration wincfg)))))

;; (defun evil-get-buffer-tree (wintree)
;;   "Extracts the buffer tree from a given window tree WINTREE."
;;   (if (consp wintree)
;;       (cons (car wintree) (mapcar #'evil-get-buffer-tree (cddr wintree)))
;;     (window-buffer wintree)))

;; (defun evil-restore-window-tree (win tree)
;;   "Restore the given buffer-tree layout as subwindows of WIN.
;; TREE is the tree layout to be restored."
;;   (cond
;;    ((and (consp tree) (cddr tree))
;;     (let ((newwin (split-window win nil (not (car tree)))))
;;       (evil-restore-window-tree win (cadr tree))
;;       (evil-restore-window-tree newwin (cons (car tree) (cddr tree)))))
;;    ((consp tree)
;;     (set-window-buffer win (cadr tree)))
;;    (t
;;     (set-window-buffer win tree))))

;; (defun evil-alternate-buffer (&optional window)
;;   "Return the last buffer WINDOW has displayed other than the
;; current one (equivalent to Vim's alternate buffer).

;; Returns the first item in `window-prev-buffers' that isn't
;; `window-buffer' of WINDOW."
;;   ;; If the last buffer visitied has been killed, then `window-prev-buffers'
;;   ;; returns a list with `current-buffer' at the head, we account for this
;;   ;; possibility.
;;   (let* ((prev-buffers (window-prev-buffers))
;;          (head (car prev-buffers)))
;;     (if (eq (car head) (window-buffer window))
;;         (cadr prev-buffers)
;;       head)))

;; (evil-define-command evilspeak-switch-to-windows-last-buffer ()
;;   "Switch to current windows last open buffer."
;;   :repeat nil
;;   (let ((previous-place (evil-alternate-buffer)))
;;     (when previous-place
;;       (switch-to-buffer (car previous-place))
;;       (goto-char (car (last previous-place))))))

;; (evil-define-command evilspeak-window-delete ()
;;   "Deletes the current window.
;; If `evil-auto-balance-windows' is non-nil then all children of
;; the deleted window's parent window are rebalanced."
;;   (let ((p (window-parent)))
;;     (delete-window)
;;     (when evil-auto-balance-windows
;;       ;; balance-windows raises an error if the parent does not have
;;       ;; any futher childs (then rebalancing is not necessary anywa)
;;       (condition-case nil
;;           (balance-windows p)
;;         (error)))))

;; (evil-define-command evilspeak-window-split (&optional count file)
;;   "Splits the current window horizontally, COUNT lines height,
;; editing a certain FILE. The new window will be created below
;; when `evil-split-window-below' is non-nil. If COUNT and
;; `evil-auto-balance-windows' are both non-nil then all children
;; of the parent of the splitted window are rebalanced."
;;   :repeat nil
;;   (interactive "P<f>")
;;   (split-window (selected-window) count
;;                 (if evil-split-window-below 'above 'below))
;;   (when (and (not count) evil-auto-balance-windows)
;;     (balance-windows (window-parent)))
;;   (when file
;;     (evil-edit file)))

;; (evil-define-command evilspeak-window-vsplit (&optional count file)
;;   "Splits the current window vertically, COUNT columns width,
;; editing a certain FILE. The new window will be created to the
;; right when `evil-vsplit-window-right' is non-nil. If COUNT and
;; `evil-auto-balance-windows'are both non-nil then all children
;; of the parent of the splitted window are rebalanced."
;;   :repeat nil
;;   (interactive "P<f>")
;;   (split-window (selected-window) count
;;                 (if evil-vsplit-window-right 'left 'right))
;;   (when (and (not count) evil-auto-balance-windows)
;;     (balance-windows (window-parent)))
;;   (when file
;;     (evil-edit file)))

;; (evil-define-command evilspeak-split-buffer (buffer)
;;   "Splits window and switches to another buffer."
;;   :repeat nil
;;   (interactive "<b>")
;;   (evil-window-split)
;;   (evil-buffer buffer))

;; (evil-define-command evilspeak-split-next-buffer (&optional count)
;;   "Splits the window and goes to the COUNT-th next buffer in the buffer list."
;;   :repeat nil
;;   (interactive "p")
;;   (evil-window-split)
;;   (evil-next-buffer count))

;; (evil-define-command evilspeak-split-prev-buffer (&optional count)
;;   "Splits window and goes to the COUNT-th prev buffer in the buffer list."
;;   :repeat nil
;;   (interactive "p")
;;   (evil-window-split)
;;   (evil-prev-buffer count))

;; (evil-define-command evilspeak-window-left (count)
;;   "Move the cursor to new COUNT-th window left of the current one."
;;   :repeat nil
;;   (interactive "p")
;;   (dotimes (i count)
;;     (windmove-left)))

;; (evil-define-command evilspeak-window-right (count)
;;   "Move the cursor to new COUNT-th window right of the current one."
;;   :repeat nil
;;   (interactive "p")
;;   (dotimes (i count)
;;     (windmove-right)))

;; (evil-define-command evilspeak-window-up (count)
;;   "Move the cursor to new COUNT-th window above the current one."
;;   :repeat nil
;;   (interactive "p")
;;   (dotimes (i (or count 1))
;;     (windmove-up)))

;; (evil-define-command evilspeak-window-down (count)
;;   "Move the cursor to new COUNT-th window below the current one."
;;   :repeat nil
;;   (interactive "p")
;;   (dotimes (i (or count 1))
;;     (windmove-down)))

;; (evil-define-command evilspeak-window-bottom-right ()
;;   "Move the cursor to bottom-right window."
;;   :repeat nil
;;   (let ((last-sibling (frame-root-window)))
;;     (while (and last-sibling (not (window-live-p last-sibling)))
;;       (setq last-sibling (window-last-child last-sibling)))
;;     (when last-sibling
;;       (select-window last-sibling))))

;; (evil-define-command evilspeak-window-top-left ()
;;   "Move the cursor to top-left window."
;;   :repeat nil
;;   (let ((first-child (window-child (frame-root-window))))
;;     (while (and first-child (not (window-live-p first-child)))
;;       (setq first-child (window-child first-child)))
;;     (when first-child
;;       (select-window
;;        first-child))))

;; (evil-define-command evilspeak-window-mru ()
;;   "Move the cursor to the previous (last accessed) buffer in another window.
;; More precisely, it selectes the most recently used buffer that is
;; shown in some other window, preferably of the current frame, and
;; is different from the current one."
;;   :repeat nil
;;   (catch 'done
;;     (dolist (buf (buffer-list (selected-frame)))
;;       (let ((win (get-buffer-window buf)))
;;         (when (and (not (eq buf (current-buffer)))
;;                    win
;;                    (not (eq win (selected-window))))
;;           (select-window win)
;;           (throw 'done nil))))))

;; (evil-define-command evilspeak-window-next (count)
;;   "Move the cursor to the next window in the cyclic order.
;; With COUNT go to the count-th window in the order starting from
;; top-left."
;;   :repeat nil
;;   (interactive "<c>")
;;   (if (not count)
;;       (select-window (next-window))
;;     (evil-window-top-left)
;;     (other-window (1- count))))

;; (evil-define-command evilspeak-window-prev (count)
;;   "Move the cursor to the previous window in the cyclic order.
;; With COUNT go to the count-th window in the order starting from
;; top-left."
;;   :repeat nil
;;   (interactive "<c>")
;;   (if (not count)
;;       (select-window (previous-window))
;;     (evil-window-top-left)
;;     (other-window (1- count))))

;; (evil-define-command evilspeak-window-new (count file)
;;   "Splits the current window horizontally
;; and opens a new buffer or edits a certain FILE."
;;   :repeat nil
;;   (interactive "P<f>")
;;   (let ((new-window (split-window (selected-window) count
;;                                   (if evil-split-window-below 'below 'above))))
;;     (when (and (not count) evil-auto-balance-windows)
;;       (balance-windows (window-parent)))
;;     (if file
;;         (evil-edit file)
;;       (let ((buffer (generate-new-buffer "*new*")))
;;         (set-window-buffer new-window buffer)
;;         (select-window new-window)
;;         (with-current-buffer buffer
;;           (funcall (default-value 'major-mode)))))))

;; (evil-define-command evilspeak-window-vnew (count file)
;;   "Splits the current window vertically
;; and opens a new buffer name or edits a certain FILE."
;;   :repeat nil
;;   (interactive "P<f>")
;;   (let ((new-window (split-window (selected-window) count
;;                                   (if evil-vsplit-window-right 'right 'left))))
;;     (when (and (not count) evil-auto-balance-windows)
;;       (balance-windows (window-parent)))
;;     (if file
;;         (evil-edit file)
;;       (let ((buffer (generate-new-buffer "*new*")))
;;         (set-window-buffer new-window buffer)
;;         (select-window new-window)
;;         (with-current-buffer buffer
;;           (funcall (default-value 'major-mode)))))))

;; (evil-define-command evilspeak-buffer-new (count file)
;;   "Creates a new buffer replacing the current window, optionaly
;;    editing a certain FILE"
;;   :repeat nil
;;   (interactive "P<f>")
;;   (if file
;;       (evil-edit file)
;;     (let ((buffer (generate-new-buffer "*new*")))
;;       (set-window-buffer nil buffer)
;;       (with-current-buffer buffer
;;         (funcall (default-value 'major-mode))))))

;; (evil-define-command evilspeak-window-increase-height (count)
;;   "Increase current window height by COUNT."
;;   :repeat nil
;;   (interactive "p")
;;   (evil-resize-window (+ (window-height) count)))

;; (evil-define-command evilspeak-window-decrease-height (count)
;;   "Decrease current window height by COUNT."
;;   :repeat nil
;;   (interactive "p")
;;   (evil-resize-window (- (window-height) count)))

;; (evil-define-command evilspeak-window-increase-width (count)
;;   "Increase current window width by COUNT."
;;   :repeat nil
;;   (interactive "p")
;;   (evil-resize-window (+ (window-width) count) t))

;; (evil-define-command evilspeak-window-decrease-width (count)
;;   "Decrease current window width by COUNT."
;;   :repeat nil
;;   (interactive "p")
;;   (evil-resize-window (- (window-width) count) t))

;; (evil-define-command evilspeak-window-set-height (count)
;;   "Sets the height of the current window to COUNT."
;;   :repeat nil
;;   (interactive "<c>")
;;   (evil-resize-window (or count (frame-height)) nil))

;; (evil-define-command evilspeak-window-set-width (count)
;;   "Sets the width of the current window to COUNT."
;;   :repeat nil
;;   (interactive "<c>")
;;   (evil-resize-window (or count (frame-width)) t))

;; (evil-define-command evilspeak-ex-resize (arg)
;;   "The ex :resize command.

;; If ARG is a signed positive integer, increase the current window
;; height by ARG.

;; If ARG is a signed negative integer, decrease the current window
;; height by ARG.

;; If ARG is a positive integer without explicit sign, set the current
;; window height to ARG.

;; If ARG is empty, maximize the current window height."
;;   (interactive "<a>")
;;   (if (or (not arg) (= 0 (length arg)))
;;       (evil-window-set-height nil)
;;     (let ((n (string-to-number arg)))
;;       (if (> n 0)
;;           (if (= ?+ (aref arg 0))
;;               (evil-window-increase-height n)
;;             (evil-window-set-height n))
;;         (evil-window-decrease-height (- n))))))

;; (evil-define-command evilspeak-window-rotate-upwards ()
;;   "Rotates the windows according to the currenty cyclic ordering."
;;   :repeat nil
;;   (let ((wlist (window-list))
;;         (blist (mapcar #'(lambda (w) (window-buffer w))
;;                        (window-list))))
;;     (setq blist (append (cdr blist) (list (car blist))))
;;     (while (and wlist blist)
;;       (set-window-buffer (car wlist) (car blist))
;;       (setq wlist (cdr wlist)
;;             blist (cdr blist)))
;;     (select-window (car (last (window-list))))))

;; (evil-define-command evilspeak-window-rotate-downwards ()
;;   "Rotates the windows according to the currenty cyclic ordering."
;;   :repeat nil
;;   (let ((wlist (window-list))
;;         (blist (mapcar #'(lambda (w) (window-buffer w))
;;                        (window-list))))
;;     (setq blist (append (last blist) blist))
;;     (while (and wlist blist)
;;       (set-window-buffer (car wlist) (car blist))
;;       (setq wlist (cdr wlist)
;;             blist (cdr blist)))
;;     (select-window (cadr (window-list)))))

;; (evil-define-command evilspeak-window-move-very-top ()
;;   "Closes the current window, splits the upper-left one horizontally
;; and redisplays the current buffer there."
;;   :repeat nil
;;   (unless (one-window-p)
;;     (save-excursion
;;       (let ((b (current-buffer)))
;;         (delete-window)
;;         (let ((btree (evil-get-buffer-tree (car (window-tree)))))
;;           (delete-other-windows)
;;           (let ((newwin (selected-window))
;;                 (subwin (split-window)))
;;             (evil-restore-window-tree subwin btree)
;;             (set-window-buffer newwin b)
;;             (select-window newwin)))))
;;     (balance-windows)))

;; (evil-define-command evilspeak-window-move-far-left ()
;;   "Closes the current window, splits the upper-left one vertically
;; and redisplays the current buffer there."
;;   :repeat nil
;;   (unless (one-window-p)
;;     (save-excursion
;;       (let ((b (current-buffer)))
;;         (delete-window)
;;         (let ((btree (evil-get-buffer-tree (car (window-tree)))))
;;           (delete-other-windows)
;;           (let ((newwin (selected-window))
;;                 (subwin (split-window-horizontally)))
;;             (evil-restore-window-tree subwin btree)
;;             (set-window-buffer newwin b)
;;             (select-window newwin)))))
;;     (balance-windows)))

;; (evil-define-command evilspeak-window-move-far-right ()
;;   "Closes the current window, splits the lower-right one vertically
;; and redisplays the current buffer there."
;;   :repeat nil
;;   (unless (one-window-p)
;;     (save-excursion
;;       (let ((b (current-buffer)))
;;         (delete-window)
;;         (let ((btree (evil-get-buffer-tree (car (window-tree)))))
;;           (delete-other-windows)
;;           (let ((subwin (selected-window))
;;                 (newwin (split-window-horizontally)))
;;             (evil-restore-window-tree subwin btree)
;;             (set-window-buffer newwin b)
;;             (select-window newwin)))))
;;     (balance-windows)))

;; (evil-define-command evilspeak-window-move-very-bottom ()
;;   "Closes the current window, splits the lower-right one horizontally
;; and redisplays the current buffer there."
;;   :repeat nil
;;   (unless (one-window-p)
;;     (save-excursion
;;       (let ((b (current-buffer)))
;;         (delete-window)
;;         (let ((btree (evil-get-buffer-tree (car (window-tree)))))
;;           (delete-other-windows)
;;           (let ((subwin (selected-window))
;;                 (newwin (split-window)))
;;             (evil-restore-window-tree subwin btree)
;;             (set-window-buffer newwin b)
;;             (select-window newwin)))))
;;     (balance-windows)))

;; ;;; Mouse handling

;; ;; Large parts of this code are taken from mouse.el which is
;; ;; distributed with GNU Emacs
;; (defun evil-mouse-drag-region (start-event)
;;   "Set the region to the text that the mouse is dragged over.
;; Highlight the drag area as you move the mouse.
;; This must be bound to a button-down mouse event.

;; If the click is in the echo area, display the `*Messages*' buffer.

;; START-EVENT should be the event that started the drag."
;;   (interactive "e")
;;   ;; Give temporary modes such as isearch a chance to turn off.
;;   (run-hooks 'mouse-leave-buffer-hook)
;;   (evil-mouse-drag-track start-event t))
;; (evil-set-command-property 'evil-mouse-drag-region :keep-visual t)

;; (defun evil-mouse-drag-track (start-event &optional
;;                                           do-mouse-drag-region-post-process)
;;   "Track mouse drags by highlighting area between point and cursor.
;; The region will be defined with mark and point.
;; DO-MOUSE-DRAG-REGION-POST-PROCESS should only be used by
;; `mouse-drag-region'."
;;   (mouse-minibuffer-check start-event)
;;   (setq mouse-selection-click-count-buffer (current-buffer))
;;   (deactivate-mark)
;;   (let* ((scroll-margin 0) ; Avoid margin scrolling (Bug#9541).
;;          (original-window (selected-window))
;;          ;; We've recorded what we needed from the current buffer and
;;          ;; window, now let's jump to the place of the event, where things
;;          ;; are happening.
;;          (_ (mouse-set-point start-event))
;;          (echo-keystrokes 0)
;;          (start-posn (event-start start-event))
;;          (start-point (posn-point start-posn))
;;          (start-window (posn-window start-posn))
;;          (start-window-start (window-start start-window))
;;          (start-hscroll (window-hscroll start-window))
;;          (bounds (window-edges start-window))
;;          (make-cursor-line-fully-visible nil)
;;          (top (nth 1 bounds))
;;          (bottom (if (window-minibuffer-p start-window)
;;                      (nth 3 bounds)
;;                    ;; Don't count the mode line.
;;                    (1- (nth 3 bounds))))
;;          (on-link (and mouse-1-click-follows-link
;;                        (or mouse-1-click-in-non-selected-windows
;;                            (eq start-window original-window))
;;                        ;; Use start-point before the intangibility
;;                        ;; treatment, in case we click on a link inside an
;;                        ;; intangible text.
;;                        (mouse-on-link-p start-posn)))
;;          (click-count (1- (event-click-count start-event)))
;;          (remap-double-click (and on-link
;;                                   (eq mouse-1-click-follows-link 'double)
;;                                   (= click-count 1)))
;;          ;; Suppress automatic hscrolling, because that is a nuisance
;;          ;; when setting point near the right fringe (but see below).
;;          (auto-hscroll-mode-saved auto-hscroll-mode)
;;          (auto-hscroll-mode nil)
;;          event end end-point)

;;     (setq mouse-selection-click-count click-count)
;;     ;; In case the down click is in the middle of some intangible text,
;;     ;; use the end of that text, and put it in START-POINT.
;;     (if (< (point) start-point)
;;         (goto-char start-point))
;;     (setq start-point (point))
;;     (if remap-double-click
;;         (setq click-count 0))

;;     (setq click-count (mod click-count 4))

;;     ;; activate correct visual state
;;     (let ((range (evil-mouse-start-end start-point start-point click-count)))
;;       (set-mark (nth 0 range))
;;       (goto-char (nth 1 range)))

;;     (cond
;;      ((= click-count 0)
;;       (when (evil-visual-state-p) (evil-exit-visual-state)))
;;      ((= click-count 1)
;;       (evil-visual-char)
;;       (evil-visual-post-command))
;;      ((= click-count 2)
;;       (evil-visual-line)
;;       (evil-visual-post-command))
;;      ((= click-count 3)
;;       (evil-visual-block)
;;       (evil-visual-post-command)))

;;     ;; Track the mouse until we get a non-movement event.
;;     (track-mouse
;;       (while (progn
;;                (setq event (read-event))
;;                (or (mouse-movement-p event)
;;                    (memq (car-safe event) '(switch-frame select-window))))
;;         (unless (evil-visual-state-p)
;;           (cond
;;            ((= click-count 0) (evil-visual-char))
;;            ((= click-count 1) (evil-visual-char))
;;            ((= click-count 2) (evil-visual-line))
;;            ((= click-count 3) (evil-visual-block))))

;;         (evil-visual-pre-command)
;;         (unless (memq (car-safe event) '(switch-frame select-window))
;;           ;; Automatic hscrolling did not occur during the call to
;;           ;; `read-event'; but if the user subsequently drags the
;;           ;; mouse, go ahead and hscroll.
;;           (let ((auto-hscroll-mode auto-hscroll-mode-saved))
;;             (redisplay))
;;           (setq end (event-end event)
;;                 end-point (posn-point end))
;;           (if (and (eq (posn-window end) start-window)
;;                    (integer-or-marker-p end-point))
;;               (evil-mouse--drag-set-mark-and-point start-point
;;                                                    end-point click-count)
;;             (let ((mouse-row (cdr (cdr (mouse-position)))))
;;               (cond
;;                ((null mouse-row))
;;                ((< mouse-row top)
;;                 (mouse-scroll-subr start-window (- mouse-row top)
;;                                    nil start-point))
;;                ((>= mouse-row bottom)
;;                 (mouse-scroll-subr start-window (1+ (- mouse-row bottom))
;;                                    nil start-point))))))
;;         (evil-visual-post-command)))

;;     ;; Handle the terminating event if possible.
;;     (when (consp event)
;;       ;; Ensure that point is on the end of the last event.
;;       (when (and (setq end-point (posn-point (event-end event)))
;;                  (eq (posn-window end) start-window)
;;                  (integer-or-marker-p end-point)
;;                  (/= start-point end-point))
;;         (evil-mouse--drag-set-mark-and-point start-point
;;                                              end-point click-count))

;;       ;; Find its binding.
;;       (let* ((fun (key-binding (vector (car event))))
;;              (do-multi-click (and (> (event-click-count event) 0)
;;                                   (functionp fun)
;;                                   (not (memq fun '(mouse-set-point
;;                                                    mouse-set-region))))))
;;         (if (and (or (/= (mark) (point))
;;                      (= click-count 1) ; word selection
;;                      (and (memq (evil-visual-type) '(line block))))
;;                  (not do-multi-click))

;;             ;; If point has moved, finish the drag.
;;             (let (last-command this-command)
;;               (and mouse-drag-copy-region
;;                    do-mouse-drag-region-post-process
;;                    (let (deactivate-mark)
;;                      (evil-visual-expand-region)
;;                      (copy-region-as-kill (mark) (point))
;;                      (evil-visual-contract-region))))

;;           ;; If point hasn't moved, run the binding of the
;;           ;; terminating up-event.
;;           (if do-multi-click
;;               (goto-char start-point)
;;             (deactivate-mark))
;;           (when (and (functionp fun)
;;                      (= start-hscroll (window-hscroll start-window))
;;                      ;; Don't run the up-event handler if the window
;;                      ;; start changed in a redisplay after the
;;                      ;; mouse-set-point for the down-mouse event at
;;                      ;; the beginning of this function.  When the
;;                      ;; window start has changed, the up-mouse event
;;                      ;; contains a different position due to the new
;;                      ;; window contents, and point is set again.
;;                      (or end-point
;;                          (= (window-start start-window)
;;                             start-window-start)))
;;             (when (and on-link
;;                        (= start-point (point))
;;                        (evil-mouse--remap-link-click-p start-event event))
;;               ;; If we rebind to mouse-2, reselect previous selected
;;               ;; window, so that the mouse-2 event runs in the same
;;               ;; situation as if user had clicked it directly.  Fixes
;;               ;; the bug reported by juri@jurta.org on 2005-12-27.
;;               (if (or (vectorp on-link) (stringp on-link))
;;                   (setq event (aref on-link 0))
;;                 (select-window original-window)
;;                 (setcar event 'mouse-2)
;;                 ;; If this mouse click has never been done by the
;;                 ;; user, it doesn't have the necessary property to be
;;                 ;; interpreted correctly.
;;                 (put 'mouse-2 'event-kind 'mouse-click)))
;;             (push event unread-command-events)))))))

;; ;; This function is a plain copy of `mouse--drag-set-mark-and-point',
;; ;; which is only available in Emacs 24
;; (defun evil-mouse--drag-set-mark-and-point (start click click-count)
;;   (let* ((range (evil-mouse-start-end start click click-count))
;;          (beg (nth 0 range))
;;          (end (nth 1 range)))
;;     (cond ((eq (mark) beg)
;;            (goto-char end))
;;           ((eq (mark) end)
;;            (goto-char beg))
;;           ((< click (mark))
;;            (set-mark end)
;;            (goto-char beg))
;;           (t
;;            (set-mark beg)
;;            (goto-char end)))))

;; ;; This function is a plain copy of `mouse--remap-link-click-p',
;; ;; which is only available in Emacs 23
;; (defun evil-mouse--remap-link-click-p (start-event end-event)
;;   (or (and (eq mouse-1-click-follows-link 'double)
;;            (= (event-click-count start-event) 2))
;;       (and
;;        (not (eq mouse-1-click-follows-link 'double))
;;        (= (event-click-count start-event) 1)
;;        (= (event-click-count end-event) 1)
;;        (or (not (integerp mouse-1-click-follows-link))
;;            (let ((t0 (posn-timestamp (event-start start-event)))
;;                  (t1 (posn-timestamp (event-end   end-event))))
;;              (and (integerp t0) (integerp t1)
;;                   (if (> mouse-1-click-follows-link 0)
;;                       (<= (- t1 t0) mouse-1-click-follows-link)
;;                     (< (- t0 t1) mouse-1-click-follows-link))))))))

;; (defun evil-mouse-start-end (start end mode)
;;   "Return a list of region bounds based on START and END according to MODE.
;; If MODE is not 1 then set point to (min START END), mark to (max
;; START END).  If MODE is 1 then set point to start of word at (min
;; START END), mark to end of word at (max START END)."
;;   (evil-sort start end)
;;   (setq mode (mod mode 4))
;;   (if (/= mode 1) (list start end)
;;     (list
;;      (save-excursion
;;        (goto-char (min (point-max) (1+ start)))
;;        (if (zerop (forward-thing evil-mouse-word -1))
;;            (let ((bpnt (point)))
;;              (forward-thing evil-mouse-word +1)
;;              (if (> (point) start) bpnt (point)))
;;          (point-min)))
;;      (save-excursion
;;        (goto-char end)
;;        (1-
;;         (if (zerop (forward-thing evil-mouse-word +1))
;;             (let ((epnt (point)))
;;               (forward-thing evil-mouse-word -1)
;;               (if (<= (point) end) epnt (point)))
;;           (point-max)))))))

;; ;;; State switching

;; (evil-define-command evilspeak-exit-emacs-state (&optional buffer message)
;;   "Exit Emacs state.
;; Changes the state to the previous state, or to Normal state
;; if the previous state was Emacs state."
;;   :keep-visual t
;;   :suppress-operator t
;;   (interactive '(nil t))
;;   (with-current-buffer (or buffer (current-buffer))
;;     (when (evil-emacs-state-p)
;;       (evil-change-to-previous-state buffer message)
;;       (when (evil-emacs-state-p)
;;         (evil-normal-state (and message 1))))))

;; (defun evil-execute-in-normal-state ()
;;   "Execute the next command in Normal state."
;;   (interactive)
;;   (evil-delay '(not (memq this-command
;;                           '(evil-execute-in-normal-state
;;                             evil-use-register
;;                             digit-argument
;;                             negative-argument
;;                             universal-argument
;;                             universal-argument-minus
;;                             universal-argument-more
;;                             universal-argument-other-key)))
;;       `(progn
;;          (evil-change-state ',evil-state)
;;          (setq evil-move-cursor-back ',evil-move-cursor-back))
;;     'post-command-hook)
;;   (setq evil-move-cursor-back nil)
;;   (evil-normal-state)
;;   (evil-echo "Switched to Normal state for the next command ..."))

;; (defun evil-stop-execute-in-emacs-state ()
;;   (when (and (not (eq this-command #'evil-execute-in-emacs-state))
;;              (not (minibufferp)))
;;     (remove-hook 'post-command-hook 'evil-stop-execute-in-emacs-state)
;;     (when (buffer-live-p evil-execute-in-emacs-state-buffer)
;;       (with-current-buffer evil-execute-in-emacs-state-buffer
;;         (if (and (eq evil-previous-state 'visual)
;;                  (not (use-region-p)))
;;             (progn
;;               (evil-change-to-previous-state)
;;               (evil-exit-visual-state))
;;           (evil-change-to-previous-state))))
;;     (setq evil-execute-in-emacs-state-buffer nil)))

;; (evil-define-command evilspeak-execute-in-emacs-state ()
;;   "Execute the next command in Emacs state."
;;   (add-hook 'post-command-hook #'evil-stop-execute-in-emacs-state t)
;;   (setq evil-execute-in-emacs-state-buffer (current-buffer))
;;   (cond
;;    ((evil-visual-state-p)
;;     (let ((mrk (mark))
;;           (pnt (point)))
;;       (evil-emacs-state)
;;       (set-mark mrk)
;;       (goto-char pnt)))
;;    (t
;;     (evil-emacs-state)))
;;   (evil-echo "Switched to Emacs state for the next command ..."))

;; (defun evil-exit-visual-and-repeat (event)
;;   "Exit insert state and repeat event.
;; This special command should be used if some command called from
;; visual state should actually be called in normal-state.  The main
;; reason for doing this is that the repeat system should *not*
;; record the visual state information for some command.  This
;; command should be bound to exactly the same event in visual state
;; as the original command is bound in normal state.  EVENT is the
;; event that triggered the execution of this command."
;;   (interactive "e")
;;   (when (evil-visual-state-p)
;;     (evil-exit-visual-state)
;;     (push event unread-command-events)))
;; (evil-declare-ignore-repeat 'evil-exit-visual-and-repeat)

;; (provide 'evil-commands)

;; ;;; evil-commands.el ends here

(load-file "~/.emacs.d/my/evilspeak/evilspeak-maps.el")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;TODO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (evil-define-operator evilspeak-invert-case (beg end type)
;;   "Invert case of text."
;;   (let (char)
;;     (if (eq type 'block)
;;         (evil-apply-on-block #'evil-invert-case beg end nil)
;;       (save-excursion
;;         (goto-char beg)
;;         (while (< beg end)
;;           (setq char (following-char))
;;           (delete-char 1 nil)
;;           (if (eq (upcase char) char)
;;               (insert-char (downcase char) 1)
;;             (insert-char (upcase char) 1))
;;           (setq beg (1+ beg)))))))

;; (evil-define-operator evilspeak-invert-char (beg end type)
;;   "Invert case of character."
;;   :motion evil-forward-char
;;   (if (eq type 'block)
;;       (evil-apply-on-block #'evil-invert-case beg end nil)
;;     (evil-invert-case beg end)
;;     (when evil-this-motion
;;       (goto-char end)
;;       (when (and evil-cross-lines
;;                  evil-move-cursor-back
;;                  (not evil-move-beyond-eol)
;;                  (not (evil-visual-state-p))
;;                  (not (evil-operator-state-p))
;;                  (eolp) (not (eobp)) (not (bolp)))
;;         (forward-char)))))

;; (evil-define-operator evilspeak-rot13 (beg end type)
;;   "ROT13 encrypt text."
;;   (if (eq type 'block)
;;       (evil-apply-on-block #'evil-rot13 beg end nil)
;;     (rot13-region beg end)))
;; (evil-define-operator evilspeak-shift-left (beg end &optional count preserve-empty)
;;   "Shift text from BEG to END to the left.
;; The text is shifted to the nearest multiple of `evil-shift-width'
;; \(the rounding can be disabled by setting `evil-shift-round').
;; If PRESERVE-EMPTY is non-nil, lines that contain only spaces are
;; indented, too, otherwise they are ignored.  The relative column
;; of point is preserved if this function is not called
;; interactively. Otherwise, if the function is called as an
;; operator, point is moved to the first non-blank character.
;; See also `evil-shift-right'."
;;   :type line
;;   (evil-shift-left beg end count preserve-empty)
;;   (evilspeak-speak-region))

;; (evil-define-operator evilspeak-shift-right (beg end &optional count preserve-empty)
;;   "Shift text from BEG to END to the right.
;; The text is shifted to the nearest multiple of `evil-shift-width'
;; \(the rounding can be disabled by setting `evil-shift-round').
;; If PRESERVE-EMPTY is non-nil, lines that contain only spaces are
;; indented, too, otherwise they are ignored.  The relative column
;; of point is preserved if this function is not called
;; interactively. Otherwise, if the function is called as an
;; operator, point is moved to the first non-blank character.
;; See also `evil-shift-left'."
;;   :type line
;;   (interactive "<r><vc>")
;;   (setq count (or count 1))
;;   (let ((beg (set-marker (make-marker) beg))
;;         (end (set-marker (make-marker) end))
;;         (pnt-indent (current-column))
;;         first-shift) ; shift of first line
;;     (save-excursion
;;       (goto-char beg)
;;       (while (< (point) end)
;;         (let* ((indent (current-indentation))
;;                (new-indent
;;                 (max 0
;;                      (if (not evil-shift-round)
;;                          (+ indent (* count evil-shift-width))
;;                        (* (+ (/ indent evil-shift-width)
;;                              count
;;                              (cond
;;                               ((> count 0) 0)
;;                               ((zerop (mod indent evil-shift-width)) 0)
;;                               (t 1)))
;;                           evil-shift-width)))))
;;           (unless first-shift
;;             (setq first-shift (- new-indent indent)))
;;           (when (or preserve-empty
;;                     (save-excursion
;;                       (skip-chars-forward " \t")
;;                       (not (eolp))))
;;             (indent-to new-indent 0))
;;           (delete-region (point) (progn (skip-chars-forward " \t") (point)))
;;           (forward-line 1))))
;;     ;; assuming that point is in the first line, adjust its position
;;     (if (called-interactively-p 'any)
;;         (evil-first-non-blank)
;;       (move-to-column (max 0 (+ pnt-indent first-shift))))))

;; (evil-define-command evilspeak-shift-right-line (count)
;;   "Shift the current line COUNT times to the right.
;; The text is shifted to the nearest multiple of
;; `evil-shift-width'. Like `evil-shift-right' but always works on
;; the current line."
;;   (interactive "<c>")
;;   (evil-shift-right (line-beginning-position) (line-beginning-position 2) count t))

;; (evil-define-command evilspeak-shift-left-line (count)
;;   "Shift the current line COUNT times to the left.
;; The text is shifted to the nearest multiple of
;; `evil-shift-width'. Like `evil-shift-left' but always works on
;; the current line."
;;   (interactive "<c>")
;;   (evil-shift-left (line-beginning-position) (line-beginning-position 2) count t))

;; (evil-define-operator evilspeak-align-left (beg end type &optional width)
;;   "Right-align lines in the region at WIDTH columns.
;; The default for width is the value of `fill-column'."
;;   :motion evil-line
;;   :type line
;;   (interactive "<R><a>")
;;   (evil-justify-lines beg end 'left (if width
;;                                         (string-to-number width)
;;                                       0)))

;; (evil-define-operator evilspeak-align-right (beg end type &optional width)
;;   "Right-align lines in the region at WIDTH columns.
;; The default for width is the value of `fill-column'."
;;   :motion evil-line
;;   :type line
;;   (interactive "<R><a>")
;;   (evil-justify-lines beg end 'right (if width
;;                                          (string-to-number width)
;;                                        fill-column)))

;; (evil-define-operator evilspeak-align-center (beg end type &optional width)
;;   "Centers lines in the region between WIDTH columns.
;; The default for width is the value of `fill-column'."
;;   :motion evil-line
;;   :type line
;;   (interactive "<R><a>")
;;   (evil-justify-lines beg end 'center (if width
;;                                           (string-to-number width)
;;                                         fill-column)))
;; (evil-define-operator evilspeak-replace (beg end type char)
;;   "Replace text from BEG to END with CHAR."
;;   :motion evil-forward-char
;;   (interactive "<R>"
;;                (evil-save-cursor
;;                  (evil-refresh-cursor 'replace)
;;                  (list (evil-read-key))))
;;   (when char
;;     (if (eq type 'block)
;;         (save-excursion
;;           (evil-apply-on-rectangle
;;            #'(lambda (begcol endcol char)
;;                (let ((maxcol (evil-column (line-end-position))))
;;                  (when (< begcol maxcol)
;;                    (setq endcol (min endcol maxcol))
;;                    (let ((beg (evil-move-to-column begcol nil t))
;;                          (end (evil-move-to-column endcol nil t)))
;;                      (delete-region beg end)
;;                      (insert (make-string (- endcol begcol) char))))))
;;            beg end char))
;;       (goto-char beg)
;;       (cond
;;        ((eq char ?\n)
;;         (delete-region beg end)
;;         (newline)
;;         (when evil-auto-indent
;;           (indent-according-to-mode)))
;;        (t
;;         (while (< (point) end)
;;           (if (eq (char-after) ?\n)
;;               (forward-char)
;;             (delete-char 1)
;;             (insert-char char 1)))
;;         (goto-char (max beg (1- end))))))))

;; (evil-define-motion evilspeak-visual-restore ()
;;   "Restore previous selection."
;;   (let* ((point (point))
;;          (mark (or (mark t) point))
;;          (dir evil-visual-direction)
;;          (type (evil-visual-type))
;;          range)
;;     (unless (evil-visual-state-p)
;;       (cond
;;        ;; No previous selection.
;;        ((or (null evil-visual-selection)
;;             (null evil-visual-mark)
;;             (null evil-visual-point)))
;;        ;; If the type was one-to-one, it is preferable to infer
;;        ;; point and mark from the selection's boundaries. The reason
;;        ;; is that a destructive operation may displace the markers
;;        ;; inside the selection.
;;        ((evil-type-property type :one-to-one)
;;         (setq range (evil-contract-range (evil-visual-range))
;;               mark (evil-range-beginning range)
;;               point (evil-range-end range))
;;         (when (< dir 0)
;;           (evil-swap mark point)))
;;        ;; If the type wasn't one-to-one, we have to restore the
;;        ;; selection on the basis of the previous point and mark.
;;        (t
;;         (setq mark evil-visual-mark
;;               point evil-visual-point)))
;;       (evil-visual-make-selection mark point type t))))

;; (evil-define-motion evilspeak-visual-exchange-corners ()
;;   "Rearrange corners in Visual Block mode.

;;         M---+           +---M
;;         |   |    <=>    |   |
;;         +---P           P---+

;; For example, if mark is in the upper left corner and point
;; in the lower right, this function puts mark in the upper right
;; corner and point in the lower left."
;;   (cond
;;    ((eq evil-visual-selection 'block)
;;     (let* ((point (point))
;;            (mark (or (mark t) point))
;;            (point-col (evil-column point))
;;            (mark-col (evil-column mark))
;;            (mark (save-excursion
;;                    (goto-char mark)
;;                    (evil-move-to-column point-col)
;;                    (point)))
;;            (point (save-excursion
;;                     (goto-char point)
;;                     (evil-move-to-column mark-col)
;;                     (point))))
;;       (evil-visual-refresh mark point)))
;;    (t
;;     (evil-exchange-point-and-mark)
;;     (evil-visual-refresh))))

;; (evil-define-command evilspeak-visual-rotate (corner &optional beg end type)
;;   "In Visual Block selection, put point in CORNER.
;; Corner may be one of `upper-left', `upper-right', `lower-left'
;; and `lower-right':

;;         upper-left +---+ upper-right
;;                    |   |
;;         lower-left +---+ lower-right

;; When called interactively, the selection is rotated blockwise."
;;   :keep-visual t
;;   (interactive
;;    (let ((corners '(upper-left upper-right lower-right lower-left)))
;;      (list (or (cadr (memq (evil-visual-block-corner) corners))
;;                'upper-left))))
;;   (let* ((beg (or beg (point)))
;;          (end (or end (mark t) beg))
;;          (type (or type evil-this-type))
;;          range)
;;     (cond
;;      ((memq type '(rectangle block))
;;       (setq range (evil-block-rotate beg end :corner corner)
;;             beg (pop range)
;;             end (pop range))
;;       (unless (eq corner (evil-visual-block-corner corner beg end))
;;         (evil-swap beg end))
;;       (goto-char beg)
;;       (when (evil-visual-state-p)
;;         (evil-move-mark end)
;;         (evil-visual-refresh nil nil nil :corner corner)))
;;      ((memq corner '(upper-right lower-right))
;;       (goto-char (max beg end))
;;       (when (evil-visual-state-p)
;;         (evil-move-mark (min beg end))))
;;      (t
;;       (goto-char (min beg end))
;;       (when (evil-visual-state-p)
;;         (evil-move-mark (max beg end)))))))


;; ;; completion
;; (evil-define-command evilspeak-complete-next (&optional arg)
;;   "Complete to the nearest following word.
;; Search backward if a match isn't found.
;; Calls `evil-complete-next-func'."
;;   :repeat change
;;   (interactive "P")
;;   (if (minibufferp)
;;       (funcall evil-complete-next-minibuffer-func)
;;     (funcall evil-complete-next-func arg)))

;; (evil-define-command evilspeak-complete-previous (&optional arg)
;;   "Complete to the nearest preceding word.
;; Search forward if a match isn't found.
;; Calls `evil-complete-previous-func'."
;;   :repeat change
;;   (interactive "P")
;;   (if (minibufferp)
;;       (funcall evil-complete-previous-minibuffer-func)
;;     (funcall evil-complete-previous-func arg)))

;; (evil-define-command evilspeak-complete-next-line (&optional arg)
;;   "Complete a whole line.
;; Calls `evil-complete-next-line-func'."
;;   :repeat change
;;   (interactive "P")
;;   (if (minibufferp)
;;       (funcall evil-complete-next-minibuffer-func)
;;     (funcall evil-complete-next-line-func arg)))

;; (evil-define-command evilspeak-complete-previous-line (&optional arg)
;;   "Complete a whole line.
;; Calls `evil-complete-previous-line-func'."
;;   :repeat change
;;   (interactive "P")
;;   (if (minibufferp)
;;       (funcall evil-complete-previous-minibuffer-func)
;;     (funcall evil-complete-previous-line-func arg)))