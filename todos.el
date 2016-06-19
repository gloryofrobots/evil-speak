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