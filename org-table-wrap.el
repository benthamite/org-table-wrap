;;; org-table-wrap.el --- Visual word-wrapping for Org mode tables -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Pablo Stafforini

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/org-table-wrap
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: outlines, wp

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-table-wrap provides visual word-wrapping for Org mode tables that
;; overflow the window width.  It uses overlays with `display' properties
;; to replace overflowing tables with a wrapped representation using
;; Unicode box-drawing characters.  The buffer text is never modified.
;;
;; When point enters a wrapped table, the overlay is removed to allow
;; normal editing.  When point leaves, the overlay is re-applied.
;;
;; Usage:
;;
;;   (add-hook 'org-mode-hook #'org-table-wrap-mode)
;;
;; Or globally:
;;
;;   (global-org-table-wrap-mode 1)

;;; Code:

(require 'org)
(require 'org-table)
(require 'org-element)

;;;; Customization

(defgroup org-table-wrap nil
  "Visual word-wrapping for Org mode tables."
  :group 'org
  :prefix "org-table-wrap-")

(defcustom org-table-wrap-min-column-width 5
  "Minimum character width for any column in a wrapped table."
  :type 'integer)

(defcustom org-table-wrap-use-unicode t
  "Use Unicode box-drawing characters for table borders.
When nil, use ASCII characters (`|', `-', `+')."
  :type 'boolean)

(defcustom org-table-wrap-padding 1
  "Number of spaces between cell content and column border."
  :type 'integer)

(defcustom org-table-wrap-width-fraction 0.75
  "Fraction of the window width to use for the wrapped table.
A value of 1.0 uses the full window width.  Lower values add a
safety margin that compensates for font rendering differences,
display scaling, and `org-indent-mode' prefix overhead.  The
default of 0.75 is conservative; increase it if the wrapped table
is too narrow for your setup."
  :type 'float)

;;;; Internal variables

;; Forward declaration; defined by `define-minor-mode' below.
(defvar org-table-wrap-mode)

(defvar-local org-table-wrap--overlays nil
  "List of (BEG END OVERLAY) entries for wrapped tables in this buffer.")

(defvar-local org-table-wrap--current-table nil
  "Cons of (BEG . END) for the table point is currently inside, or nil.")

(defvar org-table-wrap--resize-timer nil
  "Idle timer for debouncing window resize events.")

;;;; Border characters

(defun org-table-wrap--border-chars ()
  "Return an alist of border characters based on `org-table-wrap-use-unicode'."
  (if org-table-wrap-use-unicode
      '((vertical   . "│")
        (horizontal . "─")
        (cross      . "┼")
        (top-t      . "┬")
        (bottom-t   . "┴")
        (left-t     . "├")
        (right-t    . "┤")
        (top-left   . "┌")
        (top-right  . "┐")
        (bottom-left  . "└")
        (bottom-right . "┘"))
    '((vertical   . "|")
      (horizontal . "-")
      (cross      . "+")
      (top-t      . "+")
      (bottom-t   . "+")
      (left-t     . "+")
      (right-t    . "+")
      (top-left   . "+")
      (top-right  . "+")
      (bottom-left  . "+")
      (bottom-right . "+"))))

(defun org-table-wrap--char (name)
  "Return the border character for NAME."
  (alist-get name (org-table-wrap--border-chars)))

;;;; Table detection

(defun org-table-wrap--find-tables ()
  "Find all standard Org tables in the current buffer.
Return a list of (BEG END) pairs for each table.
Only includes tables that are visible (not inside folded headings/drawers)."
  (let (tables)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*|" nil t)
        (let ((start (line-beginning-position)))
          ;; Check if this line is visible
          (when (not (org-invisible-p (point)))
            ;; Find the extent of this table
            (let ((end (org-table-end)))
              ;; Skip if we already captured this table
              (unless (and tables
                          (>= start (caar tables))
                          (<= start (cadar tables)))
                (push (list start end) tables))
              (goto-char end))))))
    (nreverse tables)))

(defun org-table-wrap--table-width (beg end)
  "Return the maximum display width of the table between BEG and END.
This accounts for `line-prefix' (e.g. from `org-indent-mode') by
measuring pixel widths when a window is available, then converting
back to character columns."
  (let ((win (and (not noninteractive)
                  (or (get-buffer-window (current-buffer))
                      (selected-window)))))
    (if (and win (window-live-p win))
        ;; Pixel-accurate measurement (GUI/terminal)
        (let ((max-pixel 0))
          (save-excursion
            (goto-char beg)
            (while (< (point) end)
              (let ((pw (car (window-text-pixel-size
                              win
                              (line-beginning-position)
                              (line-end-position)))))
                (when (> pw max-pixel)
                  (setq max-pixel pw)))
              (forward-line 1)))
          ;; Convert pixels to character columns using the default char width
          (let ((char-width (frame-char-width (window-frame win))))
            (ceiling (/ (float max-pixel) char-width))))
      ;; No usable window (batch mode): fall back to character count
      (let ((max-width 0))
        (save-excursion
          (goto-char beg)
          (while (< (point) end)
            (let ((line-len (- (line-end-position) (line-beginning-position))))
              (when (> line-len max-width)
                (setq max-width line-len)))
            (forward-line 1)))
        max-width))))

(defun org-table-wrap--point-in-table-p ()
  "Return (BEG . END) if point is inside an Org table, nil otherwise."
  (when (org-at-table-p)
    (let ((beg (org-table-begin))
          (end (org-table-end)))
      (cons beg end))))

;;;; Text utilities

(defun org-table-wrap--visible-text (str)
  "Return only the visible characters from STR, preserving text properties.
Strips characters that have the `invisible' property set, which
handles Org emphasis markers hidden by `org-hide-emphasis-markers'."
  (let ((parts nil)
        (pos 0)
        (len (length str)))
    (while (< pos len)
      (let* ((invis (get-text-property pos 'invisible str))
             (next (or (next-single-property-change pos 'invisible str)
                       len)))
        (unless invis
          (push (substring str pos next) parts))
        (setq pos next)))
    (apply #'concat (nreverse parts))))

;;;; Table parsing

(defun org-table-wrap--parse-row (line)
  "Parse an Org table LINE into a list of cell content strings.
Returns nil if LINE is an hline."
  (when (string-match "^[ \t]*|" line)
    (if (string-match "^[ \t]*|[-+]" line)
        ;; This is an hline
        'hline
      ;; Data row: split by |
      (let* ((trimmed (string-trim line))
             ;; Remove leading and trailing |
             (inner (if (and (string-prefix-p "|" trimmed)
                             (string-suffix-p "|" trimmed))
                        (substring trimmed 1 -1)
                      (string-remove-prefix "|" trimmed)))
             (cells (split-string inner "|")))
        (mapcar #'string-trim cells)))))

(defun org-table-wrap--parse-table (beg end)
  "Parse the Org table between BEG and END.
Return a list of rows, where each row is either the symbol `hline'
or a list of cell content strings."
  (let (rows)
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (let* ((line (buffer-substring
                      (line-beginning-position)
                      (line-end-position)))
               (parsed (org-table-wrap--parse-row line)))
          (when parsed
            (push parsed rows)))
        (forward-line 1)))
    (nreverse rows)))

(defun org-table-wrap--num-columns (rows)
  "Return the number of columns from the parsed ROWS."
  (let ((max-cols 0))
    (dolist (row rows)
      (when (listp row)
        (let ((n (length row)))
          (when (> n max-cols)
            (setq max-cols n)))))
    max-cols))

;;;; Column width calculation

(defun org-table-wrap--natural-widths (rows ncols)
  "Compute the natural max-content width of each column.
ROWS is the parsed table, NCOLS is the number of columns.
Returns a vector of widths."
  (let ((widths (make-vector ncols 0)))
    (dolist (row rows)
      (when (listp row)
        (dotimes (i (min ncols (length row)))
          (let ((w (string-width (nth i row))))
            (when (> w (aref widths i))
              (aset widths i w))))))
    widths))

(defun org-table-wrap--allocate-widths (natural-widths available-width)
  "Allocate column widths proportionally from NATURAL-WIDTHS.
AVAILABLE-WIDTH is the usable character width after subtracting borders
and padding.  Returns a vector of allocated widths."
  (let* ((ncols (length natural-widths))
         (min-w org-table-wrap-min-column-width)
         (total-natural (apply #'+ (append natural-widths nil)))
         (allocated (make-vector ncols min-w)))
    (if (<= total-natural 0)
        ;; All columns are empty; distribute evenly
        (let ((per-col (max min-w (/ available-width (max 1 ncols)))))
          (dotimes (i ncols)
            (aset allocated i per-col)))
      ;; Proportional distribution
      (let ((remaining available-width)
            (remaining-natural total-natural)
            ;; Sort columns by natural width so smaller ones get their
            ;; minimum first
            (indices (sort (number-sequence 0 (1- ncols))
                          (lambda (a b)
                            (< (aref natural-widths a)
                               (aref natural-widths b))))))
        (dolist (i indices)
          (let* ((proportion (/ (float (aref natural-widths i))
                                (max 1.0 (float remaining-natural))))
                 (share (max min-w (floor (* proportion remaining)))))
            (aset allocated i share)
            (setq remaining (- remaining share))
            (setq remaining-natural (- remaining-natural
                                       (aref natural-widths i)))))
        ;; Distribute any leftover to the widest column
        (when (> remaining 0)
          (let ((widest-idx 0)
                (widest-val 0))
            (dotimes (i ncols)
              (when (> (aref natural-widths i) widest-val)
                (setq widest-val (aref natural-widths i))
                (setq widest-idx i)))
            (aset allocated widest-idx
                  (+ (aref allocated widest-idx) remaining))))))
    allocated))

;;;; Word wrapping

(defun org-table-wrap--wrap-text (text width)
  "Word-wrap TEXT to fit within WIDTH characters.
Returns a list of lines.  Prefers breaking at spaces; falls back to
character-level wrapping for long words."
  (if (<= (string-width text) width)
      (list text)
    (let ((words (split-string text " " t))
          lines
          current-line)
      (dolist (word words)
        (cond
         ;; Word itself is too long, need character-level wrapping
         ((> (string-width word) width)
          ;; Flush current line first
          (when current-line
            (push current-line lines)
            (setq current-line nil))
          ;; Break the long word by display width
          (let ((remaining word))
            (while (> (string-width remaining) width)
              (let ((chunk (truncate-string-to-width remaining width)))
                (push chunk lines)
                (setq remaining (substring remaining (length chunk)))))
            (when (> (length remaining) 0)
              (push remaining lines))))
         ;; First word on this line
         ((null current-line)
          (setq current-line word))
         ;; Word fits on current line
         ((<= (+ (string-width current-line) 1 (string-width word)) width)
          (setq current-line (concat current-line " " word)))
         ;; Word doesn't fit, start new line
         (t
          (push current-line lines)
          (setq current-line word))))
      (when current-line
        (push current-line lines))
      (nreverse lines))))

;;;; Display string construction

(defun org-table-wrap--pad-cell (text width)
  "Pad TEXT with spaces to fill WIDTH display columns.
Returns a string of exactly WIDTH display columns."
  (let ((text-width (string-width text)))
    (if (>= text-width width)
        (truncate-string-to-width text width)
      (concat text (make-string (- width text-width) ?\s)))))

(defun org-table-wrap--build-hline (col-widths position)
  "Build an hline string for the given COL-WIDTHS vector.
POSITION is one of `top', `middle', or `bottom'."
  (let* ((ncols (length col-widths))
         (padding org-table-wrap-padding)
         (h (org-table-wrap--char 'horizontal))
         (parts nil)
         (left (pcase position
                 ('top (org-table-wrap--char 'top-left))
                 ('bottom (org-table-wrap--char 'bottom-left))
                 (_ (org-table-wrap--char 'left-t))))
         (right (pcase position
                  ('top (org-table-wrap--char 'top-right))
                  ('bottom (org-table-wrap--char 'bottom-right))
                  (_ (org-table-wrap--char 'right-t))))
         (sep (pcase position
                ('top (org-table-wrap--char 'top-t))
                ('bottom (org-table-wrap--char 'bottom-t))
                (_ (org-table-wrap--char 'cross)))))
    (dotimes (i ncols)
      (when (> i 0)
        (push sep parts))
      (push (apply #'concat
                   (make-list (+ (aref col-widths i) (* 2 padding)) h))
            parts))
    (concat left
            (mapconcat #'identity (nreverse parts) "")
            right)))

(defun org-table-wrap--build-data-line (cells col-widths)
  "Build a single display line from CELLS content and COL-WIDTHS.
CELLS is a list of strings (one per column).  Pads to fill COL-WIDTHS."
  (let* ((ncols (length col-widths))
         (v (org-table-wrap--char 'vertical))
         (padding org-table-wrap-padding)
         (pad-str (make-string padding ?\s))
         parts)
    (dotimes (i ncols)
      (let ((cell (or (nth i cells) "")))
        (push (concat pad-str
                      (org-table-wrap--pad-cell cell (aref col-widths i))
                      pad-str)
              parts)))
    (concat v
            (mapconcat #'identity (nreverse parts) v)
            v)))

(defun org-table-wrap--render-data-row (row col-widths)
  "Render data ROW into display lines using COL-WIDTHS.
Wraps each cell to its allocated width, strips invisible characters,
and builds one display line per sub-row.  Returns a list of strings."
  (let* ((ncols (length col-widths))
         (wrapped-cells
          (let (result)
            (dotimes (i ncols)
              (let* ((cell-text (string-trim (or (nth i row) "")))
                     (cell-text (org-table-wrap--visible-text cell-text)))
                (push (org-table-wrap--wrap-text cell-text (aref col-widths i))
                      result)))
            (nreverse result)))
         (max-lines (apply #'max 1 (mapcar #'length wrapped-cells)))
         (lines nil))
    (dotimes (line-idx max-lines)
      (let (cells-for-line)
        (dotimes (i ncols)
          (push (or (nth line-idx (nth i wrapped-cells)) "")
                cells-for-line))
        (push (org-table-wrap--build-data-line
               (nreverse cells-for-line) col-widths)
              lines)))
    (nreverse lines)))

(defun org-table-wrap--build-display-string (rows col-widths)
  "Build the full display string for a wrapped table.
ROWS is the parsed table, COL-WIDTHS is the allocated width vector.
Returns a propertized string."
  (let (display-lines
        (has-top-hline (eq (car rows) 'hline))
        (has-bottom-hline (eq (car (last rows)) 'hline)))
    ;; Process each row
    (let ((row-index 0)
          (nrows (length rows)))
      (dolist (row rows)
        (let ((is-first (= row-index 0))
              (is-last (= row-index (1- nrows))))
          (if (eq row 'hline)
              ;; Hline row
              (let ((position (cond
                               (is-first 'top)
                               (is-last 'bottom)
                               (t 'middle))))
                (push (org-table-wrap--build-hline col-widths position)
                      display-lines))
            ;; Data row: wrap cells and build display lines
            (dolist (line (org-table-wrap--render-data-row row col-widths))
              (push line display-lines))))
        (setq row-index (1+ row-index))))
    ;; Reverse to get correct order, then add missing borders
    (setq display-lines (nreverse display-lines))
    (unless has-top-hline
      (push (org-table-wrap--build-hline col-widths 'top) display-lines))
    (unless has-bottom-hline
      (setq display-lines
            (append display-lines
                    (list (org-table-wrap--build-hline col-widths 'bottom)))))
    ;; Join into final string
    (let ((result (mapconcat #'identity display-lines "\n")))
      ;; Add org-table face without overwriting existing faces (bold, etc.)
      (org-table-wrap--add-face result 'org-table)
      result)))

(defun org-table-wrap--add-face (str face)
  "Add FACE to STR without overwriting existing face properties.
Merges FACE with any existing faces at each position."
  (let ((pos 0)
        (len (length str)))
    (while (< pos len)
      (let* ((existing (get-text-property pos 'face str))
             (merged (cond
                      ((null existing) face)
                      ((listp existing)
                       (if (memq face existing) existing
                         (append existing (list face))))
                      ((eq existing face) face)
                      (t (list existing face))))
             (next-change (or (next-single-property-change pos 'face str)
                              len)))
        (put-text-property pos next-change 'face merged str)
        (setq pos next-change)))))

;;;; Overlay management

(defun org-table-wrap--remove-overlays ()
  "Remove all org-table-wrap overlays from the current buffer."
  (dolist (entry org-table-wrap--overlays)
    (let ((ov (nth 2 entry)))
      (when (overlayp ov)
        (delete-overlay ov))))
  (setq org-table-wrap--overlays nil)
  ;; Remove our invisibility spec
  (remove-from-invisibility-spec '(org-table-wrap . t)))

(defun org-table-wrap--remove-overlay-at (beg end)
  "Remove the org-table-wrap overlay for the table at BEG..END."
  (setq org-table-wrap--overlays
        (cl-remove-if
         (lambda (entry)
           (when (and (= (nth 0 entry) beg)
                      (= (nth 1 entry) end))
             (let ((ov (nth 2 entry)))
               (when (overlayp ov)
                 (delete-overlay ov)))
             t))
         org-table-wrap--overlays)))

(defun org-table-wrap--apply-overlay (beg end display-string)
  "Apply an overlay to the table region BEG..END.
Uses a single overlay with `invisible' (to hide original text while
keeping point traversal) and `before-string' (to show the wrapped
rendering).  DISPLAY-STRING is the wrapped rendering."
  ;; Remove any existing overlay for this region
  (org-table-wrap--remove-overlay-at beg end)
  ;; Ensure our invisibility spec is registered
  (add-to-invisibility-spec '(org-table-wrap . t))
  (let* ((ov-end (if (and (< end (point-max))
                          (= (char-before end) ?\n))
                     (1- end)
                   end))
         (ov (make-overlay beg ov-end nil nil nil)))
    (overlay-put ov 'invisible 'org-table-wrap)
    (overlay-put ov 'before-string (concat display-string "\n"))
    (overlay-put ov 'org-table-wrap t)
    (overlay-put ov 'evaporate t)
    (push (list beg end ov) org-table-wrap--overlays)))

(defun org-table-wrap--find-overlay-at (pos)
  "Find the org-table-wrap overlay entry covering POS."
  (cl-find-if
   (lambda (entry)
     (and (<= (nth 0 entry) pos)
          (< pos (nth 1 entry))))
   org-table-wrap--overlays))

;;;; Core logic

(defun org-table-wrap--available-width (&optional pos)
  "Return the available width for table rendering in the current window.
Subtracts `line-prefix' width and applies `org-table-wrap-width-fraction'
to handle font rendering and display scaling differences."
  (let ((win (and (not noninteractive)
                  (or (get-buffer-window (current-buffer))
                      (selected-window)))))
    (if (and win (window-live-p win))
        (let* ((prefix (get-text-property (or pos (point)) 'line-prefix))
               (prefix-len (if (stringp prefix) (length prefix) 0))
               (raw-width (window-body-width win))
               (usable (- raw-width prefix-len)))
          (floor (* usable org-table-wrap-width-fraction)))
      60)))

(defun org-table-wrap--rendered-line-pixel-width (win pos)
  "Return the pixel width of the visual line at POS as rendered in WIN.
This measures the actual rendered output, accounting for overlays,
face remapping, text-scale-mode, and any other display modifications."
  (with-selected-window win
    (save-excursion
      (goto-char pos)
      (car (window-text-pixel-size
            win
            (line-beginning-position)
            (line-end-position))))))

(defun org-table-wrap--apply-and-measure (win beg end display-str)
  "Apply DISPLAY-STR as overlay at BEG..END in WIN and return pixel width.
Calls `redisplay' to ensure accurate measurement."
  (org-table-wrap--apply-overlay beg end display-str)
  (redisplay t)
  (org-table-wrap--rendered-line-pixel-width win beg))

(defun org-table-wrap--fit-display (rows col-widths beg end target-char-width)
  "Build a display string from ROWS and COL-WIDTHS that fits the window.
BEG and END are the table region bounds.  TARGET-CHAR-WIDTH is the
window width in characters.  After applying the overlay, measures the
actual rendered pixel width and shrinks columns if needed.  Returns
the display string."
  (let* ((display-str (org-table-wrap--build-display-string rows col-widths))
         (win (or (get-buffer-window (current-buffer)) (selected-window)))
         (win-px (if (and (not noninteractive) win (window-live-p win))
                     (window-body-width win t)
                   (* target-char-width (frame-char-width)))))
    (when (and (not noninteractive) win (window-live-p win))
      (let ((rendered-px (org-table-wrap--apply-and-measure
                          win beg end display-str)))
        (when (> rendered-px win-px)
          ;; Shrink all columns proportionally
          (let* ((ratio (/ (float win-px) (float rendered-px)))
                 (min-w org-table-wrap-min-column-width)
                 (ncols (length col-widths)))
            (dotimes (i ncols)
              (aset col-widths i
                    (max min-w (floor (* (aref col-widths i) ratio)))))
            (setq display-str
                  (org-table-wrap--build-display-string rows col-widths))
            (setq rendered-px
                  (org-table-wrap--apply-and-measure win beg end display-str))
            ;; Fine-tune: shrink widest column by 1 until it fits
            (let ((max-iter 5)
                  (iter 0))
              (while (and (< iter max-iter)
                          (> rendered-px win-px))
                (let ((widest-idx 0)
                      (widest-val 0))
                  (dotimes (i ncols)
                    (when (> (aref col-widths i) widest-val)
                      (setq widest-val (aref col-widths i))
                      (setq widest-idx i)))
                  (when (<= widest-val min-w)
                    (setq iter max-iter))
                  (aset col-widths widest-idx
                        (max min-w (1- (aref col-widths widest-idx)))))
                (setq display-str
                      (org-table-wrap--build-display-string rows col-widths))
                (setq rendered-px
                      (org-table-wrap--apply-and-measure
                       win beg end display-str))
                (setq iter (1+ iter))))))))
    display-str))

(defun org-table-wrap--process-table (beg end)
  "Process a single table between BEG and END.
Apply a wrapping overlay if the table overflows the window."
  (let* ((table-width (org-table-wrap--table-width beg end))
         (win-width (org-table-wrap--available-width beg)))
    (if (<= table-width win-width)
        ;; Table fits; remove any existing overlay
        (org-table-wrap--remove-overlay-at beg end)
      ;; Table overflows; build wrapped display
      (font-lock-ensure beg end)  ; ensure text properties are up to date
      (save-excursion
        (goto-char beg)  ; ensure point is at the table for prefix measurement
        (let* ((rows (org-table-wrap--parse-table beg end))
               (ncols (org-table-wrap--num-columns rows))
               (border-overhead (+ (1+ ncols) ; ncols+1 vertical separators
                                   (* 2 org-table-wrap-padding ncols)))
               (avail (max (* ncols org-table-wrap-min-column-width)
                           (- win-width border-overhead)))
               (natural (org-table-wrap--natural-widths rows ncols))
               (col-widths (org-table-wrap--allocate-widths natural avail))
               (display-str (org-table-wrap--fit-display
                             rows col-widths beg end win-width)))
          (org-table-wrap--apply-overlay beg end display-str))))))

(defun org-table-wrap--process-buffer ()
  "Process all tables in the current buffer.
Ensures processing runs with the buffer's window selected for
accurate pixel measurements.  If the buffer has no window in an
interactive session, defers processing until the buffer is displayed."
  (when (derived-mode-p 'org-mode)
    (let ((win (get-buffer-window (current-buffer))))
      (cond
       ;; Buffer has a visible window: use it for pixel-accurate measurement
       ((and win (window-live-p win))
        (with-selected-window win
          (org-table-wrap--process-buffer-1
           (org-table-wrap--find-tables))))
       ;; Interactive but no window: defer until buffer is displayed
       ((not noninteractive)
        (add-hook 'window-configuration-change-hook
                  #'org-table-wrap--deferred-process nil t))
       ;; Batch mode: use character-count fallback directly
       (t
        (org-table-wrap--process-buffer-1
         (org-table-wrap--find-tables)))))))

(defun org-table-wrap--deferred-process ()
  "Process tables when the buffer first becomes visible."
  (when (and org-table-wrap-mode (get-buffer-window (current-buffer)))
    (remove-hook 'window-configuration-change-hook
                 #'org-table-wrap--deferred-process t)
    (org-table-wrap--process-buffer)))

(defun org-table-wrap--process-buffer-1 (tables)
  "Internal: process TABLES in the current buffer."
  (when (derived-mode-p 'org-mode)
    (let ((tables (or tables (org-table-wrap--find-tables))))
      ;; Remove overlays for tables that no longer exist
      (let ((valid-overlays nil))
        (dolist (entry org-table-wrap--overlays)
          (let ((found nil))
            (dolist (tbl tables)
              (when (and (= (nth 0 entry) (nth 0 tbl))
                         (= (nth 1 entry) (nth 1 tbl)))
                (setq found t)))
            (if found
                (push entry valid-overlays)
              (when (overlayp (nth 2 entry))
                (delete-overlay (nth 2 entry))))))
        (setq org-table-wrap--overlays (nreverse valid-overlays)))
      ;; Process each table (skip the one point is in)
      (dolist (tbl tables)
        (let ((beg (nth 0 tbl))
              (end (nth 1 tbl)))
          (unless (and org-table-wrap--current-table
                       (= beg (car org-table-wrap--current-table))
                       (= end (cdr org-table-wrap--current-table)))
            (org-table-wrap--process-table beg end)))))))

;;;; Post-command hook (org-appear pattern)

(defun org-table-wrap--post-command ()
  "Track point movement relative to tables, toggling overlays.
With `invisible' overlays (not `display'), point can traverse the
table region normally, so `org-at-table-p' works."
  (when (and org-table-wrap-mode (derived-mode-p 'org-mode))
    (let* ((in-table (org-table-wrap--point-in-table-p))
           (same-table (and in-table org-table-wrap--current-table
                            (= (car in-table)
                               (car org-table-wrap--current-table))
                            (= (cdr in-table)
                               (cdr org-table-wrap--current-table)))))
      (unless same-table
        ;; Re-wrap the table we left (if any)
        (when org-table-wrap--current-table
          (org-table-wrap--process-table
           (car org-table-wrap--current-table)
           (cdr org-table-wrap--current-table)))
        ;; Remove overlay from the table we entered (if any)
        (when in-table
          (org-table-wrap--remove-overlay-at
           (car in-table) (cdr in-table)))
        (setq org-table-wrap--current-table in-table)))))

;;;; Window resize handling

(defun org-table-wrap--window-size-changed (_frame)
  "Handle window size changes by re-processing all tables after a delay."
  (when org-table-wrap--resize-timer
    (cancel-timer org-table-wrap--resize-timer))
  (setq org-table-wrap--resize-timer
        (run-with-idle-timer
         0.2 nil
         (lambda ()
           (setq org-table-wrap--resize-timer nil)
           (dolist (buf (buffer-list))
             (when (buffer-local-value 'org-table-wrap-mode buf)
               (with-current-buffer buf
                 (org-table-wrap--process-buffer))))))))

;;;; Minor modes

;;;###autoload
(define-minor-mode org-table-wrap-mode
  "Visual word-wrapping for Org mode tables.
When enabled, Org tables wider than the window are displayed with
word-wrapped cell content using overlays.  The buffer text is never
modified."
  :lighter " OTW"
  :group 'org-table-wrap
  (if org-table-wrap-mode
      (progn
        (org-table-wrap--process-buffer)
        (add-hook 'post-command-hook #'org-table-wrap--post-command nil t)
        (add-hook 'window-size-change-functions
                  #'org-table-wrap--window-size-changed))
    (remove-hook 'post-command-hook #'org-table-wrap--post-command t)
    (remove-hook 'window-configuration-change-hook
                 #'org-table-wrap--deferred-process t)
    (org-table-wrap--remove-overlays)
    (setq org-table-wrap--current-table nil)
    ;; Only remove the global resize hook when no other buffer uses the mode
    (unless (cl-some (lambda (buf)
                       (and (not (eq buf (current-buffer)))
                            (buffer-local-value 'org-table-wrap-mode buf)))
                     (buffer-list))
      (remove-hook 'window-size-change-functions
                   #'org-table-wrap--window-size-changed))))

(defun org-table-wrap--org-mode-hook ()
  "Enable `org-table-wrap-mode' in Org buffers."
  (org-table-wrap-mode 1))

;;;###autoload
(define-globalized-minor-mode global-org-table-wrap-mode
  org-table-wrap-mode
  org-table-wrap--org-mode-hook
  :group 'org-table-wrap)

(provide 'org-table-wrap)
;;; org-table-wrap.el ends here
