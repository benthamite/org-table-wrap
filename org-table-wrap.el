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
        (top-left   . "┌")
        (top-right  . "┐")
        (bottom-left  . "└")
        (bottom-right . "┘"))
    '((vertical   . "|")
      (horizontal . "-")
      (cross      . "+")
      (top-t      . "+")
      (bottom-t   . "+")
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
                          (>= start (caar (last tables)))
                          (<= start (cadar (last tables))))
                (push (list start end) tables))
              (goto-char end))))))
    (nreverse tables)))

(defun org-table-wrap--table-width (beg end)
  "Return the maximum line width of the table between BEG and END."
  (let ((max-width 0))
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (let ((line-len (- (line-end-position) (line-beginning-position))))
          (when (> line-len max-width)
            (setq max-width line-len)))
        (forward-line 1)))
    max-width))

(defun org-table-wrap--point-in-table-p ()
  "Return (BEG . END) if point is inside an Org table, nil otherwise."
  (when (org-at-table-p)
    (let ((beg (org-table-begin))
          (end (org-table-end)))
      (cons beg end))))

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
        (let* ((line (buffer-substring-no-properties
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
          ;; Break the long word
          (let ((pos 0)
                (wlen (length word)))
            (while (< pos wlen)
              (let* ((end (min wlen (+ pos width)))
                     (chunk (substring word pos end)))
                ;; If we have a partial current-line, try to fit what we can
                (push chunk lines)
                (setq pos end)))))
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
  "Pad TEXT with spaces to fill WIDTH characters.
Returns a string of exactly WIDTH characters."
  (let ((text-width (string-width text)))
    (if (>= text-width width)
        (substring text 0 (min (length text) width))
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
                 (_ (org-table-wrap--char 'vertical))))
         (right (pcase position
                  ('top (org-table-wrap--char 'top-right))
                  ('bottom (org-table-wrap--char 'bottom-right))
                  (_ (org-table-wrap--char 'vertical))))
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
            ;; Data row: wrap cells and build multiple display lines
            (let* ((ncols (length col-widths))
                   (wrapped-cells
                    (let (result)
                      (dotimes (i ncols)
                        (let* ((cell-text (or (nth i row) ""))
                               (cell-text (string-trim cell-text)))
                          (push (org-table-wrap--wrap-text
                                 cell-text (aref col-widths i))
                                result)))
                      (nreverse result)))
                   (max-lines (apply #'max 1
                                     (mapcar #'length wrapped-cells))))
              ;; Build each sub-line of this row
              (dotimes (line-idx max-lines)
                (let (cells-for-line)
                  (dotimes (i ncols)
                    (let* ((cell-lines (nth i wrapped-cells))
                           (cell-text (or (nth line-idx cell-lines) "")))
                      (push cell-text cells-for-line)))
                  (push (org-table-wrap--build-data-line
                         (nreverse cells-for-line) col-widths)
                        display-lines))))))
        (setq row-index (1+ row-index))))
    ;; If the table didn't start or end with hlines, add borders
    (unless has-top-hline
      (push (org-table-wrap--build-hline col-widths 'top) display-lines))
    (when (not has-bottom-hline)
      (setq display-lines
            (cons (org-table-wrap--build-hline col-widths 'bottom)
                  display-lines)))
    ;; Reverse and join
    (let ((result (mapconcat #'identity (nreverse display-lines) "\n")))
      ;; Apply org-table face to the entire string
      (put-text-property 0 (length result) 'face 'org-table result)
      result)))

;;;; Overlay management

(defun org-table-wrap--remove-overlays ()
  "Remove all org-table-wrap overlays from the current buffer."
  (dolist (entry org-table-wrap--overlays)
    (let ((ov (nth 2 entry)))
      (when (overlayp ov)
        (delete-overlay ov))))
  (setq org-table-wrap--overlays nil))

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
  "Apply a display overlay to the table region BEG..END.
DISPLAY-STRING is the wrapped rendering."
  ;; Remove any existing overlay for this region
  (org-table-wrap--remove-overlay-at beg end)
  ;; The overlay covers from BEG to END-1 (excluding the final newline)
  ;; so point can still be placed at END.
  (let* ((ov-end (if (and (< end (point-max))
                          (= (char-before end) ?\n))
                     (1- end)
                   end))
         (ov (make-overlay beg ov-end nil t nil)))
    (overlay-put ov 'display display-string)
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

(defun org-table-wrap--available-width ()
  "Return the available width for table rendering in the current window."
  (if (window-live-p (selected-window))
      (window-body-width)
    80))

(defun org-table-wrap--process-table (beg end)
  "Process a single table between BEG and END.
Apply a wrapping overlay if the table overflows the window."
  (let* ((table-width (org-table-wrap--table-width beg end))
         (win-width (org-table-wrap--available-width)))
    (if (<= table-width win-width)
        ;; Table fits; remove any existing overlay
        (org-table-wrap--remove-overlay-at beg end)
      ;; Table overflows; build wrapped display
      (let* ((rows (org-table-wrap--parse-table beg end))
             (ncols (org-table-wrap--num-columns rows))
             (border-overhead (+ (1+ ncols) ; ncols+1 vertical separators
                                 (* 2 org-table-wrap-padding ncols)))
             (avail (max (* ncols org-table-wrap-min-column-width)
                         (- win-width border-overhead)))
             (natural (org-table-wrap--natural-widths rows ncols))
             (col-widths (org-table-wrap--allocate-widths natural avail))
             (display-str (org-table-wrap--build-display-string
                           rows col-widths)))
        (org-table-wrap--apply-overlay beg end display-str)))))

(defun org-table-wrap--process-buffer ()
  "Process all tables in the current buffer."
  (when (derived-mode-p 'org-mode)
    (let ((tables (org-table-wrap--find-tables)))
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
  "Track point movement relative to tables, toggling overlays."
  (when (and org-table-wrap-mode (derived-mode-p 'org-mode))
    (let ((in-table (org-table-wrap--point-in-table-p)))
      (cond
       ;; Point entered a table
       ((and in-table (not org-table-wrap--current-table))
        (setq org-table-wrap--current-table in-table)
        ;; Remove the overlay so user can edit
        (org-table-wrap--remove-overlay-at
         (car in-table) (cdr in-table)))
       ;; Point moved to a different table
       ((and in-table org-table-wrap--current-table
             (not (and (= (car in-table)
                          (car org-table-wrap--current-table))
                       (= (cdr in-table)
                          (cdr org-table-wrap--current-table)))))
        ;; Re-wrap the old table
        (org-table-wrap--process-table
         (car org-table-wrap--current-table)
         (cdr org-table-wrap--current-table))
        ;; Unwrap the new table
        (setq org-table-wrap--current-table in-table)
        (org-table-wrap--remove-overlay-at
         (car in-table) (cdr in-table)))
       ;; Point left a table
       ((and (not in-table) org-table-wrap--current-table)
        ;; Re-wrap the table we left
        (org-table-wrap--process-table
         (car org-table-wrap--current-table)
         (cdr org-table-wrap--current-table))
        (setq org-table-wrap--current-table nil))))))

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
    (remove-hook 'window-size-change-functions
                 #'org-table-wrap--window-size-changed)
    (org-table-wrap--remove-overlays)
    (setq org-table-wrap--current-table nil)))

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
