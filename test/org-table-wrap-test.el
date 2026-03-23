;;; org-table-wrap-test.el --- Tests for org-table-wrap -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Pablo Stafforini

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; ERT tests for org-table-wrap.

;;; Code:

(require 'ert)
(require 'org)
(require 'org-table-wrap)

;;;; Test helpers

(defmacro org-table-wrap-test-with-buffer (content &rest body)
  "Create a temp Org buffer with CONTENT and evaluate BODY.
The buffer is set to org-mode and has a fixed window width of 40."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (org-mode)
     (insert ,content)
     (goto-char (point-min))
     ;; Override available width to 40 for deterministic tests
     (cl-letf (((symbol-function 'org-table-wrap--available-width)
                (lambda (&optional _pos) 40)))
       ,@body)))

(defmacro org-table-wrap-test-with-width (width content &rest body)
  "Create a temp Org buffer with CONTENT and evaluate BODY.
Override the available width to WIDTH."
  (declare (indent 2) (debug t))
  `(with-temp-buffer
     (org-mode)
     (insert ,content)
     (goto-char (point-min))
     (cl-letf (((symbol-function 'org-table-wrap--available-width)
                (lambda (&optional _pos) ,width)))
       ,@body)))

;;;; Parsing tests

(ert-deftest org-table-wrap-test-parse-data-row ()
  "Parse a data row into cell contents."
  (let ((result (org-table-wrap--parse-row "| foo | bar | baz |")))
    (should (equal result '("foo" "bar" "baz")))))

(ert-deftest org-table-wrap-test-parse-hline ()
  "Parse an hline row."
  (let ((result (org-table-wrap--parse-row "|---+---+---|")))
    (should (eq result 'hline))))

(ert-deftest org-table-wrap-test-parse-padded-row ()
  "Parse a row with extra whitespace."
  (let ((result (org-table-wrap--parse-row "|  hello  |  world  |")))
    (should (equal result '("hello" "world")))))

(ert-deftest org-table-wrap-test-parse-empty-cells ()
  "Parse a row with empty cells."
  (let ((result (org-table-wrap--parse-row "| | content | |")))
    (should (equal result '("" "content" "")))))

(ert-deftest org-table-wrap-test-parse-table ()
  "Parse a complete table."
  (org-table-wrap-test-with-buffer
      "| a | b |\n|---+---|\n| c | d |\n"
    (let* ((rows (org-table-wrap--parse-table (point-min) (point-max))))
      (should (= (length rows) 3))
      (should (equal (nth 0 rows) '("a" "b")))
      (should (eq (nth 1 rows) 'hline))
      (should (equal (nth 2 rows) '("c" "d"))))))

;;;; Column width tests

(ert-deftest org-table-wrap-test-natural-widths ()
  "Compute natural column widths."
  (let* ((rows '(("short" "very long content")
                 ("ab" "cd")))
         (widths (org-table-wrap--natural-widths rows 2)))
    (should (= (aref widths 0) 5))   ; "short"
    (should (= (aref widths 1) 17)))) ; "very long content"

(ert-deftest org-table-wrap-test-proportional-allocation ()
  "Wider columns get proportionally more space."
  (let* ((natural (vector 10 30))   ; 1:3 ratio
         (allocated (org-table-wrap--allocate-widths natural 40)))
    ;; Column 1 should be wider than column 0
    (should (> (aref allocated 1) (aref allocated 0)))
    ;; Total should not exceed available
    (should (<= (+ (aref allocated 0) (aref allocated 1)) 40))))

(ert-deftest org-table-wrap-test-minimum-width-enforced ()
  "All columns get at least the minimum width."
  (let* ((natural (vector 1 100))
         (org-table-wrap-min-column-width 5)
         (allocated (org-table-wrap--allocate-widths natural 20)))
    (should (>= (aref allocated 0) 5))
    (should (>= (aref allocated 1) 5))))

;;;; Word wrapping tests

(ert-deftest org-table-wrap-test-no-wrap-needed ()
  "Text fits within width: single line returned."
  (let ((result (org-table-wrap--wrap-text "hello" 10)))
    (should (equal result '("hello")))))

(ert-deftest org-table-wrap-test-word-wrap ()
  "Text wraps at word boundaries."
  (let ((result (org-table-wrap--wrap-text "hello world foo" 11)))
    (should (= (length result) 2))
    (should (equal (nth 0 result) "hello world"))
    (should (equal (nth 1 result) "foo"))))

(ert-deftest org-table-wrap-test-character-wrap ()
  "Long words fall back to character-level wrapping."
  (let ((result (org-table-wrap--wrap-text "abcdefghijklmno" 5)))
    (should (>= (length result) 3))
    ;; Each chunk should be at most 5 chars
    (dolist (line result)
      (should (<= (length line) 5)))))

(ert-deftest org-table-wrap-test-wrap-empty-string ()
  "Empty string wraps to a single empty line."
  (let ((result (org-table-wrap--wrap-text "" 10)))
    (should (equal result '("")))))

;;;; Border character tests

(ert-deftest org-table-wrap-test-unicode-borders ()
  "Unicode border characters are used when enabled."
  (let ((org-table-wrap-use-unicode t))
    (should (equal (org-table-wrap--char 'vertical) "│"))
    (should (equal (org-table-wrap--char 'horizontal) "─"))
    (should (equal (org-table-wrap--char 'cross) "┼"))
    (should (equal (org-table-wrap--char 'top-left) "┌"))
    (should (equal (org-table-wrap--char 'bottom-right) "┘"))))

(ert-deftest org-table-wrap-test-ascii-borders ()
  "ASCII border characters are used when unicode is disabled."
  (let ((org-table-wrap-use-unicode nil))
    (should (equal (org-table-wrap--char 'vertical) "|"))
    (should (equal (org-table-wrap--char 'horizontal) "-"))
    (should (equal (org-table-wrap--char 'cross) "+"))))

;;;; Hline construction tests

(ert-deftest org-table-wrap-test-hline-top ()
  "Top hline uses correct corner characters."
  (let ((org-table-wrap-use-unicode t)
        (org-table-wrap-padding 1))
    (let ((hline (org-table-wrap--build-hline (vector 3 4) 'top)))
      (should (string-prefix-p "┌" hline))
      (should (string-suffix-p "┐" hline))
      (should (string-match-p "┬" hline)))))

(ert-deftest org-table-wrap-test-hline-middle ()
  "Middle hline uses cross characters."
  (let ((org-table-wrap-use-unicode t)
        (org-table-wrap-padding 1))
    (let ((hline (org-table-wrap--build-hline (vector 3 4) 'middle)))
      (should (string-match-p "┼" hline)))))

(ert-deftest org-table-wrap-test-hline-bottom ()
  "Bottom hline uses correct corner characters."
  (let ((org-table-wrap-use-unicode t)
        (org-table-wrap-padding 1))
    (let ((hline (org-table-wrap--build-hline (vector 3 4) 'bottom)))
      (should (string-prefix-p "└" hline))
      (should (string-suffix-p "┘" hline))
      (should (string-match-p "┴" hline)))))

;;;; Display string tests

(ert-deftest org-table-wrap-test-display-string-has-face ()
  "The display string has the org-table face applied."
  (let* ((org-table-wrap-use-unicode t)
         (org-table-wrap-padding 1)
         (rows '(hline ("hello" "world") hline))
         (widths (vector 10 10))
         (display (org-table-wrap--build-display-string rows widths)))
    (should (eq (get-text-property 0 'face display) 'org-table))))

(ert-deftest org-table-wrap-test-display-string-structure ()
  "The display string has the expected number of lines."
  (let* ((org-table-wrap-use-unicode t)
         (org-table-wrap-padding 1)
         (rows '(hline ("a" "b") hline ("c" "d") hline))
         (widths (vector 5 5))
         (display (org-table-wrap--build-display-string rows widths))
         (lines (split-string display "\n")))
    ;; 3 hlines + 2 data rows = 5 lines
    (should (= (length lines) 5))))

;;;; Table narrower than window (no overlay)

(ert-deftest org-table-wrap-test-narrow-table-no-overlay ()
  "A table that fits the window gets no overlay."
  (org-table-wrap-test-with-width 80
      "| a | b |\n|---+---|\n| c | d |\n"
    (org-table-wrap-mode 1)
    ;; No overlays should exist since the table is narrow
    (should (null org-table-wrap--overlays))
    (org-table-wrap-mode -1)))

;;;; Table wider than window (overlay applied)

(ert-deftest org-table-wrap-test-wide-table-gets-overlay ()
  "A table wider than the window gets a display overlay."
  (let ((wide-table
         (concat
          "| Column one has a lot of text | Column two also has lots of text | Third column |\n"
          "|------------------------------+----------------------------------+---------------|\n"
          "| Some content here            | More content here                | Short         |\n")))
    (org-table-wrap-test-with-width 40 wide-table
      (org-table-wrap-mode 1)
      (should (= (length org-table-wrap--overlays) 1))
      ;; Verify the overlay has invisible + before-string
      (let* ((entry (car org-table-wrap--overlays))
             (ov (nth 2 entry)))
        (should (overlayp ov))
        (should (eq (overlay-get ov 'invisible) 'org-table-wrap))
        (should (stringp (overlay-get ov 'before-string))))
      (org-table-wrap-mode -1))))

;;;; Entering and leaving table toggles overlay

(ert-deftest org-table-wrap-test-enter-table-removes-overlay ()
  "When point enters a wrapped table, the overlay is removed."
  (let ((wide-table
         (concat
          "Some text before.\n"
          "| Very long column header one | Very long column header two |\n"
          "|-----------------------------+-----------------------------|\n"
          "| content                     | more content                |\n"
          "Some text after.\n")))
    (org-table-wrap-test-with-width 30 wide-table
      (org-table-wrap-mode 1)
      ;; Should have one overlay
      (should (= (length org-table-wrap--overlays) 1))
      ;; Move point into the table
      (goto-char (point-min))
      (forward-line 1)
      ;; Simulate post-command-hook
      (org-table-wrap--post-command)
      ;; Overlay should be removed (table is being edited)
      (should (null org-table-wrap--overlays))
      ;; Current table should be tracked
      (should org-table-wrap--current-table)
      ;; Move point out of the table
      (goto-char (point-min))
      (org-table-wrap--post-command)
      ;; Overlay should be re-applied
      (should (= (length org-table-wrap--overlays) 1))
      (should (null org-table-wrap--current-table))
      (org-table-wrap-mode -1))))

;;;; Multiple tables handled independently

(ert-deftest org-table-wrap-test-multiple-tables ()
  "Multiple tables in one buffer each get their own overlay."
  (let ((content
         (concat
          "| Very long column A one | Very long column A two |\n"
          "|------------------------+------------------------|\n"
          "| content A              | more A                 |\n"
          "\nSome text between tables.\n\n"
          "| Very long column B one | Very long column B two |\n"
          "|------------------------+------------------------|\n"
          "| content B              | more B                 |\n")))
    (org-table-wrap-test-with-width 30 content
      (org-table-wrap-mode 1)
      (should (= (length org-table-wrap--overlays) 2))
      (org-table-wrap-mode -1))))

;;;; Hline rows reformatted correctly

(ert-deftest org-table-wrap-test-hline-in-display ()
  "Hline rows appear correctly in the display string."
  (let* ((org-table-wrap-use-unicode t)
         (org-table-wrap-padding 1)
         (rows '(hline ("hello" "world") hline))
         (widths (vector 8 8))
         (display (org-table-wrap--build-display-string rows widths))
         (lines (split-string display "\n")))
    ;; First line should be a top hline (from the table's own hline)
    (should (string-prefix-p "│" (nth 0 lines)))
    ;; Last line should be a bottom hline (from the table's own hline)
    (should (string-prefix-p "│" (nth 2 lines)))
    ;; Note: these are 'middle' position hlines since no auto-borders
    ;; All hlines should use ─
    (should (string-match-p "─" (nth 0 lines)))
    (should (string-match-p "─" (nth 2 lines)))))

;;;; Empty cells handled

(ert-deftest org-table-wrap-test-empty-cells ()
  "Empty cells are rendered as blank space."
  (let* ((org-table-wrap-use-unicode t)
         (org-table-wrap-padding 1)
         (rows '(("" "content") ("data" "")))
         (widths (vector 5 7))
         (display (org-table-wrap--build-display-string rows widths))
         (lines (split-string display "\n")))
    ;; Should have 2 data lines (no auto-generated borders)
    (should (= (length lines) 2))
    ;; The first data line should contain empty first cell
    (let ((line (nth 0 lines)))
      (should (string-match-p "│" line)))))

;;;; Window resize triggers re-evaluation

(ert-deftest org-table-wrap-test-resize-triggers-reprocess ()
  "Window resize handler schedules re-processing."
  (let ((timer-created nil))
    (cl-letf (((symbol-function 'run-with-idle-timer)
               (lambda (_secs _repeat fn)
                 (setq timer-created t)
                 ;; Return a fake timer
                 'fake-timer)))
      (org-table-wrap--window-size-changed nil)
      (should timer-created))))

;;;; Mode cleanup

(ert-deftest org-table-wrap-test-mode-disable-cleans-up ()
  "Disabling the mode removes all overlays and hooks."
  (let ((wide-table
         (concat
          "| Very long column header one | Very long column header two |\n"
          "|-----------------------------+-----------------------------|\n"
          "| content                     | more content                |\n")))
    (org-table-wrap-test-with-width 30 wide-table
      (org-table-wrap-mode 1)
      (should (= (length org-table-wrap--overlays) 1))
      (org-table-wrap-mode -1)
      (should (null org-table-wrap--overlays))
      (should (null org-table-wrap--current-table)))))

;;;; Wrapping produces multi-line rows

(ert-deftest org-table-wrap-test-wrapped-row-has-multiple-lines ()
  "A row with content exceeding column width produces multiple display lines."
  (let* ((org-table-wrap-use-unicode t)
         (org-table-wrap-padding 1)
         ;; Give column 0 width 10, content is 23 chars -> should wrap
         (rows '(("hello world is great ok" "short")))
         (widths (vector 10 5))
         (display (org-table-wrap--build-display-string rows widths))
         (lines (split-string display "\n")))
    ;; "hello world is great ok" wrapping at width 10:
    ;; "hello" "world is" "great ok" -> 3 data lines (no auto-borders)
    (should (>= (length lines) 3))))

;;;; Text properties preservation

(ert-deftest org-table-wrap-test-display-string-preserves-org-table-face ()
  "The display string has org-table face throughout."
  (let* ((org-table-wrap-use-unicode t)
         (org-table-wrap-padding 1)
         (rows '(("bold text" "normal")))
         (widths (vector 10 7))
         (display (org-table-wrap--build-display-string rows widths)))
    ;; The entire string should have org-table face
    (should (eq (get-text-property 0 'face display) 'org-table))
    (should (eq (get-text-property (1- (length display)) 'face display)
                'org-table))))

;;;; Single data line building

(ert-deftest org-table-wrap-test-build-data-line ()
  "A data line has correct structure with borders and padding."
  (let ((org-table-wrap-use-unicode t)
        (org-table-wrap-padding 1))
    (let ((line (org-table-wrap--build-data-line '("hi" "there") (vector 5 5))))
      ;; Should start and end with │
      (should (string-prefix-p "│" line))
      (should (string-suffix-p "│" line))
      ;; Should contain three │ characters (left, middle, right)
      (should (= (cl-count ?│ line) 3)))))

;;;; Table detection

(ert-deftest org-table-wrap-test-find-tables ()
  "Find all tables in a buffer."
  (org-table-wrap-test-with-buffer
      (concat
       "* Heading\n"
       "| a | b |\n"
       "| c | d |\n"
       "\nSome text.\n\n"
       "| x | y |\n"
       "| z | w |\n")
    (let ((tables (org-table-wrap--find-tables)))
      (should (= (length tables) 2)))))

(ert-deftest org-table-wrap-test-table-width ()
  "Compute the width of a table."
  (org-table-wrap-test-with-buffer
      "| short | cell |\n| a     | b    |\n"
    (let ((width (org-table-wrap--table-width (point-min) (point-max))))
      ;; "| short | cell |" = 16 chars
      (should (= width 16)))))

(provide 'org-table-wrap-test)
;;; org-table-wrap-test.el ends here
