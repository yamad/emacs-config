;;; jyh-functions.el --- custom functions
;;
;; part of emacs config for jyamad. see init.el

;;; Commentary:
;;
;; Custom (and collected) elisp code goes here


;;; Code:

(defvar current-date-format "%Y-%m-%d %a"
  "Format of date to insert with `insert-current-date` func")

(defun insert-current-date ()
  "insert the current date using the format of `current-date-format`"
  (interactive)
  (insert (format-time-string current-date-format (current-time))))
(bind-key "C-c C-d" 'insert-current-date)

(defun set-eol-conversion (new-eol)
  "Specify new end-of-line conversion NEW-EOL for the buffer's
file coding system.  This marks the buffer as modified.  Choices
are: unix, dos, mac"
  (interactive "End-of-line conversion for visited file: \n")
  ;; Check for valid input
  (unless (or (string-equal new-eol "unix")
              (string-equal new-eol "dos")
              (string-equal new-eol "mac"))
    (error "Invalid EOL type, %s" new-eol))

  (if buffer-file-coding-system
      (let ((new-coding-system
             (coding-system-change-eol-conversion
              buffer-file-coding-system new-eol)))
        (set-buffer-file-coding-system new-coding-system))
    (let ((new-coding-system
           (coding-system-change-eol-conversion
            'undecided new-eol)))
      (set-buffer-file-coding-system new-coding-system)))
  (message "EOL conversion now %s" new-eol))

(defun xah-syntax-color-hex ()
  "Syntax color hex color spec such as 「#ff1100」 in current buffer."
  (interactive)
  (font-lock-add-keywords
   nil
   '(("#[abcdef[:digit:]]\\{6\\}"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background (match-string-no-properties 0)))))))
  (font-lock-fontify-buffer))

(defun word-count (&optional b e)
  (interactive "r")
  (let ((b (if mark-active (region-beginning) (point-min)))
        (e (if mark-active (region-end) (point-max))))
    (save-excursion
      (save-restriction
        (narrow-to-region b e)
        (goto-char (point-min))
        (count-matches "\\sw+")))))

(defun randomly-inject-string (str &optional b e)
  (interactive "r")
  (let* ((b (if mark-active (region-beginning) (point-min)))
         (e (if mark-active (region-end) (point-max)))
         (wc (word-count b e))
         (step 0))
    (save-excursion
      (save-restriction
        (narrow-to-region b e)
        (goto-char (point-min))
        (while (and (< (point) (point-max)))
          (setq step (random (/ wc 5)))
          (forward-word step)
          (insert " " str))))))

(defun calc-eval-region-replace (beg end)
  "Evaluate region of math with calc and replace with result"
  (interactive "r")
  (let ((val (calc-eval (buffer-substring beg end))))
    (delete-region beg end)
    (insert val)))

(defun calc-regexp-replace (regexp)
  (interactive "sRegexp: ")
  (save-excursion
    (while (re-search-forward regexp nil t)
      (calc-eval-region-replace (match-beginning 0) (match-end 0)))))

(defun calc-eval-region-do (beg end ops)
  "Apply math OPS to region using calc and return result as string."
  (interactive "r\nsOperations: ")
  (calc-eval (concat (buffer-substring beg end) ops)))

(defun calc-eval-region-replace-do (beg end ops)
  (interactive "r\nsOperations: ")
  (let ((val (calc-eval-region-do beg end ops)))
    (delete-region beg end)
    (insert val)))

(defun calc-regexp-replace-do (regexp ops)
  "Evaluate and apply OPS to every REGEXP match."
  (interactive "sRegexp: \nsOperations: ")
  (save-excursion
    (while (re-search-forward regexp nil t)
      (calc-eval-region-replace-do (match-beginning 0) (match-end 0) ops))))

(provide 'jyh-functions)
;;; jyh-functions.el ends here
