;;; igor-mode.el --- Major mode for editing Igor Pro procedure files

;; Copyright (C) 2011

;; Author:   Jason Yamada-Hanff <jyamada@fas.harvard.edu>
;; Keywords: languages
;; Created: Jan 13, 2011

;;; Commentary:
;;
;; Written for Igor Pro 6.12A.
;;
;; Code was initially based heavily on Fred White's visual-basic-mode
;; <http://www.emacswiki.org/cgi-bin/wiki/visual-basic-mode.el>

;;; Code:
(defvar igor-tab-width 4)

(defvar igor-mode-hook nil)

(defvar igor-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for IgorPro major mode")

;; Autoload for igor files
(setq auto-mode-alist
      (append '(("\\.ipf$" . igor-mode)) auto-mode-alist))


(defun igor-wrap-re-startline (word-re)
  "Wrap a regexp to require WORD-RE to be at the start of a line"
  (concat "^[ \t]*" word-re))

;; Igor Pro Language Keywords and Built-ins
(defvar igor-procdec-keywords
  '("End" "EndMacro" "EndStructure"
    "Function" "Macro" "Picture"
    "Proc" "Structure" "Window")
  "IgorPro Procedure Declaration Keywords")

(defvar igor-procsub-keywords
  '("ButtonControl"
    "CheckBoxControl"
    "CursorStyle"
    "FitFunc"
    "Graph"
    "GraphMarquee"
    "GraphStyle"
    "GridStyle"
    "Layout"
    "LayoutMarquee"
    "LayoutStyle"
    "Panel"
    "PopupMenuControl"
    "SetVariableControl"
    "Table"
    "TableStyle")
  "IgorPro Procedure Subtype Keywords")

(defvar igor-objrefs-keywords
  '("DFREF"
    "FUNCREF"
    "NVAR"
    "STRUCT"
    "SVAR"
    "WAVE")
  "IgorPro Object Reference Keywords")

(defvar igor-flowcontrol-keywords
  '("AbortOnRTE"
    "AbortOnValue"
    "break"
    "catch"
    "continue"
    "default"
    "do"
    "while"
    "endtry"
    "for"
    "endfor"
    "if"
    "else"
    "elseif"
    "endif"
    "return"
    "strswitch"
    "case"
    "endswitch"
    "switch"
    "try")
  "IgorPro Flow Control Keywords")

(defvar igor-hash-keywords
  '("#define"
    "#if"
    "#elif"
    "#endif"
    "#ifdef"
    "#endif"
    "#ifndef"
    "#include"
    "#pragma"
    "#undef")
  "IgorPro Hash Keywords")

(defvar igor-other-keywords
  '("Constant"
    "DoPrompt"
    "GalleryGlobal"
    "IgorVersion"
    "IndependentModule"
    "Menu"
    "ModuleName"
    "MultiThread"
    "Override"
    "popup"
    "ProcGlobal"
    "Prompt"
    "root"
    "rtGlobals"
    "Static"
    "Strconstant"
    "String"
    "Submenu"
    "ThreadSafe"
    "Variable"
    "version")
  "IgorPro Other Keywords")


;; Regexp optimized versions of word lists
(defvar igor-procdec-keywords-re
  (regexp-opt igor-procdec-keywords 'words))
(defvar igor-procsub-keywords-re
  (regexp-opt igor-procsub-keywords 'words))
(defvar igor-objrefs-keywords-re
  (regexp-opt igor-objrefs-keywords 'words))
(defvar igor-flowcontrol-keywords-re
  (regexp-opt igor-flowcontrol-keywords 'words))
(defvar igor-hash-keywords-re
  (regexp-opt igor-hash-keywords 'words))
(defvar igor-other-keywords-re
  (regexp-opt igor-other-keywords 'words))

(defconst igor-number-re "-?\\(?:[0-9]*\\.\\)?[0-9]+\\(?:e\\(?:\\+\\|-\\)?[0-9]+\\)?"
  "Number syntax in Igor")
(defconst igor-name-re "[a-zA-Z0-9_]+"
  "Legal object names in Igor")

(defvar igor-defun-re
  (concat
   igor-procdec-keywords-re "[ \t]+"    ; procedure type
   "\\(" igor-name-re "\\)[ \t]*"       ; procedure name
   "\\((" "\\(?:[ \t]*" "\\(" igor-name-re "\\)*" "[ \t]*,?[ \t]*\\)*" ")\\)" ; parameter list
   "\\([ \t]*:[ \t]*" igor-procsub-keywords-re "[ \t]*\\)?" ; procedure subtype
   )
  "Regexp for definition line of Igor functions/macros/etc.")

;; Clear memory of keyword lists (which are now saved in regexps)
;(setq igor-procdec-keywords nil)
;(setq igor-procsub-keywords nil)
;(setq igor-objrefs-keywords nil)
;(setq igor-flowcontrol-keywords nil)
;(setq igor-hash-keywords nil)
;(setq igor-other-keywords nil)

;; Syntax Highlighting

;; Syntax Table
(defvar igor-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Single-line comments "//"
    (modify-syntax-entry ?/ ". 12" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table used while in `igor-mode'")

(defvar igor-font-lock-keywords-1
  (eval-when-compile
    (list
     ;; Function names
     (list igor-defun-regexp
           '(1 font-lock-keyword-face nil t) ; procedure type
           '(2 font-lock-function-name-face nil t) ; procedure name
           '(4 font-lock-variable-name-face nil t) ; parameters
           '(6 font-lock-keyword-face))            ; procedure subtype
     (cons igor-procdec-keywords-re 'font-lock-keyword-face)
     (cons igor-procsub-keywords-re 'font-lock-keyword-face)
     (cons igor-objrefs-keywords-re 'font-lock-type-face)
     (cons igor-flowcontrol-keywords-re 'font-lock-keyword-face)
     (cons igor-other-keywords-re 'font-lock-type-face)
     ;; Numbers
     (cons igor-number-re 'font-lock-constant-face)
     (cons igor-hash-keywords-re 'font-lock-preprocessor-face))))

(defvar igor-font-lock-keywords-2
  `(append igor-font-lock-keywords-1
          (,igor-other-keywords-re . font-lock-keyword-face)))

(defvar igor-font-lock-keywords-default
  igor-font-lock-keywords-1)

(defvar igor-font-lock-keywords
  '(igor-font-lock-keywords-default     ; mode default
    igor-font-lock-keywords-1           ; level 1
    igor-font-lock-keywords-2           ; level 2
    ))

(defvar igor-font-lock-defaults
  '(igor-font-lock-keywords-1        ; keyword list
    nil                              ; perform syntactic fontification
    t                                ; ignore case
    nil))                            ; use buffer syntax table

;; Indentation
(defconst igor-defun-start-words
  '("Function" "Macro" "Picture" "Proc" "Structure" "Window")
  "Words that define the beginning of a definition block")

(defconst igor-defun-end-words
  '("End" "EndMacro" "EndStructure")
  "Words that define the end of a definition block")

(defvar igor-closeblock-words
  '("End" "EndMacro" "EndStructure"
   "while" "endtry" "endfor" "elseif"
   "endif" "endswitch" "#elif" "#endif")
  "Words that decrease indentation level")

(defvar igor-openblock-words
  '("Function" "Macro" "Picture" "Proc" "Structure" "Window"
    "default" "do" "for" "if" "else" "elseif" "case" "switch"
    "try" "catch" "#if" "#elif" "#ifdef" "#ifndef")
  "Words that increase indentation level")

(defvar igor-closeblock-re
  (concat "^[ \t]*" (regexp-opt igor-closeblock-words 'words)))

(defvar igor-openblock-re
  (concat "^[ \t]*" (regexp-opt igor-openblock-words 'words)))

(defconst igor-blank-re "^[ \t]*$")
(defconst igor-comment-re "^[ \t]*\/\/.*$")

(defconst igor-defun-start-re
  (concat "^[ \t]*" (regexp-opt igor-defun-start-words 'words)))
(defconst igor-defun-end-re
  (concat "^[ \t]*" (regexp-opt igor-defun-end-words 'words)))


;; Movement related commands
;; ==================================================

(defun igor-beginning-of-defun()
  "Set the pointer at the beginning of the Function/Macro/etc within which the pointer is located."
  (interactive)
  (re-search-backward igor-defun-start-re))

(defun igor-end-of-defun()
  "Set the pointer at the end of the Function/Macro/etc within which the pointer is located."
  (interactive)
  (re-search-forward igor-defun-end-re))

(defun igor-mark-defun()
  "Set the region pointer around Function/Macro/etc within which the pointer is located."
  (interactive)
  (beginning-of-line)
  (igor-end-of-defun)
  (set-mark (point))
  (igor-beginning-of-defun))


;; Indentation related commands
;; ==================================================

(defun igor-previous-line-of-code()
  "Set point on previous line of code, skipping any blank or comment lines."
  (interactive)
  (if (not (bobp))
      (forward-line -1))        ; previous-line depends on goal column
  (while (and (not (bobp))
              (or (looking-at igor-blank-re)
                  (looking-at igor-comment-re)))
    (forward-line -1)))

(defun igor-next-line-of-code()
  "Set point on next line of code, skipping any blank or comment lines."
  (interactive)
  (if (null (eobp))
      (forward-line 1))        ; next-line depends on goal column
  (while (and (null (eobp))
              (looking-at igor-comment-re))
    (forward-line 1)))

(defun igor-find-predicate-matching-stmt (open-p close-p)
  "Find opening statement statisfying OPEN-P predicate for which
  matching closing statement statisfies CLOSE-P predicate.

  Point is set on line statifying OPEN-P predicate, with ignoring
  any line satifying OPEN-P but for which a matching line
  statifying CLOSE-P was visited before during this search."
  ;; Searching backwards
  (let ((level 0))
    (while (and (>= level 0) (not (bobp)))
      (igor-previous-line-of-code)
      (cond ((funcall close-p)
             (setq level (+ level 1)))
            ((funcall open-p)
             (setq level (- level 1)))))))

(defun igor-find-matching-stmt (open-re close-re)
  "Same as function `igor-find-predicate-matching-stmt' except
  that regexps OPEN-RE CLOSE-RE are supplied instead of
  predicate, equivalent predicate being to be looking at those
  regexps."
  (igor-find-predicate-matching-stmt
   (lambda () (looking-at open-re))
   (lambda () (looking-at close-re))))

(defun igor-find-first-predicate-matching-stmt (open-p sub-p)
  "Find opening statement statisfying OPEN-P predicate for which
  a potentially repeating sub-statement satisfying SUB-P
  predicate.

  Point is set on first previous line satisfying OPEN-P
  predicate.  It does not account for other instances of SUB-P
  found before encountering an OPEN-P match, as occurs in
  `igor-find-predicate-matching-stmt'.  The canonical use case is
  for multiple `case' statements under a `switch' stmt."
  ;; Searching backwards
  (let (found)
    (while (and (not found) (not (bobp)))
      (igor-previous-line-of-code)
      (if (funcall open-p)
          (setq found t)))))

(defun igor-find-first-matching-stmt (open-re sub-re)
  "Same as function `igor-find-first-predicate-matching-stmt'
  except that regexps OPEN-RE and SUB-RE are supplied
  instead of predicates"
  (igor-find-first-predicate-matching-stmt
   (lambda () (looking-at open-re))
   (lambda () (looking-at sub-re))))

(defun igor-convert-pairs-str-to-re (inlist)
  "Convert pairs of strings to pairs of optimized regexps"
  (let (regexp-list)
    (dolist (curr (reverse inlist) regexp-list)
      (push (list
             (igor-wrap-re-startline
              (regexp-opt (list (car curr)) 'words))
             (igor-wrap-re-startline
              (regexp-opt (cdr curr) 'words)))
            regexp-list))))

(defun igor-flip-pairs (inlist)
  "Flip direction of start-end pairs"
  (let (newlist)
    (dolist (pair (reverse inlist) newlist)
      (dolist (endkey (cdr pair))
        (if (assoc endkey newlist)
            (push (car pair) (cdr (assoc endkey newlist)))
          (push (list endkey (car pair)) newlist))))))


;;; Indentation pairs
;; When a keyword is encountered that defines a block, it may
;;  * start a construct (following lines are indented)
;;  * end a construct (unindent to start level)
;;  * start a new section of the construct (a mid-level keyword), which may:
;;     * unindent to start and be used multiple times (e.g., if-elseif)
;;     * unindent to start and be used only once (e.g., if-else)
;;     * indent from start and be used multiple times (e.g., switch-case)
;;     * indent from start and be used only once (e.g., switch-default)
;;  * no keyword is found (indent as previous line)
(defconst igor-start-end-pairs
  '(("Function" "End")
    ("Macro" "End" "EndMacro")
    ("Picture" "End" "EndMacro")
    ("Proc" "End" "EndMacro")
    ("Structure" "End" "EndStructure")
    ("Window" "End" "EndMacro")
    ("if" "endif")
    ("for" "endfor")
    ("do" "while")
    ("switch" "endswitch")
    ("strswitch" "endswitch")
    ("try" "endtry")
    ("#if" "#endif")
    ("#ifdef" "#endif")
    ("#ifndef" "#endif"))
  "List of cons cells of start and end keywords for indentation
  blocks. cdr holds all valid end keywords of the car keyword.")

(defconst igor-start-middle-pairs
  '(("if" "else")
    ("try" "catch"))
  "List of cons cells of start and single-use mid-level keywords
  for same-level indentation.")

(defconst igor-start-middle-many-pairs
  '(("if" "elseif")
    ("#if" "#elif"))
  "List of cons cells of start and multi-use mid-level keywords
  for same-level indentation.")

(defconst igor-start-middle-inc-pairs
  '(("switch" "default")
    ("strswitch" "default"))
  "List of cons cells of start and single-use mid-level keywords
  for increased-level indentation.")

(defconst igor-start-middle-many-inc-pairs
  '(("switch" "case")
    ("strswitch" "case"))
  "List of cons cells of start and multi-use mid-level keywords
  for increased-level indentation.")

(defconst igor-indent-same-pairs
  (igor-flip-pairs
   (append igor-start-end-pairs igor-start-middle-pairs))
  "List of cons cells of single-use same-level end and start
  keywords.")

(defconst igor-indent-same-many-pairs
  (igor-flip-pairs
   (append igor-start-middle-many-pairs))
  "List of cons cells of multi-use same-level end and start
  keywords.")

(defconst igor-indent-increase-pairs
  (igor-flip-pairs
   (append igor-start-middle-inc-pairs))
  "List of cons cells of single-use increased-level end and start
  keywords.")

(defconst igor-indent-increase-many-pairs
  (igor-flip-pairs
   (append igor-start-middle-many-inc-pairs))
  "List of cons cells of multi-use increased-level end and start
  keywords.")

(defconst igor-indent-same-pairs-re
  (igor-convert-pairs-str-to-re igor-indent-same-pairs))

(defconst igor-indent-same-many-pairs-re
  (igor-convert-pairs-str-to-re igor-indent-same-many-pairs))

(defconst igor-indent-increase-pairs-re
  (igor-convert-pairs-str-to-re igor-indent-increase-pairs))

(defconst igor-indent-increase-many-pairs-re
  (igor-convert-pairs-str-to-re igor-indent-increase-many-pairs))

(defconst igor-indent-same-keys-re
  (igor-wrap-re-startline
   (regexp-opt
    (sort (mapcar 'car igor-indent-same-pairs) 'string<) 'words)))
(defconst igor-indent-same-many-keys-re
  (igor-wrap-re-startline
   (regexp-opt
    (sort (mapcar 'car igor-indent-same-many-pairs) 'string<) 'words)))
(defconst igor-indent-increase-keys-re
  (igor-wrap-re-startline
   (regexp-opt
    (sort (mapcar 'car igor-indent-increase-pairs) 'string<) 'words)))
(defconst igor-indent-increase-many-keys-re
  (igor-wrap-re-startline
   (regexp-opt
    (sort (mapcar 'car igor-indent-increase-many-pairs) 'string<) 'words)))

(defun igor-find-indent-match (inlist)
  "Return indent count for the matched regexp pair from
  inlist. Assumes the open-re is in cdr and close-re is
  in car (flipped pairs, see `igor-flip-pairs').  Finds the first
  unclosed occurence of open-re."
  (let ((elt (car inlist)))
    (if (looking-at (car elt))
        (progn
          (igor-find-matching-stmt (cadr elt) (car elt))
          (current-indentation))
      (igor-find-indent-match (cdr inlist)))))

(defun igor-find-first-indent-match (inlist)
  "Return indent count for the matched regexp pair from
  inlist. Assumes the open-re is in cdr and close-re is
  in car (flipped pairs, see `igor-flip-pairs').  Finds the first
  occurence of open-re."
  (let ((elt (car inlist)))
    (if (looking-at (car elt))
        (progn
          (igor-find-first-matching-stmt (cadr elt) (car elt))
          (current-indentation))
      (igor-find-first-indent-match (cdr inlist)))))

(defun igor-calculate-indent ()
  "Return indent count for the line of code containing pointer."
  (let ((original-point (point)))
    (save-excursion
      (beginning-of-line)
      (cond
       ;; If first line, no indentation
       ((bobp)
        0)

       ;; Statements that match start keyword indent
       ((looking-at igor-indent-same-keys-re) ; single-use words
        (igor-find-indent-match
         igor-indent-same-pairs-re))

       ((looking-at igor-indent-same-many-keys-re) ; multi-use
        (igor-find-first-indent-match
         igor-indent-same-many-pairs-re))


       ;; Keywords that increase start keyword indent
       ((looking-at igor-indent-increase-keys-re) ; single-use words
        (+ (igor-find-indent-match
            igor-indent-increase-pairs-re)
           igor-tab-width))

       ((looking-at igor-indent-increase-many-keys-re) ; multi-use
        (+ (igor-find-first-indent-match
            igor-indent-increase-many-pairs-re)
           igor-tab-width))

       ;; Cases depending on previous line indent
       (t
        (igor-previous-line-of-code)
        ;; Block open stmts increase next line indent
        (if (looking-at igor-openblock-re)
            (+ (current-indentation) igor-tab-width)
          ;; By default, just copy indent from prev line
          (current-indentation)))))))

(defun igor-indent-to-column (col)
  "Indent line of code containing pointer up to column COL."
  (let* ((bol (save-excursion
                (beginning-of-line)
                (point)))
         (point-in-whitespace
          (<= (point) (+ bol (current-indentation))))
         (blank-line-p
          (save-excursion
            (beginning-of-line)
            (looking-at igor-blank-re))))

    (cond ((/= col (current-indentation))
           (save-excursion
             (beginning-of-line)
             (back-to-indentation)
             (delete-region bol (point))
             (indent-to col))))

    ;; If point was in the whitespace, move back-to-indentation.
    (cond (blank-line-p
           (end-of-line))
          (point-in-whitespace
           (back-to-indentation)))))

(defun igor-indent-line ()
  "Indent current line for IgorPro."
  (interactive)
  (igor-indent-to-column (igor-calculate-indent)))


;; Define this mode
(define-derived-mode igor-mode fundamental-mode "Igor"
  "Major mode for editing IgorPro procedure files."
;  (set (make-local-variable 'font-lock-keywords) igor-font-lock-keywords)
  (set (make-local-variable 'indent-line-function) 'igor-indent-line)
  (set (make-local-variable 'tab-width) igor-tab-width)
  (set-syntax-table igor-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults) igor-font-lock-defaults))

(provide 'igor-mode)
;;; igor-mode.el ends here