;;; ebnf-mode.el --- Major mode for EBNF (ISO) grammars

;; Copyright (C) 2012  Jason Yamada-Hanff

;; Author: Jason Yamada-Hanff <jason@jyh-archsolo>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This mode implements the ISO standard ISO/IEC 14977:1996 for
;; Extended BNF. Note that this standard has many differences from the
;; variants of EBNF that are in common use.

;;; Code:

(defun ebnf-remove (object-list list)
  (if (equal (car object-list) nil)
      list
    (delete
     (car object-list)
     (ebnf-remove (cdr object-list) list))))

(defun ebnf-get-syntax-char (type)
  (cdr (assoc type ebnf-other-terminal-characters)))

(defvar ebnf-other-terminal-characters
  '((concatentate-symbol         ",")
    (defining-symbol             "=")
    (definition-separator-symbol "|" "/" "!")
    (end-comment-symbol          "*)")
    (end-group-symbol            ")")
    (end-option-symbol           "]" "/)")
    (end-repeat-symbol           "}" ":)")
    (except-symbol               "-")
    (first-quote-symbol          "'")
    (repitition-symbol           "*")
    (second-quote-symbol         "\"")
    (special-sequence-symbol     "?")
    (start-comment-symbol        "(*")
    (start-group-symbol          "(")
    (start-option-symbol         "[" "(/")
    (start-repeat-symbol         "{" "(:")
    (terminator-symbol           ";" "."))
  "other terminal characters in EBNF, see section 7.3 and 7.4")

(defvar ebnf-letter
  '("A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" 
    "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
    "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" 
    "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))
(defvar ebnf-decimal-digit
  '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
(defvar ebnf-other-character
  '(" " ":" "+" "_" "%" "@" "&" "#" "$" "<" ">" "\\" "^" "`" "~")
  "Other-character, see section 7.5")
(defvar ebnf-space-character
  '(" "))
(defvar ebnf-horizontal-tabulation-character
  '("\t"))
(defvar ebnf-vertical-tabulation-character
  '("\v"))
(defvar ebnf-form-feed
  '("\f"))
(defvar ebnf-terminal-character
  (append ebnf-letter
          ebnf-decimal-digit
          (apply 'append (mapcar 'cdr ebnf-other-terminal-characters))
          ebnf-other-character)
  "see section 6.2")
(defvar ebnf-first-terminal-character
  (ebnf-remove
   (ebnf-get-syntax-char 'first-quote-symbol)
   ebnf-terminal-character)
  "see section 4.17")
(defvar ebnf-second-terminal-character
  (ebnf-remove
   (ebnf-get-syntax-char 'second-quote-symbol)
   ebnf-terminal-character)
  "see section 4.18")
(defvar ebnf-special-sequence-character
  (ebnf-remove
   (ebnf-get-syntax-char 'special-sequence-symbol)
   ebnf-terminal-character)
  "see section 4.20")

(defvar ebnf-new-line-re
  "\r*\n\r*"
  "see section 7.6.c")

(defvar ebnf-gap-separator-re
  (concat
   "\\(?:"
   (regexp-opt
    (append ebnf-space-character
            ebnf-horizontal-tabulation-character
            ebnf-vertical-tabulation-character
            ebnf-form-feed))
   "\\|\\(?:" ebnf-new-line-re "\\)\\)")
  "see section 7.6")

(defvar ebnf-terminal-string-re
  (let* ((first-quote-symbol (regexp-opt (ebnf-get-syntax-char 'first-quote-symbol)))
         (second-quote-symbol (regexp-opt (ebnf-get-syntax-char 'second-quote-symbol))))
    (concat
     "\\(?:" first-quote-symbol (regexp-opt ebnf-first-terminal-character) first-quote-symbol "\\)"
     "\\|"
     "\\(?:" second-quote-symbol (regexp-opt ebnf-second-terminal-character) second-quote-symbol "\\)"))
  "see section 4.16")

(defvar ebnf-gap-free-symbol-re
  (concat
   "\\(?:"
   (regexp-opt (ebnf-remove
                (append (ebnf-get-syntax-char 'first-quote-symbol)
                        (ebnf-get-syntax-char 'second-quote-symbol))
                ebnf-terminal-character))
   "\\)\\|\\(?:"
   ebnf-terminal-string-re)
  "see section 6.3")

(defvar ebnf-special-sequence-re
  (let ((special-sequence-symbol (regexp-opt (ebnf-get-syntax-char 'special-sequence-symbol))))
    (concat
     "\\(?:" special-sequence-symbol
     (regexp-opt ebnf-special-sequence-character)
     special-sequence-symbol "\\)"))
  "see section 4.19")

(defvar ebnf-meta-character
  (append
   ebnf-letter
   ebnf-decimal-digit)
  "see section 4.15")

(defvar ebnf-meta-identifier-re
  (concat (regexp-opt ebnf-letter)
          "\\(?:" (regexp-opt ebnf-meta-character)
          "\\|" ebnf-gap-separator-re "\\)+"))

(defvar ebnf-integer-re
  (concat "\\(?:" (regexp-opt ebnf-decimal-digit) "\\)+"))

;; Syntax Table
(defvar ebnf-syntax-table
  (let ((st (make-syntax-table)))
    ;; comments "(* *)"
    (modify-syntax-entry ?\( ". 1" st)
    (modify-syntax-entry ?\* ". 23" st)
    (modify-syntax-entry ?\) ". 4" st)
    ;; strings
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\' "\"" st)
    (modify-syntax-entry ?_  "w" st)
    (modify-syntax-entry ?-  "_" st)
    ;; operators
    (modify-syntax-entry ?,   "." st)
    (modify-syntax-entry ?|   "." st)
    (modify-syntax-entry ?=   "." st)
    (modify-syntax-entry ?\;  "." st)
    (modify-syntax-entry ?\?  "." st)
    
    ;; parens
    (modify-syntax-entry ?\( "(" st)
    (modify-syntax-entry ?\) ")" st)
    (modify-syntax-entry ?[ "(" st)
    (modify-syntax-entry ?] ")" st)
    (modify-syntax-entry ?{ "(" st)
    (modify-syntax-entry ?} ")" st)
    st)
  "Syntax table used while in `ebnf-mode'")

(define-derived-mode ebnf-mode fundamental-mode "EBNF"
  "Major mode for editing EBNF (ISO) grammars."
  (set (make-local-variable 'font-lock-defaults) ebnf-font-lock-defaults)
  (set-syntax-table ebnf-syntax-table)
  (set (make-local-variable 'indent-line-function) 'ebnf-indent-line)
  (set (make-local-variable 'comment-start) "(*")
  (set (make-local-variable 'comment-end) "*)"))

(provide 'ebnf-mode)
;;; ebnf-mode.el ends here
