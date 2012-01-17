;;; binview.el ---  binary file inspector

;; Copyright (C) 2009, Chaoji Li

;; Author: Chaoji Li <lichaoji AT gmail DOT com>
;; Version: 0.1
;; Date: Apr 1, 2009

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Put this file in a folder where Emacs can find it.
;;
;; Add following lines to your .emacs initialization file:
;;
;;     (require 'binview)
;;

(require 'hexl)
(require 'calc)

(defvar binview-format-bitmap
  '(
    ("ID"        binview-char 2)
    ("FILESIZE"  binview-uint32)
    ("RESERVED"  binview-uint16 2)
    ("OFFSET"    binview-uint32)
    ("biSize"    binview-uint32)
    ("biWidth"   binview-int32)
    ("biHeight"  binview-int32)
    ("biPlanes"  binview-uint16)
    ("biBitCount" binview-uint16)
    ("biCompression" binview-uint32)
    ("biSizeImage"  binview-uint32)
    ("biXPelsPerMeter" binview-int32)
    ("biYPelsPerMeter" binview-int32)
    ("biClrUsed"  binview-uint32)
    ("biClrImportant" binview-uint32)
    )
  "Bitmap file header and bitmap information header."
  )

(defvar binview-format-alist
  '(
    ("bitmap" . binview-format-bitmap)
    )
  "Predefined format list that binview supported. Feel free to add new."
  )

(defun binview-char ()
  (interactive)
  (let (b)
    (setq b (hexl-char-after-point))
    (hexl-forward-char 1)
    (format "%c" b)
  ))

(defun binview-byte ()
  (interactive)
  (let (b)
    (setq b (hexl-char-after-point))
    (hexl-forward-char 1)
    (b)
  ))


(defun binview-uint16 ()
  (interactive)
  (let (lb hb)
    (setq lb (hexl-char-after-point))
    (hexl-forward-char 1)
    (setq hb (hexl-char-after-point))
    (hexl-forward-char 1)
    (+ lb (* hb 256))
  ))

(defun binview-uint32 ()
  (interactive)
  (let (ls hs t1 t2 w)
    (setq t1 (binview-uint16))
    (setq t2 (/ t1 1000))
    (setq t1 (% t1 1000))
    (setq ls (list 'bigpos t1 t2))
    (setq t1 (binview-uint16))
    (setq t2 (/ t1 1000))
    (setq t1 (% t1 1000))
    (setq hs (list 'bigpos t1 t2))
    (math-format-value (math-add ls (math-mul hs '(bigpos 536 65))))
    ))

(defun binview-int32 ()
  (interactive)
  (let (ls hs t1 t2 long-max)
    (setq t1 (binview-uint16))
    (setq t2 (/ t1 1000))
    (setq t1 (% t1 1000))
    (setq ls (list 'bigpos t1 t2))
    (setq t1 (binview-uint16))
    (setq t2 (/ t1 1000))
    (setq t1 (% t1 1000))
    (setq hs (list 'bigpos t1 t2))
    (setq long-max '(bigpos 647 483 147 2))
    (setq t1 (math-add ls (math-mul hs '(bigpos 536 65))))
    (setq t2 (math-sub long-max t1))
    (if (equal (car t2) 'bigpos)
	t1
      (math-sub t1 '(bigpos 296 967 294 4)))
    ))


(defun binview-to-string (value)
  (if (stringp value)
      value
    (if (numberp value)
	(number-to-string value)
      (if (listp value)
	  (math-format-value value)
	"UNKNOWN"))))

(defun binview-display-with-format (format)
  (let (bv content cnt)
    (setq content "Bin View Content:\n")
    (dolist (elem (symbol-value format))
      (if (nth 2 elem)
	  (setq cnt (nth 2 elem))
	(setq cnt 1))
      (setq content (concat content (car elem) ": "))
      (while (> cnt 0)
	(setq cnt (1- cnt))
	(setq content (concat 
		       content
		       (binview-to-string 
			(funcall (nth 1 elem)))))
	(if (> cnt 0)
	    (setq content (concat content ", "))))
      (setq content (concat content "\n"))
      )
    (setq bv (get-buffer-create "*Bin View*"))
    (set-buffer bv)
    (erase-buffer)
    (switch-to-buffer-other-window bv)
    (insert content)
    ))

(defun binview-from-point (format)
  "Display human readable information about the binary data started at 
the current location.  You must do it in a hexl-mode buffer.  The FORMAT
should be a predefined one in binview-format-alist."
  (interactive "sFormat:")
  (let (e)
    (setq e (assoc format binview-format-alist))
    (if e
	(binview-display-with-format (cdr e))
      (message "Format %s not supported!" format))))

(provide 'binview)
