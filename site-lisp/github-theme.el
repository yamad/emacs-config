;;; github-theme.el --- Github color theme for GNU Emacs.

;; Author: Jason Yamada-Hanff
;; Keywords: github color theme
;; URL: http://github.com/yamad/github-theme

;; modified from Dudley Flanders' color-theme-github and zenburn-theme
;; for new color theme support for Emacs 24

;;; License:

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

(deftheme github "Github color theme")

(defvar github-theme-colors-alist
  '(
    ("gh-red+1"   . "#dd1144")
    ("gh-red"     . "#990000")
    ("gh-magenta" . "#990073")

    ("gh-blue"     . "#4183c4")
    ("gh-blue+2"   . "#0086b3")
    ("gh-blue+4"   . "#445588")

    ("gh-teal+1"   . "#009999")
    ("gh-teal"     . "#008080")

    ("gh-orange"   . "#ba5d0f")

    ("gh-bg+1"     . "#ffffff")
    ("gh-bg"       . "#f8f8ff")

    ("gh-gray"     . "#eeeeee")
    ("gh-gray-1"   . "#dddddd")
    ("gh-gray-2"   . "grey75")
    ("gh-gray-3"   . "#a7a7a7")
    ("gh-gray-4"   . "#999999")
    ("gh-gray-5"   . "#777777")
    ("gh-gray-6"   . "#999988")

    ("gh-head-bg" . "#e6f1f6")
    ("gh-head-bdr" . "#b7c7bf")
    ("gh-border-blue" . "#d8e6ec")
    ("gh-hl-orange" . "#d26911")
    ("gh-black"    . "#000000")
    ("gh-bg-mouse" . "#bcd5fa")
    ("gh-bg-2"     . "#acc3e6")
    ("gh-bg-b7"    . "#c0ffff")
    ("gh-fg-b7"    . "#a0ffff")
    ("gh-fg-gr1"   . "#008000")
    ("gh-fg-gr2"   . "#009926")

    ("gh-green-0"  . "#eeeeee")
    ("gh-green-1"  . "#d6e685")
    ("gh-green-2"  . "#8cc665")
    ("gh-green-3"  . "#44a340")
    ("gh-green-4"  . "#1e6823")

    ("gh-fg-mag"   . "magenta")
    ("gh-bg-gr2"   . "gray33")
    ("gh-bg-gr3"   . "gray66")
    ("gh-fg-purple". "purple3")
    ("gh-fg-c1"    . "cyan4")
    ("gh-fg-r1"    . "red3")
    ("gh-fg-g5"    . "green4")
    ("gh-fg-link"  . "blue1")
    ("gh-fg-b3"    . "blue3")
    ("gh-bg-y1"    . "#fff6a9")))

(defmacro github-theme-with-color-variables (&rest body)
  "`let' bind all colors defined in `github-theme-colors-alist'
around BODY. Also bind `class' to ((class color) (min-colors
89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   github-theme-colors-alist))
     ,@body))

(github-theme-with-color-variables
 (custom-theme-set-faces
  'github
   ;; '(button ((t (:underline t))))
   ;; `(link ((t (:foreground ,gh-link))))
   ;; `(link-visited ((t (:foreground ,gh-yellow-2 :underline t :weight normal))))
   ;; `(cursor ((t (:foreground ,gh-fg :background ,gh-fg+1))))
   ;; `(escape-glyph ((t (:foreground ,gh-yellow :bold t))))
   ;; `(fringe ((t (:foreground ,gh-fg :background ,gh-bg+1))))
   ;; `(header-line ((t (:foreground ,gh-yellow
   ;;                                :background ,gh-bg-1
   ;;                                :box (:line-width -1 :style released-button)))))
   ;; `(highlight ((t (:background ,gh-bg-05))))
   ;; `(success ((t (:foreground ,gh-green :weight bold))))
   ;; `(warning ((t (:foreground ,gh-orange :weight bold))))

  '(button ((t (:underline t))))
  `(cursor ((t (:background "#a7a7a7"))))
  `(default ((t (:foreground ,gh-black :background ,gh-bg))))
  ;; font lock
  `(font-lock-builtin-face ((t (:foreground ,gh-blue+2 :weight bold))))
  `(font-lock-comment-face ((t (:foreground ,gh-gray-6 :italic t :slant italic))))
  `(font-lock-comment-delimiter-face ((t (:foreground ,gh-gray-4 :italic t :slant italic))))
  `(font-lock-constant-face ((t (:foreground ,gh-magenta))))
  `(font-lock-doc-face ((t (:foreground ,gh-red+1))))
  `(font-lock-function-name-face ((t (:foreground ,gh-red :weight bold))))
  `(font-lock-keyword-face ((t (:foreground ,gh-black :weight bold))))
  `(font-lock-negation-char-face ((t (:foreground ,gh-black :weight bold))))
  `(font-lock-preprocessor-face ((t (:foreground ,gh-gray-4 :weight bold))))
  `(font-lock-regexp-grouping-construct ((t (:foreground ,gh-red+1 :weight bold))))
  `(font-lock-regexp-grouping-backslash ((t (:foreground ,gh-red+1 :weight bold))))
  `(font-lock-string-face ((t (:foreground ,gh-red+1))))
  `(font-lock-type-face ((t (:foreground ,gh-blue+4 :weight bold))))
  `(font-lock-variable-name-face ((t (:foreground ,gh-teal))))
  `(font-lock-warning-face ((t (:foreground ,gh-orange :weight bold))))
  `(c-annotation-face ((t (:inherit font-lock-constant-face))))
  ))
     ;; ((background-color . "#f8f8ff")
     ;;  (background-mode . light)
     ;;  (border-color . "black")
     ;;  (cursor-color . "#000000")
     ;;  (foreground-color . "#000000")
     ;;  (mouse-color . "#bcd5fa"))
     ;; ()
     ;; (default ((t (:stipple nil :background "#f8f8ff" :foreground "#000000" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal))))
     ;; (css-property ((t (:foreground "#0086b3"))))
     ;; (css-selector ((t (:foreground "#990000"))))
     ;; (ecb-default-general-face ((t (:height 0.9))))
     ;; (ecb-default-highlight-face ((t (:background "#bcd5fa" :foreground "#000000"))))
     ;; (ecb-directories-general-face ((t (:bold t :weight bold))))
     ;; (ecb-source-in-directories-buffer-face ((t (:foreground "#445588"))))
     ;; (erb-comment-delim-face ((t (:italic t :bold t :slant italic :foreground "#999988" :weight bold))))
     ;; (erb-comment-face ((t (:bold t :background "#eeeeee" :foreground "#999988" :weight bold))))
     ;; (erb-delim-face ((t (:bold t :weight bold))))
     ;; (erb-exec-delim-face ((t (:bold t :weight bold))))
     ;; (erb-exec-face ((t (:background "#eeeeee"))))
     ;; (erb-face ((t (:background "#eeeeee"))))
     ;; (erb-out-delim-face ((t (:bold t :foreground "#445588" :weight bold))))
     ;; (erb-out-face ((t (:background "#eeeeee"))))
     ;; (font-lock-builtin-face ((t (nil))))
     ;; (font-lock-comment-delimiter-face ((t (:italic t :slant italic :foreground "#999988"))))
     ;; (font-lock-comment-face ((t (:italic t :foreground "#999988" :slant italic))))
     ;; (font-lock-constant-face ((t (:foreground "#990073"))))
     ;; (font-lock-doc-face ((t (:foreground "#dd1144"))))
     ;; (font-lock-function-name-face ((t (:foreground "#990000"))))
     ;; (font-lock-keyword-face ((t (:bold t :weight bold))))
     ;; (font-lock-negation-char-face ((t (nil))))
     ;; (font-lock-reference-face ((t (nil))))
     ;; (font-lock-regexp-grouping-backslash ((t (:foreground "#009926"))))
     ;; (font-lock-regexp-grouping-construct ((t (:foreground "#009926"))))
     ;; (font-lock-string-face ((t (:foreground "#dd1144"))))
     ;; (font-lock-type-face ((t (:foreground "#445588"))))
     ;; (font-lock-variable-name-face ((t (:foreground "#0086b3"))))
     ;; (highlight ((t (:background "#acc3e6"))))
     ;; (link ((t (:foreground "blue1" :underline t))))
     ;; (link-visited ((t (:underline t :foreground "magenta4"))))
     ;; (minibuffer-prompt ((t (:foreground "#445588"))))
     ;; (mode-line ((t (:background "grey75" :foreground "black" :box (:line-width -1 :style released-button) :height 0.85))))
     ;; (mouse ((t (:background "#bcd5fa"))))
     ;; (quack-about-face ((t (:family "Helvetica"))))
     ;; (quack-about-title-face ((t (:bold t :foreground "#008000" :weight bold :height 2.0 :family "Helvetica"))))
     ;; (quack-banner-face ((t (:family "Helvetica"))))
     ;; (quack-pltfile-dir-face ((t (:bold t :background "gray33" :foreground "white" :weight bold :height 1.2 :family "Helvetica"))))
     ;; (quack-pltfile-file-face ((t (:bold t :background "gray66" :foreground "black" :weight bold :height 1.2 :family "Helvetica"))))
     ;; (quack-pltfile-prologue-face ((t (:background "gray66" :foreground "black"))))
     ;; (quack-pltish-class-defn-face ((t (:bold t :weight bold :foreground "purple3"))))
     ;; (quack-pltish-comment-face ((t (:foreground "cyan4"))))
     ;; (quack-pltish-defn-face ((t (:bold t :foreground "blue3" :weight bold))))
     ;; (quack-pltish-keyword-face ((t (:bold t :weight bold))))
     ;; (quack-pltish-module-defn-face ((t (:bold t :weight bold :foreground "purple3"))))
     ;; (quack-pltish-paren-face ((t (:foreground "red3"))))
     ;; (quack-pltish-selfeval-face ((t (:foreground "green4"))))
     ;; (quack-smallprint-face ((t (:height 0.8 :family "Courier"))))
     ;; (quack-threesemi-h1-face ((t (:bold t :weight bold :height 1.4 :family "Helvetica"))))
     ;; (quack-threesemi-h2-face ((t (:bold t :weight bold :height 1.2 :family "Helvetica"))))
     ;; (quack-threesemi-h3-face ((t (:bold t :weight bold :family "Helvetica"))))
     ;; (quack-threesemi-semi-face ((t (:background "#c0ffff" :foreground "#a0ffff"))))
     ;; (quack-threesemi-text-face ((t (:background "#c0ffff" :foreground "cyan4"))))
     ;; (region ((t (:background "#bcd5fa"))))
     ;; (show-paren-match ((t (:background "#fff6a9"))))
     ;; (show-paren-mismatch ((t (:background "#dd1144")))))))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'github)
;;; github-theme.el ends here
