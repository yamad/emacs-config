;; TeX/LaTeX/ConTeXt
;; ======================================
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-engine 'xetex)
(setq TeX-PDF-mode nil)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

(setq TeX-open-quote "``")
(setq TeX-close-quote "''")


; for outline views (hide/show sections, chapters, etc.)
(add-hook 'TeX-mode-hook '(lambda () (TeX-fold-mode 1)))
(add-hook 'TeX-mode-hook '(lambda () (outline-minor-mode 1)))
; make PDF by default (can toggle with C-c C-t C-p
(add-hook 'TeX-mode-hook '(lambda () (TeX-PDF-mode 1)))
; these math abbrevs (` as prefix char) are also useful in TeX/ConTeXt files
(require 'latex); defines LaTeX-math-mode
(add-hook 'TeX-mode-hook 'LaTeX-math-mode)
; Emacs help for \label, \ref, \cite.  Normally used only with
; LaTeX-mode but also useful with plain TeX + eplain and with ConTeXt, so:
(setq reftex-plug-into-AUCTeX t)
(add-hook 'TeX-mode-hook 'reftex-mode)

(defun insert-balanced (left right)
  "Insert a left, right delmiter pair and be poised to type inside them."
  (interactive)
  (insert left)
  (save-excursion
    (insert right)))

; When start-context-math() is bound to $:
; Typing one $ gets you $$ with the insertion point between them.
; Typing a second $ turns the $$ into ConTeXt's form for displayed math:
;
;   \placeformula\startformula
;   [blank line with insertion point at beginning]
;   \stopformula
;
; Delete the \placeformula if you don't want equations numbered automatically.

(defun start-context-math ()
  (interactive)
  (let* ((start (max (point-min) (- (point) 1)))
         (stop  (min (point-max) (+ (point) 1))))
                                        ; if in the middle of a $$, turn inline math into context display math
    (if (equal "$$" (buffer-substring-no-properties start stop))
        (progn
          (delete-region start stop);get rid of the $$
                                        ; delete preceding spaces, if any
          (while (and (< (point-min) (point))
                      (equal (buffer-substring-no-properties (- (point) 1)
                                                             (point))
                             " "))
            (backward-delete-char 1))
                                        ; delete a preceding newline, if any
          (if (equal (buffer-substring-no-properties (- (point) 1)
                                                     (point))
                     "\n")
              (backward-delete-char 1))
                                        ; ConTeXt's display math with automatic equation numbering
          (insert "\n\\placeformula\\startformula\n")
          (save-excursion (insert "\n\\stopformula")))
                                        ; else: just doing inline math
      (insert-balanced ?\$ ?\$))))

                                        ; automatically insert right delimiter for $, {, [, and ( and be
                                        ; poised to type inside them.
(add-hook 'TeX-mode-hook
          '(lambda ()
             (local-set-key "$"
                            '(lambda ()
                               (interactive)
                               (insert-balanced ?\$ ?\$)))
             (local-set-key "{"
                            '(lambda ()
                               (interactive)
                               (insert-balanced ?\{ ?\})))
             (local-set-key "["
                            '(lambda ()
                               (interactive)
                               (insert-balanced ?\[ ?\])))
             (local-set-key "("
                            '(lambda ()
                               (interactive)
                               (insert-balanced ?\( ?\))))))

                                        ; For ConTeXt mode, inserting two $ signs needs to behave specially
(add-hook 'ConTeXt-mode-hook
          '(lambda ()
             (local-set-key "$" 'start-context-math)))

(defun my-latex-setup ()
  (defun LaTeX-word-count ()
    (interactive)
    (let* ((this-file (buffer-file-name))
           (word-count
            (with-output-to-string
              (with-current-buffer standard-output
                (call-process "texcount" nil t nil "-brief" this-file)))))
      (string-match "\n$" word-count)
      (message (replace-match "" nil nil word-count))))
  (define-key LaTeX-mode-map "\C-cw" 'LaTeX-word-count))
(add-hook 'LaTeX-mode-hook 'my-latex-setup t)

(if (eq system-type 'windows-nt)
    (progn
      (require 'tex-mik)
      (setq preview-image-type 'pnm)))
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(eval-after-load "tex"
  '(add-to-list 'TeX-command-list
                '("Biber" "biber %s" TeX-run-Biber nil t :help "Run Biber") t))

(defun TeX-run-Biber (name command file)
  "Create a process for NAME using COMMAND to format FILE with Biber."
  (let ((process (TeX-run-command name command file)))
    (setq TeX-sentinel-function 'TeX-Biber-sentinel)
    (if TeX-process-asynchronous
        process
      (TeX-synchronous-sentinel name file process))))

(defun TeX-Biber-sentinel (process name)
  "Cleanup TeX output buffer after running Biber."
  (goto-char (point-max))
  (cond
   ;; Check whether Biber reports any warnings or errors.
   ((re-search-backward (concat
                         "^(There \\(?:was\\|were\\) \\([0-9]+\\) "
                         "\\(warnings?\\|error messages?\\))") nil t)
    ;; Tell the user their number so that she sees whether the
    ;; situation is getting better or worse.
    (message (concat "Biber finished with %s %s. "
                     "Type `%s' to display output.")
             (match-string 1) (match-string 2)
             (substitute-command-keys
              "\\\\[TeX-recenter-output-buffer]")))
   (t
    (message (concat "Biber finished successfully. "
                     "Run LaTeX again to get citations right."))))
  (setq TeX-command-next TeX-command-default))
