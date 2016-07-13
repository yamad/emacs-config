;;; init-org.el --- org-mode config
;;
;; part of emacs config for jyamad. see init.el

;;; Code:

(use-package org
  :ensure t
  :init
  ;; use-package's :bind isn't working with :prefix(-map), so
  ;; workaround like this
  (bind-keys :prefix "C-c o"
             :prefix-map jyh-org-command-map
             ("l" . org-store-link)
             ("a" . org-agenda)
             ("c" . org-capture)
             ;; use links outside org-mode
             ("L" . org-insert-link-global)
             ("O" . org-open-at-point-global))
  :config
  (setq org-log-done t                  ; log timepoints
        org-use-fast-todo-selection t
        org-agenda-include-diary t
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t)

  (setq org-agenda-custom-commands
        '(("b" agenda "Emr - Simple"
           ((org-agenda-skip-scheduled-if-done nil)
            (org-agenda-skip-archived-trees nil)
            (org-agenda-include-diary nil)
            (org-agenda-prefix-format "  %?-12 t% s")
            (org-agenda-ndays 7)
            (org-agenda-start-on-weekday 1)
            (org-agenda-remove-times-when-in-prefix t)
            (org-agenda-remove-tags t)
            (org-agenda-use-time-grid nil)
            (org-agenda-scheduled-leaders '("" ""))
            (org-agenda-deadline-leaders '("" ""))
            (ps-landscape-mode t)
            (ps-number-of-columns 2))
           ("~/Desktop/emr_week.ps"))))

  ;; link abbreviations
  (setq org-link-abbrev-alist
        '(("doi"     . "http://dx.doi.org/")
          ("google"  . "http://www.google.com/search?q=%s")))

  ;; org-capture is the new org+remember in v8.0
  (setq org-directory "~/Dropbox/org/")
  (setq org-default-notes-file (concat org-directory "notes.org"))

  ;;
  ;; TODO: modernize the rest of this for org 8
  ;;

  (setq org-remember-templates
        '((?t "* %^{Title}\n %U\n %i\n %a" "~/notebook/org/gtd/General.org")))

  ;; OrgStruct Mode
  (add-hook 'rst-mode-hook 'turn-on-orgstruct)
  (add-hook 'rst-mode-hook 'turn-on-orgtbl)

  ;; rst export for orgtbl
  (defun tbl-line (start end sep width-list field-func)
    (concat
     start
     (mapconcat (lambda (width) (funcall field-func width))
                width-list sep)
     end))
  (defun tbl-hline (start end sep line-mark width-list)
    (tbl-line start end sep width-list
              (lambda (width)
                (apply 'string (make-list width line-mark)))))

  (defun orgtbl-to-rst (table params)
    (let* ((hline (tbl-hline "+-" "-+" "-+-" ?- org-table-last-column-widths))
           (hlline (tbl-hline "+=" "=+" "=+=" ?= org-table-last-column-widths))
           (rst-fmt (tbl-line "| " " |" " | " org-table-last-column-widths
                              (lambda (width) (format "%%-%ss" width))))
           (rst-lfmt (concat
                      rst-fmt "\n" hline))
           (rst-hlfmt (concat
                       rst-fmt "\n" hlline))
           (params_default
            (list
             :tstart hline
             :lfmt rst-lfmt
             :hlfmt rst-hlfmt
             )))
      (orgtbl-to-generic table (org-combine-plists params_default params))))

  ;; lab notebook
  (defun org-export-labnotes-add-table-format ()
    (insert org-export-labnotes-table-options)
    (newline))

  (defvar org-export-labnotes-table-options
    "#+ATTR_LaTeX: longtable* align=rclp{5cm}lp{5cm}")

  (defvar org-export-labnotes-header
    "#+LaTeX_HEADER: \\usepackage[margin=0.5in]{geometry}
#+OPTIONS: toc:nil num:nil H:3
#+LaTeX_HEADER: \\usepackage{paralist}
#+LaTeX_HEADER: \\let\\itemize\\compactitem
#+LaTeX_HEADER: \\let\\description\\compactdesc
#+LaTeX_HEADER: \\let\\enumerate\\compactenum
#+LaTeX_HEADER: \\renewcommand\\maketitle{}")

  (defun org-export-lab-notebook-as-latex-to-buffer ()
    (interactive)
    (let* ((oldbuf (current-buffer))
           (bufpath (concat
                     (make-temp-name
                      (file-name-sans-extension (buffer-file-name)))
                     ".org"))
           (bufname (file-name-nondirectory bufpath))
           (outbuf (get-buffer-create bufname)))
      (progn
        (with-current-buffer outbuf
          (set-visited-file-name bufpath)
          (insert-buffer oldbuf)
          (goto-line (point-min))
          (insert org-export-labnotes-header)
          (newline)
          (org-table-map-tables
           'org-export-labnotes-add-table-format)
          (org-export-as-latex nil nil '(title "notes") "notes.tex")
          (set-buffer-modified-p nil))
        (with-current-buffer "notes.tex"
          (set-visited-file-name "./notes.tex")
          (save-buffer))
        (kill-buffer bufname)
        (switch-to-buffer-other-window "notes.tex"))))

  (defun yas/org-very-safe-expand ()
    (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

  (add-hook 'org-mode-hook
            (lambda ()
              ;; yasnippet (using the new org-cycle hooks)
              (make-variable-buffer-local 'yas/trigger-key)
              (setq yas/trigger-key [tab])
              (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
              (define-key yas/keymap [tab] 'yas/next-field))))

(provide 'init-org)

;;; init-org.el ends here
