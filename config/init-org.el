;;; init-org.el --- org-mode config
;;
;; part of emacs config for jyamad. see init.el

;;; Code:

(add-to-list 'load-path "~/.emacs.d/site-lisp/org-mode/lisp")
(add-to-list 'load-path "~/.emacs.d/site-lisp/org-mode/contrib/lisp" t)

;;; --- HACK from straight.el to override built-in org version ---
(require 'subr-x)
(straight-use-package 'git)

(defun org-git-version ()
  "The Git version of org-mode.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (git-run "describe"
              "--match=release\*"
              "--abbrev=6"
              "HEAD"))))

(defun org-release ()
  "The release version of org-mode.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (string-remove-prefix
      "release_"
      (git-run "describe"
               "--match=release\*"
               "--abbrev=0"
               "HEAD")))))

(provide 'org-version)
;;; --- END HACK ---


(use-package org
  :straight t
  :defer t
  :init
  (setq org-directory "~/Dropbox/org/"
        jyh/main-org-file (concat org-directory "notes.org"))

  (defun jyh/jump-to-main-org-file ()
    (interactive)
    (find-file jyh/main-org-file))

  (with-eval-after-load 'general
    (general-create-definer jyh/org-command-def
      :prefix "C-c o"
      :prefix-map 'jyh/org-command-map)
    (jyh/org-command-def
     ;:prefix "C-c o"
     ;:prefix-map 'jyh-org-command-map
     "l" 'org-store-link
     "a" 'org-agenda
     "c" 'org-capture
     "o" '(jyh/jump-to-main-org-file
           :wk "switch to org")
     "L" 'org-insert-link-global
     "O" 'org-open-at-point-global))

  ;; quick jump registers
  (set-register ?o (cons 'file jyh/main-org-file))

  (setq org-log-done t                  ; log timepoints
        org-use-fast-todo-selection t
        org-agenda-include-diary t
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t))

  ;; link abbreviations
  (setq org-link-abbrev-alist
        '(("doi"     . "http://dx.doi.org/")
          ("google"  . "http://www.google.com/search?q=%s")))

  ;; org-capture is the new org+remember in v8.0
  (setq org-directory "~/Dropbox/org/")
  (setq org-default-notes-file (concat org-directory "refile.org"))
  (setq org-capture-templates
        '(("t" "todo" entry (file "")
           "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
          ("n" "note" entry (file "")
           "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
          ("m" "meeting" entry (file "")
           "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)))

  (setq org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9)))
  (setq org-refile-use-outline-path t)


  (setq org-clock-in-resume t
        org-clock-into-drawer t
        org-clock-out-remove-zero-time-clocks t)

  ;;
  ;; TODO: modernize the rest of this for org 8
  ;;

  ;; OrgStruct Mode
  (add-hook 'rst-mode-hook 'turn-on-orgstruct)
  (add-hook 'rst-mode-hook 'turn-on-orgtbl)


  (add-hook 'org-mode-hook
            (lambda ()
              ;; yasnippet (using the new org-cycle hooks)
              (make-variable-buffer-local 'yas/trigger-key)
              (setq yas/trigger-key [tab])
              (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
              (define-key yas/keymap [tab] 'yas/next-field)))

  :config
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

(use-package org-projectile             ; project-specific org files
  :straight t
  :defer t
  :after org
  :init
  (jyh/org-command-def
    "p" '(org-projectile-capture-for-current-project
          :which-key "project capture"))
  :config
  ;; per-project todo files
  (org-projectile-per-project)
  (setq org-projectile-per-project-filepath "TODO.org")

  (setq org-agenda-files (append org-agenda-files
                                 (org-projectile-todo-files)))
  (push (org-projectile-project-todo-entry)
        org-capture-templates))

(provide 'init-org)

;;; init-org.el ends here
