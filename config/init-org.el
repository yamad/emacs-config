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
  :after general
  :init
  (setq org-directory "~/Dropbox/org/"
        jyh/main-org-file (concat org-directory "notes.org"))

  (defun jyh/jump-to-main-org-file ()
    (interactive)
    (find-file jyh/main-org-file))

  (with-eval-after-load "general"
    (general-def
      :prefix-map 'jyh/org-command-map
      ""  '(nil :which-key "org")
      "l" 'org-store-link
      "a" 'org-agenda
      "c" 'org-capture
      "o" '(jyh/jump-to-main-org-file
            :wk "switch to org")
      "L" 'org-insert-link-global
      "O" 'org-open-at-point-global)

    (jyh/bind-leader-maps
     "o" '(:keymap jyh/org-command-map :wk "org")))

  ;; quick jump registers
  (set-register ?o (cons 'file jyh/main-org-file))

  (setq org-log-done t                  ; log timepoints
        org-use-fast-todo-selection t
        org-agenda-include-diary t
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t)

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

  (defun yas/org-very-safe-expand ()
    (let ((yas/fallback-behavior 'return-nil)) (yas/expand))))

(use-package org-projectile             ; project-specific org files
  :straight t
  :defer t
  :after org
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
