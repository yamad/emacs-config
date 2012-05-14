;; General Options
;; ======================================

;; Append main load-path directories
(setq load-path (append load-path (list "~/.emacs.d")))
(setq load-path (append load-path (list "~/.emacs.d/site-lisp")))


;; General requires
(require 'find-files)
(require 'rst)
(setq auto-mode-alist
      (append '(("\\.rst$" . rst-mode)
                ("\\.rest$" . rst-mode)) auto-mode-alist))


;; General Options
(setq visible-bell 1)
(tool-bar-mode 0)
(line-number-mode 1)
(column-number-mode 1)
(setq kill-whole-line t)

;; ANSI coloring
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on) ;; for shell

;; always remove trailing whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Alternate bindings for M-x
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; Bindings for replace-regexp
(global-set-key (kbd "C-x g r") 'replace-regexp)
(global-set-key (kbd "C-x g q") 'query-replace-regexp)

;; Set syntax highlighting and default color scheme
(require 'color-theme)
(color-theme-initialize)
(load-file "~/.emacs.d/site-lisp/color-theme-zenburn.el")
(color-theme-zenburn)
(global-font-lock-mode 1)

;; Set tab behavior
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)
(setq-default py-indent-offset 4)
(setq tab-width 4)

;; Unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "^\\*")

;; Line endings
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


;; CEDET
;; ======================================
(load-file "~/.emacs.d/site-lisp/cedet/common/cedet.el")
(global-ede-mode 1)
(semantic-load-enable-code-helpers)
(global-srecode-minor-mode 1)

;; Auto-Install
(require 'auto-install)

;; ERT (testing suite)
(require 'ert)
(require 'ert-x)

;; YASnippet
;; ======================================
(require 'yasnippet)
;(add-to-list 'yas/extra-mode-hooks
             ;'mako-mode-hook)
(yas/initialize)
(yas/load-directory "~/.emacs.d/site-lisp/yas-snippets/snippets")
(yas/load-directory "~/.emacs.d/site-lisp/yas-snippets/custom-snippets")

(require 'dropdown-list)
(setq yas/prompt-functions '(yas/dropdown-prompt))

;; Auto Insert Mode
;; ======================================
(require 'autoinsert)
(auto-insert-mode)
(setq auto-insert-directory "~/.emacs.d/templates/")
(setq auto-insert-query nil)
(define-auto-insert "\.org" "gtd-header.org")

;; LaTeX
;; ======================================
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-engine 'xetex)
(setq TeX-PDF-mode t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

(setq TeX-open-quote "``")
(setq TeX-close-quote "''")

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

;; C
;; ======================================
(setq c-default-style
      '((c-mode . "linux")))

(defun my-make-CR-do-indent ()
  (define-key c-mode-base-map "\C-m" 'c-context-line-break))
(add-hook 'c-initialization-hook 'my-make-CR-do-indent)

(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key (kbd "C-c o") 'ff-find-other-file)))

(require 'yacc-mode)
(require 'flex-mode)
(require 'ebnf-mode)
(add-to-list 'auto-mode-alist '("\\.y\\'" . yacc-mode))
(add-to-list 'auto-mode-alist '("\\.yy\\'" . yacc-mode))
(add-to-list 'auto-mode-alist '("\\.l\\'" . flex-mode))
(add-to-list 'auto-mode-alist '("\\.ll\\'" . flex-mode))
(add-to-list 'auto-mode-alist '("\\.bnf\\'" . ebnf-mode))
(add-to-list 'auto-mode-alist '("\\.ebnf\\'" . ebnf-mode))

;; Fortran
;; ======================================
(add-to-list 'auto-mode-alist '("\\.f\\'" . f90-mode))

;; Compile
;; ======================================
(require 'smart-compile)
(global-set-key [f12] 'smart-compile)

;; CMake
;; ======================================
(require 'cmake-mode)
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\''" . cmake-mode))

;; XML
;; ======================================
(eval-after-load 'rng-loc
  '(add-to-list 'rng-schema-locating-files
		"~/.schemas/nxml-schemas.xml"))

;; N3-Mode
(add-to-list 'load-path "~/.emacs.d/site-lisp/n3-mode.el")
(autoload 'n3-mode "n3-mode" "Major mode for OWL and N3 files" t)
(add-to-list 'auto-mode-alist '("\\.n3\\'" . n3-mode))
(add-to-list 'auto-mode-alist '("\\.owl\\'" . n3-mode))

;; nxml
(defun decorate-region (b e btxt etxt)
  "Wrap a region bounded by positions B and E with the string
BTXT at the beginning and ETXT at the end"
  (interactive "r\nMBeginning text: \nMEnding text: ")
  (save-excursion
    (save-restriction
      (narrow-to-region b e)
      (goto-char (point-min))
      (insert btxt)
      (goto-char (point-max))
      (insert etxt))))

(defun decorate-thing (thing btxt etxt)
  "Wrap current thing with BTXT and ETXT"
  (interactive "SThing: \nMBeginning text: \nMEnding text: ")
  (let ((bounds (bounds-of-thing-at-point thing)))
    (decorate-region
     (car bounds)
     (cdr bounds)
     btxt etxt)))

(require 'rng-nxml)

(defun my-rng-complete-tag ()
  (completing-read "Tag: "
                   'rng-complete-qname-function
                   nil nil ""
                   'rng-tag-history))

(defun nxml-wrap-region (b e tag)
  (interactive
   (list
    (region-beginning)
    (region-end)
    (my-rng-complete-tag)))
  (decorate-region b e
   (format "<%s>" tag)
   (format "</%s>" tag)))

(defun nxml-wrap-thing (thing tag)
  (interactive
   (list
    (intern (read-string "Thing: " nil))
    (my-rng-complete-tag)))
  (let ((bounds (bounds-of-thing-at-point thing)))
    (nxml-wrap-region (car bounds) (cdr bounds) tag)))

(defun nxml-wrap-word (tag)
  (interactive
   (list
    (my-rng-complete-tag)))
  (nxml-wrap-thing 'word tag))

;; HTML/CSS
;; ======================================
(require 'scss-mode)
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(setq scss-compile-at-save nil)

;; Scheme
;; ======================================
(require 'quack)
(add-to-list 'auto-mode-alist '("\\.rkt\\'" . scheme-mode)) ; racket files
(setq scheme-program-name "mzscheme")
(setq quack-fontify-style nil)

;; Lua
;; ======================================
(require 'lua-mode)
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;; Python
;; ======================================
(require 'python-mode)
(require 'cython-mode)
;; Strip trailing whitespace from python, C, and C++
(add-hook 'python-mode-hook
          (lambda ()
            (add-hook 'write-file-functions 'delete-trailing-whitespace)))

;; IPython
(setq ipythoncommand "/usr/bin/ipython")
(require 'ipython)

;; Anything-IPython
(require 'anything-ipython)
(add-hook 'python-mode-hook #'(lambda ()
                                (define-key py-mode-map (kbd "M-<tab>") 'anything-ipython-complete)))
(add-hook 'ipython-shell-hook #'(lambda ()
                                  (define-key py-mode-map (kbd "M-<tab>") 'anything-ipython-complete)))

;; Rope
;(setq pymacs-load-path '("~/.emacs.d/site-lispropemacs"))
;(pymacs-load "ropemacs" "rope-")

;; Eldoc
(add-hook 'python-mode-hook
          '(lambda () (eldoc-mode 1)) t)

;; Flymake and PyLint
(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "epylint" (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))

;; Nosetests
(defun py-nosetests()
  "Runs nosetests command on current file"
  (interactive)
  (let ((directory (substring (buffer-file-name) 0 (- (length
                                                       (buffer-file-name))
                                                      (+ 1 (length (buffer-name))))))
        (file (file-name-sans-extension (buffer-name))))
        (eshell-command (format "nosetests --pdb --with-doctest -w%s %s" directory file))))
(add-hook 'python-mode-hook
          '(lambda ()
             (local-set-key "\C-c\C-q" 'py-nosetests)))

;; Pdb debugger
(setq pdb-path '/usr/lib/python2.6/pdb.py
      gud-pdb-command-name (symbol-name pdb-path))
(defadvice pdb (before gud-query-cmdline activate)
  "Provide a better default command line when called interactively."
  (interactive
   (list (gud-query-cmdline pdb-path
                            (file-name-nondirectory buffer-file-name)))))

;; Pylons
;; ======================================

;; Mako
(define-derived-mode mako-mode html-mode "Mako"
  "Major mode for editing python Mako templates"
  (setq comment-start "##"))
(add-to-list 'auto-mode-alist '("\\.mako\\'" . mako-mode))

(define-derived-mode pylons-mode python-mode "Pylons"
  "Major mode for editing python Pylons projects")

;; Speedbar
;; ======================================
(require 'sr-speedbar)
(global-set-key (kbd "C-c s") 'sr-speedbar-toggle)

;; Igor Pro
;; ======================================
(add-to-list 'load-path "~/.emacs.d/site-lisp/igor-mode")
(require 'igor-mode)

;; Visual Basic
;; ======================================
(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\|vbs\\)$" .
                                 visual-basic-mode)) auto-mode-alist))

;; Matlab
;; ======================================
(add-to-list 'load-path "~/.emacs.d/site-lisp/matlab-emacs")
(load-library "matlab-load")
(matlab-cedet-setup)

;; ESS (R), statistics
;; ======================================
(add-to-list 'load-path "~/.emacs.d/site-lisp/ess")
(add-to-list 'load-path "~/.emacs.d/site-lisp/ess/lisp")
(require 'ess-site)

;; Icicles
;; ======================================
(setq load-path (append load-path (list "~/.emacs.d/site-lisp/icicles")))
(setq load-path (append load-path (list "~/.emacs.d/site-lisp/icicles/optional")))
(require 'icicles)

;; Tags
;; ======================================
(add-to-list 'load-path "~/.emacs.d/site-lisp/anything-etags-plus")
(require 'ctags-update)
(ctags-update-minor-mode 1)

(cond ((eq system-type 'linux)
       (setq path-to-ctags "/usr/bin/ctags"))
      ((eq system-type 'darwin)
       (setq path-to-ctags "/opt/local/bin/ctags"))
      ((eq system-type 'windows-nt)
       (setq path-to-ctags "C:\\cygwin\\bin\\ctags.exe")))
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -f %s/TAGS -e -R %s"
           path-to-ctags
           dir-name
           (directory-file-name dir-name))))

;; Version Control
;; ======================================

;; ditz
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-ditz")
(require 'ditz)

;; Org mode
;; ======================================
(if (eq system-type 'darwin)
    (setq load-path (cons "~/src/org-mode/lisp" load-path)))
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)

;; use links outside org-mode
(global-set-key "\C-c L" 'org-insert-link-global)
(global-set-key "\C-c o" 'org-open-at-point-global)

(setq org-log-done t)  ;; log timepoints
(setq org-use-fast-todo-selection t)
(setq org-agenda-include-diary t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)

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

;; org+remember integration
(setq org-directory "~/notebook/org/")
(setq org-default-notes-file "~/notebook/.notes")
(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)
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
           :lfmt (lambda (line) (apply 'format (cons rst-lfmt line)))
           :hlfmt (lambda (line) (apply 'format (cons rst-hlfmt line)))
           ))
         )
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
      (switch-to-buffer-other-window "notes.tex")
      (latex-mode))))

;; Constants
(require 'constants)
(define-key global-map "\C-cci" 'constants-insert)
(define-key global-map "\C-ccg" 'constants-get)
(define-key global-map "\C-ccr" 'constants-replace)
(setq constants-unit-system 'SI)

;; Unicode
;; ======================================

;; Set UTF-8 as default buffer coding
(require 'un-define "un-define" t)
(set-buffer-file-coding-system 'utf-8 'utf-8)
(set-default buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)
(set-default default-buffer-file-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-language-environment "UTF-8")

(defun unicode-insert (char)
  "Read a unicode code point and insert said character.
    Input uses `read-quoted-char-radix'.  If you want to copy
    the values from the Unicode charts, you should set it to 16."
  (interactive (list (read-quoted-char "Char: ")))
  (ucs-insert char))
(setq read-quoted-char-radix 16)

;; Set UTF-8 keys
(global-set-key "\C-z" nil)
(global-set-key "\C-z\C-q" 'unicode-insert)

(global-set-key "\C-zoo" "°") ;; degree
(global-set-key "\C-z+-" "±") ;; plus-minus
(global-set-key "\C-z.." "·") ;; center bullet
(global-set-key "\C-z<=" "≤") ;; less than
(global-set-key "\C-z>=" "≥") ;; greater than
(global-set-key "\C-z!=" "≠") ;; not equal
(global-set-key "\C-za" "α") ;; alpha
(global-set-key "\C-zb" "β") ;; beta
(global-set-key "\C-zg" "γ") ;; gamma
(global-set-key "\C-zd" "δ") ;; delta
(global-set-key "\C-zep" "ε") ;; epsilon
(global-set-key "\C-zz" "ζ") ;; zeta
(global-set-key "\C-zet" "η") ;; eta
(global-set-key "\C-zth" "θ") ;; theta
(global-set-key "\C-zi" "ι") ;; iota
(global-set-key "\C-zk" "κ") ;; kappa
(global-set-key "\C-zl" "λ") ;; lambda
(global-set-key "\C-zm" "μ") ;; mu
(global-set-key "\C-zn" "ν") ;; nu
(global-set-key "\C-zx" "ξ") ;; xi
(global-set-key "\C-zoc" "ο") ;; omicron
(global-set-key "\C-zpi" "π") ;; pi
(global-set-key "\C-zr" "ρ") ;; rho
(global-set-key "\C-zs" "σ") ;; sigma
(global-set-key "\C-zt" "τ") ;; tau
(global-set-key "\C-zu" "υ") ;; upsilon
(global-set-key "\C-zph" "φ") ;; phi
(global-set-key "\C-zc" "χ") ;; chi
(global-set-key "\C-zps" "ψ") ;; psi
(global-set-key "\C-zom" "ω") ;; omega

(global-set-key "\C-zA" "Α") ;; Alpha
(global-set-key "\C-zB" "Β") ;; Beta
(global-set-key "\C-zG" "Γ") ;; Gamma
(global-set-key "\C-zD" "Δ") ;; Delta
(global-set-key "\C-zEp" "Ε") ;; Epsilon
(global-set-key "\C-zZ" "Ζ") ;; Zeta
(global-set-key "\C-zEt" "Η") ;; Eta
(global-set-key "\C-zTh" "Θ") ;; Theta
(global-set-key "\C-zI" "Ι") ;; Iota
(global-set-key "\C-zK" "Κ") ;; Kappa
(global-set-key "\C-zL" "Λ") ;; Lambda
(global-set-key "\C-zM" "Μ") ;; Mu
(global-set-key "\C-zN" "Ν") ;; Nu
(global-set-key "\C-zX" "Ξ") ;; Xi
(global-set-key "\C-zOc" "Ο") ;; Omicron
(global-set-key "\C-zPi" "Π") ;; Pi
(global-set-key "\C-zR" "Ρ") ;; Rho
(global-set-key "\C-zS" "Σ") ;; Sigma
(global-set-key "\C-zTa" "Τ") ;; Tau
(global-set-key "\C-zU" "Υ") ;; Upsilon
(global-set-key "\C-zPh" "Φ") ;; Phi
(global-set-key "\C-zC" "Χ") ;; Chi
(global-set-key "\C-zPs" "Ψ") ;; Psi
(global-set-key "\C-zOm" "Ω") ;; Omega

; Set auto-replace
(define-abbrev-table 'global-abbrev-table '(
  ("alpha" "α" nil 0)
  ("beta" "β" nil 0)
  ("gamma" "γ" nil 0)
  ("delta" "δ" nil 0)
  ("epsilon" "ε" nil 0)
  ("zeta" "ζ" nil 0)
  ("eta" "η" nil 0)
  ("theta" "θ" nil 0)
  ("iota" "ι" nil 0)
  ("kappa" "κ" nil 0)
  ("lambda" "λ" nil 0)
  ("mu" "μ" nil 0)
  ("nu" "ν" nil 0)
  ("xi" "ξ" nil 0)
  ("omicron" "ο" nil 0)
  ("pi" "π" nil 0)
  ("rho" "ρ" nil 0)
  ("sigma" "σ" nil 0)
  ("tau" "τ" nil 0)
  ("upsilon" "υ" nil 0)
  ("phi" "φ" nil 0)
  ("chi" "χ" nil 0)
  ("psi" "ψ" nil 0)
  ("omega" "ω" nil 0)
  ("Alpha" "Α" nil 0)
  ("Beta" "Β" nil 0)
  ("Gamma" "Γ" nil 0)
  ("Delta" "Δ" nil 0)
  ("Epsilon" "Ε" nil 0)
  ("Zeta" "Ζ" nil 0)
  ("Eta" "Η" nil 0)
  ("Theta" "Θ" nil 0)
  ("Iota" "Ι" nil 0)
  ("Kappa" "Κ" nil 0)
  ("Lambda" "Λ" nil 0)
  ("Mu" "Μ" nil 0)
  ("Nu" "Ν" nil 0)
  ("Xi" "Ξ" nil 0)
  ("Omicron" "Ο" nil 0)
  ("Pi" "Π" nil 0)
  ("Rho" "Ρ" nil 0)
  ("Sigma" "Σ" nil 0)
  ("Tau" "Τ" nil 0)
  ("Upsilon" "Υ" nil 0)
  ("Phi" "Φ" nil 0)
  ("Chi" "Χ" nil 0)
  ("Psi" "Ψ" nil 0)
  ("Omega" "Ω" nil 0)
  ("degC" "°" nil 0)
  ("degreeC" "°" nil 0)
))

;; Other
;; ======================================

;; immediate access to GTD through 'M-x gtd'
(defun gtd ()
  (interactive)
  (find-files-glob "~/notebook/org/gtd/*.org")
)

;; Date-Time Functions
(defvar current-date-format "%Y-%m-%d %a"
  "Format of date to insert with `insert-current-date` func")

(defun insert-current-date ()
  "insert the current date using the format of `current-date-format`"
  (interactive)
  (insert (format-time-string current-date-format (current-time))))

(global-set-key "\C-c\C-d" 'insert-current-date)

(defun reload-igor-mode ()
  (interactive)
  (if (member major-mode '(igor-mode))
      (progn
        (text-mode)
        (unload-feature 'igor-mode t)
        (load-library "igor-mode")
        (igor-mode))))

;; Windows(OS)-specific Configuration
;; ======================================

(if (eq system-type 'windows-nt)
  (progn
    ;; Printer
    (setenv "PRINTER" "PDFCreator")
    (cond ((eq system-type 'windows-nt)
	   (setq ps-printer-name "PDFCreator")
	   (setq ps-printer-name-option "-d")
	   (setq ps-lpr-command "C:\\cygwin\\bin\\lpr.exe")))

    ;; Add cygwin to path
    (setenv "PATH" (concat (getenv "PATH") ";C:\\cygwin\\bin"))

    ;; Add font
    (set-default-font "Consolas-11")))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.40")
 '(frame-background-mode (quote dark))
 '(inhibit-startup-screen t)
 '(org-agenda-files (quote ("~/notebook/org/gtd/General.org" "~/notebook/org/gtd/P-Dev.org" "~/notebook/org/gtd/P-Projects.org" "/home/jason/notebook/org/gtd/Lists.org" "/home/jason/notebook/org/gtd/Work.org")))
 '(quack-fontify-style nil)
 '(quack-smart-open-paren-p t)
 '(rst-level-face-base-light 15))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
