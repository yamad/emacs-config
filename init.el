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

;; Alternate bindings for M-x
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; Bindings for replace-regexp
(global-set-key (kbd "C-x g r") 'replace-regexp)
(global-set-key (kbd "C-x g q") 'query-replace-regexp)

;; Set syntax highlighting and default color scheme
(require 'color-theme)
(color-theme-initialize)
(color-theme-standard)
;(color-theme-arjen)
(global-font-lock-mode 1)

;; Set tab behavior
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)
(setq-default py-indent-offset 4)
(setq tab-width 4)

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


;; Auto-Install
(require 'auto-install)

;; YASnippet
;; ======================================
(require 'yasnippet)
;(add-to-list 'yas/extra-mode-hooks
             ;'mako-mode-hook)
(yas/initialize)
(if (eq system-type 'windows-nt)
    (yas/load-directory "~/.emacs.d/site-lisp/yas-snippets/snippets")
(yas/load-directory "/usr/share/emacs/etc/yasnippet/snippets"))
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
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(add-hook 'LaTeX-math-mode 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

(setq TeX-open-quote "``")
(setq TeX-close-quote "''")

;; C
;; ======================================
(setq c-default-style
      '((c-mode . "k&r")))

(add-hook 'c-mode-hook 'c-toggle-auto-hungry-state)
(defun my-make-CR-do-indent ()
  (define-key c-mode-base-map "\C-m" 'c-context-line-break))
(add-hook 'c-initialization-hook 'my-make-CR-do-indent)

;; Compile 
;; ======================================
(require 'smart-compile)
(global-set-key [f12] 'smart-compile)

;; XML
;; ======================================

;; N3-Mode
(add-to-list 'load-path "~/.emacs.d/site-lisp/n3-mode.el")
(autoload 'n3-mode "n3-mode" "Major mode for OWL and N3 files" t)
(add-to-list 'auto-mode-alist '("\\.n3\\'" . n3-mode))
(add-to-list 'auto-mode-alist '("\\.owl\\'" . n3-mode))

;; Scheme
;; ======================================
(require 'quack)
(setq scheme-program-name "mzscheme")

;; Python
;; ======================================

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
(require 'igor-mode)

;; Matlab
;; ======================================
;(add-to-list 'load-path "~/src/matlab-emacs")
;(load-library "matlab-load")
;(matlab-cedet-setup)

;; Icicles
;; ======================================
(setq load-path (append load-path (list "~/.emacs.d/site-lisp/icicles")))
(setq load-path (append load-path (list "~/.emacs.d/site-lisp/icicles/optional")))
(require 'icicles)

;; Org mode
;; ======================================

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
(add-hook 'rst-mode-hook
          'turn-on-orgstruct)

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
    (setenv "PATH" (concat (getenv "PATH") ";C:\\cygwin\\bin"))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.40")
 '(frame-background-mode (quote dark))
 '(inhibit-startup-screen t)
 '(org-agenda-files (quote ("~/notebook/org/gtd/General.org" "~/notebook/org/gtd/P-Dev.org" "~/notebook/org/gtd/P-Projects.org" "/home/jason/notebook/org/gtd/Lists.org" "/home/jason/notebook/org/gtd/Work.org")))
 '(quack-smart-open-paren-p t)
 '(inhibit-startup-screen t)
 '(rst-level-face-base-light 15))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
