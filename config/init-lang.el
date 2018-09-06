;;; init-lang.el --- programming language configuration
;;
;; part of emacs config for jyamad. see init.el

;;; Code:


;; ======================================
;;  C and C++
;; ======================================

(use-package cc-mode
  :defer t
  :preface
  (defun c-lineup-arglist-tabs-only (ignored)
    "Line up argument lists by tabs, not spaces"
    (let* ((anchor (c-langelem-pos c-syntactic-element))
           (column (c-langelem-2nd-pos c-syntactic-element))
           (offset (- (1+ column) anchor))
           (steps (floor offset c-basic-offset)))
      (* (max steps 1)
         c-basic-offset)))

  (defun my-make-CR-do-indent ()
    (define-key c-mode-base-map "\C-m" 'c-context-line-break))

  :init
  (setq c-default-style '((c-mode . "linux")))
  (add-hook 'c-mode-common-hook #'hs-minor-mode) ; hide/show mode
  (add-hook 'c-initialization-hook 'my-make-CR-do-indent)

  (add-hook 'c-mode-common-hook
            (lambda ()
              ;; Add kernel style
              (c-add-style
               "linux-tabs-only"
               '("linux" (c-offsets-alist
                          (arglist-cont-nonempty
                           c-lineup-gcc-asm-reg
                           c-lineup-arglist-tabs-only))))))

  (add-hook 'c-mode-hook
          (lambda ()
            (let ((filename (buffer-file-name)))
              ;; Enable kernel mode for the appropriate files
              ;;              (when (and filename
              ;;                         (string-match (expand-file-name "~/src/linux-trees")
              ;;                                       filename))
              )
            (setq indent-tabs-mode t)
            (setq show-trailing-whitespace t)
            (c-set-style "linux"))))

(use-package rtags
  :straight t
  :defer t
  :init
  (add-hook 'c-mode-common-hook #'rtags-start-process-unless-running)
  (add-hook 'c++-mode-common-hook #'rtags-start-process-unless-running)
  (add-hook 'c-mode-common-hook
            #'(lambda ()
                (setq-local eldoc-documentation-function #'rtags-eldoc)))
  (jyh-company-for-mode 'c-mode-common-hook company-rtags)
  (rtags-enable-standard-keybindings)   ; default C-c r prefix
  (setq rtags-autostart-diagnostics t
        rtags-completions-enabled t)
  (rtags-diagnostics))

(use-package flycheck-rtags
  :straight t
  :defer t
  :after flycheck
  :preface
  (defun jyh-flycheck-rtags-setup ()
    (flycheck-select-checker 'rtags)
    (setq-local flycheck-highlighting-mode nil)
    (setq-local flycheck-check-syntax-automatically nil))
  :init
  (add-hook 'c-mode-common-hook #'jyh-flycheck-rtags-setup))

;; Objective-C
(setq cc-other-file-alist
      `(("\\.cpp$" (".hpp" ".h"))
        ("\\.h$" (".c" ".cpp" ".m" ".mm"))
        ("\\.hpp$" (".cpp" ".c"))
        ("\\.m$" (".h"))
        ("\\.mm$" (".h"))
        ))



;; ======================================
;;  Fortran (Fortran 90 and later)
;; ======================================

(use-package f90
  :mode ("\\.f$" . f90-mode)
  :config
  (let ((findent 4))
    (setq fortran-do-intent findent)
    (setq fortran-if-intent findent)
    (setq fortran-structure-indent findent)
    (setq fortran-continuation-indent (+ 1 findent))))


;; ======================================
;;  Haskell
;; ======================================

(use-package haskell-mode
  :defer t
  :commands (haskell-decl-scan
             haskell-doc-mode
             haskell-indentation-mode
             interactive-haskell-mode)

  :init
  (add-hook 'haskell-mode-hook
            #'(lambda ()
                (ghc-init)
                (haskell-doc-mode)
                (haskell-decl-scan-mode)
                (haskell-indentation-mode)
                (interactive-haskell-mode)
                (flycheck-mode)
                (setq-local company-backends '(company-ghc))))
  (setq haskell-interactive-popup-errors nil)
  (setq haskell-process-type 'stack-ghci)

  :bind (:map haskell-mode-map
              ("C-x C-d" . nil)
              ("C-c C-z" . haskell-interactive-switch)
              ("C-c C-l" . haskell-process-load-file)
              ("C-c C-b" . haskell-interactive-switch)
              ("C-c C-t" . haskell-process-do-type)
              ("C-c C-i" . haskell-process-do-info)
              ("C-c C-c" . haskell-compile)
              ("C-c M-." . nil)
              ("C-c C-d" . nil))
  :config
  (use-package ghc :straight t))

;;(use-package haskell-compile
;;  :ensure haskell-mode
;;  :defer t)
  ;:config
  ;; use Stack for building
  ;(setq haskell-compile-cabal-build-command "stack build"))

(use-package hindent
  :straight t
  :defer t
  :init (add-hook 'haskell-mode-hook #'hindent-mode))



;; ======================================
;; Igor Pro
;; ======================================

(use-package igor-mode
  :ensure nil
  :defer t
  :load-path "site-lisp/igor-mode"
  :mode ("\\.ipf$" . igor-mode)
  :preface
  (defun reload-igor-mode ()
    (interactive)
    (if (member major-mode '(igor-mode))
        (progn
          (text-mode)
          (unload-feature 'igor-mode t)
          (load-library "igor-mode")
        (igor-mode)))))



;; ======================================
;;  Javascript
;; ======================================

(defun jyh/find-parent-path (target)
  "Find nearest parent directory containing TARGET."
  (locate-dominating-file (buffer-file-name) target))

(defun jyh/find-path (target)
  "Find nearest path to TARGET in parent directories.

e.g. (jyh/find-path '.git') finds the nearest .git directory path"
  (let ((root (jyh/find-parent-path target)))
    (if root (expand-file-name target root) nil)))

(use-package js2-mode
  :straight t
  :mode (("\\.js$" . js2-mode)
         ("\\.json$" . js2-mode)
         ("\\.jsx$" . js2-jsx-mode))
  :init
  (use-package nodejs-repl
    :straight t
    :bind (:map js2-mode-map
                ("C-c C-z" . nodejs-repl-switch-to-repl)
                ("C-x C-e" . nodejs-repl-send-last-expression)
                ("C-c C-b" . nodejs-repl-send-buffer)
                ("C-c C-r" . nodejs-repl-send-region)))

  ;; use programs from local node environment if possible
  (defun jyh/find-node-executable (target-exec)
    "Return path to a node.js program TARGET-EXEC, preferring
local copy first."
    (or (jyh/find-path
         (concat (file-name-as-directory "node_modules/.bin")
                 target-exec))
        (executable-find target-exec)))

  (defun jyh/setup-local-node-env ()
    "Use local programs for nodejs projects."
    (interactive)
    (let* ((root (jyh/find-path "node_modules"))
           (bin-path (and root
                          (expand-file-name ".bin/" root))))
      (when bin-path
        (make-local-variable 'exec-path)
        (add-to-list 'exec-path bin-path)
        (make-local-variable 'process-environment)
        (setenv "PATH"
                (concat
                 (directory-file-name bin-path) ":"
                 (getenv "PATH"))))))


  (add-hook 'projectile-after-switch-project-hook
            'jyh/setup-local-node-env)
  (add-hook 'js2-mode-hook #'jyh/setup-local-node-env)

  ; prefer flycheck errors to builtins
  (setq js2-mode-show-strict-warnings nil
        js2-mode-show-parse-errors nil)
  (add-hook 'js2-mode-hook #'flycheck-mode))

;; tern -- javascript static analysis
(use-package tern
  :straight t
  :defer t
  :after js2-mode
  :init
  (add-hook 'js2-mode-hook #'tern-mode))

(use-package company-tern
  :straight t
  :after (company tern)
  :init
  (jyh-company-for-mode 'js2-mode-hook company-tern))

;; skewer -- run browser REPL with buffers
(use-package skewer-mode
  :defer t
  :straight t
  :after js2-mode
  :init
  (add-hook 'js2-mode #'skewer-mode))


;; ======================================
;;  JVM
;; ======================================

(use-package groovy-mode
  :straight t
  :mode ("\\.nf$" . groovy-mode)) ;; nextflow

(use-package scala-mode
  :straight t
  :defer t)
(use-package ensime
  :defer t
  :straight (ensime
             :type git
             :host github
             :branch "2.0"
             :repo "ensime/ensime-emacs"))


;; ======================================
;;  Emacs Lisp
;; ======================================

(use-package lisp-mode
  :defer t
  :hook ((emacs-lisp-mode lisp-mode)
         . (lambda () (add-hook 'after-save-hook 'check-parens nil t))))

;; ======================================
;;  Lua
;; ======================================

(use-package lua-mode
  :straight t
  :interpreter "lua")


;; ======================================
;;  Matlab/Octave
;; ======================================

(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))


;; ======================================
;;  Python
;; ======================================

(use-package anaconda-mode
  :straight t
  :defer t
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

(use-package pyenv-mode
  :straight t
  :defer t
  :commands pyenv-mode
  :init
  (add-hook 'python-mode-hook #'pyenv-mode)
  (when (file-exists-p "~/.pyenv/shims")
    (add-to-list 'exec-path "~/.pyenv/shims")))

(defun jyh/pyenv-version ()
  (s-trim (shell-command-to-string "pyenv version-name")))

(use-package pyenv-mode-auto
  :disabled)

(use-package pyvenv
  :straight t
  :defer t
  :commands pyvenv-mode
  :init
  (add-hook 'python-mode-hook #'pyvenv-mode))

(use-package py-isort                   ; sort import statements
  :straight t
  :defer t
  :commands (py-isort-buffer py-isort-before-save)
  :init
  (defun jyh-python-sort-imports ()
    (when (derived-mode-p 'python-mode)
      (py-isort-before-save)))
  (add-hook 'before-save-hook 'jyh-python-sort-imports))

(use-package pip-requirements           ; edit mode for requirements.txt
  :straight t
  :defer t)

(use-package pylookup
  :straight t
  :disabled)

(use-package pytest
  :straight t
  :defer t
  :commands (pytest-all
             pytest-module
             pytest-one
             pytest-directory
             pytest-pdb-all
             pytest-pdb-module
             pytest-pdb-one))

(use-package yapfify                    ; python code formatter
  :straight t
  :defer t
  :commands yapfify-buffer)

(use-package pydoc                      ; python documentation viewer
  :straight t
  :defer t
  :commands (pydoc
             pydoc-at-point
             pydoc-browse
             pydoc-info
             pydoc-jump-to-section))

(use-package company-anaconda
  :straight t
  :defer t
  :after (company anaconda-mode)
  :init
  (jyh-company-for-mode 'python-mode-hook company-anaconda))

;; setup IPython shell, use readline (rlipython) in IPython 6
(setq python-shell-interpreter-args "-i"
      python-shell-interpreter "python")
(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython")
  (let ((version-number
         (replace-regexp-in-string "\n$" "" (shell-command-to-string (concat (executable-find "ipython") " --version")))))
    (cond ((version< version-number "5")
           (setq python-shell-interpreter-args "-i"))
          ((version<= version-number "6")
           (setq python-shell-interpreter-args "--TerminalIPythonApp.interactive_shell_class=rlipython.TerminalInteractiveShell -i"))
          (t (setq python-shell-interpreter-args "--simple-prompt -i")))))

;; Pdb debugger
(setq pdb-path '/usr/lib/python2.6/pdb.py
      gud-pdb-command-name (symbol-name pdb-path))
(defadvice pdb (before gud-query-cmdline activate)
  "Provide a better default command line when called interactively."
  (interactive
   (list (gud-query-cmdline pdb-path
                            (file-name-nondirectory buffer-file-name)))))

;; Mako
(define-derived-mode mako-mode html-mode "Mako"
  "Major mode for editing python Mako templates"
  (setq comment-start "##"))
(add-to-list 'auto-mode-alist '("\\.mako\\'" . mako-mode))

;; Pylons
(define-derived-mode pylons-mode python-mode "Pylons"
  "Major mode for editing python Pylons projects")


;; ======================================
;;  R/ESS -- statistics software
;; ======================================

(use-package ess
  :straight t
  :defer t
  :commands R
  :mode (("\\.[rR]\\'" . R-mode)
         ("\\.[rR]profile\\'" . R-mode)
         ("\\.[Rr]out\\'" . R-transcript-mode)
         ("\\.[Bb][Uu][Gg]\\'" . ess-bugs-mode)
         ("\\.[Bb][Oo][Gg]\\'" . ess-bugs-mode)
         ("\\.[Bb][Mm][Dd]\\'" . ess-bugs-mode))
  :config
  (setq ess-eval-visibly-p nil)
  (setq ess-ask-for-ess-directory nil)

  (require 'ess-site)
  (with-eval-after-load 'ess-site
    ;; Follow Hadley Wickham's R style guide
    (setq ess-first-continued-statement-offset 2
          ess-continued-statement-offset 0
          ess-expression-offset 2
          ess-default-style 'DEFAULT))
  (use-package ess-R-data-view :defer t))

(use-package ess-smart-equals
  :straight t
  :defer t
  :init
  (hook-into-modes #'ess-smart-equals-mode
                   'ess-mode-hook
                   'inferior-ess-mode-hook))

(use-package polymode
  :ensure nil                           ; load locally
  :load-path (lambda ()
               (list "site-lisp/polymode"
                     "site-lisp/polymode/modes"))
  :mode ("\\.Rmd$" . Rmd-mode)
  :init
  (defun Rmd-mode ()
    "ESS Markdown mode for Rmd files"
    (interactive)
    (require 'poly-R)
    (require 'poly-markdown)
    (R-mode)
    (poly-markdown+r-mode))
  (defvar Rmd-mode-map nil "Keymap for `Rmd-mode'")
  (bind-keys
   :map Rmd-mode-map
   ("C-c <C-up>" . ess-Rmd-eval-buffer-from-beg-to-here)
   ("C-c <C-down>" . ess-Rmd-eval-buffer-from-here-to-end)
   ("C-c C-b" . ess-Rmd-eval-buffer)
   ("C-M-x" . ess-Rmd-eval-chunk))
  (add-hook 'poly-markdown+r-mode-hook #'visual-line-mode))

(use-package stan-mode :straight t :defer t)
(use-package stan-snippets :straight t :defer t)


;; ======================================
;;  Rust
;; ======================================

(use-package rust-mode
  :straight t
  :defer t)
(use-package flycheck-rust
  :straight t
  :defer t
  :after (flycheck rust-mode)
  :init
  (add-hook 'rust-mode-hook #'flycheck-rust-setup))


;; ======================================
;;  Scheme/Racket
;; ======================================

(use-package geiser
  :straight t
  :defer t
  :init
  (add-hook 'geiser-mode-hook
            (lambda ()
              (local-unset-key (kbd "C-.")))))


;; ======================================
;;  Visual Basic
;; ======================================

(use-package visual-basic-mode
  :mode ("\\.\\(frm\\|bas\\|cls\\|vbs\\)$" . visual-basic-mode))

(provide 'init-lang)
;;; init-lang.el ends here
