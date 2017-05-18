;;; init-lang.el --- programming language configuration
;;
;; part of emacs config for jyamad. see init.el

;;; Code:


;; ======================================
;;  C and C++
;; ======================================

(use-package cc-mode
  :defer t
  :ensure nil                           ; built-in
  :config
  (setq c-default-style '((c-mode . "linux")))
  (add-hook 'c-mode-common-hook #'hs-minor-mode) ; hide/show mode
  )

(use-package company-c-headers
  :after cc-mode
  :init
  (jyh-company-for-mode 'c-mode-common-hook company-c-headers))

(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

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
            (c-set-style "linux")))

(defun my-make-CR-do-indent ()
  (define-key c-mode-base-map "\C-m" 'c-context-line-break))
(add-hook 'c-initialization-hook 'my-make-CR-do-indent)

(use-package rtags
  :ensure t
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
  :ensure t
  :defer t
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
  (use-package ghc :ensure t))

(use-package haskell-compile
  :ensure haskell-mode
  :defer t)
  ;:config
  ;; use Stack for building
  ;(setq haskell-compile-cabal-build-command "stack build"))

(use-package hindent
  :ensure t
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

(use-package js2-mode
  :ensure t
  :after flycheck
  :mode (("\\.js$" . js2-mode)
         ("\\.json$" . js2-mode)
         ("\\.jsx$" . js2-jsx-mode))
  :init
  (use-package nodejs-repl
    :bind (:map js2-mode-map
                ("C-c C-z" . nodejs-repl-switch-to-repl)
                ("C-x C-e" . nodejs-repl-send-last-sexp)
                ("C-c C-b" . nodejs-repl-send-buffer)
                ("C-c C-r" . nodejs-repl-send-region)))
  ;; use programs from local node environment if possible
  (defun jyh/setup-local-node-env ()
    "use local programs for nodejs projects"
    (interactive)
    (let ((local-babel-node (expand-file-name "./node_modules/.bin/babel-node"))
          (local-eslint     (expand-file-name "./node_modules/.bin/eslint")))
      (if (file-exists-p local-babel-node)
          (setq nodejs-repl-command local-babel-node))
      (if (file-exists-p local-eslint)
          (setq flycheck-javascript-eslint-executable local-eslint))))
  (with-eval-after-load 'projectile
    (add-hook 'projectile-after-switch-project-hook
              'jyh/setup-local-node-env))
  (add-hook 'js2-mode-hook #'flycheck-mode))

;; tern -- javascript static analysis
(use-package tern
  :ensure t
  :defer t
  :after js2-mode
  :init
  (add-hook 'js2-mode-hook #'tern-mode))

(use-package company-tern
  :ensure t
  :after (company tern)
  :init
  (jyh-company-for-mode 'js2-mode-hook company-tern))

;; skewer -- run browser REPL with buffers
(use-package skewer-mode
  :defer t
  :after js2-mode
  :init
  (add-hook 'js2-mode #'skewer-mode))



;; ======================================
;;  Lua
;; ======================================

(use-package lua-mode
  :ensure t
  :interpreter "lua")



;; ======================================
;;  Matlab/Octave
;; ======================================

(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))



;; ======================================
;;  Python
;; ======================================

(use-package anaconda-mode
  :ensure t
  :defer t
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

(use-package company-anaconda
  :ensure t
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
         (replace-regexp-in-string "\n$" "" (shell-command-to-string "ipython --version"))))
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
  :ensure t
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
  (use-package ess-R-data-view :ensure t :defer t))

(use-package ess-smart-equals
  :ensure t
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



;; ======================================
;;  Scheme/Racket
;; ======================================

(use-package geiser
  :ensure t
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
