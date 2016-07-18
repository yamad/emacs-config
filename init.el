;;; init.el --- emacs configuration for jyamad
;;
;; .emacs config credits for many good ideas:
;;  * http://github.com/lunaryorn/.emacs.d
;;  * http://github.com/jwiegley/dot-emacs
;;  * http://github.com/syl20bnr/spacemacs
;;  * http://sachachua.com/dotemacs
;;  * http://github.com/verdammelt/dotfiles
;;  * https://github.com/purcell/emacs.d

;;; Commentary:
;; Emacs Configuration for jyamad

;;; Code:

;; document minimum version
(let ((minver "24.5.1"))
  (when (version< emacs-version minver)
    (warn "This version (%s) of Emacs is older than the oldest tested version (%s) with this configuration. Stuff might be broken."
          emacs-version minver)))

;; don't load outdated byte code
(setq load-prefer-newer t)

;; extra load paths
(eval-and-compile
  ;; initialization files directory
  (add-to-list 'load-path (locate-user-emacs-file "init.d"))
  ;; custom elisp directory
  (add-to-list 'load-path (locate-user-emacs-file "lisp"))
  ;; local package directory
  (add-to-list 'load-path (locate-user-emacs-file "site-lisp"))
  (add-to-list 'Info-default-directory-list "~/info"))

(setq custom-file (locate-user-emacs-file "init.d/init-custom.el"))

(defsubst hook-into-modes (func &rest modes)
  "Help add a function FUNC to a list of MODES (from jwiegley)."
  (dolist (mode-hook modes) (add-hook mode-hook func)))

(defconst *is-mac-os* (eq system-type 'darwin))
(defconst *is-windows-os* (eq system-type 'windows-nt))
(defconst *is-linux-os* (eq system-type 'gnu/linux))

(defconst *is-mac-display* (eq window-system 'ns))
(defconst *is-windows-display* (eq window-system 'w32))
(defconst *is-X-display* (eq window-system 'x))
(defconst *is-terminal-display* (eq window-system nil))

;; ensure required packages
(require 'init-package)

;; put set variables from customize interface in own file
(load custom-file)

;; store all backups (*~) in one place
(setq backup-directory-alist `(("." . ,(locate-user-emacs-file "backups"))))

;; pick up environment from shell
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

;; General Options
(setq visible-bell nil)
(setq ring-bell-function
      (lambda ()
        (invert-face 'mode-line)
        (run-with-timer 0.1 nil 'invert-face 'mode-line)))
(setq kill-whole-line t)
(put 'narrow-to-region 'disabled nil)

;; answer y/n instead of yes/no
(fset 'yes-or-no-p #'y-or-n-p)

(use-package desktop
  :config
  (desktop-save-mode 1)
  (setq desktop-restore-eager 3))

(use-package rst
  :defer t
  :mode (("\\.rst$" . rst-mode)
         ("\\.rest$" . rst-mode)))
(use-package markdown-mode  :defer t)
(use-package markdown-mode+ :defer t)

;; screenplay format (Fountain)
(use-package fountain-mode
  :defer t
  :config
  (add-hook 'fountain-mode-hook 'turn-off-fci-mode))
(use-package olivetti
  :config
  (add-hook 'fountain-mode-hook 'turn-on-olivetti-mode))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  (setq flycheck-check-syntax-automatically '(mode-enabled save)))

(use-package helm-flycheck
  :ensure t
  :after flycheck)

(use-package autorevert
  :init (global-auto-revert-mode)
  :diminish auto-revert-mode)

;; terminal
(setq explicit-shell-file-name "zsh")
(defun named-term (name)
  (interactive "sName: ")
  (ansi-term "zsh" name))

;; project management
(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-mode-line
        '(:eval (format " P/%s" (projectile-project-name))))
  (setq projectile-enable-caching t) ; otherwise too slow
  (use-package helm-projectile       ; use helm as interface
    :ensure t
    :config
    (setq projectile-completion-system #'helm)
    (helm-projectile-on)
    (setq projectile-switch-project-action #'helm-projectile)))

(use-package helm-descbinds
  :ensure t
  :config (helm-descbinds-mode))

;; which-key -- keybinding display
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init (which-key-mode)
  :config
  (setq which-key-sort-order #'which-key-prefix-then-key-order
        which-key-key-replacement-alist
        '(("<\\([[:alnum:]-]+\\)>" . "\\1")
          ("up"                    . "↑")
          ("right"                 . "→")
          ("down"                  . "↓")
          ("left"                  . "←"))
        which-key-description-replacement-alist
        '(("Prefix Command" . "prefix")
          ;; Lambdas
          ("\\`\\?\\?\\'" . "λ")
          ;; Prettify hydra entry points
          ("/body\\'"     . "|=")))

  (which-key-declare-prefixes
    ;;Prefixes for global prefixes and minor modes
    "C-c !" "flycheck"
    "C-c f" "files"
    "C-c h" "helm"
    "C-c j" "jump"
    "C-c m" "major mode"
    "C-c o" "org"
    "C-c p" "projectile"
    "C-c s" "search"
    "C-c x" "text"
    "C-c y" "spotify"))

(use-package highlight-parentheses
  :ensure t
  :diminish highlight-parentheses-mode
  :config
  (global-highlight-parentheses-mode))

;; ANSI coloring
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on) ;; for shell
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

;; always remove trailing whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Alternate bindings for M-x
(bind-keys ("C-x C-m" . execute-extended-command)
           ("C-c C-m" . execute-extended-command))

;; Bindings for replace-regexp
(bind-keys ("C-x g r" . replace-regexp)
           ("C-x g q" . query-replace-regexp))

;; other useful keybindings
(bind-key "C-x C-l" 'goto-line)
(bind-key "C-x a r" 'align-regexp)

;; bind frame/window switching to shift-up/down/left/right
(windmove-default-keybindings)

;; directory management
(use-package dired+
  :ensure t
  :config
  ;; don't create new buffer for every directory
  (diredp-toggle-find-file-reuse-dir 1))

;; anzu -- show search position
(use-package anzu
  :ensure t
  :config (global-anzu-mode)
  :bind
  (([remap query-replace] . anzu-query-replace)
   ([remap query-replace-regexp] . anzu-query-replace-regexp)
   :map isearch-mode-map
   ([remap isearch-query-replace] . anzu-isearch-query-replace)
   ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :diminish anzu-mode)

(use-package visual-regexp
  :ensure t
  :bind (("C-c s r" . vr/replace)
         ("C-c s R" . vr/query-replace)))

;; Unfill functions (opposes fill-paragraph and fill-region)
(defun unfill-paragraph ()
  "Turn a multi-line paragraph into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))
(defun unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single
    logical line.  This is useful, e.g., for use with
    `visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))
(define-key global-map "\M-Q" 'unfill-paragraph)
(define-key global-map "\C-\M-Q" 'unfill-region)

;; Set tab behavior
(setq-default indent-tabs-mode nil
              tab-width 4
              c-basic-offset 4
              py-indent-offset 4)

(use-package smart-tabs-mode
  :ensure t
  :config
  (smart-tabs-insinuate 'c 'c++ 'java 'javascript 'cperl 'ruby 'nxml))

(hook-into-modes #'(lambda ()
                     (setq indent-tabs-mode t
                           tab-width 4))
                 'c-mode-common-hook
                 'js2-mode-hook
                 'java-mode-hook)

;; Unique buffer names
(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets
        uniquify-ignore-buffers-re "^\\*"))

;; from http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(bind-key [remap move-beginning-of-line]
          'smarter-move-beginning-of-line)

;; helm
(use-package helm
  :ensure t
  :bind (([remap execute-extended-command] . helm-M-x)     ; M-x
         ([remap switch-to-buffer] . helm-mini)            ; C-x b
         ([remap bookmark-jump] . helm-filtered-bookmarks) ; C-x r b
         ([remap find-file] . helm-find-files)             ; C-x C-f
         ([remap yank-pop] . helm-how-kill-ring)           ; M-y
         ("C-c h" . helm-command-prefix)
         :map helm-command-map
         ("o" . helm-occur)             ; C-c h o
         ("i" . helm-imenu)             ; C-c h i
         ("s" . helm-swoop)             ; C-c h s
         :map helm-map
         ("<tab>" . helm-execute-persistent-action)
         ("C-i" . helm-execute-persistent-action) ; make tab work in terminal
         ("C-z" . helm-select-action))            ; list actions using C-z
  :diminish helm-mode
  :config
  (require 'helm-config)
  (helm-mode)
  (use-package helm-swoop :ensure t)    ; helm-based searching
  (global-unset-key (kbd "C-x c")))

;; company -- autocomplete
(use-package company
  :ensure t
  :diminish company-mode
  :config
  (global-company-mode))

(use-package helm-company
  :ensure t
  :after (helm company)
  :bind (:map company-mode-map
         ("C-:" . helm-company)
         :map company-active-map
         ("C-:" . helm-company)))

;; ag -- better grep searching
(use-package ag :ensure t)
(use-package helm-ag :ensure t)

;; hippie-expand -- dabbrev replacement for expansion and completion
(use-package hippie-exp
  :bind (([remap dabbrev-expand] . hippie-expand))
  :defer t
  :config
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-list
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol)))

;; dash documentation browser
(use-package helm-dash
  :ensure t
  :after helm
  :bind (:map helm-command-map
              ("f" . helm-dash)
              ("g" . helm-dash-at-point)
              ("h" . helm-dash-reset-connections))
  :config
  (defun jyh/dash-install (docset)
    (unless (file-exists-p (jyh/dash-path docset))
      (helm-dash-install-docset docset)))
  (defvar jyh/required-dash-docsets
    '("C"
      "CMake"
      "D3"))
  (setq helm-dash-browser-func 'eww))

(setq tramp-default-method "ssh")

(require 'init-news)
(require 'init-mail)
(require 'init-tex)
(require 'init-org)
(require 'init-irc)

;; magit (git)
(use-package magit
  :ensure t
  :bind (("C-x C-g" . magit-status))
  :config
  (when *is-windows-os*
    (setq magit-git-executable "C:\\Program Files (x86)\\Git\\bin\\git.exe"))
  (setq magit-last-seen-setup-instructions "1.4.0"))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))

(use-package avy
  :bind (("C-. c" . avy-goto-char)
         ("C-. f" . avy-goto-char-in-line)
         ("C-. l" . avy-goto-line)
         ("C-. w" . avy-goto-word-or-subword-1)
         ("C-. s" . avy-goto-char-timer)
         ("C-. p" . avy-pop-mark))
  :config
  (setq avy-background t))

(use-package ace-jump-helm-line
  :bind (:map helm-map
              ("C-." . ace-jump-helm-line))
  :config
  ;; change appearance
  (setq ace-jump-helm-line-style 'pre)
  (setq ace-jump-helm-line-background t)
  (setq ace-jump-helm-line-default-action 'move-only)
  (ace-jump-helm-line-idle-exec-remove 'helm-mini) ;; turn off autostart
  ;; set action switch keys
  (setq ace-jump-helm-line-select-key ?e)
  (setq ace-jump-helm-line-move-only-key ?o)
  (setq ace-jump-helm-line-persistent-key ?p)
  ;; disable hints preview
  (ace-jump-helm-line-autoshow-mode -1)
  ;; use `linum-mode' to show
  (setq ace-jump-helm-line-autoshow-mode-use-linum t))

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "C-. n") 'ace-window)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package expand-region
  :ensure t
  :bind (("C-c v" . er/expand-region)))

(use-package hydra :ensure t)

;; directly from lunaryorn's config
(use-package smartparens                ; Parenthesis editing and balancing
  :ensure t
  :bind (("C-c k" . lunaryorn-smartparens/hydra/body)
         :map smartparens-strict-mode-map
         ;; A fill paragraph in strict mode
         ("M-q" . sp-indent-defun))
  :init
  ;; Hydra for Smartparens
  (defhydra lunaryorn-smartparens/hydra (:hint nil)
    "
Sexps (quit with _q_)
^Nav^            ^Barf/Slurp^                 ^Depth^
^---^------------^----------^-----------------^-----^-----------------
_f_: forward     _→_:          slurp forward   _R_: splice
_b_: backward    _←_:          barf forward    _r_: raise
_u_: backward ↑  _C-<right>_:  slurp backward  _↑_: raise backward
_d_: forward ↓   _C-<left>_:   barf backward   _↓_: raise forward
_p_: backward ↓
_n_: forward ↑
^Kill^           ^Misc^                       ^Wrap^
^----^-----------^----^-----------------------^----^------------------
_w_: copy        _j_: join                    _(_: wrap with ( )
_k_: kill        _s_: split                   _{_: wrap with { }
^^               _t_: transpose               _'_: wrap with ' '
^^               _c_: convolute               _\"_: wrap with \" \"
^^               _i_: indent defun"
    ("q" nil)
    ;; Wrapping
    ("(" (lambda (_) (interactive "P") (sp-wrap-with-pair "(")))
    ("{" (lambda (_) (interactive "P") (sp-wrap-with-pair "{")))
    ("'" (lambda (_) (interactive "P") (sp-wrap-with-pair "'")))
    ("\"" (lambda (_) (interactive "P") (sp-wrap-with-pair "\"")))
    ;; Navigation
    ("f" sp-forward-sexp )
    ("b" sp-backward-sexp)
    ("u" sp-backward-up-sexp)
    ("d" sp-down-sexp)
    ("p" sp-backward-down-sexp)
    ("n" sp-up-sexp)
    ;; Kill/copy
    ("w" sp-copy-sexp)
    ("k" sp-kill-sexp)
    ;; Misc
    ("t" sp-transpose-sexp)
    ("j" sp-join-sexp)
    ("s" sp-split-sexp)
    ("c" sp-convolute-sexp)
    ("i" sp-indent-defun)
    ;; Depth changing
    ("R" sp-splice-sexp)
    ("r" sp-splice-sexp-killing-around)
    ("<up>" sp-splice-sexp-killing-backward)
    ("<down>" sp-splice-sexp-killing-forward)
    ;; Barfing/slurping
    ("<right>" sp-forward-slurp-sexp)
    ("<left>" sp-forward-barf-sexp)
    ("C-<left>" sp-backward-barf-sexp)
    ("C-<right>" sp-backward-slurp-sexp))

  (smartparens-global-mode)
  :config
  (require 'smartparens-config)
  (setq sp-autoskip-closing-pair 'always
        ;; Don't kill entire symbol on C-k
        sp-hybrid-kill-entire-symbol nil)
  :diminish smartparens-mode)

;; C
;; ======================================
(setq c-default-style
      '((c-mode . "linux")))


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
            (c-set-style "linux-tabs-only")))

(defun my-make-CR-do-indent ()
  (define-key c-mode-base-map "\C-m" 'c-context-line-break))
(add-hook 'c-initialization-hook 'my-make-CR-do-indent)

(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key (kbd "C-c o") 'ff-find-other-file)))

(use-package bison-mode
  :defer t
  :mode (("\\.yy?\\'" . bison-mode)
         ("\\.ll?\\'" . bison-mode)))
(use-package ebnf-mode
  :ensure nil
  :defer t
  :mode (("\\.bnf$" . ebnf-mode)
         ("\\.ebnf$" . ebnf-mode)))

;; f90 -- Fortran 90 and later
(use-package f90
  :mode ("\\.f$" . f90-mode)
  :config
  (let ((findent 4))
    (setq fortran-do-intent findent)
    (setq fortran-if-intent findent)
    (setq fortran-structure-indent findent)
    (setq fortran-continuation-indent (+ 1 findent))))

(use-package smarter-compile
  :ensure t
  :bind ("<f12>" . smarter-compile))

;; Objective-C
;; ======================================
(setq cc-other-file-alist
      `(("\\.cpp$" (".hpp" ".h"))
        ("\\.h$" (".c" ".cpp" ".m" ".mm"))
        ("\\.hpp$" (".cpp" ".c"))
        ("\\.m$" (".h"))
        ("\\.mm$" (".h"))
        ))

(use-package cmake-mode
  :ensure t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

(use-package geiser :ensure t)

;; XML
;; ======================================
(eval-after-load 'rng-loc
  '(add-to-list 'rng-schema-locating-files
                "~/.schemas/nxml-schemas.xml"))

(use-package n3-mode
  :ensure t
  :mode (("\\.n3\\'" . n3-mode)
         ("\\.owl\\'" . n3-mode)))

(require 'rng-nxml)

;; HTML/CSS
;; ======================================
(require 'scss-mode)
(setq scss-compile-at-save nil)

(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags
      '((js2-mode "<script[^>]*>" "</script>")
        (css-mode "<style[^>]*>" "</style>")))
(setq mweb-filename-extensions '("htm" "html"))
(multi-web-global-mode 1)

(use-package restclient
  :ensure t
  :config
  (use-package restclient-helm :ensure t)
  (use-package company-restclient :ensure t))

(use-package js2-mode
  :ensure t
  :defer t
  :after flycheck
  :mode (("\\.js$" . js2-mode)
         ("\\.json$" . js2-mode)
         ("\\.jsx$" . js2-jsx-mode))
  :bind (:map js2-mode-map
         ("C-x C-e" . js-send-last-sexp)
         ("C-\ M-x" . js-send-last-sexp-and-go)
         ("C-c b" . js-send-buffer)
         ("C-c C-b" . js-send-buffer-and-go)
         ("C-c l" . js-load-file-and-go))
  :config
  ;; use programs from local node environment if possible
  (defun jyh/setup-local-node-env ()
    "use local programs for nodejs projects"
    (interactive)
    (let ((local-babel-node (expand-file-name "./node_modules/.bin/babel-node"))
          (local-eslint     (expand-file-name "./node_modules/.bin/eslint")))
      (if (file-exists-p local-babel-node)
          (setq inferior-js-program-command local-babel-node))
      (if (file-exists-p local-eslint)
          (setq flycheck-javascript-eslint-executable local-eslint))))
  (with-eval-after-load 'projectile
    (add-hook 'projectile-after-switch-project-hook
              'jyh/setup-local-node-env))

  (setq inferior-js-program-command "node")
  (add-hook 'js2-mode-hook #'flycheck-mode))

;; tern -- javascript static analysis
(use-package tern
  :ensure t
  :defer t
  :after js2-mode
  :config
  (add-hook 'js2-mode #'tern-mode))

(use-package company-tern
  :ensure t
  :defer t
  :after (company tern)
  :config
  (add-hook 'js2-mode-hook
            #'(lambda ()
                (setq-local company-backends
                            (cons 'company-tern company-backends)))))

;; skewer -- run browser REPL with buffers
(use-package skewer-mode
  :defer t
  :after js2-mode
  :config
  (add-hook 'js2-mode #'skewer-mode))

(use-package haskell-mode
  :defer t
  :commands (haskell-indentation-mode
             haskell-decl-scan
             haskell-doc-mode
             interactive-haskell-mode)
  :init
  (add-hook 'haskell-mode-hook
            #'(lambda ()
                (haskell-indentation-mode)
                (haskell-doc-mode)
                (haskell-decl-scan-mode)
                (interactive-haskell-mode)
                (flycheck-mode)))
  (setq haskell-interactive-popup-errors nil)
  :bind (:map haskell-mode-map
              ("C-x C-d" . nil)
              ("C-c C-z" . haskell-interactive-switch)
              ("C-c C-l" . haskell-process-load-file)
              ("C-c C-b" . haskell-interactive-switch)
              ("C-c C-t" . haskell-process-do-type)
              ("C-c C-i" . haskell-process-do-info)
              ("C-c C-c" . haskell-compile)
              ("C-c M-." . nil)
              ("C-c C-d" . nil)))

(use-package haskell-compile
  :ensure haskell-mode
  :defer t
  :config
  ;; use Stack for building
  (setq haskell-compile-cabal-build-command "stack build"))

(use-package lua-mode
  :ensure t
  :interpreter "lua")

(use-package spotify
  :ensure t
  :bind (("C-c y y" . spotify-playpause)
         ("C-c y p" . spotify-previous)
         ("C-c y n" . spotify-next)
         ("C-c y c" . spotify-current)))

;; Python
;; ======================================
(require 'cython-mode)
;; Nosetests
(defun py-nosetests()
  "Runs nosetests command on current file"
  (interactive)
  (let ((directory
         (substring (buffer-file-name) 0
                    (- (length
                        (buffer-file-name))
                       (+ 1 (length (buffer-name))))))
        (file (file-name-sans-extension (buffer-name))))
    (eshell-command
     (format "nosetests --pdb --with-doctest -w%s %s" directory file))))
(add-hook 'python-mode-hook
          '(lambda ()
             (local-set-key "\C-c\C-q" 'py-nosetests)))

(if (executable-find "ipython")
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "-i"))

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

;; Igor Pro
;; ======================================
(add-to-list 'load-path "~/.emacs.d/site-lisp/igor-mode")
(require 'igor-mode)

;; Visual Basic
;; ======================================
(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\|vbs\\)$" .
                                 visual-basic-mode)) auto-mode-alist))

;; Matlab/Octave
;; ======================================
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

;; R/ESS -- statistics software
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
  :ensure t
  :defer t
  :mode ("\\.Rmd$" . Rmd-mode)
  :init
  (defun Rmd-mode ()
    "ESS Markdown mode for Rmd files"
    (interactive)
    (require 'poly-R)
    (require 'poly-markdown)
    (R-mode)
    (poly-markdown+r-mode)))

;; ledger -- accounting program
(use-package ledger-mode
  :ensure t
  :config
  (set-face-attribute 'ledger-font-xact-highlight-face nil
                      :background "midnight blue")
  (set-face-attribute 'ledger-occur-xact-face nil
                      :background "midnight blue"))

;; tags
(use-package ctags-update
  :ensure t
  :config
  (ctags-auto-update-mode 1)
  (when *is-windows-os*
    (setq path-to-ctags "C:\\cygwin\\bin\\ctags.exe"))

  (defun create-tags(dir-name)
    "Create tags file."
    (interactive "DDirectory: ")
    (shell-command
     (format "%s -f %s/TAGS -e -R %s"
             path-to-ctags
             dir-name
             (directory-file-name dir-name)))))

;; Constants
(require 'constants)
(define-key global-map "\C-cci" 'constants-insert)
(define-key global-map "\C-ccg" 'constants-get)
(define-key global-map "\C-ccr" 'constants-replace)
(setq constants-unit-system 'SI)

(require 'init-unicode)

;; Other
;; ======================================

;; Date-Time Functions
(defvar current-date-format "%Y-%m-%d %a"
  "Format of date to insert with `insert-current-date` func")

(defun insert-current-date ()
  "insert the current date using the format of `current-date-format`"
  (interactive)
  (insert (format-time-string current-date-format (current-time))))
(bind-key "C-c C-d" 'insert-current-date)

(defun reload-igor-mode ()
  (interactive)
  (if (member major-mode '(igor-mode))
      (progn
        (text-mode)
        (unload-feature 'igor-mode t)
        (load-library "igor-mode")
        (igor-mode))))

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

(defun xah-syntax-color-hex ()
  "Syntax color hex color spec such as 「#ff1100」 in current buffer."
  (interactive)
  (font-lock-add-keywords
   nil
   '(("#[abcdef[:digit:]]\\{6\\}"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background (match-string-no-properties 0)))))))
  (font-lock-fontify-buffer))

(defun word-count (&optional b e)
  (interactive "r")
  (let ((b (if mark-active (region-beginning) (point-min)))
        (e (if mark-active (region-end) (point-max))))
    (save-excursion
      (save-restriction
        (narrow-to-region b e)
        (goto-char (point-min))
        (count-matches "\\sw+")))))
(defun randomly-inject-string (str &optional b e)
  (interactive "r")
  (let* ((b (if mark-active (region-beginning) (point-min)))
         (e (if mark-active (region-end) (point-max)))
         (wc (word-count b e))
         (step 0))
    (save-excursion
      (save-restriction
        (narrow-to-region b e)
        (goto-char (point-min))
        (while (and (< (point) (point-max)))
          (setq step (random (/ wc 5)))
          (forward-word step)
          (insert " " str))))))

;; configure display
(require 'init-display)

(require 'init-windows-os)

;; Use chrome for urls on linux
(when *is-linux-os*
  (setq browse-url-browser-function 'browse-url-xdg-open))

;; Local Variables:
;; coding: utf-8
;; End:

;;; init.el ends here
