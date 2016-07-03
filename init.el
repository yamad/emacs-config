;; jyh/.emacs.d/init.el
;; ======================================

;; modularized init
;; based on http://github.com/verdammelt/dotfiles

;; load paths
(add-to-list 'load-path "~/.emacs.d/site-lisp")
(add-to-list 'Info-default-directory-list "~/info")
(defun load-init-file (file)
  "load an emacs initialize file"
  (load (locate-user-emacs-file file)))

;; move Custom set variables, so we don't have to look at it
(setq custom-file (locate-user-emacs-file "init-custom.el"))
(load custom-file)

;; ensure required packages
(load-init-file "init-package")

;; start emacs server for emacsclient service
(require 'server)
(unless (server-running-p) (server-start))

(load-init-file "init-display")

;; General requires
;(require 'find-files)
(setq auto-mode-alist
      (append '(("\\.rst$"  . rst-mode)
                ("\\.rest$" . rst-mode)
                ("\\.f$"    . f90-mode))
              auto-mode-alist))
(setq interpreter-mode-alist
      (append '(("lua" . lua-mode))
              interpreter-mode-alist))

;; General Options
(setq visible-bell nil)
(setq ring-bell-funtion
      (lambda ()
        (invert-face 'mode-line)
        (run-with-timer 0.1 nil 'invert-face 'mode-line)))
(setq kill-whole-line t)
(desktop-save-mode 1)
(setq desktop-restore-eager 3)
(setq flycheck-check-syntax-automatically '(mode-enabled save))
(put 'narrow-to-region 'disabled nil)

;; terminal
(setq explicit-shell-file-name "zsh")
(defun named-term (name)
  (interactive "sName: ")
  (ansi-term "zsh" name))

;; project management
(use-package projectile
  :config
  (projectile-global-mode)
  (setq projectile-mode-line
        '(:eval (format " P/%s" (projectile-project-name))))
  (setq projectile-enable-caching t) ; otherwise too slow
  (use-package helm-projectile       ; use helm as interface
    :config
    (setq projectile-completion-system 'helm)
    (helm-projectile-on)
    (setq projectile-switch-project-action 'helm-projectile)))

;; which-key -- keybinding display
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))
  (use-package helm-descbinds
    :config (helm-descbinds-mode))


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

;; other useful keybindings
(global-set-key (kbd "C-x C-l") 'goto-line)
(global-set-key (kbd "C-x a r") 'align-regexp)

; bind frame/window switching to shift-up/down/left/right
(windmove-default-keybindings)

;; directory management
(use-package dired+
  :config
  ; don't create new buffer for every directory)
  (diredp-toggle-find-file-reuse-dir 1))

;; anzu -- multiple search/replace
(use-package anzu
  :config
  (global-anzu-mode +1)
  (define-key isearch-mode-map
    [remap isearch-query-replace]
    #'anzu-isearch-query-replace)
  (define-key isearch-mode-map
    [remap isearch-query-replace-regexp]
    #'anzu-isearch-query-replace-regexp)
  )

;; Unfill functions (opposes fill-paragraph and fill-region)
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line
of text"
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
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)
(setq-default py-indent-offset 4)
(setq tab-width 4)
(smart-tabs-insinuate 'c 'c++ 'java 'javascript 'cperl 'ruby 'nxml)
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (setq indent-tabs-mode t)))
(add-hook 'js2-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode t)))
(add-hook 'java-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)
            (setq tab-width 4)))

;; Unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "^\\*")

;; helm
(require 'helm)
(require 'helm-config)
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "M-x")     'undefined)
(global-set-key (kbd "C-c h")   'helm-command-prefix)
(global-set-key (kbd "M-x")     'helm-M-x)
(global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-y")     'helm-show-kill-ring)
(global-set-key (kbd "C-x b")   'helm-mini)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-c h i") 'helm-imenu)

;; swiper / ivy
(global-set-key (kbd "C-c C-s") 'swiper-helm)

; rebind tab to run persistent action
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
; make tab work in terminal
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
; list actions using C-z
(define-key helm-map (kbd "C-z") 'helm-select-action)

(helm-mode)
(diminish 'helm-mode)

;; autocomplete -- company mode
(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "C-:") 'helm-company)
     (define-key company-active-map (kbd "C-:") 'helm-company)
     (diminish 'company-mode)))

;; dash documentation browser
(global-set-key (kbd "C-c h f") 'helm-dash)
(global-set-key (kbd "C-c h g") 'helm-dash-at-point)
(global-set-key (kbd "C-c h h") 'helm-dash-reset-connections)

(defun jyh/dash-install (docset)
  (unless (file-exists-p (jyh/dash-path docset))
    (helm-dash-install-docset docset)))
(defvar jyh/required-dash-docsets
  '("C"
    "CMake"
    "D3"))
(setq helm-dash-browser-func 'eww)

;; ERT (testing suite)
(require 'ert)
(require 'ert-x)

(setq tramp-default-method "ssh")

(load-init-file "init-news")
(load-init-file "init-mail")
(load-init-file "init-tex")
(load-init-file "init-org")

;; magit (git)
(if (eq system-type 'windows-nt)
    (setq magit-git-executable "C:\\Program Files (x86)\\Git\\bin\\git.exe"))
(global-set-key (kbd "C-x C-g") 'magit-status)
(setq magit-last-seen-setup-instructions "1.4.0")

;; YASnippet
;; ======================================
(require 'dropdown-list)
(yas-global-mode)
(diminish 'yas-minor-mode)
(setq yas/root-directory "~/.emacs.d/site-lisp/yas-snippets/custom-snippets")
(yas-load-directory yas/root-directory)
(setq yas-prompt-functions '(yas/dropdown-prompt))


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
;              (when (and filename
;                         (string-match (expand-file-name "~/src/linux-trees")
                                        ;                                       filename))
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

(require 'yacc-mode)
(require 'flex-mode)
;(require 'ebnf-mode)
(add-to-list 'auto-mode-alist '("\\.y\\'" . yacc-mode))
(add-to-list 'auto-mode-alist '("\\.yy\\'" . yacc-mode))
(add-to-list 'auto-mode-alist '("\\.l\\'" . flex-mode))
(add-to-list 'auto-mode-alist '("\\.ll\\'" . flex-mode))
;(add-to-list 'auto-mode-alist '("\\.bnf\\'" . ebnf-mode))
;(add-to-list 'auto-mode-alist '("\\.ebnf\\'" . ebnf-mode))

;; Fortran
;; ======================================
(let ((findent 4))
  (setq fortran-do-intent findent)
  (setq fortran-if-intent findent)
  (setq fortran-structure-indent findent)
  (setq fortran-continuation-indent (+ 1 findent)))

;; Compile
;; ======================================
(require 'smarter-compile)
(global-set-key [f12] 'smarter-compile)

;; Objective-C
;; ======================================
(setq cc-other-file-alist
      `(("\\.cpp$" (".hpp" ".h"))
        ("\\.h$" (".c" ".cpp" ".m" ".mm"))
        ("\\.hpp$" (".cpp" ".c"))
        ("\\.m$" (".h"))
        ("\\.mm$" (".h"))
        ))

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
;(require 'n3-mode)
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
(setq scss-compile-at-save nil)

(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags
  '((js2-mode "<script[^>]*>" "</script>")
    (css-mode "<style[^>]*>" "</style>")))
(setq mweb-filename-extensions '("htm" "html"))
(multi-web-global-mode 1)

;; Javascript
;; ======================================
(add-to-list 'auto-mode-alist '("\\.js$"   . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$"  . js2-jsx-mode))

(add-hook 'js2-mode-hook
          (lambda ()
            (flycheck-mode t)
            (when (executable-find "eslint")
              (flycheck-select-checker 'javascript-eslint))
            (when (executable-find "flow")
              (flycheck-add-next-checker 'javascript-flow))
            (tern-mode)))
(add-hook 'web-mode-hook 'flycheck-mode)

(eval-after-load 'tern
  '(progn
     (setq-local company-backends (cons 'company-tern company-backends))))
(setq inferior-js-program-command "node")
(add-hook 'js2-mode-hook
          '(lambda ()
             (local-set-key "\C-x\C-e" 'js-send-last-sexp)
             (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
             (local-set-key "\C-cb" 'js-send-buffer)
             (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
             (local-set-key "\C-cl" 'js-load-file-and-go)))

(with-eval-after-load 'projectile
  (add-hook 'projectile-after-switch-project-hook 'jyh/setup-local-node-env))
(defun jyh/setup-local-node-env ()
  "use local programs for nodejs projects. use with
projectile-after-switch-project-hook"
  (interactive)
  (let ((local-babel-node (expand-file-name "./node_modules/.bin/babel-node"))
        (local-eslint (expand-file-name "./node_modules/.bin/eslint")))
    (if (file-exists-p local-babel-node)
        (setq inferior-js-program-command local-babel-node))
    (if (file-exists-p local-eslint)
        (setq flycheck-javascript-eslint-executable local-eslint))))

;; Haskell
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
(add-hook 'haskell-mode-hook 'haskell-doc-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'flycheck-mode)
(setq haskell-interactive-popup-errors nil)
(eval-after-load "haskell-mode"
  '(progn
     (define-key haskell-mode-map (kbd "C-x C-d") nil)
     (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
     (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
     (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
     (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile)
     (define-key haskell-mode-map (kbd "C-c M-.") nil)
     (define-key haskell-mode-map (kbd "C-c C-d") nil)))

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

;; R/ESS
;; ======================================
(require 'ess-site)
(setq ess-eval-visibly-p nil)
(setq ess-ask-for-ess-directory nil)
(ess-toggle-underscore nil)

(require 'poly-R)
(require 'poly-markdown)
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))


;; Ledger (accounting program)
(setq load-path
      (append load-path (list "~/.emacs.d/site-lisp/ledger-mode")))
(load-file "~/.emacs.d/site-lisp/ledger-mode/ledger-mode.el")
(require 'ledger)
(set-face-attribute 'ledger-font-xact-highlight-face nil
                    :background "midnight blue")
(set-face-attribute 'ledger-occur-xact-face nil
                    :background "midnight blue")

;; Tags
;; ======================================
(ctags-auto-update-mode 1)

(cond ((eq system-type 'linux)
       (setq path-to-ctags "/usr/bin/ctags"))
      ((eq system-type 'darwin)
       (setq path-to-ctags "/opt/local/bin/ctags"))
      ((eq system-type 'windows-nt)
       (setq path-to-ctags "C:\\cygwin\\bin\\ctags.exe")))
(defun create-tags(dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -f %s/TAGS -e -R %s"
           path-to-ctags
           dir-name
           (directory-file-name dir-name))))



;; Constants
(require 'constants)
(define-key global-map "\C-cci" 'constants-insert)
(define-key global-map "\C-ccg" 'constants-get)
(define-key global-map "\C-ccr" 'constants-replace)
(setq constants-unit-system 'SI)

(load-init-file "init-unicode")

;; Other
;; ======================================

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
    (set-frame-font "Consolas-11")))

;; Use chrome for urls on linux
(if (eq system-type 'gnu/linux)
    (setq browse-url-browser-function 'browse-url-xdg-open))
