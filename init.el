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

;; added by Package.el for emacs 25
(package-initialize)

;; document minimum version
(let ((minver "24.5.1"))
  (when (version< emacs-version minver)
    (warn "This version (%s) of Emacs is older than the oldest tested version (%s) with this configuration. Stuff might be broken."
          emacs-version minver)))

;; configuration functions
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
    (add-to-list 'exec-path-from-shell-variables "NIX_PATH")
    (add-to-list 'exec-path-from-shell-variables "CURL_CA_BUNDLE")
    (exec-path-from-shell-initialize)))

;; auxillary configurations
(require 'init-display)
(require 'init-irc)
(require 'init-lang)
(require 'init-mail)
(require 'init-markup)
(require 'init-news)
(require 'init-org)
(require 'init-tex)
(require 'init-unicode)

;; operating system specific configuration
(require 'init-os-windows)
(require 'init-os-mac)
(require 'init-os-linux)


;; general options
(setq visible-bell nil)
(setq ring-bell-function
      (lambda ()
        (invert-face 'mode-line)
        (run-with-timer 0.1 nil 'invert-face 'mode-line)))
(setq kill-whole-line t)
(put 'narrow-to-region 'disabled nil)
;; always remove trailing whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; answer y/n instead of yes/no
(fset 'yes-or-no-p #'y-or-n-p)
;; ANSI coloring
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on) ;; for shell
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
;; bind frame/window switching to shift-up/down/left/right
(windmove-default-keybindings)


(use-package desktop
  :config
  (desktop-save-mode 1)
  (setq desktop-restore-eager 3))

(use-package autorevert
  :init (global-auto-revert-mode)
  :diminish auto-revert-mode)

;; unique buffer names
(use-package uniquify                   ; unique buffer names
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets
        uniquify-ignore-buffers-re "^\\*"))

;; terminal
(setq explicit-shell-file-name "zsh")
(defun named-term (name)
  (interactive "sName: ")
  (ansi-term "zsh" name))

(use-package tramp
  :config
  (setq tramp-default-method "ssh"))

(bind-keys ("C-x C-m" . execute-extended-command)
           ("C-c C-m" . execute-extended-command)
           ("C-x g r" . replace-regexp)
           ("C-x g q" . query-replace-regexp)
           ("C-x C-l" . goto-line)
           ("C-x a r" . align-regexp))


;; ======================================
;;  Emacs
;; ======================================

(use-package dired+                     ; better directory management
  :ensure t
  :config
  ;; don't create new buffer for every directory
  (diredp-toggle-find-file-reuse-dir 1))

(use-package bookmark+                  ; better bookmark management
  :ensure t)

(use-package which-key                  ; keybinding display
  :ensure t
  :diminish which-key-mode
  :init (which-key-mode)
  :config
  (setq which-key-sort-order #'which-key-prefix-then-key-order
        which-key-prefix-prefix "➤"
        which-key-key-replacement-alist
        '(("<\\([[:alnum:]-]+\\)>" . "\\1")
          ("up"                    . "↑")
          ("right"                 . "→")
          ("down"                  . "↓")
          ("left"                  . "←"))
        which-key-description-replacement-alist
        '(("Prefix Command" . "➤")
          ;; Lambdas
          ("\\`\\?\\?\\'" . "λ")
          ;; Prettify hydra entry points
          ("/body\\'"     . "|=")))

  (which-key-declare-prefixes
    ;;Prefixes for global prefixes and minor modes
    "C-c !" "flycheck"
    "C-c f" "files"
    "C-c i" "ivy"
    "C-c j" "jump"
    "C-c m" "major mode"
    "C-c o" "org"
    "C-c p" "projectile"
    "C-c s" "search"
    "C-c x" "text"
    "C-c y" "spotify"))


;; ======================================
;;  Navigation
;; ======================================

(use-package avy                        ; keyed label navigation
  :bind (("C-. c" . avy-goto-char)
         ("C-. f" . avy-goto-char-in-line)
         ("C-. l" . avy-goto-line)
         ("C-. w" . avy-goto-word-or-subword-1)
         ("C-. s" . avy-goto-char-timer)
         ("C-. p" . avy-pop-mark))
  :config
  (setq avy-background t))

(use-package ace-window                 ; keyed label window navigation
  :ensure t
  :config
  (global-set-key (kbd "C-. n") 'ace-window)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package zop-to-char                ; better zap-to-char
  :ensure t
  :config
  (global-set-key [remap zap-to-char] 'zop-to-char))

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

;; alternative directory/file navigator
(use-package ranger :ensure t)



;; ======================================
;;  Editing/Searching
;; ======================================

(use-package anzu                       ; show search position
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

(use-package visual-fill-column
  :ensure t
  :defer t
  :init (add-hook 'visual-line-mode-hook #'visual-fill-column-mode))
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
(bind-keys ("M-Q" . unfill-paragraph)
           ("C-M-Q" . unfill-region))

;; tab/spaces behavior
(setq-default indent-tabs-mode nil
              tab-width 4
              c-basic-offset 4
              py-indent-offset 4)
(use-package smart-tabs-mode            ; tabs(indentation), spaces(alignment)
  :ensure t
  :config
  (smart-tabs-insinuate 'c 'c++ 'java 'javascript 'cperl 'ruby 'nxml))

(hook-into-modes #'(lambda ()
                     (setq indent-tabs-mode t
                           tab-width 4))
                 'c-mode-common-hook
                 'js2-mode-hook
                 'java-mode-hook)


;; ======================================
;;  Programming
;; ======================================

(use-package magit                      ; git version control
  :ensure t
  :bind (("C-x C-g" . magit-status))
  :config
  (when *is-windows-os*
    (setq magit-git-executable "C:\\Program Files (x86)\\Git\\bin\\git.exe"))
  (setq magit-last-seen-setup-instructions "1.4.0"))


(use-package highlight-parentheses      ; highlight matching parens
  :ensure t
  :diminish highlight-parentheses-mode
  :config
  (global-highlight-parentheses-mode))

;; from lunaryorn's config
(use-package smartparens                ; parens editing and balancing
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

(use-package expand-region
  :ensure t
  :bind (("C-c v" . er/expand-region)))

(use-package flycheck                   ; on-the-fly error checking
  :ensure t
  :init
  (global-flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  (defhydra jyh/hydra-flycheck
    (:pre (progn (setq hydra-lv t) (flycheck-list-errors))
     :post (progn (setq hydra-lv nil) (quit-windows-on "*Flycheck errors*"))
     :hint nil)
    "Errors"
    ("f"  flycheck-error-list-set-filter                            "Filter")
    ("j"  flycheck-next-error                                       "Next")
    ("k"  flycheck-previous-error                                   "Previous")
    ("gg" flycheck-first-error                                      "First")
    ("G"  (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
    ("q"  nil)))

(use-package projectile                 ; project management
  :ensure t
  :diminish projectile-mode
  :config
  (projectile-global-mode)
  ;; uncomment this if smart-mode-line is off
  ;(setq projectile-mode-line
        ;'(:eval (format " P/%s" (projectile-project-name))))
  ;; otherwise too slow
  (setq projectile-enable-caching t))

(use-package ggtags                     ; symbol tags
  :ensure t
  :config
  (hook-into-modes
   '(lambda () (ggtags-mode 1))
   'c-mode-hook
   'c++-mode-hook
   'erlang-mode-hook
   'f90-mode-hook
   'java-mode-hook
   'js2-mode-hook
   'lua-mode-hook
   'makefile-mode-hook))

(use-package yasnippet                  ; snippets/templates
  :ensure t
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))


;; ======================================
;;  Completion
;; ======================================

(use-package ivy                        ; completion backend
  :ensure t
  :diminish ivy-mode
  :bind (("C-c C-r" . ivy-resume)
         :map ivy-minibuffer-map
         ("C-." . ivy-avy))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-re-builders-alist
        '((t . ivy--regex-ignore-order))
        ivy-height 20
        ivy-fixed-height-minibuffer nil)

  (use-package ivy-hydra :ensure t)     ; sticky keybindings within ivy
  (use-package swiper :ensure t)        ; within-buffer searching
  (use-package counsel :ensure t)
  (use-package counsel-projectile :ensure t
    :config
    (counsel-projectile-on))

  ;; setup ivy-based global command keymap
  (bind-keys
   :prefix-map ivy-command-map
   :prefix "C-c i"
   :prefix-docstring "Ivy/Counsel command keymap"
   ("s" . swiper)
   ("o" . ivy-occur)
   ("i" . counsel-imenu)
   ("k" . counsel-ag)
   ("f" . counsel-recentf)
   ("g" . counsel-git)
   ("j" . counsel-git-grep)
   ("l" . counsel-locate)
   ("u" . counsel-unicode-char)
   )

  ;; remap built-in functions
  (bind-keys
   ([remap execute-extended-command] . counsel-M-x)  ; M-x
   ([remap switch-to-buffer] . ivy-switch-buffer)    ; C-x b
   ([remap bookmark-jump] . counsel-bookmark)        ; C-x r b
   ([remap find-file] . counsel-find-file)           ; C-x C-f
   ([remap yank-pop] . counsel-yank-pop)             ; M-y
   )

  (bind-keys
   :map company-mode-map
   ("C-:" . counsel-company)
   :map company-active-map
   ("C-:" . counsel-company)) )

(use-package company                    ; autocompletion
  :ensure t
  :diminish company-mode
  :bind (("M-RET" . company-complete))
  :config
  (global-company-mode)
  (setq company-dabbrev-code-modes t
        company-dabbrev-code-everywhere t))

(use-package hippie-exp    ; dabbrev enhacements, expansion/completion
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

(use-package hydra :ensure t)           ;


;; ======================================
;;  External Utilities
;; ======================================

(use-package ag :ensure t)              ; better grep search

(use-package spotify                    ; spotify controls
  :ensure t
  :bind (("C-c y y" . spotify-playpause)
         ("C-c y p" . spotify-previous)
         ("C-c y n" . spotify-next)
         ("C-c y c" . spotify-current)))

(use-package maxima                     ; computer algebra system
  :defer t
  :ensure nil)
(use-package imaxima
  :defer t
  :ensure nil
  :after maxima)


;; ======================================
;;  Other
;; ======================================

(use-package restclient
  :ensure t
  :config
  (use-package company-restclient :ensure t))

(use-package crux :ensure t)            ; bbatsov useful utitilies

;; constants
(require 'constants)
(define-key global-map "\C-cci" 'constants-insert)
(define-key global-map "\C-ccg" 'constants-get)
(define-key global-map "\C-ccr" 'constants-replace)
(setq constants-unit-system 'SI)



;; ======================================
;;  Misc Functions
;; ======================================

;; Date-Time Functions
(defvar current-date-format "%Y-%m-%d %a"
  "Format of date to insert with `insert-current-date` func")

(defun insert-current-date ()
  "insert the current date using the format of `current-date-format`"
  (interactive)
  (insert (format-time-string current-date-format (current-time))))
(bind-key "C-c C-d" 'insert-current-date)

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

;; Local Variables:
;; coding: utf-8
;; End:

;;; init.el ends here
