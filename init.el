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

(defconst emacs-start-time (current-time)) ; cribbed from jwiegley

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
  (add-to-list 'load-path (locate-user-emacs-file "config"))
  ;; custom elisp directory
  (add-to-list 'load-path (locate-user-emacs-file "lisp"))
  ;; local package directory
  (add-to-list 'load-path (locate-user-emacs-file "site-lisp"))
  (add-to-list 'Info-default-directory-list "~/info"))

(setq custom-file (locate-user-emacs-file "config/init-custom.el"))

;; ensure required packages
(require 'init-package)

;; put set variables from customize interface in own file
(load custom-file)

;; store all backups (*~) in one place
(setq backup-directory-alist `(("." . ,(locate-user-emacs-file "backups"))))

;; pick up environment from shell
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize))

;; packages needed for config
(use-package s :ensure t)               ; string handling
(use-package f :ensure t)               ; file handling

;; auxillary configurations
(require 'init-complete)
(require 'init-display)
(require 'init-evil)
(require 'init-irc)
(require 'init-lang)
(require 'init-mail)
(require 'init-markup)
(require 'init-news)
(require 'init-org)
(require 'init-tex)
(require 'init-tmux)
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
  :init
  (desktop-save-mode 1)
  (setq desktop-restore-eager 3
        desktop-lazy-verbose nil))

(use-package autorevert
  :init (global-auto-revert-mode)
  :diminish auto-revert-mode)

;; unique buffer names
(use-package uniquify                   ; unique buffer names
  :init
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets
        uniquify-ignore-buffers-re "^\\*"))

(use-package tramp
  :defer t
  :init
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
  :defer 10
  :config
  ;; don't create new buffer for every directory
  (diredp-toggle-find-file-reuse-dir 1))

(use-package bookmark+                  ; better bookmark management
  :ensure t
  :defer 10)

(use-package which-key                  ; keybinding display
  :ensure t
  :defer t
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
    "C-c ." "avy"
    "C-." "avy"
    "C-c !" "flycheck"
    "C-c c" "constants"
    "C-c f" "files"
    "C-c g" "git"
    "C-c i" "ivy"
    "C-c j" "jump"
    "C-c m" "major mode"
    "C-c o" "org"
    "C-c p" "projectile"
    "C-c s" "search"
    "C-c x" "text"
    "C-c t" "tmux"
    "C-c y" "spotify"
    "C-c &" "yasnippet"))


;; ======================================
;;  Navigation
;; ======================================

(use-package avy                        ; keyed label navigation
  :ensure t
  :defer t
  :init
  (bind-keys
   :prefix-map avy-keymap
   :prefix "C-c ."
   ("c" . avy-goto-char)
   ("f" . avy-goto-char-in-line)
   ("l" . avy-goto-line)
   ("w" . avy-goto-word-or-subword-1)
   ("s" . avy-goto-char-timer)
   ("p" . avy-pop-mark))
  (bind-key "C-." avy-keymap)           ; shortcut for GUIs
  (setq avy-background t))

(use-package ace-window                 ; keyed label window navigation
  :ensure t
  :defer t
  :init
  (bind-keys
   :map avy-keymap
   ("n" . ace-window))
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package zop-to-char                ; better zap-to-char
  :ensure t
  :bind ([remap zap-to-char] . zop-to-char))

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

(use-package ranger                     ; alternate file navigator
  :ensure t
  :bind (("C-c a r" . ranger)))

(use-package zoom-frm                   ; better font resizing
  :ensure t
  :bind (([remap text-scale-adjust] . zoom-in/out)))

(defhydra jyh/hydra-resize-windows (:hint nil)
  "
Windows (_q_ to quit):
_h_: grow horizontal     _l_: shrink horizontal
_j_: grow vertical       _k_: shrink vertical
_o_: cycle windows (>)   _O_: cycle windows (<)
_._: split horizontal    _/_: split vertical
"
  ("h" enlarge-window-horizontally)
  ("j" enlarge-window)
  ("k" shrink-window)
  ("l" srink-window-horizontally)
  ("/" split-window-horizontally)
  ("." split-window-vertically)
  ("o" other-window)
  ("O" (other-window -1))
  ("q" nil))
(bind-key "C-c w" 'jyh/hydra-resize-windows/body)

(defhydra hydra-vi (:pre (set-cursor-color "#40e0d0")
                    :post (set-cursor-color "#ffffff"))
  "vi"
  ("l" forward-char)
  ("h" backward-char)
  ("j" next-line)
  ("k" previous-line)
  ("." hydra-repeat)
  ("q" nil "quit"))
(bind-key "C-c v" 'hydra-vi/body)

(use-package persp-mode
  :ensure t
  :bind-keymap ("C-c q" . persp-key-map))

(use-package persp-projectile
  :ensure t
  :defer t)

;; ======================================
;;  Editing/Searching
;; ======================================

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode 1))

(use-package anzu                       ; show search position
  :ensure t
  :init
  (global-anzu-mode)
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
  "Unfill the region from BEG to END, joining text paragraphs into a single logical line.  This is useful, e.g., for use with `visual-line-mode'."
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
  :defer
  :init
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
  :defer 5
  :bind (("C-x C-g" . magit-status))
  :init
  (when *is-windows-os*
    (setq magit-git-executable "C:\\Program Files (x86)\\Git\\bin\\git.exe"))
  (setq magit-last-seen-setup-instructions "1.4.0")
  (setq vc-handled-backends (delq 'Git vc-handled-backends)))

(use-package git-gutter
  :ensure t
  :defer t
  :diminish "GG"
  :init
  (defhydra jyh-hydra-git-gutter ()
      "git"
      ("p" git-gutter:previous-hunk "previous")
      ("n" git-gutter:next-hunk "next")
      ("s" git-gutter:stage-hunk "stage")
      ("=" git-gutter:popup-hunk "view")
      ("SPC" git-gutter:mark-hunk "mark")
      ("q" nil "quit")))

(bind-keys* :prefix "C-c g"
            :prefix-map git-keymap)
(bind-keys :map git-keymap
           ("s" . magit-status)
           ("t" . git-gutter:toggle)
           ("SPC" . git-gutter:mark-hunk)
           ("=" . git-gutter:popup-hunk)
           ("p" . git-gutter:previous-hunk)
           ("n" . git-gutter:next-hunk)
           ("S" . git-gutter:stage-hunk)
           ("g" . jyh-hydra-git-gutter/body))

(use-package highlight-parentheses      ; highlight matching parens
  :ensure t
  :defer t
  :diminish highlight-parentheses-mode
  :init
  (global-highlight-parentheses-mode))

;; from lunaryorn's config
(use-package smartparens                ; parens editing and balancing
  :ensure t
  :defer t
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
  :bind (("C-c e" . er/expand-region)))

(use-package flycheck                   ; on-the-fly error checking
  :ensure t
  :bind ("C-c ! !" . jyh/hydra-flycheck/body)
  :init (global-flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  (defhydra jyh/hydra-flycheck
    (:pre (progn (setq hydra-lv t) (flycheck-list-errors))
     :post (progn (setq hydra-lv nil) (quit-windows-on "*Flycheck errors*")))
    "Errors"
    ("f"  flycheck-error-list-set-filter                            "Filter")
    ("j"  flycheck-next-error                                       "Next")
    ("k"  flycheck-previous-error                                   "Previous")
    ("gg" flycheck-first-error                                      "First")
    ("G"  (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
    ("q"  nil "quit")))


(use-package projectile                 ; project management
  :ensure t
  :defer t
  :diminish projectile-mode
  :init
  (projectile-global-mode)
  ;; uncomment this if smart-mode-line is off
  ;(setq projectile-mode-line
        ;'(:eval (format " P/%s" (projectile-project-name))))
  ;; otherwise too slow
  (setq projectile-enable-caching t))

(use-package ggtags                     ; symbol tags
  :ensure t
  :defer t
  :init
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


;; ======================================
;;  Completion
;; ======================================

(use-package ivy                        ; completion backend
  :ensure t
  :diminish ivy-mode
  :bind (("C-c C-r" . ivy-resume)
         :map ivy-minibuffer-map
         ("C-." . ivy-avy)
         ("C-m" . ivy-alt-done)
         ("C-S-m" . ivy-immediate-done))
  :init
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-re-builders-alist
        '((t . ivy--regex-ignore-order))
        ivy-height 20
        ivy-fixed-height-minibuffer nil
        ivy-use-selectable-prompt t)

  (use-package ivy-hydra :ensure t :defer t)     ; sticky keybindings within ivy
  (use-package swiper :ensure t :defer t)        ; within-buffer searching
  (use-package counsel :ensure t :defer t)
  (use-package counsel-projectile
    :ensure t
    :defer t
    :init (counsel-projectile-on))

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

(use-package hydra :ensure t)           ; sticky keys


;; ======================================
;;  External Utilities
;; ======================================

(use-package ag :ensure t)              ; better grep search

(use-package spotify                    ; spotify controls
  :ensure t
  :defer t
  :init
  (bind-keys
   :prefix-map spotify-keymap
   :prefix "C-c y"
   ("y" . spotify-playpause)
   ("p" . spotify-previous)
   ("n" . spotify-next)
   ("c" . spotify-current)))

(use-package maxima                     ; computer algebra system
  :defer t
  :ensure nil)
(use-package imaxima
  :defer t
  :ensure nil
  :after maxima)

(use-package emamux                     ; tmux integration
  :ensure t
  :defer t
  :init
  (bind-keys
   :prefix-map emamux-jyh-keymap
   :prefix "C-c t"
   ("s" . emamux:send-command)
   ("y" . emamux:yank-from-list-buffers)
   ("!" . emamux:run-command)
   ("r" . emamux:run-last-command)
   ("M-s" . emamux:run-region)
   ("C-i" . emamux:inspect-runner)
   ("C-k" . emamux:close-panes)
   ("C-c" . emamux:interrupt-runner)
   ("M-k" . emamux:clear-runner-history)
   ("c" . emamux:new-window)
   ("C" . emamux:clone-current-frame)
   ("2" . emamux:split-window)
   ("3" . emamux:split-window-horizontally)))

;; ======================================
;;  Other
;; ======================================

(use-package restclient
  :load-path "site-lisp/restclient"
  :defer t
  :preface
  (defun jyh-restclient-goto-body ()
    (save-match-data
      (beginning-of-buffer)
      (while (not (looking-at restclient-empty-line-regexp))
        (forward-line))
      (forward-line)))
  (defun jyh-restclient-maybe-gunzip ()
    (save-excursion
      (jyh-restclient-goto-body)
      (maybe-zlib-decompress-region)))
  :init
  (add-hook 'restclient-response-received-hook
            'jyh-restclient-maybe-gunzip))

(use-package company-restclient
  :ensure t
  :after restclient
  :init
  (jyh-company-for-mode 'restclient-mode-hook company-restclient))

(use-package crux                       ; bbatsov useful utilities
  :ensure t
  :commands (crux-rename-file-and-buffer)
  :defer t)

;; constants
(use-package constants
  :defer t
  :init
  (bind-keys :prefix-map constants-keymap
             :prefix "C-c c"
             ("i" . constants-insert)
             ("g" . constants-get)
             ("r" . constants-replace))
  (setq constants-unit-system 'SI))

(use-package server                     ; server for emacsclient
  :commands server-running-p
  :init
  (unless (server-running-p) (server-start)))

;; custom and collected elisp stuff
(require 'jyh-functions)

;; Local Variables:
;; coding: utf-8
;; End:

;;; init.el ends here
