;;; init.el --- emacs configuration for jyamad -*- lexical-binding: t; -*-
;;
;; .emacs config credits for many good ideas:
;;  * https://github.com/lunaryorn/.emacs.d
;;  * https://github.com/jwiegley/dot-emacs
;;  * https://github.com/syl20bnr/spacemacs
;;  * https://sachachua.com/dotemacs
;;  * https://github.com/verdammelt/dotfiles
;;  * https://github.com/purcell/emacs.d
;;  * https://github.com/hlissner/doom-emacs

;;; Commentary:
;; Emacs Configuration for jyamad
(require 'seq)

;;; Code:
(defconst emacs-start-time (current-time)) ; cribbed from jwiegley

;; document minimum version
(let ((minver "26"))
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
  :straight t
  :if (memq window-system '(mac ns))
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize))

;; load environment variables per directory, if using direnv
(use-package direnv
  :straight t)

;; packages needed for config
(use-package general                    ; keybindings
  :straight t
  :commands (general-define-key
             general-def)
  :config
  (general-create-definer jyh/evil-leader-map
    :prefix "SPC"
    :non-normal-prefix "M-SPC"
    :states '(normal insert emacs))
  (general-create-definer jyh/emacs-leader-map
    :prefix "C-c")

  (defmacro jyh/bind-leader-maps (&rest bindings)
    `(progn
      (jyh/emacs-leader-map ,@bindings)
      (jyh/evil-leader-map ,@bindings)))
  (defmacro jyh/bind-leader-prefix-map (prefix map &optional display)
    "Bind key prefix PREFIX to keymap MAP.

If provided, DISPLAY is used as the which-key text"
    `(jyh/bind-leader-maps
      ,prefix
      '(:keymap ,map ,@(when display `(:wk ,display)))))

  (jyh/bind-leader-maps "SPC" '(execute-extended-command :wk "M-x"))  )

(use-package hydra                      ; sticky keys
  :straight t
  :defer t
  :functions defhydra)
;(use-package diminish                   ; control modeline status
  ;:straight t
  ;:defer t)

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
  :defer t
  :init
  (desktop-save-mode 1)
  (setq desktop-restore-eager 3
        desktop-lazy-verbose nil))

(use-package autorevert
  :defer t
  :init (global-auto-revert-mode)
  :diminish auto-revert-mode)

;; unique buffer names
(use-package uniquify                   ; unique buffer names
  :defer t
  :init
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets
        uniquify-ignore-buffers-re "^\\*"))

(use-package tramp
  :defer t
  :init
  (setq tramp-default-method "sshx"
        tramp-terminal-type "dumb"))

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
  :straight t
  :disabled				; incompatible for now with emacs 26.2
  :defer t
  :config
  ;; don't create new buffer for every directory
  (diredp-toggle-find-file-reuse-dir 1))

(use-package bookmark+                  ; better bookmark management
  :straight t
  :defer t)

(use-package which-key                  ; keybinding display
  :straight t
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
    "C-c !" "flymake"
    "C-c f" "files"
    "C-c j" "jump"
    "C-c m" "major mode"
    "C-c s" "search"
    "C-c x" "text"
    "C-c &" "yasnippet"))


;; ======================================
;;  Navigation
;; ======================================

(use-package avy                        ; keyed label navigation
  :straight t
  :defer t
  :init
  (general-def
    :prefix-map 'jyh/avy-keymap
    "c" 'avy-goto-char
    "f" 'avy-goto-char-in-line
    "l" 'avy-goto-line
    "w" 'avy-goto-word-or-subword-1
    "s" 'avy-goto-char-timer
    "p" 'avy-pop-mark)
  (jyh/bind-leader-prefix-map
   "." jyh/avy-keymap "nav")

  (bind-key "C-." jyh/avy-keymap)           ; shortcut for GUIs
  (setq avy-background t))

(use-package ace-window                 ; keyed label window navigation
  :straight t
  :defer t
  :after avy
  :init
  (bind-keys
   :map jyh/avy-keymap
   ("n" . ace-window))
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package zop-to-char                ; better zap-to-char
  :straight t
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
  :straight t
  :bind (("C-c a r" . ranger)))

(use-package zoom-frm                   ; better font resizing
  :straight t
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
  ("l" shrink-window-horizontally)
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
  :straight t
  :disabled
  :bind-keymap ("C-c q" . persp-key-map))

(use-package persp-projectile
  :defer t
  :straight t)

(use-package eyebrowse                  ; window configurations
  :straight t
  :config
  (eyebrowse-mode t))

;; ======================================
;;  Editing/Searching
;; ======================================

(use-package undo-tree
  :defer t
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode 1))

(use-package anzu                       ; show search position
  :straight t
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
  :straight t
  :defer t
  :bind (("C-c s r" . vr/replace)
         ("C-c s R" . vr/query-replace)))

(use-package visual-fill-column
  :straight t
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
  :straight t
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
  :straight t
  :defer t
  :commands (magit-status magit-file-dispatch)
  :bind (("C-x C-g" . magit-status))
  :init
  (when *is-windows-os*
    (setq magit-git-executable "C:\\Program Files (x86)\\Git\\bin\\git.exe"))
  (setq magit-last-seen-setup-instructions "1.4.0")
  (setq vc-handled-backends (delq 'Git vc-handled-backends))
  :config
  (global-magit-file-mode -1)) ; don't set some global keybindings

(use-package git-gutter
  :straight t
  :defer t
  :commands git-gutter-mode
  :diminish "GG"
  :init
  (defhydra jyh-hydra-git-gutter
    (:body-pre (git-gutter-mode +1))
    "git"
    ("p" git-gutter:previous-hunk "prev")
    ("n" git-gutter:next-hunk "next")
    ("s" git-gutter:stage-hunk "stage")
    ("t" git-gutter-mode "toggle")
    ("=" git-gutter:popup-hunk "view")
    ("SPC" git-gutter:mark-hunk "mark")
    ("q" nil "quit")
    ("Q" (git-gutter-mode -1)
     "shut off" :color blue)))

(general-def
  :prefix-map 'jyh/git-keymap
  "s" 'magit-status
  "b" 'magit-blame-addition
  "d" 'magit-diff-buffer-file
  "l" 'magit-log-buffer-file
  "g" 'magit-file-dispatch
  "t" 'git-gutter-mode
  "=" 'magit-diff-buffer-file
  "G" 'jyh-hydra-git-gutter/body)
(jyh/bind-leader-prefix-map
 "g" jyh/git-keymap "git")

(use-package ediff
  :defer t
  :config
  (setq ediff-split-window-function 'split-window-horizontally))

(use-package highlight-parentheses      ; highlight matching parens
  :straight t
  :defer t
  :diminish highlight-parentheses-mode
  :init
  (global-highlight-parentheses-mode))

;; from lunaryorn's config
(use-package smartparens                ; parens editing and balancing
  :straight t
  :defer t
  :bind (("C-c k" . lunaryorn-smartparens/hydra/body)
         :map smartparens-strict-mode-map
         ;; A fill paragraph in strict mode
         ("M-q" . sp-indent-defun))
  :commands (smartparens-mode
             smartparens-strict-mode)
  :diminish smartparens-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)
  :config
  (require 'smartparens-config)
  (setq sp-autoskip-closing-pair 'always
        ;; Don't kill entire symbol on C-k
        sp-hybrid-kill-entire-symbol nil)
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
    ("f" sp-forward-sexp)
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
    ("C-<right>" sp-backward-slurp-sexp)))

(use-package expand-region
  :straight t
  :bind (("C-c e" . er/expand-region)))

(use-package flymake
  :defer t
  :bind ("C-c ! !" . jyh-hydra-flymake/body)
  :config
  (defhydra jyh-hydra-flymake
    (:pre (progn (setq hydra-lv t) (flymake-show-diagnostics-buffer))
          :post (progn (setq hydra-lv nil)
                       (quit-windows-on
                        (format "*Flymake diagnostics for %s*" (buffer-name)))))
    "Errors"
    ("f"  "Filter")
    ("j"  flymake-goto-next-error       "Next")
    ("k"  flymake-goto-prev-error       "Previous")
    ("gg" (progn
            (goto-char (point-min))
            (flymake-goto-next-error))  "First"
            )
    ("G"  (progn
            (goto-char (point-max))
            (flymake-goto-prev-error))  "Last")
    ("q"  nil                           "quit")))


(use-package projectile                 ; project management
  :straight t
  :defer t
  :diminish projectile-mode
  :init
  (projectile-mode)
  (setq projectile-keymap-prefix nil)
  (jyh/bind-leader-prefix-map
   "p" projectile-command-map "projects")
  ;; uncomment this if smart-mode-line is off
  ;(setq projectile-mode-line
        ;'(:eval (format " P/%s" (projectile-project-name))))
  ;; otherwise too slow
  :config
  (setq projectile-enable-caching t))

(use-package ggtags                     ; symbol tags
  :straight t
  :defer t
  :disabled
  :init
  (hook-into-modes
   '(lambda () (ggtags-mode 1))
   'erlang-mode-hook
   'f90-mode-hook
   'java-mode-hook
   'lua-mode-hook
   'makefile-mode-hook))


;; ======================================
;;  Completion
;; ======================================

(use-package ivy                        ; completion backend
  :straight t
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

  (use-package ivy-hydra                ; sticky keybindings within ivy
    :straight t
    :defer t)
  (use-package swiper                   ; within-buffer searching
    :straight t
    :defer t)
  (use-package counsel
    :straight t
    :defer t)
  (use-package counsel-projectile
    :straight t
    :defer t
    :init (counsel-projectile-mode))

  ;; setup ivy-based global command keymap
  (general-def
   :prefix-map 'jyh/ivy-command-map
   :prefix-doc "Ivy/Counsel command keymap"
   "s" 'swiper
   "o" 'ivy-occur
   "i" 'counsel-imenu
   "k" 'counsel-ag
   "f" 'counsel-recentf
   "g" 'counsel-git
   "j" 'counsel-git-grep
   "l" 'counsel-locate
   "u" 'counsel-unicode-char)
  (jyh/bind-leader-prefix-map
   "i" jyh/ivy-command-map "ivy")

  ;; remap built-in functions
  (bind-keys
   ([remap execute-extended-command] . counsel-M-x)  ; M-x
   ([remap switch-to-buffer] . ivy-switch-buffer)    ; C-x b
   ([remap bookmark-jump] . counsel-bookmark)        ; C-x r b
   ([remap find-file] . counsel-find-file)           ; C-x C-f
   ([remap yank-pop] . counsel-yank-pop))            ; M-y

  (bind-keys
   :map company-mode-map
   ("C-:" . counsel-company)
   :map company-active-map
   ("C-:" . counsel-company)) )


;; ======================================
;;  External Utilities
;; ======================================

(use-package ag                         ; better grep search
  :straight t
  :defer t)
(use-package wgrep-ag                   ; modify files from grep/ag
  :straight t
  :defer t
  :commands wgrep-change-to-wgrep-mode)

(use-package spotify                    ; spotify controls
  :straight t
  :defer t
  :init
  (general-def
    :prefix-map 'jyh/spotify-command-map
   "y" 'spotify-playpause
   "p" 'spotify-previous
   "n" 'spotify-next
   "c" 'spotify-current)
  (jyh/bind-leader-prefix-map
   "y" jyh/spotify-command-map "spotify"))

(use-package maxima                     ; computer algebra system
  :disabled
  :defer t)
(use-package imaxima
  :disabled
  :defer t
  :after maxima)

(use-package emamux                     ; tmux integration
  :straight t
  :defer t
  :init
  (general-def
   :prefix-map 'jyh/emamux-keymap
   "s" 'emamux:send-command
   "y" 'emamux:yank-from-list-buffers
   "!" 'emamux:run-command
   "r" 'emamux:run-last-command
   "M-s" 'emamux:run-region
   "C-i" 'emamux:inspect-runner
   "C-k" 'emamux:close-panes
   "C-c" 'emamux:interrupt-runner
   "M-k" 'emamux:clear-runner-history
   "c" 'emamux:new-window
   "C" 'emamux:clone-current-frame
   "2" 'emamux:split-window
   "3" 'emamux:split-window-horizontally)
  (jyh/bind-leader-prefix-map
   "t" jyh/emamux-keymap "tmux"))

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
  :after restclient
  :init
  (jyh-company-for-mode 'restclient-mode-hook company-restclient))

(use-package crux                       ; bbatsov useful utilities
  :straight t
  :commands (crux-rename-file-and-buffer)
  :defer t)

;; constants
(use-package constants
  :straight t
  :defer t
  :init
  (general-def
    :prefix-map 'jyh/constants-keymap
    "i" 'constants-insert
    "g" 'constants-get
    "r" 'constants-replace)
  (jyh/bind-leader-prefix-map
   "c" jyh/constants-keymap "constants")
  (setq constants-unit-system 'SI))

(use-package command-log-mode
  :straight t
  :defer t)

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
