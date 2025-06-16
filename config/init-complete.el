;;; init-complete.el -- auto-completion settings
;;
;; part of emacs config for jyamad. see init.el

;;; Commentary:
;; Configuration, functions and macros for setting up
;; completion. Currently using vertico/consult/marginalia.

;;; Code:
(use-package vertico    ; completion ui
  :straight t
  :custom
  (vertico-scroll-margin 0)
  (vertico-count 20)
  (vertico-resize t)
  (vertico-cycle t)
  :init
  (vertico-mode))
(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))
(use-package vertico-quick  ; avy-like keys in vertico
  :after vertico
  :ensure nil ; installed with vertico
  )
(use-package vertico-multiform
  :after vertico
  :ensure nil ; installed with vertico
  :config
  (add-to-list 'vertico-multiform-categories
               '(embark-keybinding grid))
  :init
  (vertico-multiform-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package marginalia   ; minibuffer/completion annotations
  :straight t
  :after vertico
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  (marginalia-annotators
   '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package consult
  :straight t
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)            ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab) ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)         ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer) ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop) ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g g" . consult-goto-line)   ;; orig. goto-line
         ("M-g M-g" . consult-goto-line) ;; orig. goto-line
         ([remap goto-line] . consult-goto-line)
         ("M-g o" . consult-outline)     ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find) ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)   ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
         ("M-s l" . consult-line) ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi) ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)      ;; orig. next-matching-history-element
         ("M-r" . consult-history))     ;; orig. previous-matching-history-element
  :hook
  ;; automatic preview at point in the *Completions* buffer
  (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
  :custom
  (consult-project-function (lambda (_) (projectile-project-root)))
  )

(use-package consult-flycheck
  :straight t
  :after consult
  :bind
  ("M-g f" . consult-flycheck))

(use-package corfu        ; completion popup
  :straight t
  :custom
  (corfu-cycle t)
  :init
  (global-corfu-mode))

(use-package orderless    ; completion style, like ivy
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion--category-overrides
   '((file (styles basic partial-completion)))))

(use-package embark
  :straight t
  :defer t
  :bind
  (("C-'" . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :custom
  (embark-prompter 'embark-keymap-prompter))

(use-package embark-consult
  :straight t
  :defer t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package hippie-exp    ; dabbrev enhacements, expansion/completion
  :straight t
  :defer t
  :bind (([remap dabbrev-expand] . hippie-expand))
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

(use-package yasnippet                  ; snippets/templates
  :straight t
  :defer 8
  :diminish yas-minor-mode
  :commands (yas/trigger-key yas/keymap)
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :straight t
  :after yasnippet)

(provide 'init-complete)

;;; init-complete.el ends here
