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
  :ensure nil)

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
  :straight t)

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
   '((file (styles basic partial-completion))))
  )

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
