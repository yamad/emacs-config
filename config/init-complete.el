;;; init-complete.el -- auto-completion settings (company)
;;
;; part of emacs config for jyamad. see init.el

;;; Commentary:
;; Configuration, functions and macros for setting up
;; autocompletion. Currently using company.
;;
;; To define mode-specific backends, use:
;;
;;   (jyh-company-setup MODE BACKEND-1 BACKEND-2)

;;; Code:

(use-package company                    ; autocompletion
  :ensure t
  :straight t
  :diminish company-mode
  :bind (("M-RET" . company-complete))
;  :commands (company-capf company-dabbrev-code company-gtags company-etags company-keywords company-files company-dabbrev)
  :init
  (global-company-mode)
  (setq company-dabbrev-code-modes t
        company-dabbrev-code-everywhere t)
  (setq company-backends
        '(company-capf
          company-files
          (company-dabbrev-code
          company-gtags
          company-etags
          company-keywords)
          company-dabbrev)))

(defmacro jyh-company-for-mode (mode-hook &rest backends)
  "Install extra BACKENDS for mode using MODE-HOOK."
  (let ((backend-list company-backends))
     `(add-hook ,mode-hook
                (lambda ()
                  (set (make-local-variable 'company-backends)
                       ',(append backends backend-list))))))
(macroexpand '(jyh-company-for-mode 'python-mode-hook company-anaconda))

(use-package hippie-exp    ; dabbrev enhacements, expansion/completion
  :bind (([remap dabbrev-expand] . hippie-expand))
  :straight t
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

(use-package yasnippet                  ; snippets/templates
  :ensure t
  :straight t
  :diminish yas-minor-mode
  :commands (yas/trigger-key yas/keymap)
  :init
  (yas-global-mode 1))

(provide 'init-complete)

;;; init-complete.el ends here
