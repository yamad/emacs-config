;;; init-tex.el --- TeX package configuration
;;
;; part of emacs config for jyamad. see init.el

;; general options for all TeX-based modes
(use-package auctex :ensure t :defer t)

(use-package tex-site
  :defer t
  :ensure auctex)

(use-package tex
  :ensure auctex
  :defer t
  :config
  (setq TeX-parse-self t                   ; provide completion
        TeX-auto-save t                    ; save style information
        TeX-electric-sub-and-superscript t ; insert braces after sub/superscripts
        TeX-clean-confirm nil              ; don't confirm before cleaning
        TeX-quote-after-quote t
        TeX-open-quote "``"
        TeX-close-quote "''")
  (setq-default TeX-engine 'xetex
                TeX-master nil
                TeX-PDF-mode t)
  (add-hook 'TeX-mode-hook #'(lambda ()
                               (TeX-fold-mode 1)
                               (outline-minor-mode 1))))

(use-package tex-mik
  :ensure auctex
  :if (eq system-type 'windows-nt)
  :config
  (setq preview-image-type 'pnm))

(use-package reftex
  :defer t
  :init (hook-into-modes #'reftex-mode
                         'TeX-mode-hook
                         'LaTeX-mode-hook)
  :config
  (setq reftex-plug-into-AUCTeX t)

  ;; support biblatex in reftex
  (unless (assq 'biblatex reftex-cite-format-builtin)
    (add-to-list
     'reftex-cite-format-builtin
     '(biblatex "The biblatex package"
                ((?\C-m . "\\cite[]{%l}")
                 (?t . "\\textcite{%l}")
                 (?a . "\\autocite[]{%l}")
                 (?p . "\\parencite{%l}")
                 (?f . "\\footcite[][]{%l}")
                 (?F . "\\fullcite[]{%l}")
                 (?x . "[]{%l}")
                 (?X . "{%l}")))))
  (setq reftex-cite-format 'biblatex)
  :diminish reftex-mode)

(use-package bibtex
  :defer t
  :config
  (bibtex-set-dialect 'biblatex))


;; dialect-specific options

(use-package latex
  :ensure auctex
  :config
  (defun jyh/setup-LaTeX ()
    (defun LaTeX-word-count ()
    (interactive)
    (let* ((this-file (buffer-file-name))
           (word-count
            (with-output-to-string
              (with-current-buffer standard-output
                (call-process "texcount" nil t nil "-brief" this-file)))))
      (string-match "\n$" word-count)
      (message (replace-match "" nil nil word-count)))))

  (add-hook 'LaTeX-mode-hook  'auto-fill-mode)
  (add-hook 'LaTeX-mode-hook  'flyspell-mode)
  (add-hook 'LaTeX-mode-hook #'jyh/setup-LaTeX)
  ; math abbrevs (` as prefix char) useful in all TeX files
  (hook-into-modes #'LaTeX-math-mode
                   'TeX-mode-hook
                   'LaTeX-mode-hook
                   'ConTeXt-mode-hook))

(use-package context
  :ensure auctex
  :defer t
  :config
  ;; some configuration from ConTeXt wiki
  ;; When start-context-math() is bound
  ;; Typing one $ gets you $$ with the insertion point between them.
  ;; Typing a second $ turns the $$ into ConTeXt's form for displayed math:
  ;;
  ;;   \placeformula\startformula
  ;;   [blank line with insertion point at beginning]
  ;;   \stopformula
  ;;
  ;; Delete the \placeformula if you don't want equations numbered automatically.
  (defun start-context-math ()
    (defun insert-balanced (left right)
      "Insert a left, right delmiter pair and be poised to type inside them."
      (interactive)
      (insert left)
      (save-excursion
        (insert right)))

    (interactive)
    (let* ((start (max (point-min) (- (point) 1)))
           (stop  (min (point-max) (+ (point) 1))))
      ;; if in the middle of a $$, turn inline math into context display math
      (if (equal "$$" (buffer-substring-no-properties start stop))
          (progn
            (delete-region start stop)  ; get rid of the $$
            ;; delete preceding spaces, if any
            (while (and (< (point-min) (point))
                        (equal (buffer-substring-no-properties (- (point) 1)
                                                               (point))
                               " "))
              (backward-delete-char 1))
            ;; delete a preceding newline, if any
            (if (equal (buffer-substring-no-properties (- (point) 1)
                                                       (point))
                       "\n")
                (backward-delete-char 1))
            ;; ConTeXt's display math with automatic equation numbering
            (insert "\n\\placeformula\\startformula\n")
            (save-excursion (insert "\n\\stopformula")))
        ;; else: just doing inline math
        (insert-balanced ?\$ ?\$))))

  ;; For ConTeXt mode, inserting two $ signs needs to behave specially
  (add-hook 'ConTeXt-mode-hook
            '(lambda ()
               (local-set-key "$" 'start-context-math))))

(provide 'init-tex)

;;; init-tex.el ends here
