;;; init-markup.el --- markup syntax configuration
;;
;; part of emacs config for jyamad. see init.el

;;; Code:

(use-package rst
  :defer t
  :mode (("\\.rst$" . rst-mode)
         ("\\.rest$" . rst-mode)))
(use-package markdown-mode  :defer t)
(use-package markdown-mode+ :defer t)


;; screenplay format (Fountain)
(use-package fountain-mode
  :commands (fountain-export-tex-template)
  :defer t
  :config
  (assq-delete-all 'note fountain-export-tex-template)
  (add-to-list 'fountain-export-tex-template '(note "\\emph{{{content}}}\n\n")))
(use-package olivetti
  :ensure fountain-mode
  :defer t
  :config
  (add-hook 'fountain-mode-hook #'turn-on-olivetti-mode))

(use-package bison-mode
  :defer t
  :mode (("\\.yy?\\'" . bison-mode)
         ("\\.ll?\\'" . bison-mode)))

(use-package ebnf-mode
  :ensure nil
  :defer t
  :mode (("\\.bnf$" . ebnf-mode)
         ("\\.ebnf$" . ebnf-mode)))

(use-package cmake-mode
  :ensure t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

;; HTML/CSS
(use-package web-mode :ensure t)

;; nix package mangement
(use-package nix-mode :ensure t)
(use-package nix-buffer :ensure t)
(use-package nix-sandbox :ensure t)
(use-package nixos-options :ensure t)

;; XML
(eval-after-load 'rng-loc
  '(add-to-list 'rng-schema-locating-files
                "~/.schemas/nxml-schemas.xml"))
(use-package n3-mode
  :ensure t
  :mode (("\\.n3\\'" . n3-mode)
         ("\\.owl\\'" . n3-mode)))
(require 'rng-nxml)

;; ledger -- accounting program
(use-package ledger-mode
  :ensure t
  :config
  (set-face-attribute 'ledger-font-xact-highlight-face nil
                      :background "midnight blue")
  (set-face-attribute 'ledger-occur-xact-face nil
                      :background "midnight blue"))

(provide 'init-markup)
;;; init-markup.el ends here
