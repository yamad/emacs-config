;;; init-unicode.el --- unicode utilities
;;
;; part of emacs config for jyamad. see init.el

;; Set UTF-8 as default buffer coding
(require 'un-define "un-define" t)
(set-buffer-file-coding-system 'utf-8 'utf-8)
(set-default buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)
(set-default buffer-file-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-language-environment "UTF-8")

;; Set UTF-8 keys
(global-set-key "\C-z" nil)
(global-set-key "\C-z\C-q" 'unicode-insert)

(global-set-key "\C-zoo" "°") ;; degree
(global-set-key "\C-z+-" "±") ;; plus-minus
(global-set-key "\C-z.." "·") ;; center bullet
(global-set-key "\C-z<=" "≤") ;; less than
(global-set-key "\C-z>=" "≥") ;; greater than
(global-set-key "\C-z!=" "≠") ;; not equal
(global-set-key "\C-za" "α") ;; alpha
(global-set-key "\C-zb" "β") ;; beta
(global-set-key "\C-zg" "γ") ;; gamma
(global-set-key "\C-zd" "δ") ;; delta
(global-set-key "\C-zep" "ε") ;; epsilon
(global-set-key "\C-zz" "ζ") ;; zeta
(global-set-key "\C-zet" "η") ;; eta
(global-set-key "\C-zth" "θ") ;; theta
(global-set-key "\C-zi" "ι") ;; iota
(global-set-key "\C-zk" "κ") ;; kappa
(global-set-key "\C-zl" "λ") ;; lambda
(global-set-key "\C-zm" "μ") ;; mu
(global-set-key "\C-zn" "ν") ;; nu
(global-set-key "\C-zx" "ξ") ;; xi
(global-set-key "\C-zoc" "ο") ;; omicron
(global-set-key "\C-zpi" "π") ;; pi
(global-set-key "\C-zr" "ρ") ;; rho
(global-set-key "\C-zs" "σ") ;; sigma
(global-set-key "\C-zt" "τ") ;; tau
(global-set-key "\C-zu" "υ") ;; upsilon
(global-set-key "\C-zph" "φ") ;; phi
(global-set-key "\C-zc" "χ") ;; chi
(global-set-key "\C-zps" "ψ") ;; psi
(global-set-key "\C-zom" "ω") ;; omega

(global-set-key "\C-zA" "Α") ;; Alpha
(global-set-key "\C-zB" "Β") ;; Beta
(global-set-key "\C-zG" "Γ") ;; Gamma
(global-set-key "\C-zD" "Δ") ;; Delta
(global-set-key "\C-zEp" "Ε") ;; Epsilon
(global-set-key "\C-zZ" "Ζ") ;; Zeta
(global-set-key "\C-zEt" "Η") ;; Eta
(global-set-key "\C-zTh" "Θ") ;; Theta
(global-set-key "\C-zI" "Ι") ;; Iota
(global-set-key "\C-zK" "Κ") ;; Kappa
(global-set-key "\C-zL" "Λ") ;; Lambda
(global-set-key "\C-zM" "Μ") ;; Mu
(global-set-key "\C-zN" "Ν") ;; Nu
(global-set-key "\C-zX" "Ξ") ;; Xi
(global-set-key "\C-zOc" "Ο") ;; Omicron
(global-set-key "\C-zPi" "Π") ;; Pi
(global-set-key "\C-zR" "Ρ") ;; Rho
(global-set-key "\C-zS" "Σ") ;; Sigma
(global-set-key "\C-zTa" "Τ") ;; Tau
(global-set-key "\C-zU" "Υ") ;; Upsilon
(global-set-key "\C-zPh" "Φ") ;; Phi
(global-set-key "\C-zC" "Χ") ;; Chi
(global-set-key "\C-zPs" "Ψ") ;; Psi
(global-set-key "\C-zOm" "Ω") ;; Omega

; Set auto-replace
(define-abbrev-table 'global-abbrev-table '(
  ("alpha" "α" nil 0)
  ("beta" "β" nil 0)
  ("gamma" "γ" nil 0)
  ("delta" "δ" nil 0)
  ("epsilon" "ε" nil 0)
  ("zeta" "ζ" nil 0)
  ("eta" "η" nil 0)
  ("theta" "θ" nil 0)
  ("iota" "ι" nil 0)
  ("kappa" "κ" nil 0)
  ("lambda" "λ" nil 0)
  ("mu" "μ" nil 0)
  ("nu" "ν" nil 0)
  ("xi" "ξ" nil 0)
  ("omicron" "ο" nil 0)
  ("pi" "π" nil 0)
  ("rho" "ρ" nil 0)
  ("sigma" "σ" nil 0)
  ("tau" "τ" nil 0)
  ("upsilon" "υ" nil 0)
  ("phi" "φ" nil 0)
  ("chi" "χ" nil 0)
  ("psi" "ψ" nil 0)
  ("omega" "ω" nil 0)
  ("Alpha" "Α" nil 0)
  ("Beta" "Β" nil 0)
  ("Gamma" "Γ" nil 0)
  ("Delta" "Δ" nil 0)
  ("Epsilon" "Ε" nil 0)
  ("Zeta" "Ζ" nil 0)
  ("Eta" "Η" nil 0)
  ("Theta" "Θ" nil 0)
  ("Iota" "Ι" nil 0)
  ("Kappa" "Κ" nil 0)
  ("Lambda" "Λ" nil 0)
  ("Mu" "Μ" nil 0)
  ("Nu" "Ν" nil 0)
  ("Xi" "Ξ" nil 0)
  ("Omicron" "Ο" nil 0)
  ("Pi" "Π" nil 0)
  ("Rho" "Ρ" nil 0)
  ("Sigma" "Σ" nil 0)
  ("Tau" "Τ" nil 0)
  ("Upsilon" "Υ" nil 0)
  ("Phi" "Φ" nil 0)
  ("Chi" "Χ" nil 0)
  ("Psi" "Ψ" nil 0)
  ("Omega" "Ω" nil 0)
  ("degC" "°" nil 0)
  ("degreeC" "°" nil 0)
))
