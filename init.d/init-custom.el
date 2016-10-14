;;; init-custom.el --- custom variables
;;
;; part of emacs config for jyamad. see init.el

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(canlock-password "17fbb2e43ad75165c6b69f83ed8c0a5f34d16270")
 '(custom-safe-themes
   (quote
    ("1db337246ebc9c083be0d728f8d20913a0f46edc0a00277746ba411c149d7fe5" "aae95fc700f9f7ff70efbc294fc7367376aa9456356ae36ec234751040ed9168" "40f6a7af0dfad67c0d4df2a1dd86175436d79fc69ea61614d668a635c2cd94ab" "cd70962b469931807533f5ab78293e901253f5eeb133a46c2965359f23bfb2ea" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" default)))
 '(ecb-options-version "2.40")
 '(fountain-export-include-elements-alist
   (quote
    (("treatment" section-heading scene-heading action character paren lines trans center synopsis note)
     ("screenplay" scene-heading action character paren lines trans center)
     ("stageplay" section-heading scene-heading action character paren lines trans center))))
 '(frame-background-mode (quote dark))
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (smart-compile undo-tree ghc haskell-mode lua-mode crux ivy avy yasnippet auctex company projectile flycheck markdown-mode cm-mode embrace zop-to-char zenburn-theme yascroll which-key web-mode visual-regexp visual-fill-column use-package unbound typing swift-mode sunrise-x-tree sunrise-x-tabs sunrise-x-popviewer sunrise-x-modeline sunrise-x-loop stylus-mode sr-speedbar spotify speed-type solarized-theme smooth-scrolling smartparens smarter-compile smart-tabs-mode smart-mode-line skewer-mode shm scss-mode rtags restclient-helm rainbow-mode polymode paradox pandoc-mode palimpsest palette org-plus-contrib org olivetti noflet neotree n3-mode multi-web-mode multi markdown-mode+ magit linum-relative ledger-mode js2-refactor js-comint interleave imenu-list icicles hindent highlight-parentheses helm-swoop helm-pydoc helm-projectile helm-mu helm-make helm-ls-git helm-hoogle helm-flycheck helm-descbinds helm-dash helm-company helm-ag helm-R go ggtags geiser fountain-mode flycheck-irony flycheck-flow floobits fill-column-indicator expand-region exec-path-from-shell evil-nerd-commenter evil esup ess-smart-equals ess-R-data-view ein edit-server dropdown-list distinguished-theme discover-my-major dired+ dash-at-point ctags-update ctags counsel company-tern company-restclient company-math company-lua company-ghci company-ghc company-cmake company-cabal company-c-headers company-auctex color-theme-github cmake-mode bookmark+ bison-mode avy-zap avy-menu auto-complete anzu ample-zen-theme ag ace-window ace-jump-helm-line)))
 '(rst-level-face-base-light 15)
 '(safe-local-variable-values
   (quote
    ((eval when
           (require
            (quote rainbow-mode)
            nil t)
           (rainbow-mode 1)))))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
