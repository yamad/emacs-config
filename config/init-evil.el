;;; init-evil.el -- evil-mode (vim emulation) config
;;
;; part of emacs config for jyamad. see init.el

;;; Commentary:
;;
;; Configuration for evil-mode to get that operator + motion = fastness
;; references:
;;  * https://github.com/aaronbieber/dotfiles/tree/master/configs/emacs.d
;;  * https://blog.aaronbieber.com/2015/05/24/from-vim-to-emacs-in-fourteen-days.html
;;  * http://bling.github.io/blog/2015/01/06/emacs-as-my-leader-1-year-later
;;  * https://github.com/bling/dotemacs

;;; Code:

(use-package evil
  :straight t
  :init
  (evil-mode 1)
  :config
  ;; stay in emacs mode, switch to vim mode with C-z
  (setq evil-default-state 'emacs))

(use-package evil-surround
  :straight t
  :after evil
  :defer t
  :init
  (global-evil-surround-mode 1))

(use-package evil-exchange
  :straight t
  :after evil
  :defer t
  :init
  (evil-exchange-install))

(use-package evil-avy
  :straight t
  :after evil
  :defer t)

(use-package evil-commentary
  :straight t
  :after evil
  :defer t
  :diminish evil-commentary-mode
  :init
  (evil-commentary-mode t))

(use-package evil-visualstar
  :straight t
  :after evil
  :defer t
  :init
  (global-evil-visualstar-mode t))

(use-package evil-numbers
  :straight t
  :after evil
  :defer t)

;; map extra esc keys in insert mode - jk and qq
(use-package key-chord
  :straight t
  :after evil
  :defer t
  :init
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (key-chord-define evil-insert-state-map "QQ" 'evil-normal-state)
  (key-chord-define evil-visual-state-map "jk" 'evil-normal-state)
  (key-chord-define evil-visual-state-map "QQ" 'evil-normal-state))


(defvar jyh/evil-emacs-state-modes
  '(edebug-mode
   git-commit-mode
   flycheck-error-list-mode
   git-rebase-mode
   eshell-mode
   magit-blame-mode)
  "List of modes that start in Emacs state.")

(dolist (mode jyh/evil-emacs-state-modes)
  (add-to-list 'evil-emacs-state-modes mode))


(provide 'init-evil)
;;; init-evil.el ends here
