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

(use-package meow
  :straight t
  :disabled
  :custom
  (meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  :config
  (meow-motion-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore))
  (meow-global-mode 1))

(use-package evil
  :straight t
  :defer t
  :init
  (evil-mode 1)
  :config
  ;; stay in emacs mode, switch to vim mode with C-z
  (setq evil-default-state 'emacs)
  (defvar jyh/evil-emacs-state-modes
    '(edebug-mode
      flycheck-error-list-mode
      git-rebase-mode
      eshell-mode
      magit-blame-mode)
    "List of modes that start in Emacs state.")

  (dolist (mode jyh/evil-emacs-state-modes)
    (add-to-list 'evil-emacs-state-modes mode)))

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

(provide 'init-evil)
;;; init-evil.el ends here
