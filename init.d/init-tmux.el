;;; init-tmux.el --
;;
;; part of emacs config for jyamad. see init.el

;;; Commentary:
;;

;;; Code:

;; allows xterm-keys to work from within tmux.
;; from tmux FAQ: https://raw.githubusercontent.com/tmux/tmux/master/FAQ
(defadvice terminal-init-screen
  ;; The advice is named `tmux', and is run before `terminal-init-screen' runs.
  (before tmux activate)
  ;; Docstring.  This describes the advice and is made available inside emacs;
  ;; for example when doing C-h f terminal-init-screen RET
  "Apply xterm keymap, allowing use of keys passed through tmux."
  ;; This is the elisp code that is run before `terminal-init-screen'.
  (if (getenv "TMUX")
    (let ((map (copy-keymap xterm-function-map)))
    (set-keymap-parent map (keymap-parent input-decode-map))
    (set-keymap-parent input-decode-map map))))

(provide 'init-tmux)
;;; init-tmux.el ends here
