;;; init-os-mac.el --- Mac OS X specifc config
;;
;; part of emacs config for jyamad. see init.el

;;; Code:

;; Apple Dev
(when *is-mac-os*
  (use-package xcode-mode
    :straight t
    :defer t
    :init
    (setq xcode-completing-read-function 'ivy-completing-read)))

(when *is-mac-os*
  ;; read gpg-agent environment
  (defun read-env-line (line)
    "read a env line and post to environment"
    (let ((key-value-pair (split-string line "=" t)))
      (setenv (car key-value-pair) (car (last key-value-pair))))
    )
  (defvar gpg-agent-info-file)
  (setq gpg-agent-info-file (concat (getenv "HOME") "/.gpg-agent-info"))
  (when
      (file-exists-p gpg-agent-info-file)
    (with-temp-buffer
      (progn
        (insert-file-contents gpg-agent-info-file)
        (mapc 'read-env-line (split-string (buffer-string) "\n" t)))
      )))

;; required for compatibility with yabai window manager
(when *is-mac-display*
  (menu-bar-mode t))

(when *is-mac-os*
  (setq mac-option-modifier 'meta))

(provide 'init-os-mac)
;;; init-os-mac.el ends here
