;;; init-unicode.el --- unicode utilities
;;
;; part of emacs config for jyamad. see init.el

;;; Code:

;; Ensure UTF-8 as default buffer coding
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

(provide 'init-unicode)

;;; init-unicode.el ends here
