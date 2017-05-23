;;; init-os-linux.el --- Linux-specific configuration
;;
;; part of emacs config for jyamad. see init.el

;;; Code:

;; Use chrome for urls on linux
(when *is-linux-os*
  (setq browse-url-browser-function 'browse-url-xdg-open))

(provide 'init-os-linux)
;;; init-os-linux.el ends here
