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
  (defun maxima-version ()
    (car
     (last (directory-files "/usr/local/Cellar/maxima" nil "[0-9\.]+" nil))))
  (defun maxima-emacs-path ()
    (concat "/usr/local/Cellar/maxima/" (maxima-version)
            "/share/maxima/" (maxima-version) "/emacs"))
  (add-to-list 'load-path (maxima-emacs-path)))

(provide 'init-os-mac)
;;; init-os-mac.el ends here
