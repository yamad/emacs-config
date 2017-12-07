;;; init-package.el --- package management
;;
;; part of emacs config for jyamad. see init.el

(require 'package)
(setq package-enable-at-startup nil)

;; bootstrap straight.el package manager
(let ((bootstrap-file (concat user-emacs-directory "straight/bootstrap.el"))
      (bootstrap-version 2))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use straight as backend for use-package
(straight-use-package 'use-package)

;; (defvar jyh/package-archives
;;   '(("melpa" . "http://melpa.org/packages/")
;;     ("melpa-stable" . "http://stable.melpa.org/packages/")
;;     ("org" . "http://orgmode.org/elpa/")))
;; (dolist (pa jyh/package-archives)
;;   (add-to-list 'package-archives pa))

;;(package-initialize)

(setq use-package-verbose t)            ; for profiling

(provide 'init-package)

;;; init-package.el ends here
