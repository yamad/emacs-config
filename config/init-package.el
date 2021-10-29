
;;; init-package.el --- package management
;;
;; part of emacs config for jyamad. see init.el

;;; Code:

;; don't load package.el
(setq package-enable-at-startup nil)

;; fix straight elpa recipe bug
;; https://github.com/raxod502/straight.el/issues/293
(setq straight-recipes-gnu-elpa-use-mirror t)

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
(use-package diminish :straight t :defer t)
(use-package bind-key :straight t :defer t)
(setq straight-check-for-modifications 'live)

(setq use-package-verbose t)            ; for profiling

(defvar jyh/package-archives
  '(("melpa" . "http://melpa.org/packages/")
    ("gnu-elpa" . "https://elpa.gnu.org/packages/")))
;; add if not already in list
(when (boundp 'package-archives)
  (dolist (pa jyh/package-archives)
    (add-to-list 'package-archives pa)))

(provide 'init-package)

;;; init-package.el ends here
