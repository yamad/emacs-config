
;;; init-package.el --- package management
;;
;; part of emacs config for jyamad. see init.el

;;; Code:

;; don't load package.el
(setq package-enable-at-startup nil)

;; fix straight elpa recipe bug
;; https://github.com/raxod502/straight.el/issues/293
(setq straight-recipes-gnu-elpa-use-mirror t)

;; do shallow clones
(setq straight-vc-git-default-clone-depth 1)

;; bootstrap straight.el package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
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

(provide 'init-package)

;;; init-package.el ends here
