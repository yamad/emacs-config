;;; init-package.el --- required package load
;;
;; part of emacs config for jyamad. see init.el

(require 'package)
(setq package-enable-at-startup nil)

(defvar jyh/package-archives
  '(("melpa" . "http://melpa.org/packages/")
    ("melpa-stable" . "http://stable.melpa.org/packages/")
    ("org" . "http://orgmode.org/elpa/")))
(dolist (pa jyh/package-archives)
  (add-to-list 'package-archives pa))

(package-initialize)

;; bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

(use-package paradox
  :ensure t
  :defer 10
  :commands (paradox-upgrade-packages paradox-list-packages)
  :config
  (setq paradox-execute-asynchronously nil
        paradox-automatically-star nil
        paradox-spinner-type 'moon)
  (require 'init-package-private))

;; Required Packages
(defvar jyh-required-packages
  '(ctags
    exec-path-from-shell
    ghc
    helm-dash
    multi-web-mode
    pandoc-mode)
  "List of required packages to ensure are installed at launch")

(defun jyh-packages-installed-p (package-list)
  (let ((all-p t))
    (dolist (p package-list)
      (if (not (package-installed-p p))
	  (setq all-p nil)
	nil))
    all-p))

;; install missing packages
(if (not (jyh-packages-installed-p jyh-required-packages))
    (progn
      ;; check for new packages
      (message "%s" "Refreshing package database...")
      (package-refresh-contents)
      (message "%s" " done.")
      (dolist (p jyh-required-packages)
	(if (not (package-installed-p p))
	  (package-install p)))))

(provide 'init-package)

;;; init-package.el ends here