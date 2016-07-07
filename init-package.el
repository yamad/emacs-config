;;; init-package.el --- required package load
;;
;; part of emacs config for jyamad. see init.el

(require 'package)
(setq package-enable-at-startup nil)

(defvar jyh/package-archives
  '(("melpa" . "http://melpa.org/packages/")
    ("melpa-stable" . "http://stable.melpa.org/packages/")))
(setq package-archives (append package-archives jyh/package-archives))

(package-initialize)

;; bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package paradox
  :ensure t
  :defer 10
  :commands (paradox-upgrade-packages paradox-list-packages)
  :config
  (setq paradox-execute-asynchronously nil
        paradox-automatically-star nil
        paradox-spinner-type 'moon
        paradox-github-token "0f577656108e6ac7dea0da02c553b9fd9d4b7a63"))

;; Required Packages
(defvar jyh-required-packages
  '(ag
    anzu
    auctex
    bind-key
    cmake-mode
    company
    company-tern
    ctags
    ctags-update
    diminish
    dired+
    dropdown-list
    exec-path-from-shell
    edit-server
    ert
    ert-x
    ess
    ess-R-data-view
    fill-column-indicator
    flycheck
    geiser
    ghc
    haskell-mode
    helm
    helm-ag
    helm-company
    helm-dash
    helm-descbinds
    helm-flycheck
    helm-projectile
    helm-swoop
    highlight-parentheses
    js2-mode
    magit
    markdown-mode
    markdown-mode+
    multi-web-mode
    n3-mode
    lua-mode
    org
    pandoc-mode
    polymode
    projectile
    solarized-theme
    smart-mode-line
    smarter-compile
    tern
    use-package
    which-key
    yasnippet
    zenburn-theme)
  "List of required packages to ensure are installed at launch")

(defun jyh-packages-installed-p (package-list)
  (let ((all-p t))
    (dolist (p package-list)
      (if (not (package-installed-p p))
	  (setq all-p nil)
	nil))
    all-p))

;; install missing packages
;; (if (not (jyh-packages-installed-p jyh-required-packages))
;;     (progn
;;       ;; check for new packages
;;       (message "%s" "Refreshing package database...")
;;       (package-refresh-contents)
;;       (message "%s" " done.")
;;       (dolist (p jyh-required-packages)
;; 	(if (not (package-installed-p p))
;; 	  (package-install p)))))
