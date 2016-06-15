;; Package manager
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; Required Packages
(defvar jyh-required-packages
  '(ag
    auctex
    cmake-mode
    company
    ctags
    ctags-update
    diminish
    dropdown-list
    exec-path-from-shell
    edit-server
    ert
    ert-x
    ess
    ess-R-data-view
    flycheck
    geiser
    ghc
    haskell-mode
    helm
    helm-ag
    helm-company
    helm-flycheck
    helm-projectile
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
    smarter-compile
    tern
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
(if (not (jyh-packages-installed-p jyh-required-packages))
    (progn
      ;; check for new packages
      (message "%s" "Refreshing package database...")
      (package-refresh-contents)
      (message "%s" " done.")
      (dolist (p jyh-required-packages)
	(if (not (package-installed-p p))
	  (package-install p)))))
