;; Package manager
(require 'package)

(defvar jyh/package-archives
  '(("melpa" . "http://melpa.org/packages/")
    ("melpa-stable" . "http://stable.melpa.org/packages/")
    ("marmalade" . "http://marmalade-repo.org/packages/")
    ("sc" . "http://joseito.republika.pl/sunrise-commander/")))
(setq package-archives (append package-archives jyh/package-archives))
(package-initialize)

;; bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t))

;; Required Packages
(defvar jyh-required-packages
  '(ag
    auctex
    bind-key
    cmake-mode
    company
    company-tern
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
    fill-column-indicator
    flycheck
    geiser
    ghc
    haskell-mode
    helm
    helm-ag
    helm-company
    helm-dash
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
    smart-mode-line
    smarter-compile
    tern
    use-package
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
