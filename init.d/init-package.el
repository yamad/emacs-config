;;; init-package.el --- package management
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
(require 'diminish)
(require 'bind-key)
;(setq use-package-always-ensure t)
(setq use-package-verbose t)            ; for profiling

(use-package paradox
  :ensure t
  :defer 10
  :init
  (setq paradox-execute-asynchronously nil
        paradox-automatically-star nil
        paradox-spinner-type 'moon)
  :config
  (require 'init-package-private))

(provide 'init-package)

;;; init-package.el ends here
