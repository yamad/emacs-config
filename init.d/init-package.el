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

(provide 'init-package)

;;; init-package.el ends here
