;;; init-display.el --- appearance/display configuration
;;
;; part of emacs config for jyamad. see init.el

;;; Commentary:
;; Configuration for appearance (themes, colors, etc) in GUI and
;; terminal frames

;;; Code:

;; use spacemacs display init features
;; provides (remaned) `jyh/do-after-display-system-init'
;; to run code only when a UI starts
(add-to-list 'load-path (locate-user-emacs-file "lisp"))
(require 'core-display-init)

(setq inhibit-startup-screen t)

(unless *is-mac-display*
  (menu-bar-mode   -1))

(setq linum-mode t)

(jyh/do-after-display-system-init
 ;; remove unnecessary UI elements
 (scroll-bar-mode -1)
 (tool-bar-mode   -1)

 ;; reduce gutters/margins/"fringes" to 'half-width
 (set-fringe-mode 4)

 ;; unobtrusive scroll bar, only in GUI mode-line
 (if (display-graphic-p)
     (use-package yascroll
       :config
       (global-yascroll-bar-mode 1)
       (setq yascroll:delay-to-hide nil)))

 ;; mode line
 (line-number-mode 1)                    ; show line-number in mode line
 (column-number-mode 1)                  ; show col-number  in mode line
 (use-package smart-mode-line
   :ensure t
   :config
   (progn
     (setq sml/theme nil)                 ; zenburn theme provides theming
     (setq sml/no-confirm-load-theme t)
     (sml/setup)
     (diminish 'projectile-mode))))        ; sml provides its own projectile display

;; set syntax highlighting and default color scheme
(use-package zenburn-theme
  :ensure t
  :init
  (load-theme 'zenburn t)
  (global-font-lock-mode)

  :config
  ;; tone down some default colors in zenburn
  (zenburn-with-color-variables
    (custom-set-faces
     `(linum
       ((t (:foreground ,zenburn-bg+2
            :background ,zenburn-bg))))
     `(fringe
       ((t (:background ,zenburn-bg))))
     ;; no box around modeline
     `(mode-line
       ((t (:box nil :background ,zenburn-bg-1))))
     `(mode-line-inactive
       ((t (:box nil :background ,zenburn-bg-1))))
     ;; ivy
     `(ivy-current-match
       ((t (:background ,zenburn-bg+1 :underline nil))))
     `(ivy-virtual
       ((t (:foreground ,zenburn-bg+2))))))

  (defun jyh/change-modeline-by-window-count ()
    "change modeline style based on number of windows"
    (if (eq (count-windows) 1)
        ;; tone down mode line if just one window
        (zenburn-with-color-variables
          (custom-set-faces
           `(mode-line ((t (:background ,zenburn-bg-1 :box nil))))))
      ;; make active mode line darker
      (zenburn-with-color-variables
        (custom-set-faces
         `(mode-line ((t (:background ,zenburn-bg-2 :box nil))))))))

  (add-hook 'window-configuration-change-hook
            #'jyh/change-modeline-by-window-count))

(use-package darktooth-theme
  :ensure t
  :disabled
  :init
  (load-theme 'darktooth t))

(provide 'init-display)

;;; init-display.el ends here
