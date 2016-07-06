;;; init-display.el --- appearance/display configuration
;;
;; part of emacs config for jyamad. see init.el

;; use spacemacs display init features
;; provides (remaned) `jyh/do-after-display-system-init'
;; to run code only when a UI starts
(load-init-file "core-display-init")

(setq inhibit-startup-screen t)

(jyh/do-after-display-system-init
 ;; remove unnecessary UI elements
 (scroll-bar-mode -1)
 (tool-bar-mode   -1)

 ;; mode line
 (line-number-mode 1)                    ; show line-number in mode line
 (column-number-mode 1)                  ; show col-number  in mode line
 (use-package smart-mode-line
   :config
   (progn
     (setq sml/theme nil)                 ; zenburn theme provides theming
     (setq sml/no-confirm-load-theme t)
     (sml/setup)
     (diminish 'projectile-mode)))        ; sml provides its own projectile display

 ;; reduce appearance of gutters/margins/"fringes" to 'half-width
 (set-fringe-mode 4)

 ;; set syntax highlighting and default color scheme
 (use-package zenburn-theme
   :ensure t
   :config
   (load-theme 'zenburn t)
   (global-font-lock-mode)

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
        ((t (:box nil :background ,zenburn-bg-1))))))

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

 ;; hide menu bar in terminal mode
 (unless (display-graphic-p)
   (menu-bar-mode -1))

 ;; unobtrusive scroll bar, only in GUI mode-line
 (if (display-graphic-p)
     (use-package yascroll
       :config
       (global-yascroll-bar-mode 1)
       (setq yascroll:delay-to-hide nil)))

 ;; indicate desired line length (fill-column)
 (use-package fill-column-indicator
   :ensure t
   :diminish fci-mode
   :config
   (add-hook 'prog-mode-hook #'turn-on-fci-mode))
 )
