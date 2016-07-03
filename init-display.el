;;
;; init-display.el -- appearance/display specific config
;;
;; jyamad emacs config

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
   (setq sml/theme nil)                 ; zenburn theme provides theming
   (setq sml/no-confirm-load-theme t)
   (sml/setup))

 ;; indicate desired line length (fill-column)
 (use-package fill-column-indicator
   :diminish fci-mode
   :config (fci-mode))

 ;; reduce appearance of gutters/margins/"fringes"
 (setq fringe-mode 'half-width)

 ;; set syntax highlighting and default color scheme
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
    `(mode-line
      ((t (:box nil :background ,zenburn-bg-1))))
    `(mode-line-inactive
      ((t (:box nil :background ,zenburn-bg-1))))))

 (defun jyh/change-modeline-by-window-count ()
   (if (eq (count-windows) 1)
       ;; tone down mode line if just one window
         (zenburn-with-color-variables
           (custom-set-faces
            `(mode-line ((t (:background ,zenburn-bg-1 :box nil))))))
     ;; differentiate active mode line
     (zenburn-with-color-variables
       (custom-set-faces
        `(mode-line ((t (:background ,zenburn-bg-2 :box nil))))))))
 (add-hook 'window-configuration-change-hook
           #'jyh/change-modeline-by-window-count)

 ;; stuff to run only in GUI mode
 (unless (display-graphic-p)
   (menu-bar-mode -1)

   ;; unobtrusive scroll bar
   (use-package yascroll
     :config
     (global-yascroll-bar-mode 1)
     (setq yascroll:delay-to-hide nil)))
 )
