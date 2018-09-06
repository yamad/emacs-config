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

(defun contextual-menubar (&optional frame)
  "Display the menubar if FRAME is on a graphical display. Hide
menubar otherwise. from
https://stackoverflow.com/a/24958242/192780"
  (interactive)
  (set-frame-parameter frame 'menu-bar-lines
                       (if (display-graphic-p frame) 1 0)))
(add-hook 'after-make-frame-functions 'contextual-menubar)
(add-hook 'after-init-hook 'contextual-menubar)

(setq global-display-line-numbers t)

(jyh/do-after-display-system-init
 ;; remove unnecessary UI elements
 (scroll-bar-mode -1)
 (tool-bar-mode   -1)

 ;; reduce gutters/margins/"fringes" to 'half-width
 (set-fringe-mode 4)

 ;; mode line
 (line-number-mode 1)                    ; show line-number in mode line
 (column-number-mode 1)                  ; show col-number  in mode line
 (use-package smart-mode-line
   :straight t
   :config
   (progn
     (setq sml/theme nil)                 ; zenburn theme provides theming
     (setq sml/no-confirm-load-theme t)
     (sml/setup)
     (diminish 'projectile-mode))))        ; sml provides its own projectile display

;; set syntax highlighting and default color scheme
(use-package zenburn-theme
  :straight t
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
     `(line-number
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

(use-package color-theme-sanityinc-tomorrow
  :straight t
  :disabled
  :config
  (load-theme 'sanityinc-tomorrow-night t))

(provide 'init-display)

;;; init-display.el ends here
