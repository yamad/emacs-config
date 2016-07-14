;;; init-windows-os.el --- Windows OS config
;;
;; part of emacs config for jyamad. see init.el

;;; Code:
(when *is-windows-os*
  (progn
    ;; Printer
    (setenv "PRINTER" "PDFCreator")
    (setq ps-printer-name "PDFCreator")
    (setq ps-printer-name-option "-d")
    (setq ps-lpr-command "C:\\cygwin\\bin\\lpr.exe")

    ;; Add cygwin to path
    (setenv "PATH" (concat (getenv "PATH") ";C:\\cygwin\\bin"))

    ;; Add font
    (set-frame-font "Consolas-11")))

(provide 'init-windows-os)
;;; init-windows-os.el ends here
