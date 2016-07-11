;;; init-news.el --- news/gnus setup
;;
;; part of emacs config for jyamad. see init.el

;;; Code:
(use-package gnus
  :config
  (setq gnus-select-method '(nntp "news.eternal-september.org")
        gnus-secondary-select-methods '((nntp "news.gmane.org"))
        user-full-name "Jason Yamada-Hanff"
        user-mail-address "jyamada1@gmail.com"
        gnus-face-0 'font-lock-type-face
        gnus-face-1 'font-lock-string-face
        gnus-face-2 'font-lock-variable-name-face
        gnus-face-3 'font-lock-keyword-face
        gnus-group-line-format "%2{%M%S%p%} %0{%5y%} %P%1{%G%}\n"
        gnus-topic-line-format "%i%3{[ %n -- %A ]%}%v\n"
        gnus-summary-line-format "%O%U%R%z%d %B%(%[%4L: %-22,22f%]% %s\n"
        gnus-summary-mode-line-format "Gnus: %p [%A / Sc:%4z] %Z"
        gnus-summary-same-subject ""
        gnus-sum-thread-tree-root ""
        gnus-sum-thread-tree-single-indent ""
        gnus-sum-thread-tree-leaf-with-other "+-> "
        gnus-sum-thread-tree-vertical "|"
        gnus-sum-thread-tree-single-leaf "`-> "))

(provide 'init-news)

;;; init-news.el ends here
