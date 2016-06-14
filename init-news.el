;; News/gnus setup
(setq gnus-select-method '(nntp "news.eternal-september.org"))
(setq gnus-secondary-select-methods '((nntp "news.gmane.org")))
(setq user-full-name "Jason Yamada-Hanff")
(setq user-mail-address "jyamada1@gmail.com")

(setq gnus-face-0 'font-lock-type-face)
(setq gnus-face-1 'font-lock-string-face)
(setq gnus-face-2 'font-lock-variable-name-face)
(setq gnus-face-3 'font-lock-keyword-face)

(setq gnus-group-line-format "%2{%M%S%p%} %0{%5y%} %P%1{%G%}\n")
(setq gnus-topic-line-format "%i%3{[ %n -- %A ]%}%v\n")

(setq gnus-summary-line-format "%O%U%R%z%d %B%(%[%4L: %-22,22f%]%) %s\n")
(setq gnus-summary-mode-line-format "Gnus: %p [%A / Sc:%4z] %Z")
(setq gnus-summary-same-subject "")
(setq gnus-sum-thread-tree-root "")
(setq gnus-sum-thread-tree-single-indent "")
(setq gnus-sum-thread-tree-leaf-with-other "+-> ")
(setq gnus-sum-thread-tree-vertical "|")
(setq gnus-sum-thread-tree-single-leaf "`-> ")
