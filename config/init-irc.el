;;; init-irc.el --- IRC client configuration
;;
;; part of emacs config for jyamad. see init.el
;;

(use-package rcirc
  :defer t
  :config

  (defadvice rcirc (before rcirc-read-from-authinfo activate)
    "Allow rcirc to read authinfo from ~/.authinfo.gpg via the auth-source API.
This doesn't support the chanserv auth method"
    (unless arg
      (dolist (p (auth-source-search :port '("nickserv" "bitlbee" "quakenet")
                                     :require '(:port :user :secret)))
        (let ((secret (plist-get p :secret))
              (method (intern (plist-get p :port))))
          (add-to-list 'rcirc-authinfo
                       (list (plist-get p :host)
                             method
                             (plist-get p :user)
                             (if (functionp secret)
                                 (funcall secret)
                               secret)))))))

  (setq rcirc-default-nick "yamad"
        rcirc-default-user-name "jason yh"
        rcirc-default-full-name "Jason Yamada-Hanff"))

(provide 'init-irc)

;;; init-irc.el ends here
