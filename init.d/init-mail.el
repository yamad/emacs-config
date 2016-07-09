;;; init-mail.el --- email config
;;
;; part of emacs config for jyamad. see init.el

;; SMTP setup
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587
                                   "jyamada1@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

;; use mu/mu4e as a mail client
(use-package mu4e
  :defer t
  :config
  (setq mu4e-maildir "~/Maildir"
        mu4e-drafts-folder "/Gmail/[Gmail].Drafts"
        mu4e-sent-folder   "/Gmail/[Gmail].Sent_Mail"
        mu4e-trash-folder  "/Gmail/[Gmail].Trash"
        mu4e-refile-folder "/Gmail/Archive"
        mu4e-sent-messages-behavior 'delete
        mu4e-update-interval 300
        mu4e-headers-auto-update 't
        mu4e-user-mail-address-list '("jyamada1@gmail.com"
                                      "jasonyh@gmail.com"
                                      "jyamada@fas.harvard.edu"
                                      "jyamada@post.harvard.edu"
                                      "yamad@yamad.info"
                                      "jyh@jyh.me")
        user-full-name  "Jason Yamada-Hanff"
        mu4e-view-show-addresses 't
        mu4e-maildir-shortcuts
        '( ("/Gmail/INBOX" . ?i)
           ("/Gmail/[Gmail].Sent_Mail"  . ?s)
           ("/Gmail/[Gmail].Trash" . ?t)
           ("/Gmail/[Gmail].All Mail" . ?a)))
  (setq mu4e-get-mail-command "offlineimap -u basic")
  (setq mu4e-view-show-images 't)
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))
  (setq mu4e-view-prefer-html nil)
  (setq mu4e-html2text-command "html2text -nobs")
  (setq mail-user-agent 'mu4e-user-agent))

(provide 'init-mail)
