;; https://www.gnu.org/software/emacs/manual/html_node/gnus/FAQ-3_002d8.html

(setq gnus-select-method '(nnml ""))

(setq user-mail-address "wkirschbaum@gmail.com")
(setq user-full-name "Wilhelm Hugo Kirschbaum")

(setq gnus-secondary-select-methods
      '((nnmaildir "gmail"
                   (directory "~/.mail/gmail/"))
        (nnmaildir "pdg"
                   (directory "~/.mail/pdg/"))))


(setq-default smtpmail-smtp-server "smtp.gmail.com"
              smtpmail-smtp-service 587
              send-mail-function 'smtpmail-send-it)
