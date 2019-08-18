;; https://www.gnu.org/software/emacs/manual/html_node/gnus/FAQ-3_002d8.html

(setq gnus-select-method '(nnml ""))

(setq user-mail-address "wkirschbaum@gmail.com")
(setq user-full-name "Wilhelm Hugo Kirschbaum")

(with-eval-after-load "mail-source"
       (add-to-list 'mail-sources '((maildir :path "/home/wilhelm/Maildir/"
                                            :subdirs ("cur" "new")))))

(setq send-mail-function 'smtpmail-send-it)
(setq message-send-mail-function 'smtpmail-send-it)
(setq smtpmail-default-smtp-server "localhost")
