(use-package erc
  :config
  (setq-default erc-echo-notices-in-minibuffer-flag t)
  (setq erc-rename-buffers t
        erc-lurker-hide-list '("JOIN" "QUIT")
        erc-lurker-threshold-time 3600
        erc-input-line-position -2))

(defun erc-connect ()
  (interactive)
  (erc-tls :server "irc.wilhelmbot.com" :port "1555" :nick "wilkir"))
