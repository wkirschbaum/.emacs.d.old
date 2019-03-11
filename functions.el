(defun kill-filename ()
  (interactive)
  (when (buffer-file-name)
    (kill-new (file-relative-name (buffer-file-name) (projectile-project-root)))))

(defun hex-region (start end)
  "Url encode the region between START and END in the buffer."
  (interactive "r")
  (func-region start end #'url-hexify-string))

(defun unhex-region (start end)
  "Url decode the region between START and END in the buffer."
  (interactive "r")
  (func-region start end #'url-unhex-string))

(defun erc-connect ()
  (interactive)
  (erc-tls :server "irc.freenode.net" :port 6697 :nick (read-string "Enter nick: ")))

;;; END
