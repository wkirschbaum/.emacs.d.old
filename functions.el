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
  (erc-tls :server "irc.wilhelmbot.com" :port "1555" :nick "peirama"))


(defun prs/kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

;; (global-set-key (kbd "C-x k") 'prs/kill-this-buffer)

(defun whk/get-url-content (url)
  "Make a get request to the URL and return the body."
  (with-current-buffer
      (url-retrieve-synchronously url)
    (prog2
        (let ((end-of-header (progn
                             (goto-char (point-min))
                             (re-search-forward "^$")
                             (point))))
             (delete-region (point-min) end-of-header))
        (buffer-string)
      (kill-buffer))))

(defun whk/write-url-content ()
  "Yank the content of a URL in the buffer."
  (interactive)
  (insert
   (whk/get-url-content (read-string "url: "))))

(defun corporatelife ()
  (interactive)
  (progn
    (switch-to-buffer (get-buffer-create "*#corporate life*"))
    (erase-buffer)
    (sit-for 0)
    (animate-string "#corporatelife"
                    (/ (window-height) 2) (- (/ (window-width) 2) 12))
    (sit-for 1)
    (message "You are now part of the corporate life at Prodigy Finance!")))
;; end here
