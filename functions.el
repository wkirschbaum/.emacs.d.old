(defun init ()
  "Opens the init file"
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun whk/line-normal ()
  "Display line numbers as default."
  (interactive)
  (progn
    (setq display-line-numbers-type t)
    (setq display-line-numbers-current-absolute nil)
    (display-line-numbers-mode 1)))

(defun whk/line-rel ()
  "Display line numbers as relative."
  (interactive)
  (progn
    (setq display-line-numbers-type 'visual)
    (setq display-line-numbers-current-absolute t)
    (display-line-numbers-mode 1)))

(defun kill-filename ()
  "Kill the filename."
  (interactive)
  (when (buffer-file-name)
    (kill-new (file-relative-name (buffer-file-name) (projectile-project-root)))))

(defun func-region (start end func)
  "run a function over the region between START and END in current buffer."
  (save-excursion
    (let ((text (delete-and-extract-region start end)))
      (insert (funcall func text)))))

(defun hex-region (start end)
  "Url encode the region between START and END in the buffer."
  (interactive "r")
  (func-region start end #'url-hexify-string))

(defun unhex-region (start end)
  "Url decode the region between START and END in the buffer."
  (interactive "r")
  (func-region start end #'url-unhex-string))

(defun prs/kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x k") 'prs/kill-this-buffer)

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
    (animate-string "#CorporateLife"
                    (/ (window-height) 2) (- (/ (window-width) 2) 12))
    (sit-for 1)
    (message "You are now part of the corporate life at Prodigy Finance!")))

(defun restclient ()
  (interactive)
  (progn
    (switch-to-buffer (get-buffer-create "*restclient*"))
    (restclient-mode)))

;; end here
