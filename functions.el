(defun kill-filename ()
  (interactive)
  (when (buffer-file-name)
    (kill-new (file-relative-name (buffer-file-name) (projectile-project-root)))))
