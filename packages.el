;;; Packages -- My packages

;;; Commentary:
;; This is my personal configuration.

;;; Code:


(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(load "~/.emacs.d/packages/org.el")
(load "~/.emacs.d/packages/ivy.el")
(load "~/.emacs.d/packages/gnus.el")
(load "~/.emacs.d/packages/extra.el")
(load "~/.emacs.d/packages/themes.el")

;; ;;; packages.el ends here
