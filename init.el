; Init --- Initial file for my emacs configuration

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

(load "~/.emacs.d/functions.el")
(load "~/.emacs.d/garbage.el")

(load "~/.emacs.d/modules/core.el")
(load "~/.emacs.d/modules/org.el")
(load "~/.emacs.d/modules/ivy.el")
(load "~/.emacs.d/modules/gnus.el")
(load "~/.emacs.d/modules/programming.el")
(load "~/.emacs.d/modules/git.el")
(load "~/.emacs.d/modules/candy.el")
(load "~/.emacs.d/modules/erc.el")
(load "~/.emacs.d/modules/extra.el")
(load "~/.emacs.d/modules/themes.el")

;;; init.el ends here
(put 'narrow-to-region 'disabled nil)
