; Init --- Initial file for my emacs configuration

;;; Commentary:
;; This is my personal configuration.

;;; Code:


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

(require 'package)
(setq package-enable-at-startup nil)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(load "~/.emacs.d/functions.el")
(load "~/.emacs.d/garbage.el")

(load "~/.emacs.d/modules/core.el")
(load "~/.emacs.d/modules/org.el")
(load "~/.emacs.d/modules/ivy.el")
(load "~/.emacs.d/modules/dired.el")
(load "~/.emacs.d/modules/gnus.el")
(load "~/.emacs.d/modules/programming.el")
(load "~/.emacs.d/modules/git.el")
(load "~/.emacs.d/modules/candy.el")
(load "~/.emacs.d/modules/erc.el")
(load "~/.emacs.d/modules/extra.el")
(load "~/.emacs.d/modules/hyperbole.el")
(load "~/.emacs.d/modules/themes.el")

;;; init.el ends here
