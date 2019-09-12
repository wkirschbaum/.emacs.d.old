; Init --- Initial file for my emacs configuration

;;; Commentary:
;; This is my personal configuration.

;;; Code:

(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p custom-file)
    (load custom-file))


(load "~/.emacs.d/encoding.el")
(load "~/.emacs.d/system.el")
(load "~/.emacs.d/packages.el")
(load "~/.emacs.d/functions.el")
(load "~/.emacs.d/gnus.el")
(load "~/.emacs.d/external/stickywindows.el")
(load "~/.emacs.d/external/confluence-ox.el")
(load "~/.emacs.d/themes.el")

;; exeperimental

(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
        mouse-wheel-progressive-speed nil))
(setq scroll-step 1
      scroll-margin 0
      scroll-conservatively 100000)


;;; init.el ends here
