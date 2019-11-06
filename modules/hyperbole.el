;; There seems to be some loading issues, so reinstalling on start for now

;; (if (package-installed-p 'hyperbole)
;;     (package-reinstall 'hyperbole)
;;   (package-install 'hyperbole))

;; installed manually, so want to pin
;; (require 'hyperbole)n
(unless (package-installed-p 'hyperbole)
    (package-install 'hyperbole))

(require 'hyperbole)
