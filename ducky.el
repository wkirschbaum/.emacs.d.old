;;; ducky.el --- debugging help for software developers

;;; Copyright (c) 2018 Wilhelm Kirschbaum
;;; Keywords: games

;;; Commentary:

;;; The single entry point `ducky', allows a person to explain a problem to a rubber ducky.

;;; Code:

;;;###autoload
(defun ducky ()
  "Switch to *ducky* buffer and start debugging."
  (interactive)
  (switch-to-buffer "*ducky*")
  (ducky-mode))

(defvar ducky-mode-hook nil)
(defvar ducky-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\n" 'ducky-read-print)
    (define-key map "\r" 'ducky-read-print)
    map))

(define-derived-mode ducky-mode text-mode "Ducky"
  "Major mode for running the Ducky program.
Like Text mode with Auto Fill mode."
  (ducky-say "What are you struggling with? Explain your problem in as much detail as possible.")
  (run-mode-hooks 'ducky-mode-hook))
  
(defun ducky-read-print ()
  "Read ducky problem."
  (interactive)
  (insert "\n")
  (ducky-say "quick!"))

(defun ducky-say (x)
  (insert x)
  (insert "\n\n"))

(provide 'ducky)

;;; ducky.el ends here
