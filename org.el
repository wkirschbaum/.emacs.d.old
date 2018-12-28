;;; This should really all be in a use-package configuration

(setq org-agenda-files (file-expand-wildcards "~/Dropbox/Org/*.org"))
(setq org-directory "~/Dropbox/Org/")
(setq org-capture-templates
      '(("n" "Note" entry (file+headline "~/Dropbox/Org/notes.org" "Notes") "* %?\n%U\n%a")
        ("h" "Hint" entry (file+headline "~/Dropbox/Org/hints.org" "Hints") "* %?\n  %U")
        ("t" "Todo" entry (file+headline "~/Dropbox/Org/notes.org" "Todos") "* TODO %?")))

(setq org-todo-keywords
      '((sequence "TODO(t)" "DOING(b)" "|" "DONE(d)")))

(defun open-my-notes ()
  "Opens my notes"
  (interactive)
  (find-file "~/Dropbox/Org/notes.org"))

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c n") 'open-my-notes)

(use-package org-journal
  :bind ("C-c C-j" . org-journal-new-entry)
  :config
  (setq org-journal-dir (concat org-directory "journal/")))

;; Force a new line when the text goes too far to the right
;; it uses the default fill column number
(add-hook 'org-mode #'(lambda () (auto-fill-mode)))
