;;; This should really all be in a use-package configuration

(setq org-agenda-files (file-expand-wildcards "~/Dropbox/Org/*.org"))
(setq org-directory "~/Dropbox/Org/")
(setq org-capture-templates
      '(("n" "Note" entry (file+headline "~/Dropbox/Org/notes.org" "Notes") "* %?\n%U\n%a")
        ("c" "Code" entry (file+headline "~/Dropbox/Org/codes.org" "Unsorted") "* %?\n  %a")
        ("a" "Appointment" entry (file+headline "~/Dropbox/Org/calendar.org" "Appointments") "* %?\n")
        ("t" "Todo" entry (file+headline "~/Dropbox/Org/todo.org" "Today") "* TODO %?")))

(setq org-todo-keywords
      '((sequence "TODO(t)" "DOING(b)" "|" "DONE(d)")))

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(add-hook 'org-mode-hook 'auto-fill-mode)

(setq org-log-done 'time)

(use-package org-journal
  :bind ("C-c C-j" . org-journal-new-entry)
  :config
  (setq org-journal-dir (concat org-directory "journal/")))

;; Force a new line when the text goes too far to the right
;; it uses the default fill column number
(add-hook 'org-mode-hook #'toggle-word-wrap)

;; Calendar and Diary
(setq calendar-view-diary-initially-flag t
      calendar-mark-diary-entries-flag t
      european-calendar-style 't
      diary-file "~/Dropbox/Org/diary")

(setq org-catch-invisible-edits 'error)
