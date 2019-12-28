;; -*- lexical-binding: t -*-

;; Reference: https://psachin.gitlab.io/emacs_diary_desktop_notification.html

(require 'notifications)
(defcustom appt-notification-bus :session
  "D-Bus bus to use for notification."
  :version "26.3"
  :group `appt-notification
  :type '(choice (const :tag "Session bus" :session) string))

(defun whk/appt-display (msg)
  "Send notification."
  (notifications-notify :bus appt-notification-bus
                        :title "Emacs"
                        :body (format "%s" msg)
                        :replaces-id nil
                        :app-icon nil
                        :timeout 5000
                        :desktop-entry "emacs"))

(setq appt-disp-window-function (function whk/appt-display))

(defun whk/test-notifications ()
  (interactive)
  (whk/appt-display "This is a notification test"))

(defun whk/notify-visual (message)
  (progn
    (dotimes (num 10)
      (run-with-timer (* num 0.1) nil #'invert-face 'default))
    (whk/appt-display message)))

(defun whk/notify ()
  (interactive)
  (let ((time (read-string "Enter time: "))
        (message (read-string "Message: ")))
    (run-at-time time nil (lambda () (whk/notify-visual message)))))
