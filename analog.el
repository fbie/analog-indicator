;;; analog.el --- Indicates whether ITU's student café is open.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Florian Biermann

;; Author: Florian Biermann <fbie@itu.dk>
;; Keywords: convenience, games
;; URL: http://github.com/fbie/analog-indicator
;; Version: 1.0
;; Package-Requires: ((emacs "24.4.") (delight "20160305.1551"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'json)
(require 'delight)

(defconst analog/open-url "http://cafeanalog.dk/api/open")
(defconst analog/shifts-url "http://cafeanalog.dk/api/shifts/today")

(defun analog/json-read ()
  "Read JSON from HTML response in current buffer."
  (save-excursion
    (goto-char url-http-end-of-headers)
    (let ((json-false nil))
      (json-read))))

(defun analog/json-get (key dict)
  "Retrieve value for KEY from DICT.  Use nil as false value."
  (cdr (assoc key dict)))

(defvar analog/debug nil "Set to t for debugging.")
(defvar analog/debug-open t "Open value for debugging.")

(defun analog/json-open (dict)
  "Check whether the member 'open in DICT is true."
  (if analog/debug
      analog/debug-open
    (analog/json-get 'open dict)))

(defun analog/open? ()
  "Query analog API to check whether it is open."
  (with-current-buffer (url-retrieve-synchronously analog/open-url)
    (analog/json-open (analog/json-read))))

(defun analog-open? ()
  "Check whether Café Analog is open and display status in minibuffer."
  (interactive)
  (message "Café Analog is currently %s." (if (analog/open?) "open" "closed")))

(defun analog/lighter (open)
  "Return a minor-mode lighter based on whether OPEN is non-nil."
  (if open " 🍵" ""))

(defconst analog/base-interval 600 "Base interval between checks, 10 minutes.")
(defvar analog/interval analog/base-interval "The current interval between checks, used for back-off.")

(defvar analog/timer nil "The timer that runs the code to connect to cafeanalog.dk.")

(defun analog/kill-timer ()
  "Kill the Café Analog timer."
  (unless (eq analog/timer nil)
    (cancel-timer analog/timer)))

(defun analog/register-timer (interval)
  "Register a timer for periodically checking Analog's opening status every INTERVAL seconds ."
  (when (eq analog/timer nil)
    (setq analog/timer (run-at-time 5
                                    interval
                                    'analog/open?-async))))

(defun analog/check-fail ()
  "Reset analog indicator and increase update interval by a factor of two."
  (analog/update nil)
  (message "Failed to connect to cafeanalog.dk")
  (setq analog/interval (* 2 analog/interval))
  (analog/kill-timer)
  (analog/register-timer analog/interval))

(defun analog/check-succeed ()
  "Update analog indicator value based on the result in the current buffer."
  (let ((open (analog/json-open (analog/json-read))))
    (delight 'analog-indicator-mode (analog/lighter open) 'emacs)
    (setq analog/interval analog/base-interval)))

(defun analog/open?-async ()
  "Asynchronously check whether Analog is open or kill the analog/timer if the mode is turned off."
  (message "Connecting to cafeanalog.dk...")
  (if analog-indicator-mode
      (url-retrieve analog/open-url
		            (lambda (status)
		              (if (assoc :error status)
		                  (analog/check-fail)
		                (analog/check-succeed)))
		            nil t t)
    (analog/kill-timer)))



;;;###autoload
(define-minor-mode analog-indicator-mode "Indicate whether ITU's Café Analog is open."
  :lighter (analog/lighter nil)
  :global t
  (analog/register-timer analog/interval))

(provide 'analog)
;;; analog.el ends here
