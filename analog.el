;;; analog.el --- Indicates whether ITU's student café is open.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Florian Biermann

;; Author: Florian Biermann <fbie@itu.dk>
;; Keywords: convenience, games
;; URL: http://github.com/fbie/analog-indicator
;; Version: 1.0

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

(defconst analog/open-url "http://cafeanalog.dk/api/open")
(defconst analog/shifts-url "http://cafeanalog.dk/api/shifts/today")

(defun analog/json-read ()
  "Read JSON from HTML response in current buffer."
  (save-excursion
    (goto-char url-http-end-of-headers)
    (json-read)))

(defun analog/open? ()
  "Query analog API to check whether it is open."
  (with-current-buffer (url-retrieve-synchronously analog/open-url)
    (cdr (assoc 'open (analog/json-read)))))

(defun analog-open? ()
  "Check whether Café Analog is open and display status in minibuffer."
  (interactive)
  (message "Café Analog is currently %s." (if (analog/open?) "open" "closed")))

(provide 'analog)
;;; analog.el ends here
