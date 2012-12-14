;;; test-request.el --- Tests for request.el

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; test-request.el is free software: you can redistribute it
;; and/or modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; test-request.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with test-request.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'request-testing)

(let ((level (getenv "EL_REQUEST_MESSAGE_LEVEL")))
  (when (and level (not (equal level "")))
    (setq request-message-level (intern level))))
(setq request-log-level request-message-level)

(let ((backend (getenv "EL_REQUEST_BACKEND")))
  (when (and backend (not (equal backend "")))
    (setq request-backend (intern backend))
    (message "Using request-backend = %S" request-backend)))


(request-deftest request-simple-get ()
  (let* ((result (request-testing-sync
                  (request-testing-url "report/some-path")
                  :parser 'request-parser-json))
         (response-status (plist-get result :response-status))
         (data (plist-get result :data)))
    (should (equal response-status 200))
    (should (equal (assoc-default 'path data) "some-path"))
    (should (equal (assoc-default 'method data) "GET"))))

(request-deftest request-simple-post ()
  (let* ((result (request-testing-sync
                  (request-testing-url "report/some-path")
                  :type "POST" :data "key=value"
                  :parser 'request-parser-json))
         (response-status (plist-get result :response-status))
         (data (plist-get result :data)))
    (should (equal response-status 200))
    (should (equal (assoc-default 'path data) "some-path"))
    (should (equal (assoc-default 'method data) "POST"))
    (should (equal (assoc-default 'params data) '((key . "value"))))))

(request-deftest request-simple-put ()
  (let* ((result (request-testing-sync
                  (request-testing-url "report/some-path")
                  :type "PUT" :data "dummy-data"
                  :parser 'request-parser-json))
         (response-status (plist-get result :response-status))
         (data (plist-get result :data)))
    (should (equal response-status 200))
    (should (equal (assoc-default 'path data) "some-path"))
    (should (equal (assoc-default 'method data) "PUT"))
    (should (equal (assoc-default 'input data) "dummy-data"))))

(provide 'test-request)

;;; test-request.el ends here
