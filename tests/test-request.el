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

;; Quick snippets for interactive testing:
;;   (setq request-backend 'curl)
;;   (setq request-backend 'url-retrieve)
;;   (setq request-log-level 'blather)
;;   (setq request-log-level -1)



;;; GET

(request-deftest request-simple-get ()
  (let* ((result (request-testing-sync
                  (request-testing-url "report/some-path")
                  :parser 'request-parser-json))
         (response (plist-get result :response))
         (status-code (request-response-status-code response))
         (data (plist-get result :data)))
    (should (equal status-code 200))
    (should (equal (assoc-default 'path data) "some-path"))
    (should (equal (assoc-default 'method data) "GET"))))

(request-deftest request-redirection-get ()
  (let* ((result (request-testing-sync
                  (request-testing-url "redirect/redirect/report/some-path")
                  :parser 'request-parser-json))
         (response (plist-get result :response))
         (status-code (request-response-status-code response))
         (redirect (request-response-url response))
         (data (plist-get result :data))
         (path (assoc-default 'path data)))
    (if (and noninteractive (eq request-backend 'url-retrieve))
        ;; `url-retrieve' adds %0D to redirection path when the test
        ;; is run in noninteractive environment.
        ;; probably it's a bug in `url-retrieve'...
        (progn
          (string-match "^http://.*/report/some-path" redirect)
          (should (string-prefix-p "some-path" path)))
      (should (string-match "^http://.*/report/some-path$" redirect))
      (should (equal path "some-path")))
    (should (equal status-code 200))
    (should (equal (assoc-default 'method data) "GET"))))


;;; POST

(request-deftest request-simple-post ()
  (let* ((result (request-testing-sync
                  (request-testing-url "report/some-path")
                  :type "POST" :data "key=value"
                  :parser 'request-parser-json))
         (response (plist-get result :response))
         (status-code (request-response-status-code response))
         (data (plist-get result :data)))
    (should (equal status-code 200))
    (should (equal (assoc-default 'path data) "some-path"))
    (should (equal (assoc-default 'method data) "POST"))
    (should (equal (assoc-default 'form data) '((key . "value"))))))


;;; PUT

(request-deftest request-simple-put ()
  (let* ((result (request-testing-sync
                  (request-testing-url "report/some-path")
                  :type "PUT" :data "dummy-data"
                  :headers '(("Content-Type" . "text/plain"))
                  :parser 'request-parser-json))
         (response (plist-get result :response))
         (status-code (request-response-status-code response))
         (data (plist-get result :data)))
    (should (equal status-code 200))
    (should (equal (assoc-default 'path data) "some-path"))
    (should (equal (assoc-default 'method data) "PUT"))
    (should (equal (assoc-default 'data data) "dummy-data"))))

(request-deftest request-simple-put-json ()
  (let* ((result (request-testing-sync
                  (request-testing-url "report/some-path")
                  :type "PUT" :data "{\"a\": 1, \"b\": 2, \"c\": 3}"
                  :headers '(("Content-Type" . "application/json"))
                  :parser 'request-parser-json))
         (response (plist-get result :response))
         (status-code (request-response-status-code response))
         (data (plist-get result :data)))
    (should (equal status-code 200))
    (should (equal (assoc-default 'path data) "some-path"))
    (should (equal (assoc-default 'method data) "PUT"))
    (should (equal (request-testing-sort-alist (assoc-default 'json data))
                   '((a . 1) (b . 2) (c . 3))))))


;;; DELETE

(request-deftest request-simple-delete ()
  (let* ((result (request-testing-sync
                  (request-testing-url "report/some-path")
                  :type "DELETE"
                  :parser 'request-parser-json))
         (response (plist-get result :response))
         (status-code (request-response-status-code response))
         (data (plist-get result :data)))
    (should (equal status-code 200))
    (should (equal (assoc-default 'path data) "some-path"))
    (should (equal (assoc-default 'method data) "DELETE"))))


;;; `request-backend'-independent tests

;; Following tests does not depend on the value of `request-backend'.
;; Move them to another file when this test suite get bigger.

(ert-deftest request--curl-preprocess/no-redirects ()
  (with-temp-buffer
    (erase-buffer)
    (insert "\
HTTP/1.0 200 OK\r
Content-Type: application/json\r
Content-Length: 88\r
Server: Werkzeug/0.8.1 Python/2.7.2+\r
Date: Sat, 15 Dec 2012 23:04:26 GMT\r
\r
RESPONSE-BODY")
    (insert "\n(:num-redirects 0)")
    (let ((info (request--curl-preprocess)))
      (should (equal (buffer-string)
                     "\
HTTP/1.0 200 OK\r
Content-Type: application/json\r
Content-Length: 88\r
Server: Werkzeug/0.8.1 Python/2.7.2+\r
Date: Sat, 15 Dec 2012 23:04:26 GMT\r
\r
RESPONSE-BODY"))
      (should (equal info
                     (list :num-redirects 0
                           :redirect nil
                           :version "1.0" :code 200))))))

(ert-deftest request--curl-preprocess/two-redirects ()
  (with-temp-buffer
    (erase-buffer)
    (insert "\
HTTP/1.0 302 FOUND\r
Content-Type: text/html; charset=utf-8\r
Content-Length: 257\r
Location: http://example.com/redirect/a/b\r
Server: Werkzeug/0.8.1 Python/2.7.2+\r
Date: Sat, 15 Dec 2012 23:04:26 GMT\r
\r
HTTP/1.0 302 FOUND\r
Content-Type: text/html; charset=utf-8\r
Content-Length: 239\r
Location: http://example.com/a/b\r
Server: Werkzeug/0.8.1 Python/2.7.2+\r
Date: Sat, 15 Dec 2012 23:04:26 GMT\r
\r
HTTP/1.0 200 OK\r
Content-Type: application/json\r
Content-Length: 88\r
Server: Werkzeug/0.8.1 Python/2.7.2+\r
Date: Sat, 15 Dec 2012 23:04:26 GMT\r
\r
RESPONSE-BODY")
    (insert "\n(:num-redirects 2)")
    (let ((info (request--curl-preprocess)))
      (should (equal (buffer-string)
                     "\
HTTP/1.0 200 OK\r
Content-Type: application/json\r
Content-Length: 88\r
Server: Werkzeug/0.8.1 Python/2.7.2+\r
Date: Sat, 15 Dec 2012 23:04:26 GMT\r
\r
RESPONSE-BODY"))
      (should (equal info
                     (list :num-redirects 2
                           :redirect "http://example.com/a/b"
                           :version "1.0" :code 200))))))

(provide 'test-request)

;;; test-request.el ends here
