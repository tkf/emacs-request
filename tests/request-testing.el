;;; request-testing.el --- Testing framework for request.el

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; request-testing.el is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; request-testing.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with request-testing.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'ert)
(require 'request-deferred)

(defvar request-testing-source-dir
  (file-name-directory (or load-file-name (buffer-file-name))))

(defvar request-testing-timeout 3000)

(defvar request-testing-server--process nil)
(defvar request-testing-server--port nil)

(defun request-testing-server ()
  "Get running test server and return its root URL."
  (unless request-testing-server--port
    (let* ((process
            (start-process "request-testing" " *request-testing*"
                           "python"
                           (expand-file-name
                            "testserver.py"
                            request-testing-source-dir)))
           (buffer (process-buffer process)))
      (setq request-testing-server--process process)
      (loop repeat 30
            do (accept-process-output process 0 100 t)
            for port-str = (with-current-buffer buffer (buffer-string))
            do (cond
                ((string-match "^[0-9]+$" port-str)
                 (setq request-testing-server--port
                       (string-to-number port-str))
                 (return))
                ((not (eq 'run (process-status process)))
                 (error "Server startup error.")))
            finally do (error "Server timeout error."))))
  (request-testing-url))

(defun request-testing-url (&rest path)
  (loop with url = (format "http://127.0.0.1:%s" request-testing-server--port)
        for p in path
        do (setq url (concat url "/" p))
        finally return url))

(defun request-testing-sync (&rest args)
  (lexical-let (err)
    (let ((result
           (deferred:sync!
             (deferred:try
               (deferred:timeout
                 request-testing-timeout
                 (setq err '(error "timeout"))
                 (apply #'request-deferred args))
               :catch
               (lambda (x) (setq err x))))))
      (if err
          (error "Got: %S" err)
        result))))

(defmacro* request-deftest (name () &body docstring-and-body)
  "`ert-deftest' with predefined context."
  (declare (debug (&define :name test
                           name sexp [&optional stringp]
			   [&rest keywordp sexp] def-body))
           (doc-string 3)
           (indent 2))
  (let ((docstring (car docstring-and-body))
        (body (cdr docstring-and-body)))
    (unless (stringp docstring)
      (setq docstring nil)
      (setq body docstring-and-body))
    `(ert-deftest ,name ()
       ,@(when docstring (list docstring))
       (request-testing-server)
       ,@body)))

(provide 'request-testing)

;;; request-testing.el ends here
