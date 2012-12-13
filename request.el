;;; request.el --- Compatible layer for URL request in Emacs

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; request.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; request.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with request.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))
(eval-when-compile (require 'url))
(require 'json nil t)

(defgroup request nil
  "Compatible layer for URL request in Emacs."
  :group 'comm
  :prefix "request-")


;;; Customize variables

(defcustom request-curl "curl"
  "Executable for curl command."
  :group 'request)

(defcustom request-method (if (executable-find request-curl)
                              'curl
                            'url-retrieve)
  "Method to be used for HTTP request."
  :group 'request)

(defcustom request-method-alist
  '((url-retrieve . request--urllib)
    (curl         . request--curl))
  "Available request methods"
  :group 'request)

(defcustom request-timeout 1000
  "Default request timeout in millisecond."
  :group 'request)


;;; Internal variables

(defvar request--ajax-timer nil)
(make-variable-buffer-local 'request--ajax-timer)
(defvar request--ajax-canceled nil)
(make-variable-buffer-local 'request--ajax-canceled)


;;; Utilities

(defun request--safe-apply (function &rest arguments)
  (condition-case err
      (apply function arguments)
    ((debug error))))

(defun request--url-no-cache (url)
  "Imitate `cache=false' of `jQuery.ajax'.
See: http://api.jquery.com/jQuery.ajax/"
  ;; FIXME: parse URL before adding ?_=TIME.
  (concat url (format-time-string "?_=%s")))

(defun request-log (&rest args)
  ;; FIXME: implement
  )

(defun request-parser-json ()
  (goto-char (point-max))
  (backward-sexp)
  (json-read))


;;; Main

(defun* request-default-error-callback (url &key symbol-status
                                            &allow-other-keys)
  (request-log 'error
    "Error (%s) while connecting to %s.  Please retry."
    symbol-status url))

(defun* request (url &rest settings
                     &key
                     (cache t)
                     (type "GET")
                     (data nil)
                     (parser nil)
                     (headers nil)
                     (success nil)
                     (error nil)
                     (timeout request-timeout)
                     (status-code nil))
  "Send request to URL.

API of `request' is similar to `jQuery.ajax'.

:CACHE       (nil/t) : append time-stamp to URL so the URL is always loaded.
:TYPE       (string) : sets `url-request-method'
:DATA       (string) : sets `url-request-data'
:PARSER     (symbol) : a function that reads current buffer and return data
:HEADERS     (alist) : sets `url-request-extra-headers'
:SUCCESS      (cons) : called on success
:ERROR        (cons) : called on error
:TIMEOUT    (number) : timeout in millisecond
:STATUS-CODE (alist) : map status code (int) to callback (cons)

* Callback functions

Callback functions STATUS, ERROR and `cdr's in element of the
alist STATUS-CODE takes keyword arguments listed below.  For
forward compatibility, these functions must ignore unused keyword
arguments (i.e., it's better to use `&allow-other-keys'.

* :ERROR callback

:SYMBOL-STATUS (`error'/`timeout') : analogous of `textStatus'
:STATUS                     (list) : see `url-retrieve'
:RESPONSE-STATUS                   : = `url-http-response-status'

* :SUCCESS callback

This callback takes :DATA (object), which is a data object parsed
by :PARSER.  If :PARSER is not specified, this is nil.
The :SUCCESS callback also takes the :STATUS and :RESPONSE-STATUS
argument.

* :STATUS-CODE callback

Each value of this alist is a callback which is similar to :ERROR
or :SUCCESS callback.  However, current buffer of this callback
is not guaranteed to be the process buffer.

* :PARSER function

This is analogous to the `dataType' argument of `$.ajax'.
Only this function can accuses to the process buffer, which
is killed immediately after the execution of this function.

* See also: http://api.jquery.com/jQuery.ajax/"
  (request-log 'debug "REQUEST")
  (unless cache
    (setq url (request--url-no-cache url)))
  (unless error
    (setq error (apply-partially #'request-default-error-callback url))
    (plist-put settings :error error))
  (apply
   (or (assoc-default request-method request-method-alist)
       (error "%S is not valid `request-method'." request-method))
   url settings))

(defun* request--urllib (url &rest settings
                             &key type data headers timeout
                             &allow-other-keys)
  (when (and (equal type "POST") data)
    (push '("Content-Type" . "application/x-www-form-urlencoded") headers)
    (setq settings (plist-put settings :headers headers)))
  (let* ((url-request-extra-headers headers)
         (url-request-method type)
         (url-request-data data)
         (buffer (url-retrieve url #'request--callback settings)))
    (request-log 'debug "Start querying: %s" url)
    (when timeout
      (request-log 'debug "Start timer: timeout=%s ms" timeout)
      (with-current-buffer buffer
        (setq request--ajax-timer
              (apply #'run-at-time
                     (/ timeout 1000.0) nil
                     #'request--timeout-callback
                     (cons buffer settings)))))
    (set-process-query-on-exit-flag (get-buffer-process buffer) nil)
    buffer))

(defun request--parse-data (parser status-error)
  "Run PARSER in current buffer if STATUS-ERROR is nil,
then kill the current buffer."
  (let ((buffer (current-buffer)) ; NOTE: `parser' could change buffer...
        noerror)
    (unwind-protect
        (prog1
            (when (and parser (not status-error))
              (funcall parser))
          (setq noerror t))
      (unless noerror
        (request-log 'error "REQUEST--PARSE-DATA: error from parser %S"
                     parser))
      (kill-buffer buffer))))

(defun* request--callback (status &key
                                  (headers nil)
                                  (parser nil)
                                  (success nil)
                                  (error nil)
                                  (timeout nil)
                                  (status-code nil)
                                  &allow-other-keys)
  (declare (special url-http-method
                    url-http-response-status))

  (request-log 'debug "REQUEST--CALLBACK")
  (request-log 'debug "status = %S" status)
  (request-log 'debug "url-http-method = %s" url-http-method)
  (request-log 'debug "url-http-response-status = %s" url-http-response-status)
  (request-log 'debug "(buffer-string) =\n%s" (buffer-string))

  (request--cancel-timer)
  (let* ((response-status url-http-response-status)
         (status-code-callback (cdr (assq response-status status-code)))
         (status-error (plist-get status :error))
         (canceled request--ajax-canceled)
         (data (request--parse-data parser status-error)))
    (request-log 'debug "data = %s" data)
    (request-log 'debug "canceled = %s" canceled)

    (request-log 'debug "Executing success/error callback.")
    (apply #'request--safe-apply
           (append (if (or (plist-get status :error) canceled)
                       (list error :symbol-status (or canceled 'error))
                     (list success))
                   (list :status status :data data
                         :response-status response-status)))
    (when (and (not canceled) status-code-callback)
      (request-log 'debug "Executing status-code callback.")
      (request--safe-apply status-code-callback :status status :data data))))

(defun* request--timeout-callback (buffer &key (error nil) &allow-other-keys)
  (request-log 'debug "REQUEST--TIMEOUT-CALLBACK buffer = %S" buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq request--ajax-canceled 'timeout)
      (let ((proc (get-buffer-process buffer)))
        ;; This will call `request--callback'.
        (delete-process proc)))))

(defun request--cancel-timer ()
  (request-log 'debug "REQUEST--CANCEL-TIMER")
  (when request--ajax-timer
    (cancel-timer request--ajax-timer)
    (setq request--ajax-timer nil)))


;;; curl

(defun* request--curl-command (url &key type data headers timeout
                                   &allow-other-keys)
  (append
   (list request-curl)
   (when data (list "--data-urlencode" "@-"))
   (when type (list "--request" type))
   (when timeout (list "--max-time" (format "%s" (/ timeout 1000.0))))
   (loop for h in headers
         collect "--header"
         collect h)
   (list url)))

(defun* request--curl (url &rest settings
                           &key type data headers timeout
                           &allow-other-keys)
  (let* (;; Use pipe instead of pty.  Otherwise, curl process hangs.
         (process-connection-type nil)
         (proc
          (apply #'start-process
                 "request curl" " *request curl*"
                 (apply #'request--curl-command url settings)))
         (buffer (process-buffer proc)))
    (set-process-query-on-exit-flag proc nil)
    (process-put proc :request settings)
    (set-process-sentinel proc #'request--curl-callback)
    (when data
      (process-send-string proc data)
      (process-send-eof proc))
    buffer))

(defun request--curl-callback (proc event)
  (let ((buffer (process-buffer proc))
        (settings (process-get proc :request))
        ;; `request--callback' needs the following variables to be
        ;; defined.  I should refactor `request--callback' at some
        ;; point.
        url-http-method url-http-response-status)
    (cond
     ((string-match "exited abnormally" event)
      (with-current-buffer buffer
        (apply #'request--callback (list :error (cons 'error event))
               settings)))
     ((equal event "finished\n")
      (with-current-buffer buffer
        ;; FIXME: implement:
        ;; (setq url-http-method ... url-http-response-status ...)
        (apply #'request--callback nil settings))))))

(provide 'request)

;;; request.el ends here
