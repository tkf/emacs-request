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

(defcustom request-backend (if (executable-find request-curl)
                               'curl
                             'url-retrieve)
  "Backend to be used for HTTP request."
  :group 'request)

(defcustom request-backend-alist
  '((url-retrieve . request--urllib)
    (curl         . request--curl))
  "Available request backends."
  :group 'request)

(defcustom request-timeout 1
  "Default request timeout in second."
  :group 'request)

(defcustom request-log-level -1
  "Logging level for request."
  :group 'request)

(defcustom request-message-level 'warn
  "Logging level for request."
  :group 'request)


;;; Utilities

(defun request--safe-apply (function &rest arguments)
  (condition-case err
      (apply #'apply function arguments)
    ((debug error))))

(defun request--safe-call (function &rest arguments)
  (request--safe-apply function arguments))

(defun request--url-no-cache (url)
  "Imitate `cache=false' of `jQuery.ajax'.
See: http://api.jquery.com/jQuery.ajax/"
  ;; FIXME: parse URL before adding ?_=TIME.
  (concat url (format-time-string "?_=%s")))

(defun request-parser-json ()
  (goto-char (point-max))
  (backward-sexp)
  (json-read))

(defmacro request--document-function (function docstring)
  "Document FUNCTION with DOCSTRING.  Use this for defstruct accessor etc."
  (declare (indent defun)
           (doc-string 2))
  `(put ',function 'function-documentation ,docstring))


;;; Logging

(defvar request--log-level-def
  '(;; debugging
    (blather . 60) (trace . 50) (debug . 40)
    ;; information
    (verbose . 30) (info . 20)
    ;; errors
    (warn . 10) (error . 0))
  "Named logging levels.")

(defun request--log-level-as-int (level)
  (if (integerp level)
      level
    (or (cdr (assq level request--log-level-def))
        0)))

(defvar request-log-buffer-name " *request-log*")

(defun request--log-buffer ()
  (get-buffer-create request-log-buffer-name))

(defmacro request-log (level fmt &rest args)
  (declare (indent 1))
  `(let ((level (request--log-level-as-int ,level))
         (log-level (request--log-level-as-int request-log-level))
         (msg-level (request--log-level-as-int request-message-level)))
     (when (<= level (max log-level msg-level))
       (let ((msg (format "[%s] %s" ,level (format ,fmt ,@args))))
         (when (<= level log-level)
           (with-current-buffer (request--log-buffer)
             (setq buffer-read-only t)
             (let ((inhibit-read-only t))
               (goto-char (point-max))
               (insert msg "\n"))))
         (when (<= level msg-level)
           (message "REQUEST %s" msg))))))


;;; Header parser

(defun request--parse-response-at-point ()
  (re-search-forward "\\=[ \t\n]*HTTP/\\([0-9\\.]+\\) +\\([0-9]+\\)")
  (list :version (match-string 1)
        :code (string-to-number (match-string 2))))

(defun request--goto-next-body ()
  (re-search-forward "^\r\n"))


;;; Response object

(defstruct request-response
  "A structure holding all relevant information of a request."
  status-code status-text history error-thrown symbol-status settings
  ;; internal variables
  -buffer -timer)

(defmacro request--document-response (function docstring)
  (declare (indent defun)
           (doc-string 2))
  `(request--document-function ,function ,(concat docstring "

This is an accessor for `request-response' object.

\(fn RESPONSE)")))

(request--document-response request-response-status-code
  "Integer HTTP response code (e.g., 200).")

(request--document-response request-response-status-text
  "Reason phrase of HTTP response (e.g., \"OK\").")

(request--document-response request-response-history
  "Redirection history (a list of `request-response' objects).
The first element will be the oldest redirection.")

(request--document-response request-response-error-thrown
  "Any kind error thrown during request.
It takes the form of ``(ERROR-SYMBOL . DATA)``, which can be
re-raised (`signal'ed) by ``(signal ERROR-SYMBOL DATA)``.")

(request--document-response request-response-symbol-status
  "A symbol representing the status of *request* (not response).
One of success/error/timeout.")  ; FIMXE: add abort/parse-error

(request--document-response request-response-settings
  "Keyword arguments passed to `request' function.")

(defun* request-response--timeout-callback (response)
  (request-log 'debug "request-response--timeout-callback")
  (setf (request-response-symbol-status response) 'timeout)
  (let* ((buffer (request-response--buffer response))
         (proc (and (buffer-live-p buffer) (get-buffer-process buffer))))
    (when proc
      ;; This will call `request--callback':
      (delete-process proc))))

(defun request-response--cancel-timer (response)
  (request-log 'debug "REQUEST-RESPONSE--CANCEL-TIMER")
  (symbol-macrolet ((timer (request-response--timer response)))
    (when timer
      (cancel-timer timer)
      (setq timer nil))))


;;; Main

(defun* request-default-error-callback (url &key symbol-status
                                            &allow-other-keys)
  (request-log 'error
    "Error (%s) while connecting to %s." symbol-status url))

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
                     (status-code nil)
                     (response (make-request-response)))
  "Send request to URL.

API of `request' is similar to `jQuery.ajax'.

:CACHE       (nil/t) : append time-stamp to URL so the URL is always loaded.
:TYPE       (string) : type of request to make: POST/GET/PUT/DELETE
:DATA       (string) : data to be sent to the server
:PARSER     (symbol) : a function that reads current buffer and return data
:HEADERS     (alist) : additional headers to send with the request
:SUCCESS      (cons) : called on success
:ERROR        (cons) : called on error
:TIMEOUT    (number) : timeout in second
:STATUS-CODE (alist) : map status code (int) to callback (cons)

* Callback functions

Callback functions STATUS, ERROR and `cdr's in element of the
alist STATUS-CODE takes keyword arguments listed below.  For
forward compatibility, these functions must ignore unused keyword
arguments (i.e., it's better to use `&allow-other-keys'.

* :ERROR callback call signature::

    (ERROR
     :error-thrown  error-thrown   ; (ERROR-SYMBOL . DATA)
     :symbol-status symbol-status  ; error/timeout/...
     :response      response       ; `request-response' object
     ...)

* :SUCCESS callback call signature::

    (SUCCESS
     :data          data           ; whatever PARSER function returns
     :symbol-status symbol-status  ; success
     :response      response       ; `request-response' object
     ...)

* :STATUS-CODE callback

Each value of this alist is a callback which is similar to :ERROR
or :SUCCESS callback.

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
  (setq settings (plist-put settings :response response))
  (setf (request-response-settings response) settings)
  (apply
   (or (assoc-default request-backend request-backend-alist)
       (error "%S is not valid `request-backend'." request-backend))
   url settings))


;;; Backend: `url-retrieve'

(defun* request--urllib (url &rest settings
                             &key type data headers timeout response
                             &allow-other-keys)
  (when (and (equal type "POST") data)
    (push '("Content-Type" . "application/x-www-form-urlencoded") headers)
    (setq settings (plist-put settings :headers headers)))
  (let* ((url-request-extra-headers headers)
         (url-request-method type)
         (url-request-data data)
         (buffer (url-retrieve url #'request--callback
                               (nconc (list :response response) settings)))
         (proc (get-buffer-process buffer)))
    (setf (request-response--buffer response) buffer)
    (process-put proc :request-response response)
    (request-log 'debug "Start querying: %s" url)
    (when timeout
      (request-log 'debug "Start timer: timeout=%s ms" timeout)
      (with-current-buffer buffer
        (setf (request-response--timer response)
              (run-at-time timeout nil
                           #'request-response--timeout-callback
                           response))))
    (set-process-query-on-exit-flag proc nil)
    response))

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
                                  response
                                  &allow-other-keys)
  (declare (special url-http-method
                    url-http-response-status))

  (request-log 'debug "REQUEST--CALLBACK")
  (request-log 'debug "status = %S" status)
  (request-log 'debug "url-http-method = %s" url-http-method)
  (request-log 'debug "url-http-response-status = %s" url-http-response-status)
  (request-log 'debug "(buffer-string) =\n%s" (buffer-string))

  (request-response--cancel-timer response)
  (let* ((response-status url-http-response-status)
         (status-code-callback (cdr (assq response-status status-code)))
         (status-error (plist-get status :error))
         (data (request--parse-data parser status-error)))
    (request-log 'debug "data = %s" data)

    (symbol-macrolet
        ((symbol-status (request-response-symbol-status response)))

      (unless symbol-status
        (setq symbol-status (or (plist-get status :error) 'success)))
      (request-log 'debug "symbol-status = %s" symbol-status)

      (let ((args (list :status status :data data
                        :response-status response-status
                        :symbol-status symbol-status)))
        (request-log 'debug "Executing %s callback."
                     (if (eq symbol-status 'success) "success" "error"))
        (request--safe-apply
         (if (eq symbol-status 'success) success error) args)))

    (when status-code-callback
      (request-log 'debug "Executing status-code callback.")
      (request--safe-call status-code-callback :status status :data data))))


;;; Backend: curl

(defvar request--curl-write-out-template
  "\\n(:num-redirects %{num_redirects})")
;; FIXME: should % be escaped for Windows?

(defun* request--curl-command (url &key type data headers timeout
                                   &allow-other-keys)
  (append
   (list request-curl "--silent" "--include"
         "--location"
         "--write-out" request--curl-write-out-template)
   (when data (list "--data-binary" "@-"))
   (when type (list "--request" type))
   (when timeout (list "--max-time" (format "%s" timeout)))
   (loop for (k . v) in headers
         collect "--header"
         collect (format "%s: %s" k v))
   (list url)))

(defun* request--curl (url &rest settings
                           &key type data headers timeout response
                           &allow-other-keys)
  "cURL-based request backend.

Redirection handling strategy
-----------------------------

curl follows redirection when --location is given.  However,
all headers are printed when it is used with --include option.
Number of redirects is printed out sexp-based message using
--write-out option (see `request--curl-write-out-template').
This number is used for removing extra headers and parse
location header from the last redirection header.

Sexp at the end of buffer and extra headers for redicts are
removed from the buffer before it is shown to the parser function.
"
  (let* (;; Use pipe instead of pty.  Otherwise, curl process hangs.
         (process-connection-type nil)
         (buffer (generate-new-buffer " *request curl*"))
         (command (apply #'request--curl-command url settings))
         (proc (apply #'start-process "request curl" buffer command)))
    (request-log 'debug "Run: %s" (mapconcat 'identity command " "))
    (setf (request-response--buffer response) buffer)
    (process-put proc :request-response response)
    (set-process-query-on-exit-flag proc nil)
    (set-process-sentinel proc #'request--curl-callback)
    (when data
      (process-send-string proc data)
      (process-send-eof proc))
    buffer))

(defun request--curl-read-and-delete-tail-info ()
  "Read a sexp at the end of buffer and remove it and preceding character.
This function moves the point at the end of buffer by side effect.
See also `request--curl-write-out-template'."
  (let (forward-sexp-function)
    (goto-char (point-max))
    (forward-sexp -1)
    (let ((beg (1- (point))))
      (prog1
          (read (current-buffer))
        (delete-region beg (point-max))))))

(defun request--curl-preprocess ()
  "Pre-process current buffer before showing it to user."
  (let (redirect)
    (destructuring-bind (&key num-redirects)
        (request--curl-read-and-delete-tail-info)
      (goto-char (point-min))
      (when (> num-redirects 0)
        (loop repeat num-redirects
              for beg = (point)
              do (request--goto-next-body)
              finally do
              (let ((case-fold-search t)
                    (point (point)))
                (re-search-backward
                 "^location: \\([^\r\n]+\\)\r\n"
                 beg)
                (setq redirect (match-string 1))
                ;; Remove headers for redirection.
                (delete-region (point-min) point))))
      (nconc (list :num-redirects num-redirects :redirect redirect)
             (request--parse-response-at-point)))))

(defun request--curl-callback (proc event)
  (let ((buffer (process-buffer proc))
        (settings (request-response-settings
                   (process-get proc :request-response)))
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
        (destructuring-bind (&key version code num-redirects redirect error)
            (condition-case err
                (request--curl-preprocess)
              ((debug error)
               (list :error err)))
          (setq url-http-response-status code)
          (apply #'request--callback
                 (cond
                  (redirect (list :redirect redirect))
                  (error (list :error error)))
                 settings)))))))

(provide 'request)

;;; request.el ends here
