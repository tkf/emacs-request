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


;;; Internal variables

(defvar request--ajax-timer nil)
(make-variable-buffer-local 'request--ajax-timer)
(defvar request--ajax-canceled nil)
(make-variable-buffer-local 'request--ajax-canceled)


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
:TIMEOUT    (number) : timeout in second
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
   (or (assoc-default request-backend request-backend-alist)
       (error "%S is not valid `request-backend'." request-backend))
   url settings))


;;; Backend: `url-retrieve'

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
                     timeout nil
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

    (let ((args (list :status status :data data
                      :response-status response-status)))
      (if (not (or status-error canceled))
          (progn
            (request-log 'debug "Executing success callback.")
            (request--safe-apply success args))
        (request-log 'debug "Executing error callback.")
        (request--safe-apply error :symbol-status (or canceled 'error) args)))

    (when (and (not canceled) status-code-callback)
      (request-log 'debug "Executing status-code callback.")
      (request--safe-call status-code-callback :status status :data data))))

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
                           &key type data headers timeout
                           &allow-other-keys)
  (let* (;; Use pipe instead of pty.  Otherwise, curl process hangs.
         (process-connection-type nil)
         (buffer (generate-new-buffer " *request curl*"))
         (command (apply #'request--curl-command url settings))
         (proc (apply #'start-process "request curl" buffer command)))
    (request-log 'debug "Run: %s" (mapconcat 'identity command " "))
    (set-process-query-on-exit-flag proc nil)
    (process-put proc :request settings)
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
  (let (redirect-to)
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
                (setq redirect-to (match-string 1))
                ;; Remove headers for redirection.
                (delete-region (point-min) point))))
      (nconc (list :num-redirects num-redirects :redirect-to redirect-to)
             (request--parse-response-at-point)))))

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
        (destructuring-bind (&key version code num-redirects redirect-to)
            (request--curl-preprocess)
          (setq url-http-response-status code)
          (apply #'request--callback
                 (when redirect-to (list :redirect-to redirect-to))
                 settings)))))))

(provide 'request)

;;; request.el ends here
