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
(require 'url nil t)
(require 'mail-utils)

(defgroup request nil
  "Compatible layer for URL request in Emacs."
  :group 'comm
  :prefix "request-")


;;; Customize variables

(defcustom request-storage-directory
  (concat (file-name-as-directory user-emacs-directory) "request")
  "Directory to store data related to request.el."
  :group 'request)

(defcustom request-curl "curl"
  "Executable for curl command."
  :group 'request)

(defcustom request-backend (if (executable-find request-curl)
                               'curl
                             'url-retrieve)
  "Backend to be used for HTTP request."
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

;; (defun request--url-no-cache (url)
;;   "Imitate `cache=false' of `jQuery.ajax'.
;; See: http://api.jquery.com/jQuery.ajax/"
;;   ;; FIXME: parse URL before adding ?_=TIME.
;;   (concat url (format-time-string "?_=%s")))

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
  status-code redirects data error-thrown symbol-status url
  settings cookies
  ;; internal variables
  -buffer -timer -backend -tempfiles)

(defmacro request--document-response (function docstring)
  (declare (indent defun)
           (doc-string 2))
  `(request--document-function ,function ,(concat docstring "

.. This is an accessor for `request-response' object.

\(fn RESPONSE)")))

(request--document-response request-response-status-code
  "Integer HTTP response code (e.g., 200).")

(request--document-response request-response-redirects
  "Redirection history (a list of URLs).
The first element will be the oldest redirection.")

(request--document-response request-response-data
  "Response parsed by the given parser.")

(request--document-response request-response-error-thrown
  "Error thrown during request.
It takes the form of ``(ERROR-SYMBOL . DATA)``, which can be
re-raised (`signal'ed) by ``(signal ERROR-SYMBOL DATA)``.")

(request--document-response request-response-symbol-status
  "A symbol representing the status of *request* (not response).
One of success/error/timeout.")  ; FIMXE: add abort/parse-error

(request--document-response request-response-url
  "Final URL location of response.")

(request--document-response request-response-cookies
  "Cookies (alist).  Can be used only in curl backend now.")
;; FIXME: can this be implemented in `url-retrieve' backend?

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


;;; Backend dispatcher

(defvar request--backend-alist
  '((url-retrieve . ((request     . request--url-retrieve)
                     (get-cookies . request--url-retrieve-get-cookies)))
    (curl         . ((request     . request--curl)
                     (get-cookies . request--curl-get-cookies))))
  "Available request backends.")

(defun request--choose-backend (method)
  (assoc-default
   method
   (or (assoc-default request-backend request--backend-alist)
       (error "%S is not valid `request-backend'." request-backend))))


;;; Cookie

(defun request-cookie-string (host &optional localpart secure)
  "Return cookie string (like `document.cookie').

Example::

 (request-cookie-string \"127.0.0.1\" \"/\")  ; => \"key=value; key2=value2\"
"
  (let ((cookies (funcall (request--choose-backend 'get-cookies)
                          host localpart secure)))
    (mapconcat
     (lambda (nv) (concat (car nv) "=" (cdr nv)))
     cookies
     "; ")))


;;; Main

(defun* request-default-error-callback (url &key symbol-status
                                            &allow-other-keys)
  (request-log 'error
    "Error (%s) while connecting to %s." symbol-status url))

(defun* request (url &rest settings
                     &key
                     (type "GET")
                     (data nil)
                     (files nil)
                     (parser nil)
                     (headers nil)
                     (success nil)
                     (error nil)
                     (complete nil)
                     (timeout request-timeout)
                     (status-code nil)
                     (response (make-request-response)))
  "Send request to URL.

Request.el has a single entry point.  It is `request'.
API of `request' is similar to `jQuery.ajax'.

==================== ========================================================
Keyword argument      Explanation
==================== ========================================================
TYPE       (string)   type of request to make: POST/GET/PUT/DELETE
DATA       (string)   data to be sent to the server
FILES       (alist)   files to be sent to the server (see below)
PARSER     (symbol)   a function that reads current buffer and return data
HEADERS     (alist)   additional headers to send with the request
SUCCESS  (function)   called on success
ERROR    (function)   called on error
COMPLETE (function)   called on both success and error
TIMEOUT    (number)   timeout in second
STATUS-CODE (alist)   map status code (int) to callback
==================== ========================================================


* Callback functions

Callback functions STATUS, ERROR, COMPLETE and `cdr's in element of
the alist STATUS-CODE take same keyword arguments listed below.  For
forward compatibility, these functions must ignore unused keyword
arguments (i.e., it's better to use `&allow-other-keys').::

    (CALLBACK                      ; SUCCESS/ERROR/COMPLETE/STATUS-CODE
     :data          data           ; whatever PARSER function returns, or nil
     :error-thrown  error-thrown   ; (ERROR-SYMBOL . DATA), or nil
     :symbol-status symbol-status  ; success
     :response      response       ; request-ponse object
     ...)

Arguments data, error-thrown, symbol-status can be accessed by
`request-response-data', `request-response-error-thrown',
`request-response-status' assessors, i.e.::

    (request-response-data RESPONSE)  ; same as data

Response object holds other information which can be accessed by
the following assessors:
`request-response-status-code',
`request-response-redirects',
`request-response-url' and
`request-response-settings'

* STATUS-CODE callback

STATUS-CODE is an alist of the following format::

    ((N-1 . CALLBACK-1)
     (N-2 . CALLBACK-2)
     ...)

Here, N-1, N-2,... are integer status codes such as 200.


* FILES

FILES is an alist of the following format::

    ((NAME-1 . FILE-1)
     (NAME-2 . FILE-2)
     ...)

where FILE-N is a list of the form::

    (FILENAME [PATH | BUFFER] [:contents STRING] [:mime-type MIME-TYPE])

FILE-N can also be a string (path to the file) or a buffer object
where FILENAME is inferred.

Example FILES argument::

    `((\"passwd\"   . \"/etc/passwd\")                ; filename = passwd
      (\"scratch\"  . ,(get-buffer \"*scratch*\"))    ; filename = *scratch*
      (\"passwd2\"  . (\"password\" \"/etc/passwd\"))
      (\"scratch2\" . (\"scratch\" ,(get-buffer \"*scratch*\")))
      (\"data\"     . (\"data.csv\" :contents \"1,2,3\\n4,5,6\\n\")))

.. note:: FILES is implemented only for curl backend for now.
   As furl.el_ supports multipart POST, it should be possible to
   support FILES in pure elisp by making furl.el_ another backend.
   Contributions are welcome.

   .. _furl.el: http://code.google.com/p/furl-el/


* PARSER function

PARSER function takes no argument and it is executed in the
buffer with HTTP response.  The current position in the
HTTP response buffer is at the beginning of the response
body.  So, for example, you can pass `json-read' to parse
JSON object in the buffer.

This is analogous to the `dataType' argument of `$.ajax'.
Only this function can accuses to the process buffer, which
is killed immediately after the execution of this function.


* See also: http://api.jquery.com/jQuery.ajax/"
  (request-log 'debug "REQUEST")
  ;; FIXME: support CACHE argument (if possible)
  ;; (unless cache
  ;;   (setq url (request--url-no-cache url)))
  (unless error
    (setq error (apply-partially #'request-default-error-callback url))
    (setq settings (plist-put settings :error error)))
  (setq settings (plist-put settings :response response))
  (setf (request-response-settings response) settings)
  (setf (request-response-url      response) url)
  (setf (request-response--backend response) request-backend)
  (apply
   (request--choose-backend 'request)
   url settings))

(defun request--parse-data (parser error-thrown backend)
  "Run PARSER in current buffer if ERROR-THROWN is nil,
then kill the current buffer."
  (let ((buffer (current-buffer)) ; NOTE: `parser' could change buffer...
        noerror)
    (unwind-protect
        (prog1
            (when (and parser (not error-thrown))
              (goto-char (point-min))
              ;; Should be no \r.
              ;; See `url-http-clean-headers' and `request--curl-preprocess'.
              (if (eq backend 'url-retrieve)
                  ;; FIXME: make this workaround optional.
                  ;; But it looks like sometimes `url-http-clean-headers'
                  ;; fails to cleanup.  So, let's be bit permissive here...
                  (re-search-forward "^\r?$")
                (re-search-forward "^$"))
              ;; `forward-char' will fail when there is no body.
              (ignore-errors (forward-char))
              (funcall parser))
          (setq noerror t))
      (unless noerror
        (request-log 'error "REQUEST--PARSE-DATA: error from parser %S"
                     parser))
      (kill-buffer buffer))))

;; FIXME: Define `request--url-retrieve-callback' and move
;;        `url-retrieve' -specific stuff to there.  Currently,
;;        `request--callback' is defined for `url-retrieve' and
;;        `request--curl-callback' need to be complex than it really
;;        is.
(defun* request--callback (status &key
                                  (headers nil)
                                  (parser nil)
                                  (success nil)
                                  (error nil)
                                  (complete nil)
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
         (error-thrown (plist-get status :error))
         (data (request--parse-data
                parser error-thrown
                (request-response--backend response))))
    (request-log 'debug "data = %s" data)

    (symbol-macrolet
        ((symbol-status (request-response-symbol-status response)))

      (unless symbol-status
        (setq symbol-status (if error-thrown 'error 'success)))
      (request-log 'debug "symbol-status = %s" symbol-status)

      (setf (request-response-status-code response) response-status)
      (setf (request-response-data response) data)
      (setf (request-response-error-thrown response) error-thrown)
      (let ((redirect (plist-get :redirect status)))
        (when redirect
          (setf (request-response-url response) redirect)
          (setf (request-response-redirects response)
                (loop with l = nil
                      for (k v) on redirect by 'cddr
                      when (eq k :redirect)
                      do (push v l)
                      finally return l))))

      (let* ((args (list :data data
                         :symbol-status symbol-status
                         :error-thrown error-thrown
                         :response response))
             (success-p (eq symbol-status 'success))
             (cb (if success-p success error))
             (name (if success-p "success" "error")))
        (when cb
          (request-log 'debug "Executing %s callback." name)
          (request--safe-apply cb args))

        (when status-code-callback
          (request-log 'debug "Executing status-code callback.")
          (request--safe-apply status-code-callback args))

        (when complete
          (request-log 'debug "Executing complete callback.")
          (request--safe-apply complete args))))

    ;; Remove temporary files
    ;; FIXME: Make tempfile cleanup more reliable.  It is possible
    ;;        callback is never called.
    (mapc (lambda (tf) (condition-case err
                           (delete-file tf)
                         (error (request-log 'error
                                  "Failed delete temporary file %s. Got: %S"
                                  tf err))))
          (request-response--tempfiles response))))


;;; Backend: `url-retrieve'

(defun* request--url-retrieve (url &rest settings
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

(defun request--url-retrieve-get-cookies (host localpart secure)
  (mapcar
   (lambda (c) (cons (url-cookie-name c) (url-cookie-value c)))
   (url-cookie-retrieve host localpart secure)))


;;; Backend: curl

(defvar request--curl-cookie-jar nil
  "Override what the function `request--curl-cookie-jar' returns.
Currently it is used only for testing.")

(defun request--curl-cookie-jar ()
  "Cookie storage for curl backend."
  (or request--curl-cookie-jar
      (expand-file-name "curl-cookie-jar" request-storage-directory)))

(defvar request--curl-write-out-template
  "\\n(:num-redirects %{num_redirects})")
;; FIXME: should % be escaped for Windows?

(defun request--curl-mkdir-for-cookie-jar ()
  (ignore-errors
    (make-directory (file-name-directory (request--curl-cookie-jar)) t)))

(defun* request--curl-command
    (url &key type data headers timeout files*
         &allow-other-keys
         &aux
         (cookie-jar (convert-standard-filename
                      (expand-file-name (request--curl-cookie-jar)))))
  (append
   (list request-curl "--silent" "--include"
         "--location"
         ;; FIMXE: this way of using cookie might be problem when
         ;;        running multiple requests.
         "--cookie" cookie-jar "--cookie-jar" cookie-jar
         "--write-out" request--curl-write-out-template)
   (loop for (name filename path mime-type) in files*
         collect "--form"
         collect (format "%s=@%s;filename=%s%s" name path filename
                         (if mime-type
                             (format ";type=%s" mime-type)
                           "")))
   (when data (list "--data-binary" "@-"))
   (when type (list "--request" type))
   (when timeout (list "--max-time" (format "%s" timeout)))
   (loop for (k . v) in headers
         collect "--header"
         collect (format "%s: %s" k v))
   (list url)))

(defun request--curl-normalize-files (files)
  (loop with files*
        with tempfiles
        for (name . item) in files
        do (cond
            ((stringp item)
             (setq item (list (file-name-nondirectory item) item)))
            ((bufferp item)
             (setq item (list (buffer-name item) item)))
            ((symbolp (cadr item))
             (setq item (list (car item) nil (cddr item)))))
        collect
        (destructuring-bind (filename bop &key contents mime-type) item
          (cond
           ((stringp bop)
            (assert (null contents))
            (list name filename bop mime-type))
           ((bufferp bop)
            (assert (null contents))
            (let ((tf (make-temp-file "emacs-request-")))
              ;; FIXME: add more error handling
              (with-current-buffer bop
                (write-region (point-min) (point-max) tf))
              (push tf tempfiles)
              (list name filename tf mime-type)))
           ((stringp contents)
            (let ((tf (make-temp-file "emacs-request-")))
              ;; FIXME: add more error handling
              (with-temp-buffer
                (erase-buffer)
                (insert contents)
                (write-region (point-min) (point-max) tf))
              (push tf tempfiles)
              (list name filename tf mime-type)))
           (t (error "invalid FILES argument."))))
        into files*
        finally return (list files* tempfiles)))

(defun* request--curl (url &rest settings
                           &key type data files headers timeout response
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
  (request--curl-mkdir-for-cookie-jar)
  (let* (;; Use pipe instead of pty.  Otherwise, curl process hangs.
         (process-connection-type nil)
         (buffer (generate-new-buffer " *request curl*"))
         (command (destructuring-bind
                      (files* tempfiles)
                      (request--curl-normalize-files files)
                    (setf (request-response--tempfiles response) tempfiles)
                    (apply #'request--curl-command url :files* files*
                           settings)))
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

(defvar request--cookie-reserved-re
  (mapconcat
   (lambda (x) (concat "\\(^" x "\\'\\)"))
   '("comment" "commenturl" "discard" "domain" "max-age" "path" "port"
     "secure" "version" "expires")
   "\\|")
  "Uninterested keys in cookie.
See \"set-cookie-av\" in http://www.ietf.org/rfc/rfc2965.txt")

(defun request--cookie-name-value (cookies)
  (loop named outer
        with case-fold-search = t
        for c in cookies
        when
        (loop for (k . v) in (url-parse-args c t)
              unless (string-match request--cookie-reserved-re k)
              return (cons k v))
        return it))

(defun request--curl-preprocess ()
  "Pre-process current buffer before showing it to user."
  (let (redirects cookies cookies2)
    (destructuring-bind (&key num-redirects)
        (request--curl-read-and-delete-tail-info)
      (goto-char (point-min))
      (when (> num-redirects 0)
        (loop with case-fold-search = t
              repeat num-redirects
              for beg = (point)
              do (request--goto-next-body)
              for end = (point)
              ;; FIXME: use `mail-fetch-field'
              do (progn
                   (re-search-backward "^location: \\([^\r\n]+\\)\r\n" beg)
                   (push (match-string 1) redirects)
                   (goto-char end))
              ;; Remove headers for redirection.
              finally do (delete-region (point-min) end)))

      ;; Remove \r from header to use `mail-fetch-field'.
      ;; See: `url-http-clean-headers'
      (goto-char (point-min))
      (request--goto-next-body)
      (while (re-search-backward "\r$" (point-min) t)
        (replace-match ""))

      (goto-char (point-min))
      (unwind-protect
          (progn
            (narrow-to-region (point)
                              (progn (re-search-forward "^$") (point)))
            (setq cookies
                  (nreverse (mail-fetch-field "Set-Cookie" nil nil t)))
            (setq cookies2
                  (nreverse (mail-fetch-field "Set-Cookie2" nil nil t))))
        (widen))

      (goto-char (point-min))
      (nconc (list :num-redirects num-redirects :redirects redirects
                   ;; FIMXE: handle multiple key-value pairs
                   ;; FIXME: verify if this way of choosing
                   ;;        cookies/cookies2 is OK
                   :cookies
                   (cond
                    (cookies (list (request--cookie-name-value cookies)))
                    (cookies2 (list (request--cookie-name-value cookies2)))))
             (request--parse-response-at-point)))))

(defun request--curl-callback (proc event)
  (let* ((buffer (process-buffer proc))
         (response (process-get proc :request-response))
         (settings (request-response-settings response))
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
        (destructuring-bind (&key version code num-redirects redirects error
                                  cookies)
            (condition-case err
                (request--curl-preprocess)
              ((debug error)
               (list :error err)))
          (setf (request-response-cookies response) cookies)
          (setq url-http-response-status code)
          (let ((status (append
                         (loop for r in redirects
                               collect :redirect collect r)
                         (list :error
                               (or error
                                   (when (>= code 400)
                                     `(error . (http ,code))))))))
            (apply #'request--callback status settings))))))))

(defun request--curl-get-cookies (host localpart secure)
  (request--netscape-get-cookies (request--curl-cookie-jar)
                                 host localpart secure))


;;; Netscape cookie.txt parser

(defun request--netscape-cookie-parse ()
  "Parse Netscape/Mozilla cookie format."
  (goto-char (point-min))
  (let ((tsv-re (concat "^\\="
                        (loop repeat 6 concat "\\([^\t\n]+\\)\t")
                        "\\(.*\\)"))
        cookies)
    (while
        (and
         (cond
          ((re-search-forward "^\\=#" nil t))
          ((re-search-forward "^\\=$" nil t))
          ((re-search-forward tsv-re)
           (push (loop for i from 1 to 7 collect (match-string i))
                 cookies)
           t))
         (= (forward-line 1) 0)
         (not (= (point) (point-max)))))
    (setq cookies (nreverse cookies))
    (loop for (domain flag path secure expiration name value) in cookies
          collect (list domain
                        (equal flag "TRUE")
                        path
                        (equal secure "TRUE")
                        (string-to-number expiration)
                        name
                        value))))

(defun request--netscape-filter-cookies (cookies host localpart secure)
  (loop for (domain flag path secure-1 expiration name value) in cookies
        when (and (equal domain host)
                  (equal path localpart)
                  (or secure (not secure-1)))
        collect (cons name value)))

(defun request--netscape-get-cookies (filename host localpart secure)
  (when (file-readable-p filename)
    (with-temp-buffer
      (erase-buffer)
      (insert-file-contents filename)
      (request--netscape-filter-cookies (request--netscape-cookie-parse)
                                        host localpart secure))))

(provide 'request)

;;; request.el ends here
