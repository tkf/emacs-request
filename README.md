# Request.el -- Easy HTTP request for Emacs Lisp

**Links**

- [Documentation](http://tkf.github.com/emacs-request/) (at GitHub
  Pages) **currently out of date**
    - [Manual](http://tkf.github.com/emacs-request/manual.html) **currently out of date**
- [Repository](https://github.com/tkf/emacs-request) (at GitHub)
- [Issue tracker](https://github.com/tkf/emacs-request/issues) (at
  GitHub)
- [Travis CI](https://travis-ci.org/#!/tkf/emacs-request)
- [MELPA](https://melpa.org/#/request)
- [MELPA Stable](https://stable.melpa.org/#/request)

## What is it?

Request.el is a HTTP request library with multiple backends. It supports url.el which is shipped with Emacs, and also the curl command line program. Users can use curl when it is available, as curl is more reliable than url.el. Library authors can use request.el to avoid imposing external dependencies such as curl to users while giving richer experience for users who have curl.

As request.el is implemented in an extensible manner, it is possible to implement other backends such as wget. Also, if future version of Emacs support linking with libcurl, it will be possible possible to implement a backend using it. Libraries using request.el automatically can use these backend without modifying their code.

Request.el also patches url.el dynamically, to fix bugs in url.el. See
[monkey patches for
url.el](http://tkf.github.io/emacs-request/#monkey-patches-for-url-el)
for the bugs fixed by request.el.

## Installation

request.el is available on [MELPA](https://melpa.org/) and [MELPA
stable](https://stable.melpa.org).

It is also possible to install request.el via `package.el`

## Basic Concepts

Request provides a single user-accessible lisp function to execute requests. `request` takes as parameters a URL and a number of keyword arguments, and returns a response object whose properties (including the response data) can be accessed using specialized functions. See the docstring or the [manual](http://tkf.github.io/emacs-request/manual.html#api) for more details. 

Recommended usage is to set a variable equal to the response, and then access the response with these accessor functions, e.g.:

``` emacs-lisp
(let*
    ((thisrequest
      (request "httpbin.org/get"
               :params '(("key" . "value") ("key2" . "value2"))
               :parser 'json-read
               :success (cl-function
                         (lambda (&key data &allow-other-keys)
                           (message "I sent: %S" (assoc-default 'args data))))))
     (data (request-response-data thisrequest))
     (err (request-response-error-thrown thisrequest))
     (status (request-response-status-code thisrequest)))
  ;; do stuff here
  )
```

## Examples

GET:

```emacs-lisp
(request
 "http://httpbin.org/get"
 :params '(("key" . "value") ("key2" . "value2"))
 :parser 'json-read
 :success (cl-function
           (lambda (&key data &allow-other-keys)
             (message "I sent: %S" (assoc-default 'args data)))))
```

POST:

```emacs-lisp
(request
 "http://httpbin.org/post"
 :type "POST"
 :data '(("key" . "value") ("key2" . "value2"))
 ;; :data "key=value&key2=value2"  ; this is equivalent
 :parser 'json-read
 :success (cl-function
           (lambda (&key data &allow-other-keys)
             (message "I sent: %S" (assoc-default 'form data)))))
```

POST file (**WARNING**: it will send the contents of the current
buffer\!):

```emacs-lisp
(request
 "http://httpbin.org/post"
 :type "POST"
 :files `(("current buffer" . ,(current-buffer))
          ("data" . ("data.csv" :data "1,2,3\n4,5,6\n")))
 :parser 'json-read
 :success (cl-function
           (lambda (&key data &allow-other-keys)
             (message "I sent: %S" (assoc-default 'files data)))))
```

Rich callback dispatch (like
<span class="title-ref">jQuery.ajax</span>):

```emacs-lisp
(request
 "http://httpbin.org/status/418"     ; try other codes, for example:
 ;; "http://httpbin.org/status/200"  ; success callback will be called.
 ;; "http://httpbin.org/status/400"  ; you will see "Got 400."
 :parser 'buffer-string
 :success
 (cl-function (lambda (&key data &allow-other-keys)
                (when data
                  (with-current-buffer (get-buffer-create "*request demo*")
                    (erase-buffer)
                    (insert data)
                    (pop-to-buffer (current-buffer))))))
 :error
 (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                (message "Got error: %S" error-thrown)))
 :complete (lambda (&rest _) (message "Finished!"))
 :status-code '((400 . (lambda (&rest _) (message "Got 400.")))
                (418 . (lambda (&rest _) (message "Got 418.")))))
```

Flexible PARSER option:

```emacs-lisp
(request
 "https://github.com/tkf/emacs-request/commits/master.atom"
 ;; Parse XML in response body:
 :parser (lambda () (libxml-parse-xml-region (point) (point-max)))
 :success (cl-function
           (lambda (&key data &allow-other-keys)
             ;; Just don't look at this function....
             (let ((get (lambda (node &rest names)
                          (if names
                              (apply get
                                     (first (xml-get-children
                                             node (car names)))
                                     (cdr names))
                            (first (xml-node-children node))))))
               (message "Latest commit: %s (by %s)"
                        (funcall get data 'entry 'title)
                        (funcall get data 'entry 'author 'name))))))
```

PUT JSON data:

```emacs-lisp
(request
 "http://httpbin.org/put"
 :type "PUT"
 :data (json-encode '(("key" . "value") ("key2" . "value2")))
 :headers '(("Content-Type" . "application/json"))
 :parser 'json-read
 :success (cl-function
           (lambda (&key data &allow-other-keys)
             (message "I sent: %S" (assoc-default 'json data)))))
```

Another PUT JSON example (nested JSON using alist structure, how to represent a boolean & how to selectively evaluate lisp):

```emacs-lis
;; (1) Prepend alist structure with a backtick (`) rather than single quote (')
;;     to allow elisp evaluation of selected elements prefixed with a comma (,)
;; (2) This value is expected as a boolean so use the nil / t elisp alist denotation
;; (3) The function will be evaluated as it has been prefixed with a comma (,)
(request
 "http://httpbin.org/put"
 :type "PUT"
 :data (json-encode `(("jsonArray" . (("item1" . "value 1") ;; (1)
                                      ("item2" . t)         ;; (2)
                                      ("item3" . ,(your-custom-elisp-function)))))) ;; (3)
 :headers '(("Content-Type" . "application/json"))
 :parser 'json-read
 :success (cl-function
           (lambda (&key data &allow-other-keys)
             (message "I sent: %S" (assoc-default 'json data)))))
```

GET with Unix domain socket data:

```emacs-lisp
(request
 "http:/hello.txt"
 :unix-socket "/tmp/app.sock"
 :parser (lambda () (buffer-string))
 :success (cl-function
           (lambda (&key data &allow-other-keys)
             (message "Got: %s" data))))
```

## Known Bugs

- Synchronous requests can be affected by a [suspected race condition](https://github.com/tkf/emacs-request/issues/92). Please see the bug report for possible workarounds. 
- Multibyte characters can cause the request to fail.  See [this pull request](https://github.com/tkf/emacs-request/pull/85) for one of several solutions to this problem. Fix expected soon. 

## Development

To help in the development, please submit Issues and Pull Requests on Github. Since MELPA builds directly form the `master` branch, please [use the development branch](https://github.com/tkf/emacs-request/tree/development) rather than `master` as the base branch for any Prs, so they can be tested before release!

emacs-request uses [cask](https://github.com/cask/cask) and [ERT](https://www.gnu.org/software/emacs/manual/html_node/ert/index.html) as a testing framework. Tests run against a simple python server that uses [flask](http://flask.pocoo.org/) and [tornado](https://www.tornadoweb.org/en/stable/), and requres the **python2** versions of these libraries. Several of these tests fail unpredictably/without obvious reasons; see [#103](https://github.com/tkf/emacs-request/issues/103) for some examples.

To run tests, first install python2, flask, and tornado, and then run `cask install` to retrieve the Emacs dependencies. 

Documentation assistance is particularly welcome!

### Debugging

Debugging levels are set om two places, `request-log-level` and `request-message-level`.

``` lisp
(defcustom request-log-level -1
  "Logging level for request.
One of `error'/`warn'/`info'/`verbose'/`debug'.
-1 means no logging."
  :group 'request)

(defcustom request-message-level 'warn
  "Logging level for request.
See `request-log-level'."
  :group 'request)
```

Here are the detailed possible levels:

``` lisp
(defconst request--log-level-def
  '(;; debugging
    (blather . 60) (trace . 50) (debug . 40)
    ;; information
    (verbose . 30) (info . 20)
    ;; errors
    (warn . 10) (error . 0))
  "Named logging levels.")
```

So, to make reques to be ultra verbose this should do: 

``` lisp
(custom-set-variables '(request-log-level 'blather)
                                   '(request-message-level 'blather))
```


## Compatibility / backends

Supported Emacs versions (**status confirmation is in progress**):

| Emacs version           | Does request.el work?        | Tested on Travis CI     |
| ----------------------- | ---------------------------- | ----------------------- |
| GNU Emacs 26.1          | yes (as of this writing)     | yes                     |
| GNU Emacs 25.1          | yes (as of this writing)     | yes                     |
| GNU Emacs 24.5          | yes (as of this writing)     | yes                     |
| GNU Emacs 24.4          | yes (as of this writing)     | yes                     |
|                         |                              |                         |

Supported backends:

| Backends | Remarks           | Multipart Form | Automatic Decompression | Unix Socket |
|----------|-------------------|----------------|-------------------------|-------------|
| url.el   | Included in Emacs |                |                         |             |
| curl     | Reliable          | ✔              | ✔                       | ✔           |


## Related projects

- [leathekd/grapnel · GitHub](https://github.com/leathekd/grapnel):   "HTTP request for Emacs lib built on curl with flexible callback dispatch"

- [cinsk/emacs-curl · GitHub](https://github.com/cinsk/emacs-curl):   "CURL wrapper for Emacs"

- [furl-el - Google Project
  Hosting](http://code.google.com/p/furl-el/):  "A wrapper for url.el that adds a nicer API and the ability to make multipart POST requests."
## License

Request.el is free software under GPL v3. See COPYING file for details.
