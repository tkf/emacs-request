================================================
 Request.el -- Easy HTTP request for Emacs Lisp
================================================

* `Documentation (at GitHub Pages) <http://tkf.github.com/emacs-request/>`_
* `Repository (at GitHub) <https://github.com/tkf/emacs-request>`_
* `Issue tracker (at GitHub) <https://github.com/tkf/emacs-request/issues>`_
* `Travis CI <https://travis-ci.org/#!/tkf/emacs-request>`_ |build-status|

.. |build-status|
   image:: https://secure.travis-ci.org/tkf/emacs-request.png
           ?branch=master
   :target: http://travis-ci.org/tkf/emacs-request
   :alt: Build Status


What is it?
===========

Request.el is a HTTP request library with multiple backends.  It
supports url.el which is shipped with Emacs and curl command line
program.  User can use curl when s/he has it, as curl is more reliable
than url.el.  Library author can use request.el to avoid imposing
external dependencies such as curl to users while giving richer
experience for users who have curl.

As request.el is implemented in extensible manner, it is possible to
implement other backend such as wget.  Also, if future version of
Emacs support linking with libcurl, it is possible to implement a
backend using it.  Libraries using request.el automatically can
use these backend without modifying their code.

Request.el also patches url.el dynamically, to fix bugs in url.el.
See `monkey patches for url.el`_ for the bugs fixed by request.el.
Don't waste your time on url.el.  Use request.el.


Examples
========

GET::

  (request
   "http://search.twitter.com/search.json"
   :params '((q . "emacs awesome"))
   :parser 'json-read
   :success (function*
             (lambda (&key data &allow-other-keys)
               (let* ((tweet (elt (assoc-default 'results data) 0))
                      (text (assoc-default 'text tweet))
                      (user (assoc-default 'from_user_name tweet)))
                 (message "%s says %s" user text)))))

POST::

  (request
   "http://httpbin.org/post"
   :type "POST"
   :data '(("key" . "value") ("key2" . "value2"))
   ;; :data "key=value&key2=value2"  ; this is equivalent
   :parser 'json-read
   :success (function*
             (lambda (&key data &allow-other-keys)
               (message "I sent: %S" (assoc-default 'form data)))))


Compatibility / backends
========================

Supported Emacs versions:

====================== ========================== =================
 Emacs version          Does request.el work?      Continues build
====================== ========================== =================
 GNU Emacs 24.3-devel   yes (as of this writing)   yes
 GNU Emacs 24.2         yes                        yes
 GNU Emacs 23.1         yes                        yes
 GNU Emacs < 23         ?                          no
====================== ========================== =================


Supported backends:

========== ============================================
 Backends   Remarks
========== ============================================
 url.el     Included in Emacs.
 curl       Reliable.  Needed for multipart form POST.
========== ============================================


Monkey patches for url.el
=========================

Patches for following bugs are applied when request.el is loaded.
If the patches are not required for newer Emacs versions, it will
not be applied.

- `#12374 - 24.1.50;
  Incorrect redirect in url-retrieve when URL contains port number -
  GNU bug report logs
  <http://debbugs.gnu.org/cgi/bugreport.cgi?bug=12374>`_

  (patch: `PATCH Fix bug 12374 treat port number when expanding URL
  <http://article.gmane.org/gmane.emacs.devel/155698>`_)


License
=======

Request.el is free software under GPL v3.
See COPYING file for details.
