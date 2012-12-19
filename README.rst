================================================
 Request.el -- Easy HTTP request for Emacs user
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
backend using it.  Libraries using request.el automatically get can
use these backend without modifying their code.


Examples
========

GET::

  (request
   "http://search.twitter.com/search.json?q=emacs"
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
