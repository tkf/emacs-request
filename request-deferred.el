;;; request-deferred.el --- Wrap request.el by deferred

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; request-deferred.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; request-deferred.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with request-deferred.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'request)
(require 'deferred)

(defun request-deferred (url &rest args)
  (let ((d (deferred:new #'identity)))
    (plist-put args :success (apply-partially #'request-deferred--cb d))
    (plist-put args :error (apply-partially #'request-deferred--cb d))
    (apply #'request url args)
    d))

(defun request-deferred--cb (d &rest args)
  (deferred:callback-post d args))

(provide 'request-deferred)

;;; request-deferred.el ends here
