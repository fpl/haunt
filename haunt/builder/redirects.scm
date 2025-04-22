;;; Haunt --- Static site generator for GNU Guile
;;; Copyright © 2023 David Thompson <davet@gnu.org>
;;;
;;; This file is part of Haunt.
;;;
;;; Haunt is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Haunt is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Haunt.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Redirect builder.
;;
;;; Code:

(define-module (haunt builder redirects)
  #:use-module (haunt artifact)
  #:use-module (haunt site)
  #:use-module (haunt utils)
  #:use-module (haunt html)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (web uri)
  #:export (redirects))

(define (redirects specs)
  "Return a procedure that transforms a list of redirect tuples in SPECS,
with the form (FROM TO), into a list of pages that trigger a
browser-initiated redirect.  This is a convenient way to redirect
without needing to modify web server configuration to issue 302
permanent redirects.

FROM values must be local page file names, not URLs, but TO values may
be either local page file names or full URLs to other websites."
  (lambda (site posts)
    (define (render-redirect url)
      `((doctype "html")
        (head
         (meta (@ (http-equiv "Refresh")
                  (content ,(string-append "0; url='" url "'")))))
        (body
         "Redirecting to "
         (a (@ (href ,url)) ,url))))

    (map (match-lambda
           ((from to)
            (serialized-artifact from (render-redirect to) sxml->html)))
         specs)))
