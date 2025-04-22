;;; Haunt --- Static site generator for GNU Guile
;;; Copyright © 2015 David Thompson <davet@gnu.org>
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
;; Skribe helper procedures.
;;
;;; Code:

(define-module (haunt skribe utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:export (post

            p blockquote em
            h1 h2 h3 h4 h5 h6
	    section
	    nav aside
            code pre strong
            ul ol li dl dt dd
            anchor
            image
            source-code

            make-date*))

(define (post . metadata+sxml)
  "Create a new Skribe post by parsing out the metadata and SXML
contents from METADATA+SXML."
  (let loop ((stuff metadata+sxml)
             (metadata '()))
    (match stuff
      (() (values metadata '()))
      (((and (? keyword?) (= keyword->symbol key)) value . rest)
       (loop rest (alist-cons key value metadata)))
      (_ (values metadata stuff)))))

;; Basic SXML constructors.
(define-syntax-rule (define-simple-sxml-constructors tag ...)
  (begin
    (define (tag . contents)
      `(tag ,@contents)) ...))

(define-simple-sxml-constructors
  p blockquote
  em strong
  code samp pre kbd var
  cite dfn abbr
  h1 h2 h3 h4 h5 h6
  section
  nav aside
  ul ol li dl dt dd)

(define (anchor text uri)
  "Return an anchor SXML node that contains TEXT and points to to URI."
  `(a (@ (href ,uri)) ,text))

(define* (image uri #:key (alt-text ""))
  "Return an image SXML node that points to a URI for an image.
Optionally, the ALT-TEXT keyword argument may be a string that
contains a description of the image."
  `(img (@ (src ,uri) (alt ,alt-text))))

(define (source-code . code)
  "Return an SXML node that wraps CODE in a 'pre' and 'code' tag to
create a code block."
  `(pre (code ,code)))

(define* (make-date* year month day #:optional (hour 0) (minute 0))
  "Create a SRFI-19 date for the given YEAR, MONTH, DAY, HOUR (24-hour
format), and MINUTE."
  (let ((tzoffset (tm:gmtoff (localtime (time-second (current-time))))))
    (make-date 0 0 minute hour day month year tzoffset)))
