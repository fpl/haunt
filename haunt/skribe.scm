;;; Haunt --- Static site generator for GNU Guile
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright 2005, 2006 Ludovic Courtès <ludovic.courtes@laas.fr>
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
;; Skribe reader.
;; Much code taken from the excellent Skribilo project.
;;
;;; Code:

(define-module (haunt skribe)
  #:use-module ((system reader) #:renamer (symbol-prefix-proc 'r:))
  #:export (%skribe-reader))

;; Taken from Skribilo
(define (make-colon-free-token-reader tr)
  ;; Stolen from `guile-reader' 0.3.
  "If token reader @var{tr} handles the @code{:} (colon) character, remove it
from its specification and return the new token reader."
  (let* ((spec (r:token-reader-specification tr))
	 (proc (r:token-reader-procedure tr)))
    (r:make-token-reader (filter (lambda (chr)
				   (not (char=? chr #\:)))
				 spec)
			 proc)))

(define &sharp-reader
  ;; The reader for what comes after a `#' character.
  (let* ((dsssl-keyword-reader  ;; keywords Ã  la `#!key'
          (r:make-token-reader #\!
 			       (r:token-reader-procedure
 				(r:standard-token-reader 'keyword)))))
      (r:make-reader (cons dsssl-keyword-reader
			   (map r:standard-token-reader
				'(character srfi-4 vector
				  number+radix boolean
				  srfi30-block-comment
				  srfi62-sexp-comment)))
		     #f ;; use default fault handler
		     'reader/record-positions)))

(define (make-skribe-reader)
  (let ((colon-keywords ;; keywords Ã  la `:key' fashion
	 (r:make-token-reader #\:
			      (r:token-reader-procedure
			       (r:standard-token-reader 'keyword))))
	(symbol-misc-chars-tr
	 ;; Make sure `:' is handled only by the keyword token reader.
	 (make-colon-free-token-reader
	  (r:standard-token-reader 'r6rs-symbol-misc-chars))))

    ;; Note: we use the `r6rs-symbol-*' and `r6rs-number' token readers since
    ;; they consider square brackets as delimiters.
    (r:make-reader (cons* (r:make-token-reader #\# &sharp-reader)
			  colon-keywords
			  symbol-misc-chars-tr
			  (map r:standard-token-reader
			       `(whitespace
				 sexp string r6rs-number
				 r6rs-symbol-lower-case
				 r6rs-symbol-upper-case
				 quote-quasiquote-unquote
				 semicolon-comment
				 skribe-exp)))
		   #f ;; use the default fault handler
		   'reader/record-positions)))

(define %skribe-reader (make-skribe-reader))
