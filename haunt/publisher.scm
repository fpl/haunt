;;; Haunt --- Static site generator for GNU Guile
;;; Copyright © 2022 David Thompson <davet@gnu.org>
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
;; Site publishing abstraction.
;;
;;; Code:

(define-module (haunt publisher)
  #:use-module (srfi srfi-9)
  #:export (%default-publisher-name
            make-publisher
            publisher?
            publisher-name
            publisher-proc
            publish
            run-command))

(define %default-publisher-name 'production)

(define-record-type <publisher>
  (%make-publisher name proc)
  publisher?
  (name publisher-name)
  (proc publisher-proc))

(define (make-publisher name proc)
  (unless (symbol? name)
    (error "expected symbol for publisher name" name))
  (%make-publisher name proc))

(define (publish publisher site)
  ((publisher-proc publisher) site))

(define (run-command program . args)
  (let ((status (apply system* program args)))
    (unless (zero? status)
      (error "program exited with non-zero status"
             (status:exit-val status) program args))))
