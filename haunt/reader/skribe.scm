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
;; Skribe post reader.
;;
;;; Code:

;; Hack to mark this module as non-declarative on Guile 3+ (which
;; would otherwise print a warning) but not break when compiling on
;; earlier versions of Guile.
(define-syntax-rule (define-module* name args ...)
  (cond-expand
   (guile-3
    (define-module name
      #:declarative? #f
      args ...))
   (guile
    (define-module name args ...))))

(define-module* (haunt reader skribe)
  #:use-module (haunt reader)
  #:use-module (haunt skribe)
  #:use-module (haunt skribe utils)
  #:use-module (haunt utils)
  #:export (make-skribe-reader
            skribe-reader))

(define* (make-skribe-reader #:key (modules '((haunt skribe utils))))
  "Return a new Skribe post reader that imports MODULES by default
before reading a document."
  (make-reader (make-file-extension-matcher "skr")
               (lambda (file)
                 (let ((file (absolute-file-name file)))
                   (save-module-excursion
                    (lambda ()
                      (set-current-module (make-user-module modules))
                      (load file %skribe-reader)))))))

(define skribe-reader (make-skribe-reader))
