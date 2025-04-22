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
;; Texinfo post reader.
;;
;;; Code:

(define-module (haunt reader texinfo)
  #:use-module (texinfo)
  #:use-module (texinfo html)
  #:use-module (haunt post)
  #:use-module (haunt reader)
  #:use-module (haunt utils)
  #:export (texinfo-reader))

(define texi->shtml
  (compose stexi->shtml texi-fragment->stexi))

(define texinfo-reader
  (make-reader (make-file-extension-matcher "texi")
               (lambda (file)
                 (call-with-input-file file
                   (lambda (port)
                     (values (read-metadata-headers port)
                             (texi->shtml port)))))))
