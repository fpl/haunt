;;; Haunt --- Static site generator for GNU Guile
;;; Copyright © 2024 Daniel Meißner <dan_m@posteo.de>
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

(define-module (test-html)
  #:use-module (haunt html)
  #:use-module (srfi srfi-64)
  #:use-module (tests helper))

(with-tests "html"
  (test-equal "content of raw elements are not escaped"
    "<script>console.log(\"Hello, world!\");</script>"
    (sxml->html-string '(script "console.log(\"Hello, world!\");"))))
