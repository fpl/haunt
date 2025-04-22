;;; Haunt --- Static site generator for GNU Guile
;;; Copyright © 2015, 2022 David Thompson <davet@gnu.org>
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

(define-module (test-utils)
  #:use-module (haunt utils)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-64)
  #:use-module (tests helper))

(with-tests "utils"
  (test-equal "flatten, all"
    '(1 2 3 4 5 6)
    (flatten '(1 (2 3 (4) (5 (6))))))

  (test-equal "flatten, limited depth"
    '(1 2 3 4 5 (6))
    (flatten '(1 (2 3 (4) (5 (6)))) 2))

  (test-equal "flat-map"
    '(5 7 9)
    (flat-map (compose list +) '(1 2 3) '(4 5 6)))

  (test-equal "string-split-at, no match"
    '("foo")
    (string-split-at "foo" #\z))

  (test-equal "string-split-at, match"
    '("foo" "bar")
    (string-split-at "foo/bar" #\/))

  (test-equal "file-name-components, empty string"
    '()
    (file-name-components ""))

  (test-equal "file-name-components, root directory"
    '("")
    (file-name-components "/"))

  (test-equal "file-name-components, full file name"
    '("share" "haunt")
    (file-name-components "/share/haunt"))

  (test-equal "join-file-name-components"
    "share/haunt/info/haunt.info"
    (join-file-name-components '("share" "haunt" "info" "haunt.info")))

  (test-equal "absolute-file-name, already absolute"
    "/share/haunt"
    (absolute-file-name "/share/haunt"))

  (test-equal "absolute-file-name, relative file name"
    (string-append (getcwd) "/share/haunt")
    (absolute-file-name "share/haunt"))

  (test-equal "take-up-to, less than n elements"
    '(1 2 3)
    (take-up-to 4 '(1 2 3)))

  (test-equal "take-up-to, more than n elements"
    '(1 2)
    (take-up-to 2 '(1 2 3)))

  (test-equal "string->date*"
    (make-date 0 0 15 06 05 09 2015
               (date-zone-offset (string->date "2015-09-05" "~Y~m~d")))
    (string->date* "2015-09-05 06:15")))
