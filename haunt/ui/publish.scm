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
;; Haunt publish sub-command.
;;
;;; Code:

(define-module (haunt ui publish)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 match)
  #:use-module (haunt config)
  #:use-module (haunt publisher)
  #:use-module (haunt site)
  #:use-module (haunt ui)
  #:export (haunt-publish))

(define (show-help)
  (format #t "Usage: haunt publish [OPTION] [NAME]
Deploy the site using the publisher named NAME, or '~a' if no name is
specified.~%" %default-publisher-name)
  (show-common-options-help)
  (newline)
  (display "
  -h, --help             display this help and exit")
  (display "
  -V, --version          display version information and exit")
  (newline))

(define %options
  (cons* (option '(#\h "help") #f #f
                 (lambda _
                   (show-help)
                   (exit 0)))
         (option '(#\V "version") #f #f
                 (lambda _
                   (show-version-and-exit "haunt publish")))
         %common-options))

(define (haunt-publish . args)
  (let* ((opts (args-fold args %options
                          (lambda (opt name arg result)
                            (leave "~A: unrecognized option" name))
                          (lambda (arg result)
                            (if (assq-ref result 'name)
                                (leave "extraneous argument: ~A" arg)
                                (cons (cons 'name (string->symbol arg)) result)))
                          %default-common-options))
         (site (load-config (assq-ref opts 'config)))
         (name (or (assq-ref opts 'name) %default-publisher-name)))
    (format #t "publishing to '~a'...~%" name)
    (publish-site site name)
    (display "publish complete!\n")))
