;;; Haunt --- Static site generator for GNU Guile
;;; Copyright © 2023 Filip Lajszczak <filip@lajszczak.dev>
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
;; Sourcehut publisher.
;;
;;; Code:

(define-module (haunt publisher sourcehut)
  #:use-module (haunt config)
  #:use-module (haunt publisher)
  #:use-module (haunt site)
  #:export (sourcehut-publisher))

(define* (sourcehut-publisher #:key
                              (name %default-publisher-name)
                              (hut %hut)
                              (tar %tar))
  "Return a new publisher named NAME that publishes a site to
Sourcehut pages.  Passing HUT and/or TAR overrides the default
executables used."
  (define (publish site)
    (let ((tarball (string-append (or (getenv "TMPDIR") "/tmp")
                                  "/haunt-publish-sourcehut-"
                                  (number->string (current-time))
                                  ".tar.gz")))
      (dynamic-wind
        (lambda () #t)
        (lambda ()
          (run-command tar
                       "--directory" (site-absolute-build-directory site)
                       "--create" "--gzip"
                       "--file" tarball
                       ".")
          (run-command hut "pages"
                       "publish"
                       "--domain" (site-domain site)
                       tarball))
        (lambda ()
          (when (file-exists? tarball)
            (delete-file tarball))))))
  (make-publisher name publish))
