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
;; Rsync publisher.
;;
;;; Code:

(define-module (haunt publisher rsync)
  #:use-module (haunt config)
  #:use-module (haunt publisher)
  #:use-module (haunt site)
  #:use-module (haunt utils)
  #:export (%default-rsync-flags
            rsync-publisher))

(define %default-rsync-flags
  '("--compress" "--delete" "--progress" "--recursive" "--verbose"))

(define* (rsync-publisher #:key destination user host
                          (name %default-publisher-name)
                          (flags %default-rsync-flags)
                          (rsync %rsync))
  "Return a new publisher named NAME that publishes a site to
DESTINATION, either locally or to a remote host if HOST and/or USER
arguments are specified.  Passing RSYNC overrides the default rsync
executable used.  Passing FLAGS overrides the default set of command
line flags used."
  (let ((dest (cond
               ((and user host)
                (string-append user "@" host ":" destination))
               (host
                (string-append host ":" destination))
               (else
                destination))))
    (define (publish site)
      ;; Trailing slash so rsync copies the contents of the site build
      ;; directory to the destination but doesn't include the
      ;; directory itself.
      ;;
      ;; Good: /my/destination/index.html
      ;; Bad: /my/destination/site/index.html
      (let ((build-dir (string-append (site-absolute-build-directory site)
                                      "/")))
        (apply run-command rsync (append flags (list build-dir dest)))))
    (make-publisher name publish)))
