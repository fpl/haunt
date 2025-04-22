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
;; Fallback file update watcher that uses a naive tree walking
;; implementation.
;;
;;; Code:

(define-module (haunt watch fallback)
  #:use-module (haunt site)
  #:use-module (ice-9 ftw)
  #:export (watch))

(define (watch thunk check-dir? check-file?)
  (define cwd (getcwd))

  (define (any-files-changed? time)
    (define (enter? name stat result)
      ;; Don't bother descending if we already know that a file has
      ;; changed.
      (and (not result) (check-dir? name)))

    (define (leaf name stat result)
      ;; Test if file has been modified since the last time we
      ;; checked.
      (or result
          (and (check-file? name)
               (or (>= (stat:mtime stat) time)
                   (>= (stat:ctime stat) time)))))

    (define (no-op name stat result) result)

    (file-system-fold enter? leaf no-op no-op no-op no-op #f cwd))

  (let loop ((time (current-time)))
    (when (any-files-changed? time)
      (display "watch: file changes detected")
      (thunk))
    (let ((next-time (current-time)))
      (sleep 1)
      (loop next-time))))
