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
;; Inotify-based file watching for Linux.
;;
;;; Code:

(define-module (haunt watch linux)
  #:use-module (haunt inotify)
  #:use-module (haunt site)
  #:use-module (ice-9 ftw)
  #:export (watch))

;; TODO: Detect new directories and watch them, too.
(define (watch thunk check-dir? check-file?)
  (let ((inotify (make-inotify)))
    (define (no-op name stat result) result)
    (define (watch-directory name stat result)
      (and (check-dir? name)
           (inotify-add-watch! inotify name
                               '(create delete close-write moved-to moved-from))
           #t))
    ;; Drop .scm extension, remove working directory,
    ;; and transform into a symbolic module name.
    (define (file-name->module-name file-name)
      (map string->symbol
           (string-split (string-drop (string-take file-name
                                                   (- (string-length file-name)
                                                      4))
                                      (+ (string-length (getcwd)) 1))
                         #\/)))
    (file-system-fold watch-directory no-op no-op no-op no-op no-op #t (getcwd))
    (let loop ((processed-event? #f))
      (cond
       ((inotify-pending-events? inotify)
        (let* ((event (inotify-read-event inotify))
               (type (inotify-event-type event))
               (file-name (string-append (inotify-watch-file-name
                                          (inotify-event-watch event))
                                         "/"
                                         (inotify-event-file-name event))))
          (if (and (check-dir? file-name) (check-file? file-name))
              (let ((action (case type
                              ((create) "create")
                              ((delete) "delete")
                              ((close-write) "write")
                              ((moved-to moved-from) "move"))))
                (format #t "watch: observed ~a '~a'~%" action file-name)
                ;; Reload Scheme modules when they are changed.
                (when (%search-load-path file-name)
                  (let ((module (resolve-module
                                 (file-name->module-name file-name))))
                    (when (module-filename module)
                      (format #t "watch: reload module ~s~%"
                              (module-name module))
                      (reload-module module))))
                (loop #t))
              (loop processed-event?))))
       (processed-event?
        (thunk)
        (loop #f))
       (else
        (sleep 1)
        (loop #f))))))
