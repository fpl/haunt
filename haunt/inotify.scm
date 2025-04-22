;;;;; Haunt --- Static site generator for GNU Guile
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
;; Inotify bindings.
;;
;;; Code:

(define-module (haunt inotify)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (system foreign)
  #:export (make-inotify
            inotify?
            inotify-watches
            inotify-add-watch!
            inotify-pending-events?
            inotify-read-event
            inotify-watch?
            inotify-watch-id
            inotify-watch-file-name
            inotify-watch-remove!
            inotify-event?
            inotify-event-watch
            inotify-event-type
            inotify-event-cookie
            inotify-event-file-name
            inotify-event-within-directory?))

(define libc (dynamic-link))

(define inotify-init
  (pointer->procedure int (dynamic-func "inotify_init" libc) '()))

(define inotify-add-watch
  (pointer->procedure int (dynamic-func "inotify_add_watch" libc)
                      (list int '* uint32)))

(define inotify-rm-watch
  (pointer->procedure int (dynamic-func "inotify_rm_watch" libc)
                      (list int int)))

(define IN_ACCESS #x00000001) ; file was accessed.
(define IN_MODIFY #x00000002) ; file was modified.
(define IN_ATTRIB #x00000004) ; metadata changed
(define IN_CLOSE_WRITE #x00000008) ; file opened for writing closed
(define IN_CLOSE_NOWRITE #x00000010) ; file not opened for writing closed
(define IN_OPEN #x00000020) ; file was opened
(define IN_MOVED_FROM #x00000040) ; file was moved from X
(define IN_MOVED_TO #x00000080) ; file was moved to Y
(define IN_CREATE #x00000100) ; subfile was created
(define IN_DELETE #x00000200) ; subfile was deleted
(define IN_DELETE_SELF #x00000400) ; self was deleted
(define IN_MOVE_SELF #x00000800) ; self was moved
;; Kernel flags
(define IN_UNMOUNT #x00002000) ; backing fs was unmounted
(define IN_Q_OVERFLOW #x00004000) ; event queue overflowed
(define IN_IGNORED #x00008000) ; file was ignored
;; Special flags
(define IN_ONLYDIR #x01000000) ; only watch if directory
(define IN_DONT_FOLLOW #x02000000) ; do not follow symlink
(define IN_EXCL_UNLINK #x04000000) ; exclude events on unlinked objects
(define IN_MASK_ADD #x20000000) ; add to the mask of an existing watch
(define IN_ISDIR #x40000000) ; event occurred against directory
(define IN_ONESHOT #x80000000) ; only send event once

(define mask/symbol (make-hash-table))
(define symbol/mask (make-hash-table))

(for-each (match-lambda
            ((sym mask)
             (hashq-set! symbol/mask sym mask)
             (hashv-set! mask/symbol mask sym)))
          `((access ,IN_ACCESS)
            (modify ,IN_MODIFY)
            (attrib ,IN_ATTRIB)
            (close-write ,IN_CLOSE_WRITE)
            (close-no-write ,IN_CLOSE_NOWRITE)
            (open ,IN_OPEN)
            (moved-from ,IN_MOVED_FROM)
            (moved-to ,IN_MOVED_TO)
            (create ,IN_CREATE)
            (delete ,IN_DELETE)
            (delete-self ,IN_DELETE_SELF)
            (move-self ,IN_MOVE_SELF)
            (only-dir ,IN_ONLYDIR)
            (dont-follow ,IN_DONT_FOLLOW)
            (exclude-unlink ,IN_EXCL_UNLINK)
            (is-directory ,IN_ISDIR)
            (once ,IN_ONESHOT)))

(define (symbol->mask sym)
  (hashq-ref symbol/mask sym))

(define (mask->event-symbol mask)
  ;; Only check the first 4 bits, of which only one bit should be set
  ;; containing the event type.  The other 4 bits may have additional
  ;; information.
  (hashq-ref mask/symbol (logand #x0000ffff mask)))

(define-record-type <inotify>
  (%make-inotify port buffer buffer-pointer watches)
  inotify?
  (port inotify-port)
  (buffer inotify-buffer)
  (buffer-pointer inotify-buffer-pointer)
  (watches inotify-watches))

(define-record-type <inotify-watch>
  (make-inotify-watch id file-name owner)
  inotify-watch?
  (id inotify-watch-id)
  (file-name inotify-watch-file-name)
  (owner inotify-watch-owner))

(define-record-type <inotify-event>
  (make-inotify-event watch type cookie file-name)
  inotify-event?
  (watch inotify-event-watch)
  (type inotify-event-type)
  (cookie inotify-event-cookie)
  (file-name inotify-event-file-name))

(define (display-inotify inotify port)
  (format port "#<inotify port: ~a>" (inotify-port inotify)))

(define (display-inotify-watch watch port)
  (format port "#<inotify-watch id: ~d file-name: ~a>"
          (inotify-watch-id watch)
          (inotify-watch-file-name watch)))

(define (display-inotify-event event port)
  (format port "#<inotify-event type: ~s cookie: ~d file-name: ~a watch: ~a>"
          (inotify-event-type event)
          (inotify-event-cookie event)
          (inotify-event-file-name event)
          (inotify-event-watch event)))

(set-record-type-printer! <inotify> display-inotify)
(set-record-type-printer! <inotify-watch> display-inotify-watch)
(set-record-type-printer! <inotify-event> display-inotify-event)

(define (make-inotify)
  (let ((fd (inotify-init))
        (buffer (make-bytevector 4096)))
    (%make-inotify (fdopen fd "r")
                   buffer
                   (bytevector->pointer buffer)
                   (make-hash-table))))

(define (inotify-fd inotify)
  (port->fdes (inotify-port inotify)))

(define (absolute-file-name file-name)
  (if (absolute-file-name? file-name)
      file-name
      (string-append (getcwd) "/" file-name)))

(define (inotify-add-watch! inotify file-name modes)
  (let* ((abs-file-name (absolute-file-name file-name))
         (wd (inotify-add-watch (inotify-fd inotify)
                                (string->pointer abs-file-name)
                                (apply logior (map symbol->mask modes))))
         (watch (make-inotify-watch wd abs-file-name inotify)))
    (hashv-set! (inotify-watches inotify) wd watch)
    watch))

(define (inotify-watch-remove! watch)
  (inotify-rm-watch (inotify-fd (inotify-watch-owner watch))
                    (inotify-watch-id watch))
  (hashv-remove! (inotify-watches (inotify-watch-owner watch))
                 (inotify-watch-id watch)))

(define (inotify-pending-events? inotify)
  ;; Sometimes an interrupt happens during the char-ready? call and an
  ;; exception is thrown.  Just return #f in that case and move on
  ;; with life.
  (false-if-exception (char-ready? (inotify-port inotify))))

(define (read-int port buffer)
  (get-bytevector-n! port buffer 0 (sizeof int))
  (bytevector-sint-ref buffer 0 (native-endianness) (sizeof int)))

(define (read-uint32 port buffer)
  (get-bytevector-n! port buffer 0 (sizeof uint32))
  (bytevector-uint-ref buffer 0 (native-endianness) (sizeof uint32)))

(define (read-string port buffer buffer-pointer length)
  (and (> length 0)
       (begin
         (get-bytevector-n! port buffer 0 length)
         (pointer->string buffer-pointer))))

(define (inotify-read-event inotify)
  (let* ((port (inotify-port inotify))
         (buffer (inotify-buffer inotify))
         (wd (read-int port buffer))
         (event-mask (read-uint32 port buffer))
         (cookie (read-uint32 port buffer))
         (len (read-uint32 port buffer))
         (name (read-string port buffer (inotify-buffer-pointer inotify) len)))
    (make-inotify-event (hashv-ref (inotify-watches inotify) wd)
                        (mask->event-symbol event-mask)
                        cookie name)))
