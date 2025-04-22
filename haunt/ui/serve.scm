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

;;; Commentary:
;;
;; Haunt serve sub-command.
;;
;;; Code:

(define-module (haunt ui serve)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 threads)
  #:use-module (haunt config)
  #:use-module (haunt serve web-server)
  #:use-module (haunt site)
  #:use-module (haunt ui)
  #:export (haunt-serve))

(define %linux?
  (string=? (utsname:sysname (uname)) "Linux"))

(define (show-help)
  (format #t "Usage: haunt serve [OPTION]
Start an HTTP server for the current site.~%")
  (display "
  -p, --port             port to listen on")
  (display "
  -h, --host             host address (IP address, \"localhost\", or \"any\")")
  (display "
  -w, --watch            rebuild site when files change")
  (newline)
  (show-common-options-help)
  (newline)
  (display "
  -h, --help             display this help and exit")
  (display "
  -V, --version          display version and exit")
  (newline))

(define %options
  (cons* (option '(#\h "help") #f #f
                 (lambda _
                   (show-help)
                   (exit 0)))
         (option '(#\V "version") #f #f
                 (lambda _
                   (show-version-and-exit "haunt serve")))
         (option '(#\p "port") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'port (string->number* arg) result)))
         (option '(#\b "host") #t #f
                 (lambda (opt name arg result)
                   (define host-addr
                     (match arg
                       ((or "loopback" "localhost") INADDR_LOOPBACK)
                       ("any"      INADDR_ANY)
                       ;; lazy way of doing this...
                       ;; "." or ":" indicates ipv4 or ipv6
                       ((? (cut string-contains <> "."))
                        (inet-pton AF_INET arg))
                       ((? (cut string-contains <> ":"))
                        (inet-pton AF_INET6 arg))
                       (_
                        (format #t "Unrecognized option for --host: ~a\n" arg)
                        (display "Must be an IP address, \"localhost\", or \"any\".\n")
                        (exit 1))))
                   (alist-cons 'host host-addr result)))
         (option '(#\w "watch") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'watch? #t result)))
         %common-options))

(define %default-options
  (append `((port . 8080)
            (host . ,INADDR_LOOPBACK))
          %default-common-options))

(define (call-with-error-handling thunk)
  (catch #t
    thunk
    (lambda (key . args)
      (let ((cep (current-error-port))
            (stack (make-stack #t 1)))
        (display "ERROR: site rebuild failed\n\n" cep)
        (display "Backtrace:\n" cep)
        (display-backtrace stack cep)
        (newline cep)
        (apply display-error (stack-ref stack 0) cep args)
        (newline cep)))))

(define (haunt-serve . args)
  (let* ((opts     (simple-args-fold args %options %default-options))
         (port     (assq-ref opts 'port))
         (host     (assq-ref opts 'host))
         (watch?   (assq-ref opts 'watch?))
         (config   (assq-ref opts 'config))
         (site     (load-config config))
         (doc-root (site-build-directory site)))
    (format #t "serving ~a on port ~d~%" doc-root port)

    (when watch?
      ;; Resolve correct watch module at runtime so that we don't try
      ;; to load inotify bindings on Mac OS or something, which would
      ;; crash.
      (let ((watch (if %linux?
                       (let ((module (resolve-module '(haunt watch linux))))
                         (display "watch: using inotify mode\n")
                         (module-ref module 'watch))
                       (let ((module (resolve-module '(haunt watch fallback))))
                         (display "watch: using fallback mode\n")
                         (module-ref module 'watch)))))
        (call-with-new-thread
         (lambda ()
           (watch (lambda ()
                    (display "rebuilding...\n")
                    (call-with-error-handling
                     (lambda ()
                       (build-site (load-config config)))))
                  (let ((build-dir (string-append (getcwd) "/"
                                                  (site-build-directory site))))
                    (lambda (dir)
                      (not
                       (string-prefix? build-dir dir))))
                  (site-file-filter site))))))
    (serve doc-root #:open-params `(#:port ,port
                                    #:addr ,host))))
