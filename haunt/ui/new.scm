;;; Haunt --- Static site generator for GNU Guile
;;; Copyright © 2024 jgart <jgart@dismail.de>
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
;; Haunt new sub-command.
;;
;;; Code:

(define-module (haunt ui new)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (haunt ui)
  #:use-module (haunt utils)
  #:export (haunt-new))

(define (show-help)
  (format #t "Usage: haunt new [NAME]
Create a new Haunt project.~%")
  (display "
  -p, --project-dir       project directory")
  (display "
  -t, --title             project title")
  (display "
  -d, --domain            project domain")
  (display "
  -a, --author            project author")
  (display "
  -e, --email             project email")
  (show-common-options-help)
  (newline)
  (display "
  -h, --help             display this help and exit")
  (display "
  -V, --version          display version information and exit")
  (newline))

(define %options
  (list (option '(#\h "help") #f #f
		(lambda _
		  (show-help)
		  (exit 0)))
	(option '(#\V "version") #f #f
		(lambda _
		  (show-version-and-exit "haunt new")))
        (option '(#\p "project-dir") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'project-dir arg result)))
        (option '(#\t "title") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'title arg result)))
        (option '(#\d "domain") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'domain arg result)))
        (option '(#\a "author") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'author arg result)))
        (option '(#\e "email") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'email arg result)))))

(define %default-options
  (append '((project-dir . "blog")
            (title . "Built with Guile")
            (domain . "example.com")
            (author . "Eva Luator")
            (email . "eva@example.com"))
          %default-common-options))

(define (write-template project-dir build-file title domain author email)
  (let ((path (string-append project-dir "/" build-file)))
    (call-with-output-file path
      (lambda (port)
        (pretty-print
         '(use-modules (haunt asset)
		       (haunt builder blog)
		       (haunt builder atom)
		       (haunt builder assets)
		       (haunt reader commonmark)
		       (haunt site))
         port)
        (newline port)
        (pretty-print
         `(site #:title ,title
	        #:domain ,domain
	        #:default-metadata
	        '((author . ,author)
                  (email  . ,email))
	        #:readers (list commonmark-reader)
	        #:builders (list (blog)
			         (atom-feed)
			         (atom-feeds-by-tag)
			         (static-directory "images")))
         port)))))

(define %first-post-file-name "hello.md")

(define %first-post-contents
  "title: First post!
date: 2018-03-13 18:00
tags: hello
summary: hello!
---

Hello, world!
")

(define (write-first-post project-dir)
  (let* ((first-post-file %first-post-file-name)
	 (first-post-contents %first-post-contents)
	 (output-path
	  (string-append project-dir "/posts/" first-post-file)))
    (call-with-output-file output-path
      (lambda (port)
	(display first-post-contents port)))))

(define (build-template project-dir build-file title domain author email)
  (mkdir-p (string-append project-dir "/images"))
  (mkdir-p (string-append project-dir "/posts"))
  (write-template project-dir build-file title domain author email)
  (write-first-post project-dir)
  (format #t "new blog was created in project directory ~a\n" project-dir))

(define (haunt-new . args)
  (let* ((opts (simple-args-fold args %options %default-options))
         (project-dir (assq-ref opts 'project-dir))
	 (build-file  (assq-ref opts 'config))
         (title       (assq-ref opts 'title))
         (domain      (assq-ref opts 'domain))
         (author      (assq-ref opts 'author))
         (email       (assq-ref opts 'email)))
    (build-template project-dir build-file title domain author email)))
