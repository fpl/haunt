;;; Haunt --- Static site generator for GNU Guile
;;; Copyright © 2015-2024 David Thompson <davet@gnu.org>
;;; Copyright © 2016 Christine Lemmer-Webber <cwebber@dustycloud.org>
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
;; Simple static web pages.
;;
;;; Code:

(define-module (haunt builder flat-pages)
  #:use-module (haunt artifact)
  #:use-module (haunt html)
  #:use-module (haunt post)
  #:use-module (haunt reader)
  #:use-module (haunt site)
  #:use-module (haunt utils)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-11)
  #:export (flat-pages))

(define (ugly-page-template site metadata body)
  `((doctype "html")
    (head
     (meta (@ (charset "utf-8")))
     (title ,(string-append (or (assq-ref metadata 'title) "Untitled")
                            " — " (site-title site))))
    (body
     (h1 ,(site-title site))
     ,body)))

(define* (flat-pages #:optional
                     (directory "pages")
                     #:key
                     (template ugly-page-template)
                     (prefix "/"))
  "Return a procedure that parses the files in DIRECTORY and returns a
list of HTML pages, one for each file.  The files are parsed using the
readers configured for the current site.  The structure of DIRECTORY
is preserved in the resulting pages and may be optionally nested
within the directory PREFIX.

The content of each flat page is inserted into a complete HTML
document by the TEMPLATE procedure.  This procedure takes three
arguments: the site object, the page title string, and an SXML tree of
the page body.  It returns one value: a new SXML tree representing a
complete HTML page that presumably wraps the page body."
  (lambda (site posts)
    ;; Recursively scan the directory and generate a page for each
    ;; file found.
    (define (enter? file-name stat memo) #t)
    (define (noop file-name stat memo) memo)
    (define keep? (site-file-filter site))
    (define (leaf file-name stat memo)
      (if (keep? file-name) (cons file-name memo) memo))
    (define (err file-name stat errno memo)
      (error "flat page directory scanning failed" file-name errno))
    (define src-files
      (file-system-fold enter? leaf noop noop noop err '() directory))
    (define (strip-extension file-name)
      (basename file-name
                (string-append "." (file-extension file-name))))
    (map (lambda (file-name)
           (match (reader-find (site-readers site) file-name)
             (#f (error "no reader available for page" file-name))
             (reader
              (let-values (((metadata body) (reader-read reader file-name)))
                (let* ((index? (equal? (assq-ref metadata 'index) "true"))
                       (dir (substring (dirname file-name)
                                       (string-length directory)))
                       (out (string-append prefix dir
                                           (if (string-null? dir) "" "/")
                                           (strip-extension file-name)
                                           (if index? "/index" "")
                                           ".html")))
                  (serialized-artifact out (template site metadata body)
                                       sxml->html))))))
         src-files)))
