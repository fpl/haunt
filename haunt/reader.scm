;;; Haunt --- Static site generator for GNU Guile
;;; Copyright © 2015 David Thompson <davet@gnu.org>
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
;; Post readers.
;;
;;; Code:

;; Hack to mark this module as non-declarative on Guile 3+ (which
;; would otherwise print a warning) but not break when compiling on
;; earlier versions of Guile.
(define-syntax-rule (define-module* name args ...)
  (cond-expand
   (guile-3
    (define-module name
      #:declarative? #f
      args ...))
   (guile
    (define-module name args ...))))

(define-module* (haunt reader)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (sxml simple)
  #:use-module (haunt post)
  #:use-module (haunt utils)
  #:export (make-reader
            reader?
            reader-matcher
            reader-proc
            reader-match?
            reader-find
            reader-read
            read-post
            read-posts

            make-file-extension-matcher
            sxml-reader
            html-reader))

(define-record-type <reader>
  (make-reader matcher proc)
  reader?
  (matcher reader-matcher)
  (proc reader-proc))

(define (reader-match? reader file-name)
  "Return #t if FILE-NAME is a file supported by READER."
  ((reader-matcher reader) file-name))

(define (reader-find readers file-name)
  "Return the first reader in READERS that can parse FILE-NAME, or #f if
there is no such reader."
  (find (cut reader-match? <> file-name) readers))

(define (reader-read reader file-name)
  "Parse FILE-NAME using READER and return two values: an alist of
metadata and an SXML tree."
  ((reader-proc reader) file-name))

(define* (read-post reader file-name #:optional (default-metadata '()))
  "Read a post object from FILE-NAME using READER, merging its
metadata with DEFAULT-METADATA."
  (let-values (((metadata sxml) (reader-read reader file-name)))
    (make-post file-name
               (append metadata default-metadata)
               sxml)))

(define* (read-posts directory keep? readers #:optional (default-metadata '()))
  "Read all of the files in DIRECTORY that match KEEP? as post
objects.  The READERS list must contain a matching reader for every
post."
  (define enter? (const #t))

  (define (leaf file-name stat memo)
    (if (keep? file-name)
        (match (reader-find readers file-name)
          (#f (error "no reader available for post: " file-name))
          (reader (cons (read-post reader file-name default-metadata) memo)))
        memo))

  (define (noop file-name stat result)
    result)

  (define (err file-name stat errno result)
    (error "file processing failed with errno: " file-name errno))

  (file-system-fold enter? leaf noop noop noop err '() directory stat))

;;;
;;; Simple readers
;;;

(define (make-file-extension-matcher ext . exts)
  "Return a procedure that returns #t when a file name ends with
'.EXT' or any additional extension in EXTS."
  (let ((exts (map (lambda (ext) (string-append "." ext)) (cons ext exts))))
    (lambda (file-name)
      (any (lambda (ext) (string-suffix? ext file-name)) exts))))

(define sxml-reader
  (make-reader (make-file-extension-matcher "sxml" "scm")
               (lambda (file-name)
                 (let ((contents (load (absolute-file-name file-name))))
                   (values (alist-delete 'content contents eq?)
                           (assq-ref contents 'content))))))

(define (read-html-post port)
  (values (read-metadata-headers port)
          (let loop ()
            (let ((next-char (peek-char port)))
              (cond
               ((eof-object? next-char)
                '())
               ((char-set-contains? char-set:whitespace next-char)
                (read-char port)
                (loop))
               (else
                (match (xml->sxml port)
                  (('*TOP* sxml) (cons sxml (loop))))))))))

(define html-reader
  (make-reader (make-file-extension-matcher "html")
               (cut call-with-input-file <> read-html-post)))
