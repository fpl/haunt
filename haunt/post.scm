;;; Haunt --- Static site generator for GNU Guile
;;; Copyright © 2015 David Thompson <davet@gnu.org>
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
;; Post data type.
;;
;;; Code:

(define-module (haunt post)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-19)
  #:use-module (haunt utils)
  #:export (make-post
            post?
            post-file-name
            post-sxml
            post-metadata
            post-ref
            post-ref-all
            post-slug
            post-slug-v2
            %default-date
            post-date
            post-author
            post-tags
            post-title
            posts/reverse-chronological
            posts/group-by-tag

            register-metadata-parser!
            parse-metadata
            read-metadata-headers))

(define-record-type <post>
  (make-post file-name metadata sxml)
  post?
  (file-name post-file-name)
  (metadata post-metadata)
  (sxml post-sxml))

(define (post-ref post key)
  "Return the metadata corresponding to KEY within POST."
  (assq-ref (post-metadata post) key))

(define (post-ref-all post key)
  "Return a list of all metadata values for KEY within POST."
  (filter-map (match-lambda
                ((k . v)
                 (and (eq? key k) v)))
              (post-metadata post)))

(define char-set:slug
  (char-set-union char-set:letter+digit (char-set #\-)))

(define (post-slug post)
  "Transform the title of POST into a URL slug."
  (or (post-ref post 'slug)
      (string-join (map (lambda (s)
                          (string-filter char-set:slug s))
                        (string-split (string-downcase (post-title post))
                                      char-set:whitespace))
                   "-")))

(define char-set:slug-v2 char-set:letter+digit)
(define char-set:!slug-v2 (char-set-complement char-set:slug-v2))

(define (post-slug-v2 post)
  "Transform the title of POST into a URL slug."
  (or (post-ref post 'slug)
      (string-join (remove string-null?
                           (string-split (string-downcase (post-title post))
                                         char-set:!slug-v2))
                   "-")))

(define %default-date
  (make-date 0 0 0 0 1 1 1970 0)) ; UNIX epoch

(define (post-date post)
  "Return the date for POST, or '%default-date' if no date is
specified."
  (or (post-ref post 'date) %default-date))

(define (post-time post)
  (date->time-utc (post-date post)))

(define (posts/reverse-chronological posts)
  "Returns POSTS sorted in reverse chronological order."
  (sort posts
        (lambda (a b)
          (time>? (post-time a) (post-time b)))))

(define (post-author post)
  "Return the author of POST, or #f if no author is specified."
  (post-ref post 'author))

(define (post-tags post)
  "Return list of tags for POST, or the empty list if no tags are
specified."
  (or (post-ref post 'tags) '()))

(define (post-title post)
  "Return the title of POST, or #f if no title is specified."
  (post-ref post 'title))

(define (posts/group-by-tag posts)
  "Return an alist of tags mapped to the posts that used them."
  (let ((table (make-hash-table)))
    (for-each (lambda (post)
                (for-each (lambda (tag)
                            (let ((current (hash-ref table tag)))
                              (if current
                                  (hash-set! table tag (cons post current))
                                  (hash-set! table tag (list post)))))
                          (post-tags post)))
              posts)
    (hash-fold alist-cons '() table)))

;;;
;;; Metadata
;;;

(define %metadata-parsers
  (make-hash-table))

(define (metadata-parser key)
  (or (hash-ref %metadata-parsers key) identity))

(define (register-metadata-parser! name parser)
  (hash-set! %metadata-parsers name parser))

(define (parse-metadata key value)
  ((metadata-parser key) value))

(define (read-metadata-headers port)
  (let loop ((metadata '()))
    (let ((line (read-line port)))
      (cond
       ((eof-object? line)
        (error "end of file while reading metadata: " (port-filename port)))
       ((string=? line "---")
        metadata)
       (else
        (match (map string-trim-both (string-split-at line #\:))
          (((= string->symbol key) value)
           (loop (alist-cons key (parse-metadata key value) metadata)))
          (_ (error "invalid metadata format: " line))))))))

(register-metadata-parser!
 'tags
 (lambda (str)
   (map string-trim-both (string-split str #\,))))

(register-metadata-parser! 'date string->date*)
