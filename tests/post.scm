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

(define-module (test-post)
  #:use-module (haunt post)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-64)
  #:use-module (tests helper))

(define (make-date* year month day)
  (make-date 0 0 0 0 day month year 0))

(define %tzoffset
  (date-zone-offset (string->date "2015-09-05" "~Y~m~d")))

(with-tests "post"
  (test-equal "post-ref"
    '(hello test)
    (post-ref (make-post "foo.skr" '((tags hello test)) '()) 'tags))

  (test-equal "post-slug"
    "hello-world"
    (post-slug (make-post "foo.skr" '((title . "Hello, world!")) '())))

  (test-equal "post-date, no date metadata"
    %default-date
    (post-date (make-post "foo.skr" '() '())))

  (let ((date (make-date* 2015 10 15)))
    (test-equal "post-date, date metadata"
      date
      (post-date (make-post "foo.skr" `((date . ,date)) '()))))

  (let ((oldest (make-post "foo.skr" `((date . ,(make-date* 2015 10 13))) '()))
        (newest (make-post "bar.skr" `((date . ,(make-date* 2015 10 15))) '()))
        (middle (make-post "baz.skr" `((date . ,(make-date* 2015 10 14))) '())))
    (test-equal "posts/reverse-chronological"
      (list newest middle oldest)
      (posts/reverse-chronological (list oldest newest middle))))

  (let ((example-post (make-post "foo.skr"
                        '((author . "Eva Luator")
                          (tags "foo" "bar" "baz")
                          (title . "Hello, world!"))
                        '())))
    (test-equal "post-author, author metadata"
      "Eva Luator"
      (post-author example-post))
    (test-equal "post-tags, tags metadata"
      '("foo" "bar" "baz")
      (post-tags example-post))
    (test-equal "post-title, title metadata"
      "Hello, world!"
      (post-title example-post)))

  (let ((no-metadata-post (make-post "foo.skr" '() '())))
    (test-equal "post-author, no metadata"
      #f
      (post-author no-metadata-post))
    (test-equal "post-tags, no metadata"
      '()
      (post-tags no-metadata-post))
    (test-equal "post-title, no metadata"
      #f
      (post-title no-metadata-post)))

  (let ((foo-post (make-post "foo.skr" '((tags "foo")) '()))
        (another-foo-post (make-post "another-foo.skr" '((tags "foo")) '()))
        (bar-post (make-post "bar.skr" '((tags "bar")) '())))
    (test-equal "posts/group-by-tag"
      `(("foo" ,foo-post ,another-foo-post) ("bar" ,bar-post))
      (posts/group-by-tag (list another-foo-post foo-post bar-post))))

  (test-equal "parse-metadata, tags"
    '("foo" "bar" "baz")
    (parse-metadata 'tags "foo, bar, baz"))

  (test-equal "parse-metadata, date"
    (make-date 0 0 30 22 15 10 2015 %tzoffset)
    (parse-metadata 'date "2015-10-15 22:30"))

  (test-equal "read-metadata-headers"
    `((tags "foo" "bar" "baz")
      (date . ,(make-date 0 0 30 22 15 10 2015 %tzoffset))
      (title . "Hello, World!"))
    (call-with-input-string "title: Hello, World!
date: 2015-10-15 22:30
tags: foo, bar, baz
---
"
      read-metadata-headers))
  )
