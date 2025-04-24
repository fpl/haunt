;;; Haunt --- Static site generator for GNU Guile
;;; Copyright © 2025 Francesco P Lovergine <pobox@lovergine.com>
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
;; Tag pages builder.
;;
;;; Code:

(define-module (haunt builder tag-pages)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (haunt artifact)
  #:use-module (haunt site)
  #:use-module (haunt post)
  #:use-module (haunt utils)
  #:use-module (haunt html)
  #:use-module (haunt builder blog)
  #:export (tag-pages
            tag-index))

(define* (tag-pages #:key
                    (theme #f)
                    prefix
                    (title "Posts by Tag")
                    (filter posts/reverse-chronological)
                    (posts-per-page #f))
  "Return a procedure that builds HTML pages for each tag. Each page
contains a list of posts with that tag, sorted according to FILTER.

THEME: The theme object to use for rendering
PREFIX: The URL prefix for the tag pages
TITLE: The title prefix for tag pages
FILTER: A procedure to sort the posts
POSTS-PER-PAGE: How many posts to include per page (optional)"
  (lambda (site posts)
    (define tag-groups (posts/group-by-tag posts))
    (define theme* (or theme (theme #:name "Default")))

    (define (tag-file-name tag)
      (string-append "tags/"
                     tag
                     ".html"))

    (define (make-tag-page tag posts)
      (let ((file-name (tag-file-name tag))
            (page-title (string-append title ": " tag)))
        (serialized-artifact file-name
                            (with-layout theme* site page-title
                                        (render-collection theme* site page-title
                                                          (filter posts)
                                                          ""))
                            sxml->html)))

    (map (match-lambda
           ((tag . posts)
            (make-tag-page tag posts)))
         tag-groups)))

(define* (tag-index #:key
                    (theme #f)
                    prefix
                    (title "Tag Index")
                    (file-name "tags/index.html"))
  "Return a procedure that builds an index page listing all tags with
the count of posts for each tag.

THEME: The theme object to use for rendering
PREFIX: The URL prefix for the tag links
TITLE: The title for the tag index page
FILE-NAME: The file name for the tag index page"
  (lambda (site posts)
    (define tag-groups (posts/group-by-tag posts))
    (define theme* (or theme (theme #:name "Default")))

    (define (sort-tag-groups groups)
      (sort groups
            (lambda (a b)
              (string<? (car a) (car b)))))

    (define (tag-file-name tag)
      (string-append "/tags/"
                     tag
                     ".html"))

    (define (tag-feed-name tag)
      (string-append "/feeds/tags/" tag ".xml"))

    (define (render-tag-list)
      `((h1 ,title)
        (ul (@ (id "tags-index"))
         ,@(map (lambda (tag-group)
                 (match tag-group
                   ((tag . posts)
                    `(li
                      (a (@ (href ,(tag-file-name tag)))
                         ,tag
                         " (" ,(number->string (length posts)) ")")
                      " "
                      (a (@ (href ,(tag-feed-name tag))
                            (class "feed-icon")
                            (title ,(string-append "Atom feed for tag: " tag)))
                         "[feed]")))))
               (sort-tag-groups tag-groups)))))

    (serialized-artifact file-name
                        (with-layout theme* site title
                                     (render-tag-list))
                        sxml->html)))

