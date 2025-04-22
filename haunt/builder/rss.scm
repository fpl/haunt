;;; Haunt --- Static site generator for GNU Guile
;;; Copyright © 2018 Christopher Lemmer Webber <cwebber@dustycloud.org>
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
;; RSS feed builder.
;;
;;; Code:

(define-module (haunt builder rss)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (sxml simple)
  #:use-module (haunt artifact)
  #:use-module (haunt site)
  #:use-module (haunt post)
  #:use-module (haunt utils)
  #:use-module (haunt html)
  #:use-module (haunt serve mime-types)
  #:use-module (haunt builder atom)
  #:use-module (web uri)
  #:export (rss-feed))

;; Reader beware: this isn't as nice as atom.scm, because rss isn't
;; as nice as atom.  Worse beats better on the play field again...

;; RFC 822 dates are inferior to ISO 8601, but it's
;; what RSS wants, so...
(define (date->rfc822-str date)
  (date->string date "~a, ~d ~b ~Y ~T ~z"))

(define (sxml->xml* sxml port)
  "Write SXML to PORT, preceded by an <?xml> tag."
  (display "<?xml version=\"1.0\" encoding=\"utf-8\"?>" port)
  (sxml->xml sxml port))

(define* (post->rss-item site post #:key (blog-prefix ""))
  "Convert POST into an RSS <item> node."
  (let ((uri (uri->string
              (build-uri (site-scheme site)
                         #:host (site-domain site)
                         #:path (string-append (if (string-prefix? "/" blog-prefix)
                                                   "" "/")
                                               blog-prefix
                                               (if (or (string-null? blog-prefix)
                                                       (string-suffix? "/" blog-prefix))
                                                   "" "/")
                                               (site-post-slug site post)
                                               ".html")))))
    `(item
      (title ,(post-ref post 'title))
      ;; Looks like: <author>lawyer@boyer.net (Lawyer Boyer)</author>
      ,@(let ((email (post-ref post 'email))
              (author (post-ref post 'author)))
          (cond ((and email author)
                 `((author ,(string-append email " (" author ")"))))
                (email
                 `((author ,email)))
                (else '())))
      (pubDate ,(date->rfc822-str (post-date post)))
      (guid ,uri)
      (link ,uri)
      (description ,(sxml->html-string (post-sxml post)))
      ,@(map (lambda (enclosure)
               `(enclosure (@ (title ,(enclosure-title enclosure))
                              (url ,(enclosure-url enclosure))
                              (type ,(enclosure-mime-type enclosure))
                              ,@(map (match-lambda
                                       ((key . value)
                                        (list key value)))
                                     (enclosure-extra enclosure)))))
             (post-ref-all post 'enclosure)))))

(define* (rss-feed #:key
                   (file-name "rss-feed.xml")
                   (subtitle "Recent Posts")
                   (filter posts/reverse-chronological)
                   (max-entries 20)
                   (publication-date (current-date))
                   (blog-prefix ""))
  "Return a builder procedure that renders a list of posts as an RSS
feed.  All arguments are optional:

FILE-NAME: The page file name.

SUBTITLE: The feed subtitle.

FILTER: The procedure called to manipulate the posts list before
rendering.

MAX-ENTRIES: The maximum number of posts to render in the feed.

PUBLICATION-DATE: The feed publication date.

BLOG-PREFIX: The prefix for all post URLs, which is the combination of
the blog's prefix and post prefix."
  (lambda (site posts)
    (serialized-artifact file-name
                         `(rss (@ (version "2.0")
                                  (xmlns:atom "http://www.w3.org/2005/Atom"))
                               (channel
                                (title ,(site-title site))
                                ;; It looks like RSS's description and atom's subtitle
                                ;; are equivalent?
                                (description ,subtitle)
                                (pubDate ,(date->rfc822-str publication-date))
                                (link
                                 ,(string-append (symbol->string
                                                  (site-scheme site))
                                                 "://" (site-domain site) "/"))
                                (atom:link
                                 (@ (href ,(string-append (symbol->string
                                                           (site-scheme site))
                                                          "://" (site-domain site)
                                                          "/" file-name))
                                    (rel "self")
                                    (type "application/rss+xml")))
                                ,@(map (cut post->rss-item site <>
                                            #:blog-prefix blog-prefix)
                                       (take-up-to max-entries
                                                   (filter posts)))))
                         sxml->xml*)))
