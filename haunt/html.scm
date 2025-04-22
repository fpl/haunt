;;; Haunt --- Static site generator for GNU Guile
;;; Copyright © 2015, 2025 David Thompson <davet@gnu.org>
;;; Copyright © 2024 Daniel Meißner <dan_m@posteo.de>
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
;; SXML to HTML conversion.
;;
;;; Code:

(define-module (haunt html)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:export (sxml->html
            sxml->html-string))

(define* (sxml->html tree #:optional (port (current-output-port)))
  "Write the serialized HTML form of @var{tree} to @var{port}."
  (define (write-escaped-string str)
    (define (escape ch)
      (case ch
        ((#\") (put-string port "&quot;"))
        ((#\&) (put-string port "&amp;"))
        ((#\<) (put-string port "&lt;"))
        ((#\>) (put-string port "&gt;"))
        (else (put-char port ch))))
    (string-for-each escape str))
  (define (obj->string x)
    (match x
      ((? string?) x)
      ((? symbol?) (symbol->string x))
      ((? number?) (number->string x))
      (_
       (call-with-output-string
         (lambda (port)
           (display x port))))))
  (define (write-escaped x)
    (write-escaped-string (obj->string x)))
  (define (write-raw x)
    (put-string port (obj->string x)))
  (define (write-attribute attr value)
    (put-string port (symbol->string attr))
    (put-string port "=\"")
    (write-escaped value)
    (put-char port #\"))
  (define (write-element tag attrs body)
    (let ((tag-str (symbol->string tag)))
      (put-char port #\<)
      (put-string port tag-str)
      (let lp ((attrs attrs))
        (match attrs
          (() (values))
          ((((? symbol? attr) value) . rest)
           (put-char port #\space)
           (write-attribute attr value)
           (lp rest))))
      (cond
       ((null? body)
        (case tag
          ;; Tags that self-close like <br />
          ((area base br col command embed hr img input
                 keygen link meta param source track wbr)
           (put-string port " />"))
          (else
           (put-string port "></")
           (put-string port tag-str)
           (put-char port #\>))))
       (else
        (put-char port #\>)
        (case tag
          ;; The body of <script> and <style> tags are plain text and
          ;; *not* escaped.
          ((script style)
           (for-each write-raw body))
          (else
           (for-each write-tree body)))
        (put-string port "</")
        (put-string port tag-str)
        (put-char port #\>)))))
  (define (write-doctype doctype)
    (put-string port "<!DOCTYPE ")
    (put-string port doctype)
    (put-char port #\>))
  (define (write-tree tree)
    (match tree
      (() (values))
      (('doctype (? string? type))
       (write-doctype type))
      (((? symbol? tag) ('@ . attrs) . body)
       (write-element tag attrs body))
      (((? symbol? tag) . body)
       (write-element tag '() body))
      ((? pair? elems)
       (for-each write-tree elems))
      (x (write-escaped x))))
  (write-tree tree))

(define (sxml->html-string sxml)
  "Render @var{sxml} as an HTML string."
  (call-with-output-string
   (lambda (port)
     (sxml->html sxml port))))
