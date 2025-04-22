(use-modules (haunt asset)
             (haunt builder blog)
             (haunt builder atom)
             (haunt builder assets)
             (haunt builder flat-pages)
             (haunt builder rss)
             (haunt publisher rsync)
             (haunt publisher sourcehut)
             (haunt reader)
             (haunt reader skribe)
             (haunt reader texinfo)
             (haunt reader commonmark)
             (haunt site))

(site #:title "Built with Guile"
      #:domain "example.com"
      #:default-metadata
      '((author . "Eva Luator")
        (email  . "eva@example.com"))
      #:readers (list commonmark-reader texinfo-reader skribe-reader sxml-reader html-reader)
      #:builders (list (blog)
                       (atom-feed)
                       (atom-feeds-by-tag)
                       (rss-feed)
                       (flat-pages)
                       (static-directory "images"))
      #:publishers (list (rsync-publisher #:name 'rsync
                                          #:destination "/tmp/haunt-example")
                         (sourcehut-publisher #:name 'sourcehut)))
