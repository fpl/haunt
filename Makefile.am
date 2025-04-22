## Haunt --- Static site generator for GNU Guile
## Copyright © 2015 David Thompson <davet@gnu.org>
## Copyright © 2015 Mathieu Lirzin <mthl@gnu.org>
## Copyright © 2016 Erik Edrosa <erik.edrosa@gmail.com>
##
## This file is part of Haunt.
##
## Haunt is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## Haunt is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Haunt.  If not, see <http://www.gnu.org/licenses/>.

GOBJECTS = $(SOURCES:%.scm=%.go)

nobase_mod_DATA = $(SOURCES) $(NOCOMP_SOURCES)
nobase_go_DATA = $(GOBJECTS)

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>
# for details.
guile_install_go_files = install-nobase_goDATA
$(guile_install_go_files): install-nobase_modDATA

EXTRA_DIST = $(SOURCES) $(NOCOMP_SOURCES)
GUILE_WARNINGS = -Wunbound-variable -Warity-mismatch -Wformat
SUFFIXES = .scm .go
.scm.go:
	$(AM_V_GEN)$(top_builddir)/pre-inst-env $(GUILE_TOOLS) compile $(GUILE_WARNINGS) -o "$@" "$<"

moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache

bin_SCRIPTS =					\
  scripts/haunt

SOURCES =					\
  haunt/config.scm				\
  haunt/utils.scm				\
  haunt/post.scm				\
  haunt/page.scm				\
  haunt/asset.scm				\
  haunt/artifact.scm				\
  haunt/reader.scm				\
  haunt/publisher.scm				\
  haunt/site.scm				\
  haunt/html.scm				\
  haunt/builder/assets.scm			\
  haunt/builder/atom.scm			\
  haunt/builder/blog.scm			\
  haunt/builder/rss.scm				\
  haunt/builder/redirects.scm			\
  haunt/builder/flat-pages.scm			\
  haunt/reader/texinfo.scm			\
  haunt/publisher/rsync.scm			\
  haunt/publisher/sourcehut.scm			\
  haunt/watch/fallback.scm			\
  haunt/ui.scm					\
  haunt/ui/build.scm				\
  haunt/ui/new.scm				\
  haunt/ui/publish.scm				\
  haunt/ui/serve.scm				\
  haunt/serve/mime-types.scm			\
  haunt/serve/web-server.scm

if HAVE_GUILE_READER

SOURCES +=					\
  haunt/skribe.scm				\
  haunt/skribe/utils.scm			\
  haunt/reader/skribe.scm

endif

if HAVE_GUILE_COMMONMARK

SOURCES +=					\
  haunt/reader/commonmark.scm

endif

if HAVE_INOTIFY

SOURCES += 					\
  haunt/inotify.scm				\
  haunt/watch/linux.scm

endif

TESTS =						\
  tests/helper.scm				\
  tests/html.scm				\
  tests/post.scm				\
  tests/utils.scm

TEST_EXTENSIONS = .scm
SCM_LOG_COMPILER = $(top_builddir)/test-env $(GUILE)
AM_SCM_LOG_FLAGS = --no-auto-compile -L "$(top_srcdir)"

info_TEXINFOS = doc/haunt.texi
doc_haunt_TEXINFOS = doc/fdl-1.3.texi

AM_MAKEINFOHTMLFLAGS = \
  --css-ref=https://dthompson.us/css/dthompson.css \
  --css-include=doc/manual.css

dvi: # Don't build dvi docs

EXTRA_DIST +=					\
  pre-inst-env.in				\
  README					\
  guix.scm					\
  $(TESTS)

SUBDIRS =					\
  example

CLEANFILES =					\
  $(GOBJECTS)					\
  $(TESTS:tests/%.scm=%.log)

publish_tarball: distcheck
	gpg --sign --detach-sign --armor --yes haunt-$(VERSION).tar.gz && \
        scp haunt-$(VERSION).tar.gz haunt-$(VERSION).tar.gz.asc \
	    publish@dthompson.us:/var/www/files/releases/haunt/

publish_docs: html
	scp -r doc/haunt.html publish@dthompson.us:/var/www/files/docs/haunt/$(VERSION)  && \
        ssh publish@dthompson.us ln -sfn /var/www/files/docs/haunt/$(VERSION) /var/www/files/docs/haunt/latest

publish: publish_tarball publish_docs
