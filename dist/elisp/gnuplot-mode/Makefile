# Generated automatically from Makefile.in by configure.
#
# Makefile.in for gnuplot-mode
#
# Based on original Makefile
# by Lars Hecking  <lhecking@nmrc.ucc.ie> 1999-02-25
#
# Modified to install .el files along with .elc files
# BR May 17 2002

SHELL = /bin/sh

prefix = /usr/local
datadir = ${prefix}/share
lispdir = $(datadir)/emacs/site-lisp
srcdir = .
subdir = lisp
top_builddir = .
top_srcdir = .


EMACS = emacs
MAKEINFO = /usr/local/bin/makeinfo
LATEX = latex
DVIPS = dvips
INSTALL = /usr/local/gnu/bin/install -c
INSTALL_DATA = ${INSTALL} -m 644

# Command to byte-compile
BYTEC = $(EMACS) -batch -q -no-site-file -l $(srcdir)/dot.el -f batch-byte-compile

ELCS = info-look.elc gnuplot.elc gnuplot-gui.elc

DIST_COMMON = ChangeLog Makefile.dst Makefile.in README aclocal.m4 configure \
configure.in

EXTRA_DIST = dot.el dotemacs gnuplot-gui.el gnuplot.el gnuplot.el.old \
gpelcard.tex info-look.20.2.el info-look.20.3.el

DISTFILES = $(DIST_COMMON) $(EXTRA_DIST)

default: $(ELCS)

gnuplot.elc: gnuplot.el
	test "$(srcdir)" = "$(top_builddir)" || cp $(srcdir)/gnuplot.el .
	$(BYTEC) gnuplot.el

gnuplot-gui.elc: gnuplot.elc gnuplot-gui.el
	test "$(srcdir)" = "$(top_builddir)" || cp $(srcdir)/gnuplot-gui.el .
	$(BYTEC) gnuplot-gui.el

## There are many possibilities for info-look:
##
##     EMACS version            use
##  -----------------------------------------
##   Emacs or XEmacs 19    info-look.20.2.el
##   Emacs 20.2 or less    info-look.20.2.el
##   Emacs 20.3            nothing
##   XEmacs 20+            info-look.20.3.el
##
## want to use my modified version even if 20.2 is installed because a
## bug is fixed
##
## the first 6 lines attempt to ascertain the version number of
## $(EMACS), then multiply by 100 to convert it to an integer for the
## sake of the integer comparisons in the following lines.  Is this a
## hassle, or what?!
##
MESSAGE = compiling info-look for $(EMACS) $$vnum
info-look.elc:  info-look.20.2.el info-look.20.3.el
	@if [ $(EMACS) = "emacs" ]; \
	  then vnum=`emacs  --version | grep 'Emacs [12]' | awk '{print $$3}'`; \
	  else vnum=`xemacs --version | grep 'Emacs [12]' | awk '{print $$2}'`; \
	fi; \
	vn=`echo "$$vnum" | awk 'BEGIN{FS="."}{print $$1 "." $$2}'`; \
	version=`echo "$$vn" | awk '{print 100*$$1}'`; \
	if [ $(EMACS) = "emacs" -a $$version -ge 2030 ]; \
	  then echo "no need to compile info-look for $(EMACS) $$vnum"; \
	  else echo "$(MESSAGE)"; \
	fi; \
	if   [ $(EMACS) = "emacs"  -a $$version -lt 2030 ]; \
	  then echo Using info-look.20.2.el; \
	  cp $(srcdir)/info-look.20.2.el info-look.el; \
	elif [ $(EMACS) = "xemacs" -a $$version -ge 2000 ]; \
	  then echo Using info-look.20.3.el; \
	  cp $(srcdir)/info-look.20.3.el info-look.el; \
	elif [ $(EMACS) = "xemacs" -a $$version -lt 2000 ]; \
	  then echo Using info-look.20.2.el; \
	  cp $(srcdir)/info-look.20.2.el info-look.el; \
	fi ;\
	if [ ! \( $(EMACS) = "emacs" -a $$version -ge 2030 \) ]; \
	  then $(BYTEC) info-look.el; fi;

ps: gpelcard.ps

gpelcard.ps: gpelcard.dvi
	$(DVIPS) -o gpelcard.ps gpelcard.dvi

gpelcard.dvi: gpelcard.tex
	$(LATEX) $(srcdir)/gpelcard.tex

all: gnuplot.elc info-look.elc gnuplot-gui.elc

install:
	mkinstalldirs $(lispdir)
	@for p in *.el *.elc; do \
	  echo " $(INSTALL_DATA) $$p $(lispdir)/$$p"; \
	  $(INSTALL_DATA) $$p $(lispdir)/$$p; \
	done

distdir = $(top_builddir)/$(PACKAGE)-$(VERSION)/$(subdir)

subdir = lisp

distdir: $(DISTFILES)
	distdir=`cd $(distdir) && pwd`
	@for file in $(DISTFILES); do \
	  d=$(srcdir); \
	  if test -d $$d/$$file; then \
	    cp -pr $$d/$$file $(distdir)/$$file; \
	  else \
	    test -f $(distdir)/$$file \
	    || ln $$d/$$file $(distdir)/$$file 2> /dev/null \
	    || cp -p $$d/$$file $(distdir)/$$file || :; \
	  fi; \
	done
check-am: all-am
check: check-am
installcheck-am:
installcheck: installcheck-am
install-exec-am:
install-exec: install-exec-am

.PHONY: tags distdir info-am info dvi-am dvi check check-am \
installcheck-am installcheck install-exec-am install-exec \
install-data-am install-data install-am install uninstall-am uninstall \
all-redirect all-am all installdirs mostlyclean-generic \
distclean-generic clean-generic maintainer-clean-generic clean \
mostlyclean distclean maintainer-clean

clean:
	-rm -f *.elc info-look.el gpelcard.ps gpelcard.dvi gpelcard.log \
	  gpelcard.aux
	test "$(srcdir)" = "$(top_builddir)" || rm -f gnuplot.el gnuplot-gui.el

distclean: clean
	rm -f Makefile config.status config.log config.cache

# Tell versions [3.59,3.63) of GNU make to not export all variables.
# Otherwise a system limit (for SysV at least) may be exceeded.
.NOEXPORT:
