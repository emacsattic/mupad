##############################################################################
# Automake configuration
##############################################################################

ACLOCAL_AMFLAGS	= -I config

##############################################################################
# Targets
##############################################################################

nobase_dist_lisp_LISP = \
	mupad-bus.el mupad.el mupad-help.el mupad-xemacs.el \
	mupad-cpl.el mupad-fontification.el mupad-run.el\
	sli-tools.el \
	mupad.el-info mupad-run.el-info
# Don't byte-compile site-start.d/mupacs.el and mupad.el-info:
ELCFILES = \
	mupad-bus.elc mupad.elc mupad-help.elc mupad-xemacs.elc \
	mupad-cpl.elc mupad-fontification.elc mupad-run.elc \
	sli-tools.elc

EXTRA_DIST = $(PKG_DOC) config/mupad.m4 config/mupad.am config/cvs.am config/sourceforge.am config/packages.m4 config/packages.am config/packages.spec.in config/packages.info.in site-start.d/mupacs.el.in.in .cvsignore bootstrap test-indentation.mu
DISTCLEANFILES = $(PKG_RPM_NAME).info $(PKG_RPM_NAME).spec -r packages/rpm site-start.d/mupacs.el

#MAINTAINERCLEANFILES = $(shell cat packages/.cvsignore)
MAINTAINERCLEANFILES = site-start.d/mupacs.el.in

KNOWN_EMACS_SITE_START_DIRECTORIES = /etc/emacs/site-start.d /usr/share/emacs/site-lisp/site-start.d ${lispdir}/site-start.d /usr/local/share/emacs/site-lisp/site-start.d

# Yes, $(DESTDIR) is missing in test -d $$dir.
# This is voluntary so that when building the rpm package, the
# initialization file gets properly installed $(DESTDIR)/$$dir
# coherently with actual host's site-start.d dir.
# I hope this won't break something else.
install-data-local:
	@echo Finalizing the variable substitution in site-start.d/mupacs.el;
	@sed site-start.d/mupacs.el.in -e 's,$${datadir},${datadir},' > site-start.d/mupacs.el
	@found=no;							\
	for dir in $(KNOWN_EMACS_SITE_START_DIRECTORIES); do		\
	  if test -d $$dir; then					\
		found=yes;						\
		echo Installing the emacs startup file site-start.d/mupacs.el in $(DESTDIR)$$dir; \
		$(INSTALL) -d $(DESTDIR)$$dir;	\
		$(INSTALL_DATA) $(srcdir)/site-start.d/mupacs.el $(DESTDIR)$$dir;	\
	  fi;								\
	done;								\
	if [ $$found = no ]; then					\
	    echo "WARNING: Emacs site-wide startup directory site-start.d not found.";\
	    echo "You may want to add by hand the contents of site-start.d/mupacs.el to the site-wide emacs startup file site-start.el";				\
	fi

uninstall-local:
	echo coucou $(DESTDIR)
	for dir in $(KNOWN_EMACS_SITE_START_DIRECTORIES); do		\
	  if test -d $$dir; then					\
		rm $(DESTDIR)$$dir/mupacs.el;				\
		break;							\
	  fi;								\
	done

##############################################################################
# Extra compilation rules
##############################################################################

# Package creation
include $(top_srcdir)/config/packages.am

# CVS goodies
include $(top_srcdir)/config/cvs.am

# Sourceforge goodies
include $(top_srcdir)/config/sourceforge.am
