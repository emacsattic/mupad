##############################################################################
# Automake configuration
##############################################################################

ACLOCAL_AMFLAGS	= -I $(ac_aux_dir)

##############################################################################
# Targets
##############################################################################

nobase_dist_lisp_LISP = \
	mupad-bus.el mupad.el mupad-help.el mupad-xemacs.el \
	mupad-cpl.el mupad-fontification.el mupad-run.el\
	sli-tools.el \
	site-start.d/mupacs.el mupad.el-info
# Don't byte-compile site-start.d/mupacs.el and mupad.el-info:
ELCFILES = \
	mupad-bus.elc mupad.elc mupad-help.elc mupad-xemacs.elc \
	mupad-cpl.elc mupad-fontification.elc mupad-run.elc \
	sli-tools.elc

EXTRA_DIST = $(PKG_DOC) config/mupad.m4 config/mupad.am config/cvs.am config/sourceforge.am config/packages.m4 config/packages.am config/packages.spec.in config/packages.info.in site-start.d/mupacs.el.in .cvsignore bootstrap
DISTCLEANFILES = $(PKG_RPM_NAME).info $(PKG_RPM_NAME).spec -r packages/rpm

#MAINTAINERCLEANFILES = $(shell cat packages/.cvsignore)

install-data-local:
	if test -d $(DESTDIR)/etc/emacs/site-start.d; then	\
	  $(INSTALL_DATA) $(srcdir)/site-start.d/mupacs.el	\
            $(DESTDIR)/etc/emacs/site-start.d;			\
	fi

uninstall-local:
	    rm -f $(DESTDIR)/etc/emacs/site-start.d/mupacs.el


##############################################################################
# Extra compilation rules
##############################################################################

# Package creation
include $(top_srcdir)/config/packages.am

# CVS goodies
include $(top_srcdir)/config/cvs.am

# Sourceforge goodies
include $(top_srcdir)/config/sourceforge.am
