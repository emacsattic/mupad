##############################################################################
# $Id$
# Autoconf configuration
#
# Process this file with autoconf to produce a configure script.
##############################################################################

##############################################################################
# Initialization of autoconf and automake
##############################################################################

AC_INIT(mupacs, 3.2, nthiery@users.sf.net)
AC_CONFIG_AUX_DIR(config)
AC_CONFIG_SRCDIR(mupad.el)
AM_INIT_AUTOMAKE([gnu check-news 1.5c])
AM_MAINTAINER_MODE

AC_SUBST(ac_aux_dir)

##############################################################################
# Configuration for package building (rpms, ...)
##############################################################################

PKG_INIT_RPM(		[1])dnl
#PKG_INIT_DEB(		[1])dnl
# The long description should not contain ','
PKG_INIT_DESCRIPTION(
[mupacs -- a MuPAD mode for emacs],
[mupacs is an open-source MuPAD mode for the text editor emacs.\
Features:\
 - Syntax highlighting and automatic indentation of MuPAD code;\
 - Running MuPAD in a buffer with full access to the documentation\
   and debugger interaction.\
])dnl
PKG_INIT_GROUP(		[Applications/Engineering])dnl
PKG_INIT_LICENSE(	[GPL])dnl
PKG_INIT_SOURCE(	[http://download.sourceforge.net/mupacs/])dnl
PKG_INIT_URL(		[http://mupacs.sourceforge.net/])dnl
PKG_INIT_VENDOR(	[Nicolas M. Thiéry <nthiery@users.sourceforge.net>])dnl
PKG_INIT_PACKAGER(	[Nicolas M. Thiéry <nthiery@users.sourceforge.net>])dnl
PKG_INIT_DOC(		README index.html INSTALL QuickInstall NEWS AUTHORS THANKS TODO BUGS ChangeLog)dnl
PKG_INIT_BUILD_ARCHS(	noarch)dnl

##############################################################################
# Checks
##############################################################################

# Checks for programs.

AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AM_PROG_MUPAD

AM_PATH_LISPDIR

# Checks for libraries.

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

##############################################################################
# Configured files
##############################################################################

AC_CONFIG_FILES([
	README
	index.html
	Makefile
	site-start.d/mupacs.el
])

##############################################################################
# Do the configuration
##############################################################################

AC_OUTPUT

# * Local Variables: 
# * mode: m4
# * End: 
