##############################################################################
# $Id$
# Name:	mupad.m4
# Purpose:	Automake / Autoconf macros for MuPAD (www.mupad.de)
# Author:	Nicolas M. Thiéry <nthiery@users.sf.net>
# Created:	2000/09
# License:	LGPL
##############################################################################

##############################################################################
# AM_PROG_MUPAD
#
# Look for MuPAD, setting:
#  - MUPAD		path to the MuPAD program
#			e.g. mupad
#  - MUPAD_VERSION	version of MuPAD
#			e.g. 2.0.0
#  - MUPADdir		root path of the MuPAD installation
#			/usr/local/MuPAD/
#  - MUPADbindir	directory containing MuPAD programs
#			e.g. /usr/local/MuPAD/share/bin
#  - MUPADpkgdir	directory where the package will be installed
#			e.g. /usr/local/MuPAD/share/packages/bla/
#			Note: should this be pkgMUPADdir, to be consistent
#			with pkgpythondir ?

AC_DEFUN([AM_PROG_MUPAD],
[
	# MUPAD
	AC_CHECK_PROGS([MUPAD], [mupad], "")
	test -n "$MUPAD" || AC_MSG_ERROR([Not found.
Please check the installation of MuPAD or override with
./configure MUPAD=<.../>mupad])

	# MUPAD_VERSION
	AC_CACHE_CHECK([for MuPAD version],
          [mupad_cv_mupad_version],
	  [if test "${MUPAD_VERSION+set}" = set; then
	     mupad_cv_mupad_version=$MUPAD_VERSION
           else
	     mupad_cv_mupad_version=`echo 'fprint(Unquoted,0,(v:=version();"".op(v,1).".".op(v,2).".".op(v,3))):quit' | $MUPAD -n -P pe -S`
	   fi
	   case $mupad_cv_mupad_version in
	     ?.?.?) ;;
	     *) AC_MSG_ERROR([Not found.
Please check the installation of MuPAD or override with
./configure MUPAD_VERSION=2.4.2]);;
	   esac
	   ])
	MUPAD_VERSION=$mupad_cv_mupad_version
	AC_SUBST([MUPAD_VERSION])

	# MUPADdir
	AC_CACHE_CHECK([for MuPAD root directory],
          [mupad_cv_mupaddir],
          [if test "${MUPADdir+set}" = set; then
	     mupad_cv_mupaddir=$MUPADdir
           else
	     case $MUPAD_VERSION in
	       1.?.?)
		 mupad_cv_mupaddir=`echo 'fprint(Unquoted,0,LIB_PATH):quit' | $MUPAD -n -P pe -S | sed 's/\/share\/lib\(\/lib.tar.lib\)\?\/*$//'`;;
	       2.0.?)
		 mupad_cv_mupaddir=`echo 'fprint(Unquoted,0,op(LIBPATH,nops(LIBPATH))):quit' | $MUPAD -n -P pe -S | sed 's/\/share\/lib\(\/lib.tar.lib\)\?\/*$//'`;;
	       *.?.?)
		 mupad_cv_mupaddir=`$MUPAD -r`;;
	       *);;
	     esac;
	   fi
	   test -d "$mupad_cv_mupaddir" || AC_MSG_ERROR([Not found.
Please check the installation of MuPAD or override with
./configure MUPADdir=<MuPAD_ROOT_PATH>])
        ]);
	MUPADdir=$mupad_cv_mupaddir
	AC_SUBST([MUPADdir])

	# MUPADbindir
	AC_CACHE_CHECK([for MuPAD bin directory],
	  [mupad_cv_mupadbindir],
	  [mupad_cv_mupadbindir=${MUPADbindir="$MUPADdir/share/bin"}
	   test -d "$mupad_cv_mupadbindir" ||\
	     AC_MSG_ERROR([Not found.
Please check the installation of MuPAD or override with
./configure MUPADbindir=<MuPAD_ROOT_PATH/share/bin>])
        ])
	MUPADbindir=$mupad_cv_mupadbindir
	AC_SUBST([MUPADbindir])

	# MUPADpkgdir
	AC_CACHE_CHECK([for MuPAD package installation directory],
          [mupad_cv_mupadpkgdir],
	  [mupad_cv_mupadpkgdir=${MUPADpkgdir="$MUPADdir/packages/$PACKAGE_NAME"}
        ])
	MUPADpkgdir=$mupad_cv_mupadpkgdir
	AC_SUBST([MUPADpkgdir])
])# AM_PROG_MUPAD

# AM_PROG_MUPAD2
#
# Same as AM_PROG_MUPAD, but issues an error if the version of MuPAD
# is < 2.0.0

AC_DEFUN([AM_PROG_MUPAD2],
[
	AC_REQUIRE([AM_PROG_MUPAD])
	case $MUPAD_VERSION in
	  1.?.?)
	    AC_MSG_ERROR([Version >= 2.0.0 of MuPAD required]);;
	esac;
])# AM_PROG_MUPAD2

##############################################################################
# AM_PROG_MUPAD_MMG
#
# Look for the MuPAD module generator, setting the variables:
#  - MUPAD_MMG		path to the module generator mmg
#			e.g. /usr/local/MuPAD/share/bin/mmg
#  - MUPAD_MMG_CFLAGS   compiler options required for MuPAD dynamic modules
#  - MUPAD_MMG_LDFLAGS  linker options required for MuPAD dynamic modules
#  - MUPAD_SYSINFO	path to the script sysinfo
#			e.g. /usr/local/MuPAD/share/bin/sysinfo
#  - MUPAD_ARCH		MuPAD's name for the architecture
#			e.g. linux
#  - MUPADpkgmdmdir	directory where modules are installed
#			e.g. /usr/local/MuPAD/share/packages/bla/modules/linux
#  - MUPADlocalmdmdir	local directory where modules are copied
#			at compile-time to enable using them
#			before installation (typically in tests)
#			e.g. <builddir>/modules/<MuPAD_ARCH>/<MUPAD_VERSION>

AC_DEFUN([AM_PROG_MUPAD_MMG],
[
	# Warning: this might interfer with the building of other
	# parts of the package ...
	AC_REQUIRE([AC_DISABLE_STATIC])
	AC_REQUIRE([AC_PROG_LIBTOOL])
	# Note: there is a bug in autoconf 2.13: calling AC_PROG_LIBTOOL
	# after AC_LANG_CPLUSPLUS causes the executable suffix to be
	# set to .C instead of nothing ...
	AC_REQUIRE([AC_PROG_CXX])
	AC_REQUIRE([AC_PROG_CXXCPP])
	AC_REQUIRE([AC_LANG_CPLUSPLUS])

	# MUPAD_MMG
	AC_PATH_PROG([MUPAD_MMG], [mmg], , [$MUPADbindir])
	test -n "$MUPAD_MMG" ||\
	  AC_MSG_ERROR([MuPAD module generator not found.
Please check the installation of MuPAD or override with
./configure MUPAD_MMG=...])

	# MUPAD_SYSINFO
	AC_PATH_PROG([MUPAD_SYSINFO], [sysinfo], , [$MUPADbindir])
	test -n "$MUPAD_SYSINFO" ||\
	  AC_MSG_WARN([MuPAD sysinfo script not found.
Please check the installation of MuPAD or override with
./configure MUPAD_SYSINFO=...])

	# MUPAD_ARCH
	AC_CACHE_CHECK([for MuPAD arch],
          [mupad_cv_mupad_arch],
	  [mupad_cv_mupad_arch=${MUPAD_ARCH=`$MUPAD_SYSINFO`}
 	   test -n "$mupad_cv_mupad_arch" ||\
	     AC_MSG_ERROR([Could not determine MuPAD architecture.
Please check the installation of MuPAD or override with
./configure MUPAD_ARCH=...])
        ])
	MUPAD_ARCH=$mupad_cv_mupad_arch
	AC_SUBST([MUPAD_ARCH])

	# MUPAD_MMG_CFLAGS
	AC_CACHE_CHECK([for mmg compiler options],
          [mupad_cv_mupad_mmg_cflags],
	  [if test "${MUPAD_MMG_CFLAGS+set}" = set; then
	     mupad_cv_mupad_mmg_cflags=$MUPAD_MMG_CFLAGS
	   else
	     # Obtained by examining the output of mmg on a fake module
	     # Suggestion from Andreas: use the -n option
	     echo "MFUNC(init, MCnop) { } MFEND" > conftestmmg.cc
	     mupad_cv_mupad_mmg_cflags=`MMG_CC=echo $MUPAD_MMG -nog -nol conftestmmg.cc | sed 's/-c MMGconftestmmg.cpp -o conftestmmg.o //'` ||\
	       AC_MSG_ERROR([Not found.
Please check the installation of MuPAD or override with
./configure MUPAD_MMG_CFLAGS=-D... -I...]);
	   fi
	   case $mupad_cv_mupad_mmg_cflags in
	     -D*) ;; # very basic sanity check
	     *) AC_MSG_ERROR([Inconsistent value: $mupad_cv_mupad_mmg_cflags.
Please check the installation of MuPAD or override with
./configure MUPAD_MMG_CFLAGS=-D... -I...]);;
	   esac
        ])
	MUPAD_MMG_CFLAGS=$mupad_cv_mupad_mmg_cflags
	AC_SUBST([MUPAD_MMG_CFLAGS])

	# Check that the mmg headers are available
	CPPFLAGS_BACKUP="$CPPFLAGS"
	CPPFLAGS="$CPPFLAGS $MUPAD_MMG_CFLAGS"
	AC_CHECK_HEADER([MDM_base.h], ,
		        [AC_MSG_ERROR([MuPAD MMG headers not found.
Please check the installation of MuPAD])],
			[/* checkCompilation */])
	CPPFLAGS="$CPPFLAGS_BACKUP"

	# MUPAD_MMG_LDFLAGS
	AC_CACHE_CHECK([for mmg linker options],
          [mupad_cv_mupad_mmg_ldlags],
          [if test "${MUPAD_MMG_LDFLAGS+set}" = set; then
	     mupad_cv_mupad_mmg_ldlags=$MUPAD_MMG_LDFLAGS
	   else
	     # Obtained by examining the output of mmg on a fake module
	     # It seems that the link flags of mmg always contain
	     # -L/usr/local/MuPAD/linux/lib. If the libstdc++ provided
	     # by the MuPAD distribution is not compatible with the c++
	     # standard headers provided by the system then the symbols
	     # are set incorrectly and the module cannot be loaded.
	     # Temporary fix: erase this flag from the link flags
	     # Also always add -avoid-version. Is this portable
	     # outside of linux/gcc ?
	     echo "MFUNC(init, MCnop) { } MFEND" > conftestmmg.cc
	     mupad_cv_mupad_mmg_ldlags=-avoid-version `MMG_LD=echo $MUPAD_MMG -nog -noc conftestmmg.cc | sed "s/-o conftestmmg.mdm conftestmmg.o //; s,-L *$MUPADdir/$MUPAD_ARCH/lib,,"` ||\
	       AC_MSG_ERROR([Not found.
Please check the installation of MuPAD or override with
./configure MUPAD_MMG_LDFLAGS=...])
	   fi
        ])
	MUPAD_MMG_LDFLAGS=$mupad_cv_mupad_mmg_ldlags
	AC_SUBST([MUPAD_MMG_LDFLAGS])

	# MUPADpkgmdmdir
	AC_CACHE_CHECK([for MuPAD dynamic modules package installation directory],
	  [mupad_cv_mupadpkgmdmdir],
	  [mupad_cv_mupadpkgmdmdir=${MUPADpkgmdmdir="$MUPADpkgdir/modules/$MUPAD_ARCH"}
	])
	MUPADpkgmdmdir=$mupad_cv_mupadpkgmdmdir
	AC_SUBST([MUPADpkgmdmdir])

	# MUPADlocalmdmdir
	AC_CACHE_CHECK([for MuPAD dynamic modules local installation directory],
	  [mupad_cv_mupadlocalmdmdir],
	  [# MUPADlocalmdmdir expansion is delayed to ensure a proper
	   # definition in Makefiles of subdirectories
	   mupad_cv_mupadlocalmdmdir=${MUPADlocalmdmdir='$(top_builddir)/modules/$(MUPAD_ARCH)/$(MUPAD_VERSION)'}
	])
	MUPADlocalmdmdir=$mupad_cv_mupadlocalmdmdir
	AC_SUBST([MUPADlocalmdmdir])
])# AM_PROG_MUPAD_MMG

##############################################################################
# AM_PROG_MUPAD_PACKAGE
#
# Standard mupad package configuration (documentation building, ...)
# Defines the following configure options:
#  --enable-dvi-documentation
#  --enable-ps-documentation
#  --enable-pdf-documentation
#  --enable-html-documentation
#  --enable-ascii-documentation
#  --enable-a4-documentation
# Defines, if needed, the following variables:
#  - LATEX PDFLATEX DVIPS MUPAD_LATEX_ENV

AC_DEFUN([AM_PROG_MUPAD_PACKAGE],
[
##############################################################################
# DVI documentation
##############################################################################

# List of the main documented MuPAD libraries contained or extended in
# this package
AC_SUBST(MUPAD_LIBS)

AC_ARG_ENABLE(dvi-documentation,
  AC_HELP_STRING([--enable-dvi-documentation],
		 [Build dvi documentation and test its examples]),
  [], [enable_dvi_documentation=yes])
AM_CONDITIONAL(ENABLE_DVI_DOCUMENTATION,
	       test x$enable_dvi_documentation = xyes)
if test x$enable_dvi_documentation = xyes; then
  AC_CHECK_PROGS(LATEX, latex, "")
  test -n "$LATEX" || AC_MSG_ERROR([Not found.
LaTeX is required for building the dvi documentation,
and to test the examples in the documentation.
You can fix this manually with:
	./configure LATEX=<.../>latex
or disable the building of dvi documentation with:
	./configure --disable-dvi-documentation])
fi

# LaTeX setup to use MuPAD's style files and fonts

MUPAD_LATEX_ENV='TEXINPUTS=$(top_srcdir):$(top_srcdir)/doc/STYLES//:$$TEXINPUTS MFINPUTS=$$MFINPUTS:$(top_srcdir)/doc/FONTS TFMFONTS=$(top_srcdir)/doc/FONTS:$$TFMFONTS TEXMFCNF=$(srcdir):$$TEXMFCNF'
AC_SUBST(MUPAD_LATEX_ENV)

##############################################################################
# PS documentation
##############################################################################

AC_CHECK_PROGS(DVIPS, dvips, "")

AC_ARG_ENABLE(ps-documentation,
  AC_HELP_STRING([--enable-ps-documentation],
		 [Build ps documentation (implies --enable-dvi-documentation)]),
  [], [enable_ps_documentation=no])
AM_CONDITIONAL(ENABLE_PS_DOCUMENTATION,
               test x$enable_ps_documentation = xyes)
if test x$enable_ps_documentation = xyes; then
  enable_dvi_documentation=yes
  test -n "DVIPS" || AC_MSG_ERROR([Not found.
dvips is required for building the ps documentation.
You can fix this manually hand with:
	./configure DVIPS=<.../>dvips
or disable the building of ps documentation with:
	./configure --disable-ps-documentation])
fi

##############################################################################
# PDF documentation
##############################################################################

AC_CHECK_PROGS(PDFLATEX, pdflatex, "")

AC_ARG_ENABLE(pdf-documentation,
  AC_HELP_STRING([--enable-pdf-documentation],
		 [Build pdf documentation (implies --enable-dvi-documentation)]),
  [], [enable_pdf_documentation=no])
AM_CONDITIONAL(ENABLE_PDF_DOCUMENTATION,
               test x$enable_pdf_documentation = xyes)
if test x$enable_pdf_documentation = xyes; then
  enable_dvi_documentation=yes 
  test -n "$PDFLATEX" || AC_MSG_ERROR([Not found.
pdflatex is required for building the pdf documentation.
You can fix this manually hand with:
	./configure PDFLATEX=<.../>pdflatex
or disable the building of pdf documentation with:
	./configure --disable-pdf-documentation])
fi

##############################################################################
# HTML documentation
##############################################################################

AC_ARG_ENABLE(html-documentation,
  AC_HELP_STRING([--enable-html-documentation],
		 [Build html documentation]),
  [], [enable_html_documentation=yes])
AM_CONDITIONAL(ENABLE_HTML_DOCUMENTATION,
               test x$enable_html_documentation = xyes)

##############################################################################
# ASCII documentation
##############################################################################

AC_ARG_ENABLE(ascii-documentation,
  AC_HELP_STRING([--enable-ascii-documentation],
		 [Build ascii documentation (implies --enable-html-documentation)]),
  [], [enable_ascii_documentation=no])
AM_CONDITIONAL(ENABLE_ASCII_DOCUMENTATION,
               test x$enable_ascii_documentation = xyes)
if test x$enable_ascii_documentation = xyes; then
  enable_html_documentation=yes 
fi

##############################################################################
# a4 documentation
##############################################################################

AC_ARG_ENABLE(a4-documentation,
  AC_HELP_STRING([--enable-a4-documentation],
		 [Build a4 documentation]),
  [], [enable_a4_documentation=no])
AM_CONDITIONAL(ENABLE_A4_DOCUMENTATION,
               test x$enable_a4_documentation = xyes)

##############################################################################
# Optional compilation of the dynamic modules
##############################################################################

MODULES_SUBDIRS=""
AC_SUBST(MODULES_SUBDIRS)

AC_ARG_ENABLE(dynamic-modules,
  AC_HELP_STRING([--enable-dynamic-modules],
		 [Compile and use dynamic modules]),
  [], [enable_dynamic_modules=yes])
AM_CONDITIONAL(DYNAMIC_MODULES, test x$enable_dynamic_modules = xyes)

])# AM_PROG_MUPAD_PACKAGE
