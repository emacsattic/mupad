dnl *
dnl * PKG_* - macros for package creation
dnl *

dnl *
dnl * PKG_SUBST_NO_DEF(VAR)
dnl *
dnl * Same as AC_SUBST(VAR), but tricks automake into not defining it
dnl * in the Makefiles.
dnl * Usefull for variables containing weird characters as newlines
dnl *
dnl * automake >= 1.6 asks directly to autoconf which variables are AC_SUBST.
dnl * So just hiding it below an alias does not work anymore.
dnl * The definition below is copied from the definition of AC_SUBST in
dnl * /usr/share/autoconf/autoconf/general.m4, autoconf 2.57
dnl * WARNING: that's probably not robust w.r.t. future changes in autoconf.
dnl *
dnl * Old comment: with autoconf 2.52, it does not work properly when called
dnl * directly in aclocal.m4.

m4_define([PKG_SUBST_NO_DEF],
[m4_ifvaln([$2], [$1=$2])[]dnl
m4_append_uniq([_AC_SUBST_VARS], [$1], [ ])dnl
])# PKG_SUBST_NO_DEF

dnl Former definition:
dnl AC_DEFUN([PKG_SUBST_NO_DEF],
dnl [
dnl 	AC_SUBST($1)
dnl ])

dnl
dnl * Mandatory macros
dnl

AC_DEFUN([PKG_INIT_RPM],
[
	if test -z '$1'; then
		AC_MSG_ERROR([PKG INIT RPM requires package release number])
	else
		PKG_RPM_RELEASE='$1'
	fi
	AC_SUBST(PKG_RPM_RELEASE)

	PKG_RPM_NAME="$PACKAGE_TARNAME-$VERSION-$PKG_RPM_RELEASE"
	AC_SUBST([PKG_RPM_NAME])

	PKG_TESTINSTALLDIR="packages/testinstall"
	AC_SUBST([PKG_TESTINSTALLDIR])
	PKG_RPM_BUILD_ROOT="packages/rpm/buildroot"
	AC_SUBST([PKG_RPM_BUILD_ROOT])

	dnl The following macros are optionnal
	dnl We make sure they are at least substituted properly

	AC_SUBST([PKG_RPM_OPT_REQUIRES])
	AC_SUBST([PKG_REQUIRES])
	AC_SUBST([PKG_RPM_OPT_BUILD_ARCHS])
	AC_SUBST([PKG_BUILD_ARCHS])
	PKG_SUBST_NO_DEF([PKG_PREINSTALL])
	PKG_SUBST_NO_DEF([PKG_PREUNINSTALL])
	PKG_SUBST_NO_DEF([PKG_POSTINSTALL])
	PKG_SUBST_NO_DEF([PKG_POSTUNINSTALL])
	AC_SUBST([PKG_SIGN])

	AC_MSG_CHECKING([creation of RPM directory tree])
	if $INSTALL -d \
		packages/rpm/BUILD	\
		packages/rpm/RPMS	\
		packages/rpm/SOURCES	\
		packages/rpm/SPECS	\
		packages/rpm/SRPMS	\
		packages/rpm/buildroot;
	then
		AC_MSG_RESULT([ok])
	else
		AC_MSG_RESULT([failed])
		AC_MSG_WARN([
Note: this is only required for building the RPM package
	])
	fi

	AC_CONFIG_FILES([
		$PKG_RPM_NAME.spec:config/packages.spec.in
		$PKG_RPM_NAME.info:config/packages.info.in
	])
])dnl

AC_DEFUN([PKG_INIT_DEB],
[
	if test -z '$1'; then

		AC_MSG_ERROR([PKG INIT DEB requires package release number])
	else
		PKG_DEB_RELEASE='$1'
	fi

	AC_SUBST(PKG_DEB_RELEASE)

	AC_MSG_WARN([Debian package building not implemented yet])
])dnl

AC_DEFUN([PKG_INIT_GROUP],
[
	if test -z '$1'; then
		AC_MSG_WARN([PKG INIT GROUP requires param])
	else
		PKG_RPM_GROUP='$1'
		PKG_DEB_GROUP='$1'
	fi
	AC_SUBST(PKG_RPM_GROUP)
	AC_SUBST(PKG_DEB_GROUP)
])dnl

dnl If the group shall be different for rpm and other package systems

AC_DEFUN([PKG_INIT_RPM_GROUP],
[
	if test -z '$1'; then
		AC_MSG_WARN([PKG INIT RPM GROUP requires param])
	else
		PKG_RPM_GROUP='$1'
	fi
	AC_SUBST([PKG_RPM_GROUP])
])dnl

AC_DEFUN([PKG_INIT_DEB_GROUP],
[
	if test -z '$1'; then
		AC_MSG_WARN([PKG INIT RPM GROUP requires param])
	else
		PKG_DEB_GROUP='$1'
	fi
	AC_SUBST(PKG_DEB_GROUP)
])dnl

AC_DEFUN([PKG_INIT_DESCRIPTION],
[
	if test -z '$1'; then
		AC_MSG_WARN([PKG INIT DESCRIPTION requires two params])
	else
		PKG_SHORTDESCRIPTION='$1'
	fi
	AC_SUBST(PKG_SHORTDESCRIPTION)
	if test -z '$2'; then
		AC_MSG_WARN([PKG INIT DESCRIPTION requires two params])
	else
		PKG_LONGDESCRIPTION='$2'
	fi
	PKG_SUBST_NO_DEF(PKG_LONGDESCRIPTION)
])dnl

AC_DEFUN([PKG_INIT_LICENSE],
[
	if test -z '$1'; then
		AC_MSG_WARN([PKG INIT LICENSE requires param])
	else
		PKG_LICENSE='$1'
		AC_SUBST(PKG_LICENSE)
	fi
])dnl

AC_DEFUN([PKG_INIT_SOURCE],
[
	if test -z '$1'; then
		AC_MSG_WARN([PKG INIT SOURCE requires param])
	else
		PKG_SOURCE='$1'
		AC_SUBST(PKG_SOURCE)
	fi
])dnl

AC_DEFUN([PKG_INIT_URL],
[
	if test -z '$1'; then
		AC_MSG_WARN([PKG INIT URL requires param])
	else
		PKG_URL='$1'
		AC_SUBST(PKG_URL)
	fi
])dnl

AC_DEFUN([PKG_INIT_VENDOR],
[
	if test -z '$1'; then
		AC_MSG_WARN([PKG INIT VENDOR requires param])
	else
		PKG_VENDOR='$1'
		AC_SUBST(PKG_VENDOR)
	fi
])dnl

AC_DEFUN([PKG_INIT_PACKAGER],
[
	if test -z '$1'; then
		AC_MSG_WARN([PKG INIT PACKAGER requires param])
	else
		PKG_PACKAGER='$1'
		AC_SUBST(PKG_PACKAGER)
	fi
])dnl

AC_DEFUN([PKG_INIT_DOC],
[
	if test -z '$1'; then
		AC_MSG_WARN([PKG INIT DOC requires param])
	else
		PKG_DOC='$1'
	fi
	AC_SUBST(PKG_DOC)
])dnl

dnl
dnl * Optional macros
dnl

dnl PKG_RPM_OPT_REQUIRES is used as a hack around the fact that rpm
dnl does not accept an empty 'Requires:' tag

AC_DEFUN([PKG_INIT_REQUIRES],
[
	if test -z '$1'; then
		AC_MSG_WARN([PKG INIT REQUIRES requires param])
	else
		PKG_REQUIRES='$1'
		PKG_RPM_OPT_REQUIRES='Requires:'
	fi
])dnl

dnl PKG_RPM_OPT_BUILD_ARCHS is used as a hack around the fact that rpm
dnl does not accept an empty 'Requires:' tag

AC_DEFUN([PKG_INIT_BUILD_ARCHS],
[
	if test -z '$1'; then
		AC_MSG_WARN([PKG INIT BUILD ARCHS requires param])
	else
		PKG_BUILD_ARCHS='$1'
		PKG_RPM_OPT_BUILD_ARCHS='BuildArchitectures:'
	fi
])dnl

AC_DEFUN([PKG_INIT_PREINSTALL],
[
	if test -z '$1'; then
		AC_MSG_WARN([PKG INIT PREINSTALL requires param])
	else
		PKG_PREINSTALL='$1'
	fi
])dnl

AC_DEFUN([PKG_INIT_POSTINSTALL],
[
	if test -z '$1'; then
		AC_MSG_WARN([PKG INIT POSTINSTALL requires param])
	else
		PKG_POSTINSTALL='$1'
	fi
])dnl

AC_DEFUN([PKG_INIT_PREUNINSTALL],
[
	if test -z '$1'; then
		AC_MSG_WARN([PKG INIT PREUNINSTALL requires param])
	else
		PKG_PREUNINSTALL='$1'
	fi
])dnl

AC_DEFUN([PKG_INIT_POSTUNINSTALL],
[
	if test -z '$1'; then
		AC_MSG_WARN([PKG INIT POSTUNINSTALL requires param])
	else
		PKG_POSTUNINSTALL='$1'
	fi
])dnl

AC_DEFUN([PKG_INIT_SIGN],
[
	PKG_SIGN=' --sign '
])dnl
