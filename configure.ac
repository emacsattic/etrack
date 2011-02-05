## configure.ac for ETRACK

## Copyright (C) 2001-2009, 2011 Thien-Thi Nguyen
## This file is part of ETRACK, released under GNU GPL with
## ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

AC_INIT([ETRACK],[0.9913],[ttn@gnuvola.org])

#
# Guile
#

GUILE_PROGS

#
# scheme module checks
#

AUTOFRISK_CHECKS
GUILE_MODULE_REQUIRED_EXPORT((database postgres-table), compile-outspec)

#
# Emacs (also checks for Emacs, autoconf 2.52 .../lispdir.m4)
#

AM_PATH_LISPDIR

#
# Misc
#

AC_PROG_INSTALL

#
# Write it out
#

AC_CONFIG_FILES([GNUmakefile])

AC_OUTPUT

## configure.ac ends here
