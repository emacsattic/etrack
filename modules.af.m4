AC_DEFUN([AUTOFRISK_CHECKS],[

GUILE_MODULE_REQUIRED(ttn-do zzz ciabattone)
GUILE_MODULE_REQUIRED(ice-9 accumulate)
GUILE_MODULE_REQUIRED(ice-9 format)
GUILE_MODULE_REQUIRED(ice-9 common-list)
GUILE_MODULE_REQUIRED(srfi srfi-13)
GUILE_MODULE_REQUIRED(srfi srfi-14)
GUILE_MODULE_REQUIRED(database tmpfile)
GUILE_MODULE_REQUIRED(database postgres-qcons)
GUILE_MODULE_REQUIRED(database postgres-resx)
GUILE_MODULE_REQUIRED(database postgres-resdisp)
GUILE_MODULE_REQUIRED(database postgres-table)
GUILE_MODULE_REQUIRED(database postgres)
GUILE_MODULE_REQUIRED(database postgres-col-defs)
GUILE_MODULE_REQUIRED(ice-9 editing-buffer)
GUILE_MODULE_REQUIRED(ice-9 gap-buffer)
GUILE_MODULE_REQUIRED(ttn-do zzz filesystem)
GUILE_MODULE_REQUIRED(ice-9 rdelim)
GUILE_MODULE_REQUIRED(database postgres-types)
GUILE_MODULE_REQUIRED(database postgres-meta)

probably_wont_work=""

AC_SUBST(probably_wont_work)
])


AC_DEFUN([AUTOFRISK_SUMMARY],[
if test ! "$probably_wont_work" = "" ; then
  AC_MSG_WARN([[
  The following modules either depend on presently unavailable modules,
  or on behavior that currently installed modules do not provide:
    $probably_wont_work
  They can be installed anyway, although they may not work properly.
  ]])
fi
])
