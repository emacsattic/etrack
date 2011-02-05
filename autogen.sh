#!/bin/sh
#
# Copyright (C) 2002-2009, 2011 Thien-Thi Nguyen
# This file is part of ETRACK, released under GNU GPL with
# ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

set -e
guile-tools autofrisk
aclocal -I `guile-config info datadir`/aclocal -I .
autoconf

../.common/symlink-am-files install-sh INSTALL

# autogen.sh ends here
