#!/bin/sh
aclocal
autoconf -Wall --force
# automake --force will overwrite COPYING with the GPL.
automake -Wall --add-missing
