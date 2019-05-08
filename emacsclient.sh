#!/bin/bash
#
# Script to launch emacsclient with or without a line number
# $1 is the filename
# $2 is the optional linenumber
#
# To configure iTerm2 to use this script when CMD-clicking on a file
# Set "Profiles" -> "Advanced" -> "Semantic History" to
# "Run command" -> /Users/jes/.emacs.d/emacsclient.sh \1 \2
#
LINENUM=${2-1}
/usr/local/bin/emacsclient -n +$LINENUM $1
