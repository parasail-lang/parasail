#!/bin/csh -f
    # -f -- don't read startup files
    # -e -- exit on errors
    # -x -- echo commands

# This normalizes all locally modified .ad? files.
# to reduce the chances of merge conflicts.

# This does not normalize non-Ada files, but it does svn update them.

################

# Note: If this file needs to update *itself*, it fails on Windows with
# "cannot rename file .new.svnnorm.csh to svnnorm.csh: Permission denied".
# It works fine on Unix.  Sigh.

set UNAME=x`uname`
set PNAME=x`uname -p`
if ( $UNAME == xLinux ) then
  set path = ( /bin /usr/bin /usr/local/bin /opt/CollabNet_Subversion/bin )
else if ( $UNAME == xSunOS ) then
  if ( $PNAME == xi386 ) then
      set path = ( /bin /usr/local/bin /home/dsd/4/cpl2/svn/bin )
  else
      set path = ( /bin /usr/local/bin )
  endif
else if ( $UNAME =~ xCYGWIN_NT* ) then
  set path = ( /bin /usr/lib/subversion/bin )
else if ( $UNAME =~ xDarwin ) then
  set path = ( /opt/local/bin /bin /usr/bin )
else
  echo "Unknown operating system:" `uname`
  exit (-1)
endif

# First run svn status without doing anything to find out what it's
# going to do.
svn status $* > update.temp.$$

# Search for lines containing something like "M foo.ada" or "C bar.ada"
# or "A baz.ada", and filter to produce a list like
# "foo.ada bar.ada baz.ada":
foreach file (`egrep '^M .*\.ad.$|^C .*\.ad.$|^A .*\.ad.$' update.temp.$$ | sed -e 's/^M [ +]*//' | sed -e 's/^C [ +]*//' | sed -e 's/^A [ +]*//'`)
  # Normalize all files in that list:
  echo -n $file
  mv $file $file.temp.$$
  tr -d '\r' < $file.temp.$$ # Delete carriage returns (i.e convert Windows to Unix form). \
    | expand # Expand tabs. \
    | sed -E -e 's/ *$//' \
          -e '/-- [^ ].*--/\!s/(^ *-*)-- ([^ ])/\1--  \2/' \
          -e '/(--.*[a-zA-Z_0-9]\()|(".*[a-zA-Z_0-9]\(")/\!s/([a-zA-Z_0-9])\(/\1 \(/g' \
      # Delete trailing blanks, add 2 spaces after comments, 1 space before ( \
    > $file

  cmp -s $file.temp.$$ $file
  if ($status) then
    echo " normalized."
  else
    echo ""
  endif

  # Set timestamp back to what it was to avoid unnecessary
  # recompilations:
  touch -r $file.temp.$$ $file
  rm -f $file.temp.$$
end

echo ""

rm -f update.temp.$$
