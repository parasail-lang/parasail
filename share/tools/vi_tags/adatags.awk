BEGIN {
  low["A"] = "a"
  low["B"] = "b"
  low["C"] = "c"
  low["D"] = "d"
  low["E"] = "e"
  low["F"] = "f"
  low["G"] = "g"
  low["H"] = "h"
  low["I"] = "i"
  low["J"] = "j"
  low["K"] = "k"
  low["L"] = "l"
  low["M"] = "m"
  low["N"] = "n"
  low["O"] = "o"
  low["P"] = "p"
  low["Q"] = "q"
  low["R"] = "r"
  low["S"] = "s"
  low["T"] = "t"
  low["U"] = "u"
  low["V"] = "v"
  low["W"] = "w"
  low["X"] = "x"
  low["Y"] = "y"
  low["Z"] = "z"
}
/^[ 	]*([Pp]rivate  *)?([Pp]rocedure|[Ff]unction|[Tt]ask|[Pp]rotected|[Pp]ackage|[Tt]ype|[Ss]ub[Tt]ype|[Ee]ntry|[Aa]ccept)[^a-zA-Z_]/ {
  if ($1 ~ /^[Pp]rivate$/) {
    id = $3
  } else if ($2 ~ /^[Tt]ype$/ || $2 ~ /^[Bb]ody$/) {
    id = $3
  } else {
    id = $2
  }

  # remove trailing '(' and ';'
  split(id, idar, "(");
  id = idar[1];
  split(id, idar, ";");
  id = idar[1];
  # if a child unit (e.g. "a.b.c") get just the last id (e.g. "c")
  numids = split(id, idar, ".");
  id = idar[numids];

  # Ignore if id not a legal identifier or if line contains "is separate"
  if (id ~ /^["A-Za-z]/ && $0 !~ /is separate/) {
    # truncate pattern to 50 chars max
    if (length($0) < 50) lin = $0 "$";
    else lin = substr($0, 1, 50);

    # insert backslashes as needed to escape characters in pattern
    # as of 9/6/2012, some of these escapes are unnecessary
    if (lin ~ /[?]/) {
      newlin = "";
      ln = length(lin);
      for (i = 1; i <= ln; i++) {
        j = substr(lin, i, 1);
        if (j ~ /[?]/) newlin = newlin "\\" j;
        else newlin = newlin j;
      }
      lin = newlin;
    }
    # put out with all lower-case id
    if (id ~ /[A-Z]/) {
      newid = "";
      ln = length(id);
      for (i = 1; i <= ln; i++) {
        j = substr(id,i,1);
        if (j ~ /[A-Z]/) newid = newid low[j];
        else newid = newid j;
      }
      printf("%s\t%s\t?^%s?\n", newid, FILENAME, lin);
    }
    # put out with upper/lower case id
    printf("%s\t%s\t?^%s?\n", id, FILENAME, lin);
  }
}

