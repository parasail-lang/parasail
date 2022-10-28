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
/^[ 	]*(abstract  *)?(queued *)?(concurrent *)?(func|op|interface|class|type|var|const|ref( *var)?)[^a-zA-Z_]/ {
  id = $2
  if ($1 ~ /^(abstract|queued|concurrent)$/) {
    if ($2 ~ /^(abstract|queued|concurrent)$/) {
       id = $4
    } else {
       id = $3
    }
  }

  # remove trailing '(', '<', and ';'
  split(id, idar, "(");
  id = idar[1];
  split(id, idar, "<");
  id = idar[1];
  split(id, idar, ";");
  id = idar[1];
  # if a child unit (e.g. "a.b.c") get just the last id (e.g. "c")
  numids = split(id, idar, "::");
  id = idar[numids];

  # Ignore if id not a legal identifier
  if (id ~ /^["A-Za-z_]/) {
    # remove preceding and trailing '"'
    numids = split(id, idar, "\"");
    if (numids > 1) {
       id = idar[2]
    }
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

