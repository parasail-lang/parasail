#
foreach i (*/test.sh)
  echo $i
  cat $i | sed -e 's/sklc/skli/g' > $i.new
  mv $i $i.old
  mv $i.new $i
end
