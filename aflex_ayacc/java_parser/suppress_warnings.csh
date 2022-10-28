#

foreach i ($*)
    echo "suppress warnings and style-checks for $i"
    echo "pragma Warnings(Off); pragma Style_Checks(Off);" > $i.tmp
    grep -v "pragma Warnings(Off); pragma Style_Checks(Off);" $i >> $i.tmp
    touch -r $i $i.tmp
    mv $i.tmp $i
    rm -f $i.tmp
end
