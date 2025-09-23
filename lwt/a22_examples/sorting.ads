generic
    type Elem_Type is (<>);
    type Index_Type is range <>;
    type Array_Type is array (Index_Type range <>) of Elem_Type;
    with function Image(Index : Index_Type) return String;
package Sorting is
    procedure Quicksort(Arr : in out Array_Type; Debug : Boolean := False);
          -- Sort Arr according to the sorting op "<" which returns
          -- True if Left must appear before Right in the sorted order.
          -- "<" returns False if Left need not appear before Right.
end Sorting;

