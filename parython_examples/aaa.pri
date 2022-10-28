# Parython Prototype Standard Library

# Copyright (C) 2011-2013, AdaCore, New York, NY
# To be used only for Personal, Academic, or Evaluation Purposes
# Not for Commercial Production Use.
# Report errors at http://www.parython.org

interface PSL<>

interface PSL::Core<>

interface PSL::Containers<>

interface PSL::Test<>

interface PSL::Core::Any<>

interface PSL::Core::Assignable<>

class interface PSL::Core::Boolean<> :
    defop "from_univ"(Lit : Univ_Enumeration) 
      {> Lit in [#false, #true] <}
      -> Boolean is import(#bool_from_univ)

    defop "to_univ"(Val : Boolean) -> Univ_Enumeration 
      is import(#bool_to_univ)

    defop "=?"(Left, Right : Boolean) -> Ordering
      is import("=?")
	# NOTE: #true > #false

    defop "not"(Boolean) -> Boolean is import("not")
    defop "and"(Left, Right : Boolean) -> Boolean is import("and")
    defop "or"(Left, Right : Boolean) -> Boolean is import("or")
    defop "xor"(Left, Right : Boolean) -> Boolean is import("xor")

    defop "and="(var Left : Boolean; Right : Boolean)
    defop "or="(var Left : Boolean; Right : Boolean)
    defop "xor="(var Left : Boolean; Right : Boolean)

    def Hash(Val : Boolean) -> Univ_Integer
      is import(#identity)

    # For Imageable
    def To_String(Val : optional Boolean) -> Univ_String
    def From_String(Str : Univ_String) -> optional Boolean

end interface PSL::Core::Boolean

class interface PSL::Containers::Set<Element_Type is Hashable<>> :
  # A hashed-set module
    defop "[]"() -> Set

    def Singleton(Elem : Element_Type) -> Set

    defop "|"(Left, Right : Element_Type) -> Set
    defop "|"(Left : Set; Right : Element_Type) -> Set
    defop "|"(Left : Element_Type; Right : Set) -> Set
    defop "|"(Left : Set; Right : Set) -> Set

    defop "|="(var Left : Set; Right : Set)

    defop "|="(var Left : Set; Right : Element_Type)
	# Add element to Set.

    defop "<|="(var Left : Set; var Right : optional Element_Type)
	# Move element into Set.

    defop "<|="(var Left : Set; var Right : Set)
	# Move all elements of Right into Left, leaving Right empty.

    defop "in"(Left : Element_Type; Right : Set) -> Boolean<>

    defop "=?"(Left, Right : Set) -> Ordering
	# Return #equal if Left and Right have the same elements
	# Return #less if Left is a proper subset of Right
	# Return #greater if Left is a proper superset of Right
	# Return #unordered otherwise

    defop "or"(Left, Right : Set) -> Set is "|"  # Union
    defop "or="(var Left : Set; Right : Set) is "|="

    defop "+"(Left, Right : Set) -> Set is "|"   # Union
    defop "+="(var Left : Set; Right : Set) is "|="
    defop "+="(var Left : Set; Right : Element_Type) is "|="   # aka Include

    defop "and"(Left, Right : Set) -> Set
	# Intersection
    defop "and="(var Left : Set; Right : Set)

    defop "xor"(Left, Right : Set) -> Set
	# Symmetric difference
    defop "xor="(var Left : Set; Right : Set)

    defop "-"(Left, Right : Set) -> Set
	# Set difference

    defop "-="(var Left : Set; Right : Set)
	# Compute Set difference

    defop "-="(var S : Set; Elem : Element_Type)   # aka Exclude
	# Remove one element, if present

    def Count(S : Set) -> Univ_Integer

    defop "magnitude"(Set) -> Univ_Integer is Count

    def Is_Empty(S : Set) -> Boolean<>

    def Remove_Any(var S : Set) -> optional Element_Type
      # Remove and return an arbitrary element of the Set S

    def Dump_Statistics(S : Set)
      # A debugging routine to show bucket sizes of Set

end interface PSL::Containers::Set

class interface PSL::Core::Univ_Enumeration<> :
    defop "=?"(Left, Right : Univ_Enumeration) -> Ordering 
      is import(#unordered_compare)
    def Print(Val : Univ_Enumeration) 
      is import(#print_univ_enum)

    def To_String(Val : Univ_Enumeration) -> Univ_String
      is import(#to_string_enum)

    def From_String(Str : Univ_String) -> optional Univ_Enumeration
      is import(#from_string_enum)

    def Hash(Val : Univ_Enumeration) -> Univ_Integer
      is import(#hash_enum)

    defop "in"(Left : Univ_Enumeration; Right : Set<Univ_Enumeration>) 
      -> Boolean<>
      is in Set<Univ_Enumeration>

end interface PSL::Core::Univ_Enumeration

interface PSL::Core::Optional<> :
    defop "null"() -> Optional
    defop "is null"(Optional) -> Boolean
end interface PSL::Core::Optional

class interface PSL::Core::Ordering<> :
  # Enumeration type used to represent value of "=?" operator
    defop "from_univ"(Lit : Univ_Enumeration) 
      {> Lit in [#less, #equal, #greater, #unordered] <}
      -> Ordering is import(#ordering_from_univ)

    defop "to_univ"(Val : Ordering) -> Univ_Enumeration 
      is import(#ordering_to_univ)

    defop "to_bool"(Ord : Ordering; Mask : Univ_Integer) -> Boolean 
      is import(#ordering_to_bool)

    defop "=?"(Left, Right : Ordering) -> Ordering is import("=?")

    def Hash(Val : Ordering) -> Univ_Integer
      is import(#identity)

    # For Imageable
    def To_String(Val : Ordering) -> Univ_String
    def From_String(Str : Univ_String) -> optional Ordering

    # Operators for Countable
    defop "+"(Left : Univ_Integer; Right : Ordering) -> Ordering 
      is import("+")
    defop "+"(Left : Ordering; Right : Univ_Integer) -> Ordering 
      is import("+")
    defop "-"(Left, Right : Ordering) -> Univ_Integer 
      is import("-")
    defop "-"(Left : Ordering; Right : Univ_Integer) -> Ordering 
      is import("-")

    def Min(Left, Right : optional Ordering) -> optional Ordering
      is import(#min)
    def Max(Left, Right : optional Ordering) -> optional Ordering
      is import(#max)

    # TBD: These should be properties some day (e.g. Ordering#first).
    def First()->Ordering
    def Last()->Ordering

    defop ".."(Left, Right : Ordering) -> Countable_Set<Ordering>
    defop "<.."(Left, Right : Ordering) -> Countable_Set<Ordering>
    defop "..<"(Left, Right : Ordering) -> Countable_Set<Ordering>
    defop "<..<"(Left, Right : Ordering) -> Countable_Set<Ordering>
    defop "|"(Left, Right : Ordering) -> Countable_Set<Ordering>
end interface PSL::Core::Ordering

class PSL::Core::Ordering :
    type Ordering_Set is Countable_Set<Ordering>

  exports :
    def First()->Ordering :
	return #less
    end def First

    def Last()->Ordering :
	return #unordered
    end def Last

    def To_String(Val : Ordering) -> Univ_String :
	switch Val :
	  case #less :
	    return "#less"
	  case #equal :
	    return "#equal"
	  case #greater :
	    return "#greater"
	  case #unordered:
	    return "#unordered"
    end def To_String

    def From_String(Str : Univ_String) -> optional Ordering :
	switch Str :
	  case "#less" :
	    return #less
	  case "#equal" :
	    return #equal
	  case "#greater" :
	    return #greater
	  case "#unordered" :
	    return #unordered
	  else :
	    return null
	end switch
    end def From_String

    # NOTE: For implementation reasons, we put these here rather than defining
    #       them in the interface using "is in Countable_Set<Ordering>"
    defop ".."(Left, Right : Ordering) -> Countable_Set<Ordering> :
	return Ordering_Set::".."(Left, Right)
    end defop ".."

    defop "<.."(Left, Right : Ordering) -> Countable_Set<Ordering> :
        return Ordering_Set::"<.."(Left, Right)
    end defop "<.."

    defop "..<"(Left, Right : Ordering) -> Countable_Set<Ordering> :
        return Ordering_Set::"..<"(Left, Right)
    end defop "..<"
    defop "<..<"(Left, Right : Ordering) -> Countable_Set<Ordering> :
        return Ordering_Set::"<..<"(Left, Right)
    end defop "<..<"

    defop "|"(Left, Right : Ordering) -> Countable_Set<Ordering> :
	return Ordering_Set::"|"(Left, Right)
    end defop "|"
end class PSL::Core::Ordering

interface PSL::Core::Comparable<> implements Assignable<> :
    defop "=?"(Left, Right : Comparable) -> Ordering
end interface PSL::Core::Comparable

interface PSL::Core::Ordered<> implements Comparable<> :
  # The "=?" operator on Ordered types never returns #unordered
  # They also provide a Min and Max operator.

  # Note that Min and Max may be given null operands.
  # They each return null if both operands are null, and return the
  # non-null operand if only one of the operands is null.
  # This allows null to be used as the initial value when computing
  # the "Max" or "Min" of a possibly-empty sequence, rather than
  # having to start with the equivalent of negative or positive infinity.

    type Full_Ordering is Ordering
      # {> Full_Ordering in #less | #equal | #greater <}

    defop "=?"(Left, Right : Ordered) -> Full_Ordering

    def Min(Left, Right : optional Ordered) -> optional Ordered
    def Max(Left, Right : optional Ordered) -> optional Ordered
end interface PSL::Core::Ordered

interface PSL::Core::Hashable<> implements Comparable<> :
    # Types which aren't ordered nevertheless are often
    # hashable, which makes it possible to create an efficient
    # set or map using them as the index type.
    defop "=?"(Left, Right : Hashable) -> Ordering
    def Hash(Val : Hashable) -> Univ_Integer
end interface PSL::Core::Hashable

class interface PSL::Containers::Countable_Set<Element_Type is Countable<>> :
    defop "[]"() -> Countable_Set

    def Singleton(Elem : Element_Type) -> Countable_Set

    defop ".."(Left, Right : Element_Type) -> Countable_Set
    defop "<.."(Left, Right : Element_Type) -> Countable_Set
    defop "..<"(Left, Right : Element_Type) -> Countable_Set
    defop "<..<"(Left, Right : Element_Type) -> Countable_Set

    defop "|"(Left, Right : Element_Type) -> Countable_Set
    defop "|"(Left : Countable_Set; Right : Element_Type) -> Countable_Set
    defop "|"(Left : Element_Type; Right : Countable_Set) -> Countable_Set
    defop "|"(Left : Countable_Set; Right : Countable_Set) -> Countable_Set

    defop "|="(var Left : Countable_Set; Right : Element_Type)
    defop "|="(var Left : Countable_Set; Right : Countable_Set)

    defop "<|="(var Left : Countable_Set; var Right : optional Element_Type)
        # Move element into set, leaving Right null afterward.

    defop "<|="(var Left : Countable_Set; var Right : Countable_Set)
	# Move all elements of Right into Left, leaving Right empty.

    defop "-"(Left, Right : Countable_Set) -> Countable_Set
      # Set difference
    defop "-"(Left : Countable_Set; Right : Element_Type) -> Countable_Set
      # Remove one element
    defop "-="(var S : Countable_Set; Elem : Element_Type)
      # Remove the given element from the set, if present
    defop "-="(var Left : Countable_Set; Right : Countable_Set)
      # Remove all elements of Right from Left, if present

    defop "or"(Left : Countable_Set; Right : Countable_Set) 
      -> Countable_Set is "|"   # union
    defop "or="(var Left : Countable_Set; Right : Countable_Set) is "|="

    defop "+"(Left : Countable_Set; Right : Countable_Set) 
      -> Countable_Set is "|"   # Union
    defop "+="(var Left : Countable_Set; Right : Countable_Set) is "|="
    defop "+="(var Left : Countable_Set; Right : Element_Type) is "|="
   
    defop "and"(Left, Right : Countable_Set) -> Countable_Set
	# Intersection
    defop "and="(var Left : Countable_Set; Right : Countable_Set)

    defop "xor"(Left, Right : Countable_Set) -> Countable_Set
	# Symmetric difference
    defop "xor="(var Left : Countable_Set; Right : Countable_Set)

    defop "in"(Left : Element_Type; Right : Countable_Set) -> Boolean

    defop "=?"(Left, Right : Countable_Set) -> Ordering
	# Return #equal if Left and Right have the same elements
	# Return #less if Left is a proper subset of Right
	# Return #greater if Left is a proper superset of Right
	# Return #unordered otherwise

    def Count(S : Countable_Set) -> Univ_Integer

    defop "magnitude"(Countable_Set) -> Univ_Integer is Count

    def Is_Empty(S : Countable_Set) -> Boolean

    def First(S : Countable_Set) -> optional Element_Type
    def Last(S : Countable_Set) -> optional Element_Type

    def Remove_First(var S : Countable_Set) -> optional Element_Type
	# Remove first element of set (lowest value)

    def Remove_Last(var S : Countable_Set) -> optional Element_Type
	# Remove last element of set (highest value)

    def Remove_Any(var S : Countable_Set) -> optional Element_Type
	# Remove an arbitrary element of set

end interface PSL::Containers::Countable_Set

interface PSL::Core::Countable<> implements Ordered<> :
    defop "+"(Left : Countable; Right : Univ_Integer) -> Countable
    defop "+"(Left : Univ_Integer; Right : Countable) -> Countable

    defop "-"(Left : Countable; Right : Univ_Integer) -> Countable
    defop "-"(Left, Right : Countable) -> Univ_Integer

    defop "=?"(Left, Right : Countable) -> Ordered::Full_Ordering

    def First() -> Countable

    def Last() -> Countable

    def Min(Left, Right : optional Countable) -> optional Countable
    def Max(Left, Right : optional Countable) -> optional Countable

    def Hash(Val : Countable) -> Univ_Integer

    defop ".."(Left, Right : Countable) -> Countable_Set<Countable>
      is in Countable_Set<Countable>
    defop "<.."(Left, Right : Countable) -> Countable_Set<Countable>
      is in Countable_Set<Countable>
    defop "..<"(Left, Right : Countable) -> Countable_Set<Countable>
      is in Countable_Set<Countable>
    defop "<..<"(Left, Right : Countable) -> Countable_Set<Countable>
      is in Countable_Set<Countable>
    defop "|"(Left, Right : Countable) -> Countable_Set<Countable>
      is in Countable_Set<Countable>
end interface PSL::Core::Countable
    
class interface PSL::Core::Univ_Integer<> :
    defop "+"(Right : Univ_Integer) -> Univ_Integer
      is import(#identity)

    defop "-"(Right : Univ_Integer) -> Univ_Integer
      is import(#negate)

    defop "abs"(Right : Univ_Integer) -> Univ_Integer
      is import("abs")

    defop "magnitude"(Univ_Integer) -> Univ_Integer is "abs"

    defop "+"(Left, Right : Univ_Integer) -> Result : Univ_Integer 
      is import("+")

    defop "-"(Left, Right : Univ_Integer) -> Result : Univ_Integer
      is import("-")

    defop "*"(Left, Right : Univ_Integer) -> Result : Univ_Integer 
      is import("*")

    defop "/"(Left, Right : Univ_Integer) -> Result : Univ_Integer
      is import("/")

    defop "**"(Left, Right : Univ_Integer) -> Univ_Integer
      is import("**")

    defop "mod"(Left, Right : Univ_Integer) -> Univ_Integer
      is import("mod")

    defop "rem"(Left, Right : Univ_Integer) -> Univ_Integer
      is import("rem")


    defop "+="(var Left : Univ_Integer; Right : Univ_Integer) 
      is import("+=")

    defop "-="(var Left : Univ_Integer; Right : Univ_Integer) 
      is import("-=")

    defop "*="(var Left : Univ_Integer; Right : Univ_Integer) 
      is import("*=")

    defop "/="(var Left : Univ_Integer; Right : Univ_Integer) 
      is import("/=")

    defop "**="(var Left : Univ_Integer; Right : Univ_Integer) 
      is import("**=")


    defop "=?"(Left, Right : Univ_Integer) -> Ordering
      is import("=?")

    defop ">>"(Univ_Integer; Univ_Integer) -> Univ_Integer is import(">>")

    defop "<<"(Univ_Integer; Univ_Integer) -> Univ_Integer is import("<<")

    def Min(Left, Right : optional Univ_Integer) -> optional Univ_Integer
      is import(#min)
    def Max(Left, Right : optional Univ_Integer) -> optional Univ_Integer
      is import(#max)

    def Hash(Val : Univ_Integer) -> Univ_Integer
      is import(#identity)

    def Print(X : Univ_Integer) is import(#print_int)

    def To_String(Val : Univ_Integer) -> Univ_String
      is import(#to_string_int)

    def From_String(Str : Univ_String) -> optional Univ_Integer
      is import(#from_string_int)

    def First() -> Univ_Integer
      is import(#univ_integer_first)

    def Last() -> Univ_Integer
      is import(#univ_integer_last)

    defop ".."(Left, Right : Univ_Integer) -> Countable_Set<Univ_Integer>
      is in Countable_Set<Univ_Integer>
    defop "<.."(Left, Right : Univ_Integer) -> Countable_Set<Univ_Integer>
      is in Countable_Set<Univ_Integer>
    defop "..<"(Left, Right : Univ_Integer) -> Countable_Set<Univ_Integer>
      is in Countable_Set<Univ_Integer>
    defop "<..<"(Left, Right : Univ_Integer) -> Countable_Set<Univ_Integer>
      is in Countable_Set<Univ_Integer>
    defop "|"(Left, Right : Univ_Integer) -> Countable_Set<Univ_Integer>
      is in Countable_Set<Univ_Integer>
end interface PSL::Core::Univ_Integer

class interface PSL::Core::Countable_Range<Bound_Type is Countable<>> :
    # Simple contiguous Countable_Range of integers (i.e. an interval)
    const First : Bound_Type
    const Last : Bound_Type
    defop ".."(Left, Right : Bound_Type) -> Countable_Range
    defop "<.."(Left, Right : Bound_Type) -> Countable_Range
    defop "..<"(Left, Right : Bound_Type) -> Countable_Range
    defop "<..<"(Left, Right : Bound_Type) -> Countable_Range
    defop "in"(Val : Bound_Type; Int : Countable_Range) -> Boolean
    def Length(R : Countable_Range) -> Univ_Integer
    defop "magnitude"(Countable_Range) -> Univ_Integer is Length
    defop "[..]"() -> Countable_Range
    defop "[]"() -> Countable_Range
    def Singleton(Bound : Bound_Type) -> Countable_Range

    def Remove_First(var S : Countable_Range) -> optional Bound_Type
    def Remove_Last(var S : Countable_Range) -> optional Bound_Type
    def Remove_Any(var S : Countable_Range) -> optional Bound_Type
end interface PSL::Core::Countable_Range

class PSL::Core::Countable_Range :
exports :   # check that indent of 0 works
    defop ".."(Left, Right : Bound_Type) -> Countable_Range :
	return (First => Left, Last => Right)
    end defop ".."

    defop "<.."(Left, Right : Bound_Type) -> Countable_Range :
	return (First => Left+1, Last => Right)
    end defop "<.."

    defop "..<"(Left, Right : Bound_Type) -> Countable_Range :
	return (First => Left, Last => Right-1)
    end defop "..<"

    defop "<..<"(Left, Right : Bound_Type) -> Countable_Range :
	return (First => Left+1, Last => Right-1)
    end defop "<..<"

    defop "in"(Val : Bound_Type; Int : Countable_Range) -> Boolean :
	return Val >= Int.First and then Val <= Int.Last
    end defop "in"

    def Length(R : Countable_Range) -> Univ_Integer :
	return R.Last - R.First + 1
    end def Length

    defop "[]"() -> Countable_Range :
	return Bound_Type::First()+1 .. Bound_Type::First()
    end defop "[]"

    defop "[..]"() -> Countable_Range :
        return Bound_Type::First() .. Bound_Type::Last()
    end defop "[..]"

    def Singleton(Bound : Bound_Type) -> Countable_Range :
	return Bound .. Bound
    end def Singleton

    def Remove_First(var S : Countable_Range) 
      -> Result : optional Bound_Type :
	if S.First <= S.Last :
	    Result = S.First
	    S = (First => S.First+1, Last => S.Last)
	else :
	    Result = null
	end if
    end def Remove_First
	
    def Remove_Last(var S : Countable_Range) 
      -> Result : optional Bound_Type :
	if S.First <= S.Last :
	    Result = S.Last
	    S = (First => S.First, Last => S.Last-1)
	else :
	    Result = null
	end if
    end def Remove_Last

    def Remove_Any(var S : Countable_Range) 
      -> Result : optional Bound_Type :
	if S.First <= S.Last :
	    if (S.Last - S.First) mod 2 == 0 :
		return Remove_First(S)
	    else :
		return Remove_Last(S)
	    end if
	else :
	    return null
	end if
    end def Remove_Any

end class PSL::Core::Countable_Range

class interface PSL::Core::Univ_Character<> :
    def Print(C : Univ_Character) is import(#print_char)

    defop "+"(Left : Univ_Character; Right : Univ_Integer) -> Univ_Character
      is import("+")
    defop "+"(Left : Univ_Integer; Right : Univ_Character) -> Univ_Character
      is import("+")

    defop "-"(Left : Univ_Character; Right : Univ_Integer) -> Univ_Character
      is import("-")
    defop "-"(Left, Right : Univ_Character) -> Univ_Integer
      is import("-")

    defop "*"(Left : Univ_Integer; Right : Univ_Character) -> Univ_String
	# Produce specified number of "Right" chars in a row
    defop "*"(Left : Univ_Character; Right : Univ_Integer) -> Univ_String
	# Produce specified number of "Left" chars in a row

    defop "=?"(Left, Right : Univ_Character) -> Ordering
      is import("=?")

    def To_String(Val : Univ_Character) -> Univ_String
      is import(#to_string_char)

    def From_String(Str : Univ_String) -> optional Univ_Character
      is import(#from_string_char)

    def Hash(Val : Univ_Character) -> Univ_Integer
      is import(#identity)

    def First() -> Univ_Character

    def Last() -> Univ_Character

    def Min(Left, Right : optional Univ_Character) -> optional Univ_Character
      is import(#min)
    def Max(Left, Right : optional Univ_Character) -> optional Univ_Character
      is import(#max)

    defop ".."(Left, Right : Univ_Character) -> Countable_Set<Univ_Character>
      is in Countable_Set<Univ_Character>
    defop "<.."(Left, Right : Univ_Character) -> Countable_Set<Univ_Character>
      is in Countable_Set<Univ_Character>
    defop "..<"(Left, Right : Univ_Character) -> Countable_Set<Univ_Character>
      is in Countable_Set<Univ_Character>
    defop "<..<"(Left, Right : Univ_Character) -> Countable_Set<Univ_Character>
      is in Countable_Set<Univ_Character>
    defop "|"(Left, Right : Univ_Character) -> Countable_Set<Univ_Character>
      is in Countable_Set<Univ_Character>
end interface PSL::Core::Univ_Character
    
class PSL::Core::Univ_Character :
  exports :
    defop "*"(Left : Univ_Integer; Right : Univ_Character) -> Univ_String :
	# Produce specified number of "Right" chars in a row
	return Left * To_String(Right)   # Just pass the buck to the string defop
    end defop "*"

    defop "*"(Left : Univ_Character; Right : Univ_Integer) -> Univ_String :
	# Produce specified number of "Left" chars in a row
	return Right * To_String(Left)   # Just pass the buck to the string defop
    end defop "*"

    def First() -> Univ_Character :
	return '\0'
    end def First

    def Last() -> Univ_Character :
	return '\0' + 2**31-1
    end def Last
end class PSL::Core::Univ_Character

interface PSL::Core::Imageable<> :
    def To_String(Val : Imageable) -> Univ_String<>

    def From_String(Str : Univ_String<>) -> optional Imageable

    # NOTE: We include Hashable<> operations here
    #       so that Set<Imageable+> works nicely.
    #       Clearly if something is Imageable it is possible
    #       to implement "=?" and Hash using the string image,
    #       so we might as well requires these operations too.

    defop "=?"(Left, Right : Imageable) -> Ordering
    def Hash(Val : Imageable) -> Univ_Integer
end interface PSL::Core::Imageable

class interface PSL::Core::Univ_String<> :
    def Print(Univ_String) is import(#print_string)
    def Println(Univ_String) is import(#println_string)
    def Readln() -> optional Univ_String is import(#read_string)

    defop "*"(Left : Univ_Integer; Right : Univ_String) -> Univ_String
	# Produce specified number of "Right" strings in a row
    defop "*"(Left : Univ_String; Right : Univ_Integer) -> Univ_String
	# Produce specified number of "Left" strings in a row

    defop "|"(Left, Right : Univ_String) -> Univ_String 
      is import(#concat_string)

    defop "=?"(Left, Right : Univ_String) -> Ordering
      is import(#string_compare)

    defop "|="(var Left : Univ_String; Right : Univ_String)
      is import(#assign_concat_string)

    defop "indexing"(Str : Univ_String; Index : Univ_Integer<>) -> Univ_Character
      is import(#string_indexing)
	# a "read-only" element, indexed 1 .. |Str|

    defop "index_set"(Str : Univ_String) -> Countable_Set<Univ_Integer>
        # Return set of indices for string

    defop "slicing"(Str : Univ_String;
      Index_Set : Countable_Range<Univ_Integer>) 
      -> Univ_String is import(#string_slicing)
	# a "read-only" slice

    def Length(Str : Univ_String) -> Univ_Integer
      is import(#string_length)

    defop "magnitude"(Univ_String) -> Univ_Integer
      is import(#string_length)

    def Hash(Val : Univ_String) -> Univ_Integer
      is import(#identity)

    defop "|"(Left : Univ_String; Right : Right_Type is Imageable<>) 
      -> Univ_String

    defop "|"(Left : Left_Type is Imageable<>; Right : Univ_String)
      -> Univ_String

    defop "|="(var Left : Univ_String; Right : Right_Type is Imageable<>)

    # Operations to convert to/from a Vector of Univ_Character's
    def To_Vector(Str : Univ_String) -> Vector<Univ_Character>
    def From_Vector(Vec : Vector<Univ_Character>) -> Univ_String

  implements for Imageable
    # These operations are needed so Univ_String satifies
    # requirements of "Imageable" interface, but these
    # operations are not directly callable, to avoid ambiguities.

    def To_String(Val : Univ_String) -> Univ_String
      is import (#identity)
    def From_String(Str : Univ_String) -> optional Univ_String
      is import (#identity)

end interface PSL::Core::Univ_String
    
class PSL::Core::Univ_String :
  exports :
    defop "index_set"(Str : Univ_String) -> Countable_Set<Univ_Integer> :
        # Return set of indices for string
        return 1..|Str|
    end defop "index_set"

    defop "*"(Left : Univ_Integer; Right : Univ_String) -> Univ_String :
	# Produce specified number of "Right" strings in a row
	if Left <= 0 :
	    return ""
	elsif Left == 1 :
	    return Right
	else :
	    # Recurse to produce half-length, and then combine
	    const Partial : Univ_String = (Left/2) * Right
	    if Left mod 2 == 1 :
		return Partial | Partial | Right
	    else :
		return Partial | Partial
	    end if
	end if
    end defop "*"

    defop "*"(Left : Univ_String; Right : Univ_Integer) -> Univ_String :
	# Produce specified number of "Left" strings in a row
	return Right * Left   # Just pass the buck to other "*"
    end defop "*"

    defop "|"(Left : Univ_String; Right : Right_Type is Imageable<>) 
      -> Univ_String :
        if Right is null :
            return Left | "null"
        else :
            return Left | Right_Type::To_String(Right)
        end if
    end defop "|"

    defop "|"(Left : Left_Type is Imageable<>; Right : Univ_String)
      -> Univ_String :
        if Left is null :
            return "null" | Right
        else :
            return Left_Type::To_String(Left) | Right
        end if
    end defop "|"

    defop "|="(var Left : Univ_String; Right : Right_Type is Imageable<>) :
        if Right is null :
            Left |= "null"
        else :
            Left |= Right_Type::To_String(Right)
        end if
    end defop "|="

    def To_Vector(Str : Univ_String) -> Vector<Univ_Character> :
        return [Str[I] for I in 1 .. |Str|]
    end def To_Vector
    def From_Vector(Vec : Vector<Univ_Character>) -> Univ_String :
        return (for each C of Vec forward => <""> | C)
end class PSL::Core::Univ_String

class interface PSL::Core::Random<> :
    def Start(Seed : Univ_Integer = 1) -> Random
      # Start a new random number sequence with a standard multiplier/modulus

    def Start(Seed : Univ_Integer;
       Mult, Mod : Univ_Integer) -> Random
      # Start a new random number sequence with given multiplier and modulus

    def Next(var Seq : Random) -> Univ_Integer
      # Get next value in random number sequence
end interface PSL::Core::Random

class PSL::Core::Random :
    var Last_Value : Univ_Integer
    const Mult : Univ_Integer
    const Mod : Univ_Integer
    const Debugging : Boolean = #false
exports :   # check that indent of 0 works
    def Start(Seed : Univ_Integer) -> Random :
      # Start a new random number sequence with a standard multiplier/modulus
	return (Last_Value => Seed,
	  Mult => 7**5, Mod => 2**31 - 1)
    end def Start

    def Start(Seed : Univ_Integer;
      Mult, Mod : Univ_Integer) -> Random :
      # Start a new random number sequence with given multiplier and modulus
	if Debugging :
	    Println("Random: Seed = " | Seed | ", Mult = " | Mult | 
	      ", Mod = " | Mod)
	end if
	return (Last_Value => Seed, Mult => Mult, Mod => Mod)
    end def Start

    def Next(var Seq : Random) -> Univ_Integer :
      # Get next value in random number sequence
	Seq.Last_Value = Seq.Last_Value * Seq.Mult mod Seq.Mod
	return Seq.Last_Value
    end def Next
end class PSL::Core::Random

def PSL::Test::Test_String() :
    const U = "tab\t"
    const T = U[4]
    Println("U = " | U | ", |U| = " | |U| | ", U[4] = '" | T | "'")
    const X = "this is a string"
    const Y = X[3]
    {> Y == 'i' <} # string indexing problem
    const Z = X[6..7]
    {> Z == "is" <} # string slicing problem, expected 'is', found Z 
    Println("X = " | X | ", X[3] = " | Y | ", X[6..7] = " | Z)

    Println("About to indent 4 and then print 'hello'")
    Println(4 * " " | "hello")

    const Seven_Xs = "x" * 7
    {> Seven_Xs == "xxxxxxx" <} # Char multiplication check

    Println("Here are seven x's: " | Seven_Xs)
    
    Print("Here are 6 y's: ")
    Println('y' * 6)
end def PSL::Test::Test_String

interface PSL::Core::Sequence<Element_Type is Assignable<>> :
    def Remove_First(var S : Sequence) -> optional Element_Type
      # Returns null when sequence is empty
end interface PSL::Core::Sequence

class interface PSL::Core::Direction<> :
    # This is passed to the "to_sequence" operator to generate
    # appropriate direction of sequence
    defop "from_univ"(Lit : Univ_Enumeration)
      {> Lit in #unordered | #forward | #reverse | #concurrent <} -> Direction 
      is import(#direction_from_univ)

    defop "to_univ"(Val : Direction) -> Univ_Enumeration 
      is import(#direction_to_univ)

    defop "=?"(Left, Right : Direction) -> Ordering
      is import("=?")
end interface PSL::Core::Direction

class interface PSL::Core::Integer
  <Range : Countable_Range<Univ_Integer> = -2**62 .. +2**62> :
    # NOTE: We restrict Integer to this range so Univ_Integer can use
    #       values outside this range as indices into an extended-range
    #       integer table.
    defop "from_univ"(Lit : Univ_Integer) -> Integer 
      is import(#integer_from_univ)

    defop "to_univ"(Val : Integer) -> Univ_Integer 
      is import(#integer_to_univ)

    defop "+"(Right : Integer) -> Integer
      is import(#identity)

    defop "-"(Right : Integer) -> Integer
      is import(#negate)

    defop "abs"(Right : Integer) -> Integer
      is import("abs")

    defop "magnitude"(Integer) -> Integer is "abs"

    defop "+"(Left, Right : Integer) -> Result : Integer 
      is import("+")

    defop "-"(Left, Right : Integer) -> Result : Integer
      is import("-")

    defop "*"(Left, Right : Integer) -> Result : Integer 
      is import("*")

    defop "/"(Left, Right : Integer) -> Result : Integer
      is import("/")

    defop "mod"(Left, Right : Integer) -> Integer
      is import("mod")

    defop "rem"(Left, Right : Integer) -> Integer
      is import("rem")

    defop "**"(Left, Right : Integer) -> Result : Integer
      is import("**")

    defop "+="(var Left : Integer; Right : Integer) 
      is import("+=")

    defop "-="(var Left : Integer; Right : Integer) 
      is import("-=")

    defop "*="(var Left : Integer; Right : Integer) 
      is import("*=")

    defop "/="(var Left : Integer; Right : Integer) 
      is import("/=")

    defop "**="(var Left : Integer; Right : Integer) 
      is import("**=")

    defop "=?"(Left, Right : Integer) -> Ordering
      is import("=?")

    defop ">>"(Integer; Integer) -> Integer is import(">>")

    defop "<<"(Integer; Integer) -> Integer is import("<<")

    def Min(Left, Right : optional Integer) -> optional Integer
      is import(#min)
    def Max(Left, Right : optional Integer) -> optional Integer
      is import(#max)

    def Hash(Val : Integer) -> Univ_Integer
      is import(#identity)

    def Print(X : Integer) is import(#print_int)

    def To_String(Val : Integer) -> Univ_String
      is import(#to_string_int)

    def From_String(Str : Univ_String) -> optional Integer
      is import(#from_string_int)

    def First() -> Integer

    def Last() -> Integer

    defop "[..]"()->Countable_Range<Integer> is in Countable_Range<Integer>

    defop ".."(Left, Right : Integer) -> Countable_Set<Integer>
      is in Countable_Set<Integer>
    defop "<.."(Left, Right : Integer) -> Countable_Set<Integer>
      is in Countable_Set<Integer>
    defop "..<"(Left, Right : Integer) -> Countable_Set<Integer>
      is in Countable_Set<Integer>
    defop "<..<"(Left, Right : Integer) -> Countable_Set<Integer>
      is in Countable_Set<Integer>
    defop "|"(Left, Right : Integer) -> Countable_Set<Integer>
      is in Countable_Set<Integer>
  implements for Countable
    # These operations are needed so Integer satifies
    # requirements of "Countable" interface, but these
    # operations are not directly callable (if they were callable,
    # we would have ambiguity when adding an integer to an int-literal).

    defop "+"(Left : Integer; Right : Univ_Integer) -> Result : Integer 
      is import("+")

    defop "+"(Left : Univ_Integer; Right : Integer) -> Result : Integer 
      is import("+")

    defop "-"(Left : Integer; Right : Univ_Integer) -> Result : Integer 
      is import("-")

    defop "-"(Left, Right : Integer) -> Result : Univ_Integer
      is import("-")

end interface PSL::Core::Integer

class PSL::Core::Integer :
    const Content : Univ_Integer;  # So this ends up as a wrapper
  exports :
    def First() -> Integer :
	return Range.First
    end def First

    def Last() -> Integer :
	return Range.Last
    end def Last

end class PSL::Core::Integer
    
class PSL::Core::Boolean :
  exports :
    defop "and="(var Left : Boolean; Right : Boolean) :
	Left = Left and Right
    end defop "and="

    defop "or="(var Left : Boolean; Right : Boolean) :
	Left = Left or Right
    end defop "or="

    defop "xor="(var Left : Boolean; Right : Boolean) :
	Left = Left xor Right
    end defop "xor="

    def To_String(Val : optional Boolean) -> Univ_String :
	if Val is null :
            return "null"
        elsif Val :
	    return "#true"
	else :
	    return "#false"
	end if
    end def To_String

    def From_String(Str : Univ_String) -> optional Boolean :
	if Str == "#true" :
	    return #true
	elsif Str == "#false" :
	    return #false
	else :
	    return null
	end if
    end def From_String

end class PSL::Core::Boolean

def PSL::Test::Test_Boolean(UX, UY : Univ_Enumeration) :
    const X : Boolean = UX
    const Y : Boolean = UY

    Println(X | " and " | Y | " = " | (X and Y))
    Println(X | " or " | Y | " = " | (X or Y))
    Println(X | " xor " | Y | " = " | (X xor Y))

    var XX = X
    XX and= Y
    Println(X | " and= " | Y | " = " | XX)
    XX = X
    XX or= Y
    Println(X | " or= " | Y | " = " | XX)
    XX = X
    XX xor= Y
    Println(X | " xor= " | Y | " = " | XX)
end def PSL::Test::Test_Boolean

class interface PSL::Core::Basic_List<List_Elem is Assignable<>> :
    var Elem : List_Elem
    var Next : optional Basic_List

    defop "|"(Left : List_Elem; Right : List_Elem) -> Basic_List
    defop "|"(Left : List_Elem; Right : Basic_List) -> Basic_List
    defop "|"(Left : Basic_List; Right : Basic_List) -> Basic_List
    defop "|"(Left : Basic_List; Right : List_Elem) -> Basic_List

    defop "|="(var Left : Basic_List; Right : Basic_List)
    defop "|="(var Left : Basic_List; Right : List_Elem)
    def Length(L : optional Basic_List) -> Univ_Integer
    defop "magnitude"(L : optional Basic_List) -> Univ_Integer is Length

    defop "[]"() -> optional Basic_List is (null)

    def Remove_First(var L : Basic_List) -> optional List_Elem
    def Remove_Last(var L : Basic_List) -> optional List_Elem
    def Remove_Any(var L : Basic_List) -> optional List_Elem
end interface PSL::Core::Basic_List

class PSL::Core::Basic_List :
  exports :
    defop "|"(Left : List_Elem; Right : List_Elem) -> Basic_List :
        return (Elem => Left, Next => (Elem => Right, Next => null))
    end defop "|"

    defop "|"(Left : List_Elem; Right : Basic_List) -> Basic_List :
        return (Elem => Left, Next => Right)
    end defop "|"

    defop "|"(Left : Basic_List; Right : Basic_List) -> Basic_List :
        if Left is null :
            # Left is null, so just return Right
            return Right
        else :
            # Recurse with tail of left Basic_list
            return (Elem => Left.Elem, Next => Left.Next | Right)
        end if
    end defop "|"

    defop "|"(Left : Basic_List; Right : List_Elem) -> Basic_List :
        return Left | (Elem => Right, Next => null)
    end defop "|"

    defop "|="(var Left : Basic_List; Right : Basic_List) :
        if Left is null :
            Left = Right
        else :
            # Recurse with tail of Basic_list
            Left.Next |= Right
        end if
    end defop "|="

    defop "|="(var Left : Basic_List; Right : List_Elem) :
        # Just pass the buck
        Left |= (Elem => Right, Next => null)
    end defop "|="

    def Length(L : optional Basic_List) -> Result : Univ_Integer :
        Result = 0
        for Lst = L then Lst.Next while Lst not null :
            Result += 1
        end loop
    end def Length

    def Remove_First(var L : Basic_List) 
      -> Result : optional List_Elem :
        if L is null :
            return null
        else :
            Result = L.Elem
            L <== L.Next
              # carve off L.Next and set L to that
        end if
    end def Remove_First

    def Remove_Last(var L : Basic_List) 
      -> Result : optional List_Elem :
        if L is null :
            return null
        elsif L.Next is null :
            Result = L.Elem
            L = null
        else :
            # Recurse to remove last element
            return Remove_Last(L.Next)
        end if
    end def Remove_Last

    def Remove_Any(var L : Basic_List) 
      -> Result : optional List_Elem :
        # Easiest to remove first element
        return Remove_First(L)
    end def Remove_Any

end class PSL::Core::Basic_List


def PSL::Test::Test_List(X, Y : Univ_Integer) :
    type Univ_List is Basic_List<Univ_Integer>

    def Println(L : Univ_List) :
        # Print a univ Basic_list
        var N = L
        while N not null :
            const E : Univ_Integer = N.Elem
            Print("" | E)
            N = N.Next
            if N not null :
                Print(", ")
            end if
        end loop

        Print("\n")
    end def Println

    def Println_It(L : Univ_List) :
        # Print a univ Basic_list using destructive iterator
        Print("Forward: ")
        var F = L
        while F not null :
            const E : Univ_Integer = Remove_First(F)
            Print("" | E)
            if F not null :
                Print(", ")
            end if
        end loop

        Print("\n")

        Print("Reverse: ")
        var R = L
        while R not null :
            const Z : Univ_Integer = Remove_Last(R)
            Print("" | Z)
            if R not null :
                Print(", ")
            end if
        end loop

        Print("\n")
    end def Println_It

    var L1 : Univ_List = (Elem => X, Next => null)
    var L2 : Univ_List = (Elem => Y, Next => null)
    var L3 = L1 | L2

    Println("Combining " | X | " and " | Y | " produces ")
    Println(L3)
    Println("Combining X | Y | X | X | Y | Y produces ")
    Println_It(L3 | X | L3 | Y)
end def PSL::Test::Test_List

class interface PSL::Core::Closed_Interval<Bound_Type is Comparable<>> :
    # This provides a simple "closed" interval X..Y
    # If a type is countable, then half-open or fully open intervals
    # can be converted into the equivalent closed interval.
    var Low : Bound_Type
    var High : Bound_Type
    defop "in"(Left : Bound_Type; Right : Closed_Interval) -> Boolean
    defop "=?"(Left, Right : Closed_Interval) -> Ordering
end interface PSL::Core::Closed_Interval

class PSL::Core::Closed_Interval :
  exports :
    defop "in"(Left : Bound_Type; Right : Closed_Interval) -> Boolean :
        return Left >= Right.Low and then Left <= Right.High
    end defop "in"

    defop "=?"(Left, Right : Closed_Interval) -> Ordering :
        # Compare two intervals, and consider them #unordered
        # if they overlap at all, unless they are identical.
        # If they don't overlap, then return #less or #greater as appropriate.

        if Left.High < Right.Low :
            return #less
        elsif Left.Low > Right.High :
            return #greater
        elsif Left.Low == Right.Low and then
          Left.High == Right.High :
            return #equal
        else :
            return #unordered
        end if
    end defop "=?"

end class PSL::Core::Closed_Interval

class interface PSL::Core::Interval<Bound_Type is Comparable<>> :
    # This supports closed, half-open, and open intervals.
    # This is appropriate for uncountable types where you
    # can't normalize all intervals into closed intervals.
    var Low : Bound_Type
    var Low_Is_Open : Boolean
    var High : Bound_Type
    var High_Is_Open : Boolean

    def Singleton(Val : Bound_Type) -> Interval
      # Return interval consisting of a single value

    def Is_Empty(IV : optional Interval) -> Boolean
      # Return True if interval is null or it represents
      # no values

    defop "in"(Left : Bound_Type; Right : Interval) -> Boolean

    defop "=?"(Left, Right : Interval) -> Ordering
      # "#greater" means Left is strictly greater than Right
      # "#less" means Left is strictly less than Right
      # "#equal" means Left and Right are the same interval
      # "#unordered" means anything else

    defop "and"(Left, Right : Interval) -> optional Interval
      # Return intersection of the two intervals

    defop "and="(var Left : optional Interval; Right : Interval)
      # Intersect Right into Left

    def Is_Strictly_Within(Left, Right : optional Interval) -> Boolean
      # Return #true if Left is strictly within Right,
      # meaning low bound of Left is higher, and high bound is lower.
      # NOTE: If #true, then removing Right from Left will produce two 
      #       non-overlapping intervals.

    defop "-"(Left, Right : Interval) {> not Is_Strictly_Within(Right, Left) <} 
      -> optional Interval
      # Subtract out Right interval from Left interval
      # Right must not be strictly within Left, since
      # that would require returning two intervals.

    defop "-="(var Left : optional Interval; Right : Interval) 
      {> not Is_Strictly_Within(Right, Left) <}
      # Subtract Right interval from Left

    def Overlaps(Left, Right : optional Interval) -> Boolean
      # Return True if intervals overlap

    defop "or"(Left, Right : Interval) {> Overlaps(Left, Right) <} -> Interval
      # Return union of two intervals.  Must have some overlap
      # to ensure that result can be represented as a single interval.
    defop "|"(Left, Right : Interval) {> Overlaps(Left, Right) <} -> Interval is "or"

    defop "or="(var Left : optional Interval; Right : Interval) 
      {> Overlaps(Left, Right) <}
      # Compute union of two intervals.  Must have some overlap
      # to ensure that result can be represented as a single interval.
    defop "|="(var Left : optional Interval; Right : Interval) 
      {> Overlaps(Left, Right) <} is "or="

    defop "<|="(var Left, Right : optional Interval) {> Overlaps(Left, Right) <}
      # Compute union of two intervals; leave Right empty.  
      # Must have some overlap to ensure that result can be 
      # represented as a single interval.

end interface PSL::Core::Interval

class PSL::Core::Interval :
  exports :
    def Singleton(Val : Bound_Type) -> Interval :
      # Return interval consisting of a single value
	return (Low => Val, Low_Is_Open => #false,
	  High => Val, High_Is_Open => #false)
    end def Singleton

    def Is_Empty(IV : optional Interval) -> Boolean :
      # Return True if interval is null or it represents
      # no values
	return IV is null or else
	  IV.Low > IV.High or else
	  (IV.Low == IV.High and then (IV.Low_Is_Open or IV.High_Is_Open))
    end def Is_Empty

    defop "in"(Left : Bound_Type; Right : Interval) -> Boolean :
        switch Left =? Right.Low :
          case #less :
            return #false
          case #equal : 
            return not Right.Low_Is_Open
          case #greater :
            switch Left =? Right.High :
              case #less :
                return #true
              case #equal :
                return not Right.High_Is_Open
              case #greater :
                return #false
            end switch
        end switch
    end defop "in"

    defop "=?"(Left, Right : Interval) -> Ordering :
        # Compare two intervals, and consider them #unordered
        # if they overlap at all, unless they are identical.
        # If they don't overlap, then return #less or #greater as appropriate.
        # Return #unordered if any of the comparisons return #unordered.

        # First check for perfect equality
        if Left.Low == Right.Low and then
          Left.High == Right.High and then
          Left.Low_Is_Open == Right.Low_Is_Open and then
          Left.High_Is_Open == Right.High_Is_Open :
            # NOTE: We are considering X..Y-1 != X..<Y since
            #       we don't require countable elements.
            return #equal
        end if

        switch Left.High =? Right.Low :
          case #unordered :
            return #unordered
          case #less :
            return #less
          case #equal :
            if Left.High_Is_Open or else Right.Low_Is_Open :
                # No overlap
                return #less
            else :
                # We have already ruled out #equal
                return #unordered
            end if
          case #greater :
            # Not clearly less, see whether clearly greater.
            switch Left.Low =? Right.High :
              case #unordered :
                return #unordered
              case #less :
                # We have already ruled out #equal
                return #unordered
              case #equal :
                if Left.Low_Is_Open or else Right.High_Is_Open :
                    # No overlap
                    return #greater
                else :
                    # We have already ruled out #equal
                    return #unordered
                end if
              case #greater :
                return #greater
            end switch
        end switch
    end defop "=?"

    defop "and"(Left, Right : Interval) -> optional Interval :
      # Return intersection of the two intervals
	var New_Low : Bound_Type
	var New_High : Bound_Type
        var New_Low_Is_Open : Boolean
	var New_High_Is_Open : Boolean
	
	switch Left.Low =? Right.Low :
	  case #less : 
	    New_Low = Right.Low
	    New_Low_Is_Open = Right.Low_Is_Open
	  case #greater : 
	    New_Low = Left.Low
	    New_Low_Is_Open = Left.Low_Is_Open
	  case #equal : 
	    New_Low = Left.Low
	    New_Low_Is_Open = Left.Low_Is_Open or Right.Low_Is_Open
	  case #unordered : 
	    return null
	end switch

	switch Left.High =? Right.High :
	  case #less : 
	    New_High = Left.High
	    New_High_Is_Open = Left.High_Is_Open
	  case #greater : 
	    New_High = Right.High
	    New_High_Is_Open = Right.High_Is_Open
	  case #equal : 
	    New_High = Left.High
	    New_High_Is_Open = Left.High_Is_Open or Right.High_Is_Open
	  case #unordered : 
	    return null
	end switch

	return (Low => New_Low, Low_Is_Open => New_Low_Is_Open,
	  High => New_High, High_Is_Open => New_High_Is_Open)
    end defop "and"

    defop "and="(var Left : optional Interval; Right : Interval) :
      # Intersect Right into Left
	if Left not null :
	    Left = Left and Right
	end if
    end defop "and="

    def Is_Strictly_Within(Left, Right : optional Interval) -> Boolean :
      # Return #true if Left is strictly within Right,
      # meaning low bound of Left is higher, and high bound is lower.
      # NOTE: If #true, then removing Right from Left will produce two 
      #       non-overlapping intervals.
	if Right is null or else Left is null :
	    return #false
	end if
	return (Left.Low > Right.Low or else
	  (Left.Low == Right.Low and then Left.Low_Is_Open > Right.Low_Is_Open))
	  and then
	    (Left.High < Right.High or else
	     (Left.High == Right.High and then
	      Left.High_Is_Open > Right.High_Is_Open))
    end def Is_Strictly_Within

    defop "-"(Left, Right : Interval) {> not Is_Strictly_Within(Right, Left) <} 
      -> Result : optional Interval :
      # Subtract out Right interval from Left interval
      # Right must not be strictly within Left, since
      # that would require returning two intervals.
	var New_Low : Bound_Type
	var New_High : Bound_Type
        var New_Low_Is_Open : Boolean
	var New_High_Is_Open : Boolean
	
	switch Left.Low =? Right.Low :
	  case #less : 
	    # Return left part of Left
	    Result = (Low => Left.Low, Low_Is_Open => Left.Low_Is_Open,
	      High => Right.Low, High_Is_Open => not Right.Low_Is_Open)
	  case #greater : 
	    # Return right part of Left
	    Result = (Low => Right.High, Low_Is_Open => not Right.High_Is_Open,
	      High => Left.High, High_Is_Open => Left.High_Is_Open)
	  case #equal : 
	    if Left.Low_Is_Open >= Right.Low_Is_Open :
		# Return right part of Left
		Result = (Low => Right.High, 
		  Low_Is_Open => not Right.High_Is_Open,
		  High => Left.High, 
		  High_Is_Open => Left.High_Is_Open)
	    else :
		# Only one element is left
		return (Left.Low, #false, Left.Low, #false)
	    end if
	  case #unordered : 
	    return null
	end switch

	if Result.Low > Result.High :
	    # Empty interval
	    return null
	elsif Result.Low == Result.High and then
	  (Result.Low_Is_Open or Result.High_Is_Open) :
	    # Empty interval
	    return null
	else :
	    return Result
	end if
    end defop "-"

    defop "-="(var Left : optional Interval; Right : Interval) 
      {> not Is_Strictly_Within(Right, Left) <} :
      # Subtract Right interval from Left
	if Left not null :
	    Left = Left - Right
	end if
    end defop "-="

    def Overlaps(Left, Right : optional Interval) -> Boolean :
	if Left is null or else Right is null :
	    return #false
	else :
	    switch Left =? Right :
	      case #equal | #unordered :
                return #true
	      case #less | #greater :
                return #false
	    end switch
	end if
    end def Overlaps

    defop "or"(Left, Right : Interval) {> Overlaps(Left, Right) <} 
      -> Result : Interval :
      # Return union of two intervals.  Must have some overlap
      # to ensure that result can be represented as a single interval.
	
	Result = Left
	Result or= Right
    end defop "or"

    defop "or="(var Left : optional Interval; Right : Interval) 
      {> Overlaps(Left, Right) <} :
      # Compute union of two intervals.  Must have some overlap
      # to ensure that result can be represented as a single interval.
	var Right_Copy for Left = Right
	Left <|= Right_Copy
    end defop "or="

    defop "<|="(var Left, Right : optional Interval) {> Overlaps(Left, Right) <} :
      # Compute union of two intervals; leave Right empty.  
      # Must have some overlap to ensure that result can be 
      # represented as a single interval.
	if Left is null :
	    Left <== Right
	elsif Right not null :
	    var Right_Copy <== Right
		# Make copy and null out Right, so
		# we don't end up with Right partially nulled out
	    
	    switch Left.Low =? Right_Copy.Low :
	      case #less : 
		# No change to Left.Low
		null
	      case #greater : 
		Left.Low <== Right_Copy.Low
		Left.Low_Is_Open = Right_Copy.Low_Is_Open
	      case #equal : 
		Left.Low_Is_Open and= Right_Copy.Low_Is_Open
	    end switch

	    switch Left.High =? Right_Copy.High :
	      case #less : 
		Left.High <== Right_Copy.High
		Left.High_Is_Open = Right_Copy.High_Is_Open
	      case #greater : 
		# No change to Left.High
		null
	      case #equal : 
		Left.High_Is_Open and= Right_Copy.High_Is_Open
	    end switch
	end if
    end defop "<|="

end class PSL::Core::Interval

def PSL::Test::Test_Interval(X, Y, Z : Univ_Integer) :
    var Y_Up_To_Z : Interval<Univ_Integer> = 
      (Low => Y, Low_Is_Open => #false, High => Z, High_Is_Open => #true)

    Println(X | " in " | Y | "..<" | Z | " = " | (X in Y_Up_To_Z))

    var Y_To_Z : Closed_Interval<Univ_Integer> = (Low => Y, High => Z)

    Println(X | " in " | Y | ".." | Z | " = " | (X in Y_To_Z))

    var One_To_X : Closed_Interval<Univ_Integer> =
      (Low => 1, High => X)

    Println(1 | ".." | X | "=?" | Y | ".." | Z | " = " | 
      (One_To_X =? Y_To_Z))

end def PSL::Test::Test_Interval

class interface PSL::Core::AA_Tree<Element is Comparable<>> :

    # This module implements a balanced "AA" tree, originally
    # described by Arne Andersson in the "Proceedings of the Workshop
    # on Algorithms and Data Structures," pp 60-71, Springer Verlag, 1993.
    # The following algorithm and descriptions were taken from the
    # WikiPedia article on AA_Tree: 
    #       http://en.wikipedia.org/wiki/AA_tree
    # Note that various additional checks for a null tree have been added.

    # Only two operations are needed for maintaining balance in an AA tree.
    # These operations are called skew and split. Skew is a right rotation
    # when an insertion or deletion creates a left horizontal link. Split
    # is a conditional left rotation when an insertion or deletion creates two
    # horizontal right links, which once again corresponds to two
    # consecutive red links in red-black trees.

    defop "[]"() -> optional AA_Tree
        # Create an empty tree

    def Insert(var T : optional AA_Tree; X : Element)
        # input: X, the value to be inserted, and 
        # T, the root of the tree to insert it into.
        # output: A balanced T' including X.

    def Delete(var T : optional AA_Tree; X : Element)
        # input: X, the value to delete, and T, 
        # the root of the tree from which it should be deleted.
        # output: T', balanced, without the value X.

    defop "in"(X : Element; T : optional AA_Tree) -> Boolean

    def Overlapping(T : optional AA_Tree; X : Element) -> optional Element
        # input: X, the value to find, and T, 
        # the root of the tree to be searched.
        # output: the element equal to or "unordered" relative to X.

    defop "|="(var T : optional AA_Tree; X : Element) is Insert

    defop "<|="(var T : optional AA_Tree; var X : optional Element)
	# Move X into AA_Tree, leaving X null.

    def First(T : optional AA_Tree) -> optional Element
      # Return first (smallest) element in tree

    def Last(T : optional AA_Tree) -> optional Element
      # Return last (greatest) element in tree

    def Remove_First(var T : optional AA_Tree) -> optional Element
      # Remove first (smallest) element in tree

    def Remove_Last(var T : optional AA_Tree) -> optional Element
      # Remove last (greatest) element in tree

    def Remove_Any(var T : optional AA_Tree) -> optional Element
      # Remove some element from tree

    def Count(T : optional AA_Tree) -> Univ_Integer
      # Return a count of the nodes in the tree

    defop "magnitude"(AA_Tree) -> Univ_Integer is Count

    def Is_Empty(T : optional AA_Tree) -> Boolean
      # Return True if the tree is empty

end interface PSL::Core::AA_Tree

class PSL::Core::AA_Tree :
    var Value : Element
    var Level : Univ_Integer = 0
    var Left : optional AA_Tree
    var Right : optional AA_Tree

    def Node(var Value : optional Element; Level : Univ_Integer;
      Left, Right : optional AA_Tree) -> AA_Tree :
        # Create a new tree; move Value into it.
        return (Value <== Value, Level => Level, Left => Left, Right => Right)
    end def Node

    def Is_Leaf(T : optional AA_Tree) -> Boolean :
        return T not null and then
          T.Left is null and then T.Right is null
    end def Is_Leaf

    def Leftmost(ref T : optional AA_Tree) -> ref optional AA_Tree :
        for L => T :
            if L not null and then L.Left not null :
                # Continue with Left until we reach null
                continue loop with L => L.Left
            else :
                # Found left-most
                return L
            end if
        end loop
    end def Leftmost

    def Successor(T : optional AA_Tree) -> optional Element :
        # Return element in tree greater than but closest to T.Value
        if T.Right not null :
            const Succ = Leftmost(T.Right)
            {> Succ not null <}
            return Succ.Value
        else :
            return null
        end if
    end def Successor

    def Rightmost(ref T : optional AA_Tree) -> ref optional AA_Tree :
        for R => T :
            if R not null and then R.Right not null :
                # Keep following down Right side
                continue loop with R => R.Right
            else :
                # Found right-most
                return R
            end if
        end loop
    end def Rightmost

    def Predecessor(T : optional AA_Tree) -> optional Element :
        # Return element in tree less than but closest to T.Value
        if T.Left not null :
            return Rightmost(T.Left).Value
        else :
            return null
        end if
    end def Predecessor

    def Skew(var T : optional AA_Tree) :
      # input: T, a node representing an AA tree that needs to be rebalanced.
      # output: T' Another node representing the rebalanced AA tree.

        if T not null and then
          T.Left not null and then
          T.Left.Level == T.Level :
            # The current T.Left becomes new root

            # Exchange value of T.Left with root
            T.Value <=> T.Left.Value
           
            # Move old root and T.Left.Right over to right side of tree
            T.Left.Right <=> T.Right
            T.Left.Left <=> T.Right
            T.Left <=> T.Right
        end if
    end def Skew

    def Split(var T : optional AA_Tree) :
        # input: T, a node representing an AA tree that needs to be rebalanced.
        # output: T' Another node representing the rebalanced AA tree.

        if T not null and then
          T.Right not null and then
          T.Right.Right not null and then
          T.Level == T.Right.Right.Level :
            # T.Right becomes the new root
            # Exchange value and level between root and T.Right
            T.Value <=> T.Right.Value
            T.Level <=> T.Right.Level

            # Move old root and T.Right.Left to left side of tree
            T.Left <=> T.Right.Right
            T.Right.Left <=> T.Right.Right
            T.Left <=> T.Right

            # Increment level
            T.Level += 1
        end if
    end def Split

    def Decrease_Level(var T : optional AA_Tree) :
        # input: T, a tree for which we want to remove links that skip levels.
        # output: T with its level decreased.

        if T is null :
            return
        end if
           
        var Should_Be : Univ_Integer = 1

        if T.Left not null :
            Should_Be = T.Left.Level + 1
        end if

        if T.Right not null :
            Should_Be = Min(Should_Be, T.Right.Level + 1)
        end if
            
        if Should_Be < T.Level :
            T.Level = Should_Be
            if T.Right not null and then
              Should_Be < T.Right.Level :
                T.Right.Level = Should_Be
            end if
        end if
    end def Decrease_Level

  exports :

    defop "[]"() -> optional AA_Tree :
        # Create an empty tree
        return null
    end defop "[]"

    # Insertion begins with the normal binary tree search and insertion
    # procedure. Then, as the call stack unwinds (assuming a recursive
    # implementation of the search), it's easy to check the validity of the
    # tree and perform any rotations as necessary. If a horizontal left link
    # arises, a skew will be performed, and if two horizontal right links
    # arise, a split will be performed, possibly incrementing the level of the
    # new root node of the current subtree. Note, in the code as given above,
    # the increment of T.Level. This makes it necessary to continue checking
    # the validity of the tree as the modifications bubble up from the leaves.
    
    defop "<|="(var T : optional AA_Tree; var X : optional Element) :
	# Move X into AA_Tree, leaving X null.
        # input: X, the value to be inserted, and 
        # T, the root of the tree to insert it into.
        # output: A balanced T' including X.

        # Do the normal binary tree insertion procedure. 
        # Set the result of the recursive call to the correct 
        # child in case a new node was created or the
        # root of the subtree changes.

        if T is null :
            # Create a new leaf node with X.
            T = Node(X, 1, null, null)
            return
        end if

        switch X =? T.Value :
          case #less :
            T.Left <|= X
          case #greater :
            T.Right <|= X
          case #equal | #unordered :
            # Note that the case of X == T.Value is unspecified. 
            # As given, an insert will have no effect. 
            # The implementor may desire different behavior.
	    X = null
            return
        end switch

        # Perform skew and then split. 
        # The conditionals that determine whether or
        # not a rotation will occur or not are inside 
        # of the procedures, as given above.

        Skew(T)
        Split(T)
    end defop "<|="

    def Insert(var T : optional AA_Tree; X : Element) :
	# Just pass the buck to the "<|=" operation
	var X_Copy for T = X
	T <|= X_Copy
    end def Insert

    # As in most balanced binary trees, the deletion of an internal node can
    # be turned into the deletion of a leaf node by swapping the internal node
    # with either its closest predecessor or successor, depending on which are
    # in the tree or on the implementor's whims. Retrieving a predecessor is
    # simply a matter of following one left link and then all of the remaining
    # right links. Similarly, the successor can be found by going right once
    # and left until a null pointer is found. Because of the AA property of
    # all nodes of level greater than one having two children, the successor
    # or predecessor node will be in level 1, making their removal trivial.
    # 
    # To re-balance a tree, there are a few approaches. The one described by
    # Andersson in his original paper is the simplest, and it is described
    # here, although actual implementations may opt for a more optimized
    # approach. After a removal, the first step to maintaining tree validity
    # is to lower the level of any nodes whose children are two levels below
    # them, or who are missing children. Then, the entire level must be skewed
    # and split. This approach was favored, because when laid down
    # conceptually, it has three easily understood separate steps:
    # 
    #     Decrease the level, if appropriate.
    #     Skew the level.
    #     Split the level.
    # 
    # However, we have to skew and split the entire level this time instead of
    # just a node, complicating our code.

    def Delete(var T : optional AA_Tree; X : Element) :
        # input: X, the value to delete, and T, 
        # the root of the tree from which it should be deleted.
        # output: T', balanced, without the value X.

        if T is null :
            # Not in tree -- should we complain?
            return
        end if

        switch X =? T.Value :
          case #less :
            Delete(T.Left, X)
          case #greater :
            Delete(T.Right, X)
          case #equal :
            # If we're a leaf, easy, otherwise reduce to leaf case. 
            if Is_Leaf(T) :
                T = null
            elsif T.Left is null :
                # Get successor value and delete it from right tree,
                # and set root to have that value
                const Succ = Successor(T)
                Delete(T.Right, Succ)
                T.Value = Succ
            else :
                # Get predecessor value and delete it from left tree,
                # and set root to have that value
                const Pred = Predecessor(T)
                Delete(T.Left, Pred)
                T.Value = Pred
            end if
          case #unordered :
            # Not in tree; should we complain?
            return
        end switch

        # Rebalance the tree. Decrease the level of all nodes in this level if
        # necessary, and then skew and split all nodes in the new level.

        if T is null :
            return
        end if

        Decrease_Level(T)
        Skew(T)
        Skew(T.Right)
        if T.Right not null :
            Skew(T.Right.Right)
        end if
        Split(T)
        Split(T.Right)
    end def Delete

    defop "in"(X : Element; T : optional AA_Tree) -> Result : Boolean :
        for P => T while P not null :
            switch X =? P.Value :
              case #less :
                continue loop with P => P.Left
              case #greater :
                continue loop with P => P.Right
              case #equal :
                return #true
              case #unordered :
                return #false
            end switch
        end loop
        return #false   # Not found
    end defop "in"

    def First(T : optional AA_Tree) -> optional Element :
      # Return first (smallest) element in tree
        if T is null :
            return null
        else :
            return Leftmost(T).Value
        end if
    end def First

    def Last(T : optional AA_Tree) -> optional Element :
      # Return last (greatest) element in tree
        if T is null :
            return null
        else :
            return Rightmost(T).Value
        end if
    end def Last


    def Remove_First(var T : optional AA_Tree) -> Result : optional Element :
      # Remove first (smallest) element in tree
        Result = First(T)
        if Result not null :
            Delete(T, Result)
        end if
    end def Remove_First

    def Remove_Last(var T : optional AA_Tree) -> Result : optional Element :
      # Remove last (greatest) element in tree
        Result = Last(T)
        if Result not null :
            Delete(T, Result)
        end if
    end def Remove_Last

    def Remove_Any(var T : optional AA_Tree) -> Result : optional Element :
      # Remove some element from tree
        if T is null :
            return null
        end if
        Result = T.Value
        if Result not null :
            Delete(T, Result)
        end if
    end def Remove_Any

    def Is_Empty(T : optional AA_Tree) -> Boolean :
      # Return True if the tree is empty
        return T is null
    end def Is_Empty

    def Count(T : optional AA_Tree) -> Univ_Integer :
      # Return a count of the nodes in the tree
        if T is null :
            return 0
        else :
            return Count(T.Left) + Count(T.Right) + 1
        end if
    end def Count

    def Overlapping(T : optional AA_Tree; X : Element) -> optional Element :
        # input: X, the value to find, and T, 
        # the root of the tree to be searched.
        # output: the element equal to or "unordered" relative to X.
        if T is null or else T.Value is null :
            return null
        else :
            switch X =? T.Value :
              case #less :
                return Overlapping(T.Left, X)
              case #greater :
                return Overlapping(T.Right, X)
              case #equal | #unordered :
                # Close enough
                return T.Value
            end switch
        end if
    end def Overlapping

end class PSL::Core::AA_Tree

def PSL::Test::Test_AA_Tree
  (A : Univ_Integer; B : Univ_Integer; C : Univ_Integer) :
    type Univ_Tree is AA_Tree<Univ_Integer>
    var T : Univ_Tree = []
    var X : Univ_Integer = A

    Insert(T, A)
    Println("Count = " | Count(T) | " after insert of " | A)
    Insert(T, B)
    Println("Count = " | Count(T) | " after insert of " | B)
    Insert(T, C)
    Println("Count = " | Count(T) | " after insert of " | C)

    Insert(T, A)
    Println("Count = " | Count(T) | " after another insert of " | A)

    Println(A | " in T = " | (A in T))
    Println(B | " in T = " | (B in T))
    Println(C | " in T = " | (C in T))
    Println("7 in T = " | (7 in T))

    for E = Remove_First(T) then Remove_First(T) while E not null :
        Println("Remove_First = " | E)
    end loop

    Println("Count after loop : " | Count(T))

    for I in 1..10 forward loop
        Insert(T, I)
        Println("Count = " | Count(T) | " after insert of " | I)
    end loop

    for L = Remove_Last(T) then Remove_Last(T) while L not null :
        Println("Remove_Last = " | L)
    end loop

    Println("Count after loop : " | Count(T))

    for J in 1..10 reverse :
        Insert(T, J)
        Println("Count = " | Count(T) | " after insert of " | J)
    end loop

    Println("Count after loop : " | Count(T))

    Println("Overlapping(T, 5) = " | Overlapping(T, 5))

    for Z = Remove_Any(T) then Remove_Any(T) while Z not null :
        Println("Remove_Any = " | Z)
    end loop

    Println("Count after loop : " | Count(T))

    for K in 1..10 :
        Insert(T, K)
        Println("Count = " | Count(T) | " after insert of " | K)
    end loop

    for F = Remove_First(T) then Remove_First(T) while F not null :
        Println("Remove_First = " | F)
    end loop

    Println("Count after loop : " | Count(T))

end def PSL::Test::Test_AA_Tree

interface PSL::Containers::Keyed<Key_Type is Hashable<>> :
    def Key_Of(ref const KV : Keyed) -> ref const Key_Type
    def Has_Value(KV : Keyed) -> Boolean
	# Return #true if Keyed object has a non-null value
    def Key_Only(Key : Key_Type) -> Keyed
	# Return a Keyed object given a key, having no associated value
end interface PSL::Containers::Keyed

class interface PSL::Containers::Key_Value
  <Key_Type is Assignable<>; Value_Type is Assignable<>> 
  implements Keyed<Key_Type> :
  # This supports the use of [Key => Value] as a way to
  # add a single element to an existing indexable container of some sort.
    var Key : optional Key_Type
    var Value : optional Value_Type
    defop "[]"() -> Key_Value
    defop "var_indexing"(ref var KV : Key_Value; Index : Key_Type) 
      -> ref var Value_Type
    def Key_Of(ref const KV : Key_Value) -> ref const Key_Type
    def Has_Value(KV : Key_Value) -> Boolean
	# Return #true if Key_Value object has a non-null value
    def Key_Only(Key : Key_Type) -> Key_Value
	# Return a Key_Value object given a key, having no associated value
end interface PSL::Containers::Key_Value

class PSL::Containers::Key_Value :
  exports :
    defop "[]"() -> Key_Value :
	return (Key => null, Value => null)
    end defop "[]"

    defop "var_indexing"(ref var KV : Key_Value; Index : Key_Type) 
      -> ref var Value_Type :
	KV.Key = Index
	return KV.Value
    end defop "var_indexing"

    def Key_Of(ref const KV : Key_Value) -> ref const Key_Type :
	return KV.Key
    end def Key_Of

    def Has_Value(KV : Key_Value) -> Boolean :
	# Return #true if Key_Value object has a non-null value
	return KV.Value not null
    end def Has_Value

    def Key_Only(Key : Key_Type) -> Key_Value :
	# Return a Key_Value object given a key, having no associated value
	return [Key : null]
    end def Key_Only
end class PSL::Containers::Key_Value

class interface PSL::Containers::Basic_Map<KV_Type is Keyed<>> :
  # A basic hashed-map module

    defop "[]"() -> Basic_Map

    defop "|="(var Left : Basic_Map; Right : KV_Type)
	# Add Key=>Value to Basic_Map, replacing pre-existing Basic_Mapping
	# for Key, if any.

    defop "<|="(var Left : Basic_Map; var Right : optional KV_Type)
	# Move Key=>Value into Basic_Map, replacing pre-existing Basic_Mapping
	# for Key, if any, leaving Right null.

    defop "+="(var Left : Basic_Map; Right : KV_Type) is "|="
	# A synonym for adding a key=>value KV_Type

    defop "in"(Left : KV_Type::Key_Type; Right : Basic_Map) -> Boolean
	# Return True if given key has a Basic_Mapping in the Basic_Map

    defop "-="(var M : Basic_Map; Key : KV_Type::Key_Type)   # aka Exclude
	# Remove Basic_Mapping for Right, if present

    defop "index_set"(M : Basic_Map) -> Set<KV_Type::Key_Type>
	# Return set of keys with Basic_Mappings

    defop "indexing"(ref M : Basic_Map; Key : KV_Type::Key_Type) {> Key in M <} 
      -> ref KV_Type
	# Used for references to M[Key]; requires the Key to be in M.

    defop "var_indexing"(ref var M : Basic_Map; Key : KV_Type::Key_Type) 
      -> ref var optional KV_Type
	# Used for assignments to M[Key]; Key is added to M if not present

    def Remove_Any(var M : Basic_Map) -> optional KV_Type
	# Remove one Basic_Mapping from the Basic_Map.  
	# Return null if Basic_Map is empty

    def Count(M : Basic_Map) -> Univ_Integer
	# Number of Basic_Mappings in the table

    defop "magnitude"(Basic_Map) -> Univ_Integer is Count

    def Is_Empty(M : Basic_Map) -> Boolean
	# Return True if map has no mappings

    def Dump_Statistics(M : Basic_Map)
      # A debugging routine to show bucket sizes of Basic_Map

end interface PSL::Containers::Basic_Map

class PSL::Containers::Basic_Map :
  # A basic hashed-map module

  # A Basic_Map is represented as a hash table, where each bucket is
  # a linked list of key/value KV_Types.
  # When key/value KV_Types are deleted from the Basic_Map they end 
  # up as "null"s in the list.  
  # We expand the table when the Count gets to be twice
  # that of the length of the table.

    interface Hash_Bucket<> :
      # a simple linked-list is used as a hash bucket
	var Elem : optional KV_Type
	var Tail : optional Hash_Bucket
    end interface Hash_Bucket

    var Count : Univ_Integer
    var Table : optional Basic_Array<optional Hash_Bucket<>>
    const Initial_Table_Size = 4
    const Debugging : Boolean = #false
    
    def Empty(Table_Size : Univ_Integer) -> Basic_Map :
	# Create an empty Basic_Map with the given table size
        return (Count => 0, Table => Create(Table_Size, null))
    end def Empty

    def Move_One(var To : Basic_Map; var Elem : optional KV_Type) :
	# Move Element into table, without expanding table.
	# Elem is set to null as a result.
	const Index = Hash(Key_Of(Elem)) mod |To.Table| + 1
	ref Bucket => To.Table[Index]

	if Bucket is null :
	    # Bucket is now empty, so create bucket
	    # with Elem as its only element.
	    Bucket = (Elem <== Elem, Tail => null)
	else :
	    # See whether Elem already in bucket
	    var Has_Empty_Slot : Boolean = #false
	    for B => Bucket then B.Tail while B not null :
		if B.Elem is null :
		    # Remember there is an empty slot
		    Has_Empty_Slot = #true
		elsif Key_Of(Elem) == Key_Of(B.Elem) :
		    # Already there; replace it in case Value is different.
		    B.Elem <== Elem
		    return
		end if
	    end loop

	    if Has_Empty_Slot :
		# Fill in the empty slot
		for B => Bucket then B.Tail while B not null :
		    if B.Elem is null :
			# Use the empty slot
			B.Elem <== Elem
			exit loop
		    end if
		end loop
	    else :
		# Make old bucket the new tail of the new bucket.
		Bucket = (Elem <== Elem, Tail <== Bucket)
	    end if
	end if

	To.Count += 1
    end def Move_One

    def Add_One(var To : Basic_Map; Elem : KV_Type) :
	# Add Element to table, without expanding it
	var Elem_Copy for To = Elem

	# Just pass the buck to "Move_One"
	Move_One(To, Elem_Copy)
    end def Add_One

    def Expand_Table(var Expanding : Basic_Map) :
	# Expand table of given Basic_Map.
	if Debugging :
	    Println(" Expanding hash table, Count = " | Expanding.Count | 
	      ", Length = " | |Expanding.Table|)
	end if
	var Old_Basic_Map <== Expanding
	Expanding = Empty(2 * |Old_Basic_Map.Table|)
	# Move elements into new table
	loop
	    var Elem for Expanding = Remove_Any(Old_Basic_Map)
		# "for Expanding" means to allocate Elem in
		# region associated with Expanding.
	    if Elem is null :
		exit loop
	    end if
	    Move_One(Expanding, Elem)
	end loop
	if Debugging :
	    Println(" After expansion, Count = " | Expanding.Count |
	      ", Length = " | |Expanding.Table|)
	end if
    end def Expand_Table

  exports :
    defop "[]"() -> Basic_Map :
        return (Count => 0, Table => null)
    end defop "[]"

    defop "|="(var Left : Basic_Map; Right : KV_Type) :
	if not Has_Value(Right) :
	    # Putting in Key => null is equivalent to deleting Key
	    Left -= Key_Of(Right)
	    return
	end if

	if Left.Table is null :
	    Left = Empty(Initial_Table_Size)
	elsif Left.Count >= 2*Length(Left.Table) :
	    # Expand table if averaging 2 or more per hash bucket
	    Expand_Table(Left)
	end if
        Add_One(Left, Right)
    end defop "|="

    defop "<|="(var Left : Basic_Map; var Right : optional KV_Type) :
	# Move Key=>Value into Basic_Map, replacing pre-existing Basic_Mapping
	# for Key, if any, leaving Right null.
	if not Has_Value(Right) :
	    # Putting in Key => null is equivalent to deleting Key
	    Left -= Key_Of(Right)
	    Right = null
	    return
	end if

	if Left.Table is null :
	    Left = Empty(Initial_Table_Size)
	elsif Left.Count >= 2*Length(Left.Table) :
	    # Expand table if averaging 2 or more per hash bucket
	    Expand_Table(Left)
	end if
        Move_One(Left, Right)
    end defop "<|="

    defop "in"(Left : KV_Type::Key_Type; Right : Basic_Map) -> Boolean :
	if Right.Count == 0 :
	    # Empty Basic_Map
	    return #false
	end if
	const Index = Hash(Left) mod Length(Right.Table) + 1
	ref Bucket => Right.Table[Index]
	if Bucket is null :
	    # Hash bucket is empty
	    return #false
	end if
	# Scan for Elem in hash bucket
	for B => Bucket then B.Tail while B not null :
	    if B.Elem not null and then
	      Has_Value(B.Elem) and then
	      Key_Of(B.Elem) == Left :
		# Found it
		return #true
	    end if
	end loop
	# Not in Basic_Map
	return #false
    end defop "in"

    defop "-="(var M : Basic_Map; Key : KV_Type::Key_Type) :
      # Remove the given key from the Basic_Map, if present
	if M.Count == 0 :
	    # Empty Basic_Map
	    return
	end if

	const Index = Hash(Key) mod Length(M.Table) + 1
	ref Bucket => M.Table[Index]
	if Bucket is null :
	    # Hash bucket is empty
	    return
	end if

	# Scan for Key in bucket
	for B => Bucket then B.Tail while B not null :
	    if B.Elem not null and then
	      Key_Of(B.Elem) == Key :
		# Found it.  Map it to null, and decrement Basic_Map count.
		B.Elem = null
		M.Count -= 1
		return
	    end if
	end loop
	# Not found
    end defop "-="
   
    defop "index_set"(M : Basic_Map) -> Result : Set<KV_Type::Key_Type> :
	# Return set of keys with non-null Basic_Mappings
	Result = []
        if M.Count == 0 :
            return
        end if
	for each Bucket of M.Table :
	    for B => Bucket then B.Tail while B not null :
		if B.Elem not null and then
		  Has_Value(B.Elem) :
		    Result |= Key_Of(B.Elem)
		end if
	    end loop
	end loop
    end defop "index_set"

    defop "indexing"(ref M : Basic_Map; Key : KV_Type::Key_Type) {> Key in M <} 
      -> ref KV_Type :
	# Used for references to M[Key]; requires the Key to be in M.
	{> M not null; M.Count > 0 <}
	const Index = Hash(Key) mod Length(M.Table) + 1
	# Scan for Key in bucket
	for B => M.Table[Index] then B.Tail while B not null :
	    if B.Elem not null and then
	      Key_Of(B.Elem) == Key :
		# Found it.  Return reference to element of KV_Type
		{> Has_Value(B.Elem) <}
		return B.Elem
	    end if
	end loop
	{> #false <}
    end defop "indexing"

    defop "var_indexing"(ref var M : Basic_Map; Key : KV_Type::Key_Type) 
      -> ref var optional KV_Type :
	# Used for assignments to M[Key]; Key is added to M if not present
	if M.Table not null :
	    const Index = Hash(Key) mod Length(M.Table) + 1
	    # Scan for Key in bucket
	    for B => M.Table[Index] then B.Tail while B not null :
		if B.Elem not null and then
		  Key_Of(B.Elem) == Key :
		    # Found it.  Return reference to element of KV_Type
		    return B.Elem
		end if
	    end loop
	end if

	# Not in table.  Add it, and then return reference
	if M.Table is null :
	    M = Empty(Initial_Table_Size)
	elsif M.Count >= 2*Length(M.Table) :
	    # Expand table if averaging 2 or more per hash bucket
	    Expand_Table(M)
	end if

	# Add [Key => null] to front of appropriate bucket.
	const Index = Hash(Key) mod Length(M.Table) + 1
	ref Bucket => M.Table[Index]
	Bucket = (Elem => Key_Only(Key), Tail <== Bucket)
	M.Count += 1

	# Return ref to new element
	return Bucket.Elem
    end defop "var_indexing"

    def Count(M : Basic_Map) -> Univ_Integer :
        if M is null :
            return 0
        else :
            return M.Count
        end if
    end def Count

    def Is_Empty(M : Basic_Map) -> Boolean :
	# Return True if map has no mappings
	return M.Count == 0
    end def Is_Empty

    def Remove_Any(var M : Basic_Map) -> Result : optional KV_Type :
	if M.Count == 0 :
	    # Basic_Map is empty
	    return null
	else :
	    # Find a non-empty bucket and pull out an item.
	    for each Bucket of M.Table :
		if Bucket not null :
		    for B => Bucket then B.Tail while B not null :
			if B.Elem not null :
			    # Found an item, remove from Bucket and return
			    Result <== B.Elem
			    M.Count -= 1
			    return
			end if
		    end loop
		    # This bucket is completely empty, so might
		    # as well empty it out.
		    Bucket = null
		end if
		# Go on to the next bucket, this one's empty
	    end loop
	    # Should never get here
	    return null
	end if
    end def Remove_Any

    def Dump_Statistics(M : Basic_Map) :
      # A debugging routine to show bucket sizes of Basic_Map
	Println("Basic_Map statistics: Count = " | M.Count)
	if M.Table is null :
	    Println(" Table is null")
	else :
	    Println(" Table of length " | Length(M.Table))
	    for each [I => Bucket] of M.Table forward loop
		Print("  Bucket #" | I)
		if Bucket is null :
		    Println(" is null")
		else :
		    var Len = 0
		    var Holes = 0
		    for B => Bucket then B.Tail while B not null :
			Len += 1
			if B.Elem is null :
			    # This list has a hole
			    Holes += 1
			end if
		    end loop
		    if Holes > 0 :
			Println(" of length " | Len | " with " | 
			  Holes | " holes")
		    else :
			Println(" of length " | Len)
		    end if
		end if
	    end loop
	end if
    end def Dump_Statistics

end class PSL::Containers::Basic_Map

def PSL::Test::Test_Basic_Map(X : Univ_Enumeration; Y : Univ_String;
  A : Univ_Enumeration; B : Univ_String) :
    type Enum_String_KV_Type is Key_Value<Univ_Enumeration, Univ_String>
    type Enum_String_Basic_Map is Basic_Map<Enum_String_KV_Type>

    var M : Enum_String_Basic_Map = [X : [X : Y], A : [A : B]]

    Println("Count = " | Count(M))

    for each KV of M :
	ref const K => KV.Key
	ref const V => KV.Value
	Println("Basic_Mapping " | K | " => " | V)
	Println(K | " in M = " | (K in M))
    end loop

    Println("#xy in M = " | (#xy in M))

    M |= [X : null]

    Println("Count after deletion = " | Count(M))

    for each KV of M :
	ref const K => KV.Key
	ref const V => KV.Value
	Println("Basic_Mapping " | K | " => " | V)
    end loop

    M |= [X : "a new value"]

    Println("Count after addition = " | Count(M))

    for each KV of M :
	ref const K => KV.Key
	ref const V => KV.Value
	Println("Basic_Mapping " | K | " => " | V)
    end loop

    M |= [X : "a third value"]

    Println("Count after replacement = " | Count(M))

    for each KV of M :
	ref const K => KV.Key
	ref const V => KV.Value
	Println("Basic_Mapping " | K | " => " | V)
    end loop

    M -= A

    Println("Count after deletion = " | Count(M))

    for each KV of M :
	ref const K => KV.Key
	ref const V => KV.Value
	Println("Basic_Mapping " | K | " => " | V)
    end loop

    var Ran = Random::Start(Hash(A))
    var MUI : Basic_Map<Key_Value<Univ_Integer, Univ_Integer>> = []

    Println("Adding 100 random KV_Typeings to Basic_Map.")
    for I in 1..100 :
	const Key = Next(Ran) mod 100
	const Value = Next(Ran) mod 100
	MUI[Key] = [Key : Value]
    end loop
    Println("Basic_Map is now of count = " | Count(MUI))

    for each [K => KV] of MUI :
	ref const V => KV.Value
	Println("Basic_Mapping " | K | " => " | V)
	Println("MUI[" | K | "] = " | MUI[K].Value)
    end loop

    var I = 0
    for KV_Type in MUI :
	I += 1
	Print(" [" | Key_Of(KV_Type) | " => " | KV_Type.Value | "]")
	if I mod 5 == 0 :
	    Print('\n')
	end if
    end loop
    if I mod 5 != 0 :
	Print('\n')
    end if

end def PSL::Test::Test_Basic_Map

class PSL::Containers::Set :
  # A hashed-set module

  # A Set is represented as a hash table, where each bucket is a linked list.
  # When elements are deleted from the Set they end up a "null"s in the
  # list.  We expand the table when the Count gets to be twice
  # that of the length of the table.

    class interface KV_Wrapper<> implements Keyed<Element_Type> :
      # Create a wrapper for a key that implements the Keyed interface
	var Key : Element_Type
	def Key_Of(ref const KV : KV_Wrapper) -> ref const Element_Type
	def Has_Value(KV : KV_Wrapper) -> Boolean
	    # Return #true if KV_Wrapper object has a non-null value
	def Key_Only(Key : Element_Type) -> KV_Wrapper
	    # Return a KV_Wrapper object given a key, 
	    # having no associated value
    end interface KV_Wrapper

    class KV_Wrapper :
      # Create a wrapper for a key that implements the Keyed interface
      exports :
	def Key_Of(ref const KV : KV_Wrapper) -> ref const Element_Type :
	    return KV.Key
	end def Key_Of

	def Has_Value(KV : KV_Wrapper) -> Boolean :
	    # Return #true if KV_Wrapper object has a non-null value
	    return #true
	end def Has_Value

	def Key_Only(Key : Element_Type) -> KV_Wrapper :
	    # Return a KV_Wrapper object given a key, 
	    # having no associated value.
	    # NOTE: This is not really meaningful for sets, since there
	    #       isn't a value.
	    return (Key => Key)
	end def Key_Only
    end class KV_Wrapper
	
    var Data : Basic_Map<KV_Wrapper<>>
	# Set is represented as a map from keys to nothing.

  exports :
    defop "[]"() -> Set :
        return (Data => [])
    end defop "[]"

    def Singleton(Elem : Element_Type) -> Result : Set :
	Result = []
	Result.Data |= (Key => Elem)
    end def Singleton

    defop "|"(Left, Right : Element_Type) -> Result : Set :
	Result = []
	Result.Data |= (Key => Left)
	Result.Data |= (Key => Right)
    end defop "|"

    defop "|"(Left : Set; Right : Element_Type) -> Result : Set :
	Result = Left
	Result |= Right
    end defop "|"

    defop "|"(Left : Element_Type; Right : Set) -> Result : Set :
	Result = Right
	Result |= Left
    end defop "|"

    defop "|"(Left : Set; Right : Set) -> Result : Set :
	# Union, iterate over smaller Set
	if Count(Left.Data) <= Count(Right.Data) :
	    Result = Right
	    Result |= Left
	else :
	    Result = Left
	    Result |= Right
	end if
    end defop "|"

    defop "|="(var Left : Set; Right : Set) :
        if Count(Left.Data) == 0 :
            Left = Right
        else :
	    for Elem in Right :
		Left |= Elem
	    end loop
	end if
    end defop "|="

    defop "|="(var Left : Set; Right : Element_Type) :
	Left.Data |= (Key => Right)
    end defop "|="

    defop "<|="(var Left : Set; var Right : optional Element_Type) :
	# Move Right into Set Left
	var KV : KV_Wrapper = (Key <== Right)
	Left.Data <|= KV
    end defop "<|="

    defop "<|="(var Left : Set; var Right : Set) :
	# Move all elements of Right into Left, leaving Right empty.
	loop
	    # Extract element from Right, in region for Left
	    var Elem for Left = Remove_Any(Right)
	    if Elem is null :
		# All done
		return
	    end if
	    # Move element into Left
	    Left <|= Elem
	end loop
    end defop "<|="

    defop "in"(Left : Element_Type; Right : Set) -> Boolean :
	return Left in Right.Data
    end defop "in"

    defop "=?"(Left, Right : Set) -> Ordering :
	# Return #equal if Left and Right have the same elements
	# Return #less if Left is a proper subset of Right
	# Return #greater if Left is a proper superset of Right
	# Return #unordered otherwise
        var Overlaps = 0
        var Missing = 0
        for Elem in Left :
            if Elem not in Right :
                Missing += 1
            else :
                Overlaps += 1
            end if
        end loop

        if Missing > 0 :
            # Can't be equal, but Left might be a proper superset
            if Overlaps < Count(Right.Data) :
                return #unordered
            else :
                # Left is a superset
                return #greater
            end if
        else :
            # Might be equal or Left might be a proper subset
            if Overlaps < Count(Right.Data) :
                # Left is a proper subset of Right
                return #less
            else :
                return #equal
            end if
        end if
    end defop "=?"

    defop "and"(Left, Right : Set) -> Result : Set :
	# Intersection, iterate over smaller Set
	if Count(Left.Data) < Count(Right.Data) :
	    # Left is smaller
	    Result = []
	    for Elem in Left :
		if Elem in Right :
		    Result |= Elem
		end if
	    end loop
	else :
	    # Left is bigger
	    Result = Left
	    for Elem in Right :
		if Elem in Left :
		    Result |= Elem
		end if
	    end loop
	end if
    end defop "and"

    defop "and="(var Left : Set; Right : Set) :
	# Intersection, iterate over smaller Set
	if Count(Left.Data) <= Count(Right.Data) :
	    # Left is smaller
	    for Elem in Left :
		if Elem not in Right :
		    Left -= Elem
		end if
	    end loop
	else :
	    # Left is bigger
	    var Result : Set for Left = []
	    for Elem in Right :
		if Elem in Left :
		    Result |= Elem
		end if
	    end loop
	    Left <== Result
	end if
    end defop "and="

    defop "xor"(Left, Right : Set) -> Result : Set :
	# Symmetric difference
	# Want elements that are only in one of the two inputs
	if Count(Left.Data) < Count(Right.Data) :
	    # Swap order to shorten iteration
	    Result = Right
	    Result xor= Left
	else :
	    Result = Left
	    Result xor= Right
	end if
    end defop "xor"
    
    defop "xor="(var Left : Set; Right : Set) :
	# Want elements that are only in one of the two inputs
	for Elem in Right :
	    if Elem in Left :
		Left -= Elem
	    else :
		Left += Elem
	    end if
	end loop
    end defop "xor="

    defop "-"(Left, Right : Set) -> Result : Set :
	# Set difference, iterate over smaller Set
	if Count(Left.Data) < Count(Right.Data) :
	    # Left is smaller, build up 
	    Result = []
	    for Elem in Left :
		if Elem not in Right :
		    Result |= Elem
		end if
	    end loop
	else :
	    # Left is bigger, tear down
	    Result = Left
	    Result -= Right
	end if
    end defop "-"

    defop "-="(var Left : Set; Right : Set) :
	# Compute Set difference
	for Elem in Right :
	    Left -= Elem
	end loop
    end defop "-="

    defop "-="(var S : Set; Elem : Element_Type) :
      # Remove the given element from the Set, if present
	S.Data -= Elem
    end defop "-="
   
    def Count(S : Set) -> Univ_Integer :
	return Count(S.Data)
    end def Count

    def Is_Empty(S : Set) -> Boolean :
	return Is_Empty(S.Data)
    end def Is_Empty

    def Remove_Any(var S : Set) -> Result : optional Element_Type :
	var Result_Wrapper for Result = Remove_Any(S.Data)
	if Result_Wrapper is null :
	    return null
	else :
	    Result <== Result_Wrapper.Key
	end if
    end def Remove_Any

    def Dump_Statistics(S : Set) :
      # A debugging routine to show bucket sizes of Set
	Dump_Statistics(S.Data)
    end def Dump_Statistics

end class PSL::Containers::Set

def PSL::Test::Test_Set(A, X, Y, Z : Univ_Integer) :
    var S : Set<Univ_Integer> = X | Y | Z
    if A in S :
        Println(A | " is in " | X | "|" | Y | "|" | Z)
    else :
        Println(A | " is *not* in " | X | "|" | Y | "|" | Z)
    end if

    const Save_Set = S

    const CSet : Set<Univ_Integer> = [Z, Y, X]
    Println("[Z, Y, X] =? (X | Y | Z) --> " |
      ( CSet =? S ))

    var Ran = Random::Start(A)
    Println("Adding 100 random digits to Set.")
    for I in 1..100 :
	S |= Next(Ran) mod 100
    end loop
    Println("Set is now of count = " | Count(S))
    Println("Contents of Set:")
    var I = 1
    for Elem in S :
	Print(Elem | " ")
	if I mod 10 == 0 :
	    Print('\n')
	end if
	I += 1
    end loop
    if Count(S) mod 10 != 0 :
	Print('\n')
    end if
    Println("S Before adding random elements =? now --> " |
      (Save_Set =? S))
    Println("S =? S --> " | (S =? S))

    var Small_Set : Set<Univ_Integer> = []
    Small_Set += -1
    Small_Set or= [2]

    Println("S =? (-1 | 2) --> " | (S =? Small_Set))
    Println("[2 , -1] =? (-1 | 2) --> " | ([2, -1] =? Small_Set))

    Println("S =? [] --> " | (S =? []))

    Println("Count(S) = " | Count(S))
    
    const Before_Exclude = S
    S -= X
    Println("After Exclude(S, " | X | "), Count(S) = " | Count(S))
    Println("Before_Exclude =? After Exclude --> " | (Before_Exclude =? S))

    Dump_Statistics(S)
end def PSL::Test::Test_Set


class PSL::Containers::Countable_Set :

    type Element_Interval is Closed_Interval<Element_Type>

    var Items : optional AA_Tree<Element_Interval>

  exports :
    defop "[]"() -> Countable_Set :
        return (Items => [])
    end defop "[]"

    def Singleton(Elem : Element_Type) -> Result : Countable_Set :
	Result = []
	Result.Items |= (Low => Elem, High => Elem)
    end def Singleton

    defop ".."(Left, Right : Element_Type) -> Result : Countable_Set :
        Result = []
        if Left <= Right :
            Result.Items |= (Low => Left, High => Right)
        end if
    end defop ".."
    
    defop "<.."(Left, Right : Element_Type) -> Result : Countable_Set :
        Result = []
        if Left < Right :
            Result.Items |= (Low => Left+1, High => Right)
        end if
    end defop "<.."
    
    defop "<..<"(Left, Right : Element_Type) -> Result : Countable_Set :
        Result = []
        if Left < Right-1 :
            Result.Items |= (Low => Left+1, High => Right-1)
        end if
    end defop "<..<"
    
    defop "..<"(Left, Right : Element_Type) -> Result : Countable_Set :
        Result = []
        if Left < Right :
            Result.Items |= (Low => Left, High => Right-1)
        end if
    end defop "..<"
    
    defop "|"(Left, Right : Element_Type) -> Result : Countable_Set :
        Result = []
        if Left >= Right-1 and then Left <= Right + 1 :
            # Can combine elements into a single interval
            if Left <= Right :
                Result.Items |= (Low => Left, High => Right)
            else :
                Result.Items |= (Low => Right, High => Left)
            end if
        else :
            # Make each element its own interval
            Result.Items |= (Low => Left, High => Left)
            Result.Items |= (Low => Right, High => Right)
        end if
    end defop "|"

    defop "|"(Left : Countable_Set; Right : Element_Type) 
      -> Result : Countable_Set :
        Result = Left
        Result |= Right
    end defop "|"

    defop "|"(Left : Element_Type; Right : Countable_Set) -> Countable_Set :
        return Right | Left
    end defop "|"

    defop "|"(Left : Countable_Set; Right : Countable_Set) 
      -> Result : Countable_Set :
        Result = Left
        Result |= Right
    end defop "|"

    defop "|="(var Left : Countable_Set; Right : Element_Type) :
        const Right_IV : Element_Interval = (Low => Right, High => Right)
        const Left_IV = Overlapping(Left.Items, Right_IV)
        if Left_IV is null :
            # Nothing overlaps, need to add it (might want to merge someday)
            Left.Items |= Right_IV
        end if
    end defop "|="

    defop "<|="(var Left : Countable_Set; var Right : optional Element_Type) :
        # Move element into set, leaving Right null afterward.
	# NOTE: No copy minimization done for countable types.
	Left |= Right
	Right = null
    end defop "<|="

    defop "<|="(var Left : Countable_Set; var Right : Countable_Set) :
	# Move all elements of Right into Left, leaving Right empty.
        if Count(Left.Items) == 0 :
            Left.Items <== Right.Items
        else :
            # Iterate through the tree
	    loop
		# Extract interval from Right
		var Right_IV for Left = Remove_Any(Right.Items)

		if Right_IV is null :
		    return   # All done
		end if

                # See whether it overlaps with an existing interval
                # in Left tree
                var Left_IV for Left = Overlapping(Left.Items, Right_IV)
                while Left_IV not null :
                    if Left_IV.Low <= Right_IV.Low and then
                      Left_IV.High >= Right_IV.High :
                        # Right_IV is subsumed; nothing to add in
                        Right_IV = null
                        exit loop
                    else :
                        # Need to delete Left_IV and incorporate
                        # into Right_IV
                        Delete(Left.Items, Left_IV)
                        if Left_IV.Low < Right_IV.Low :
                            Right_IV.Low = Left_IV.Low
                        end if
                        if Left_IV.High > Right_IV.High :
                            Right_IV.High = Left_IV.High
                        end if

                        # Now see if there is anything still overlapping
                        Left_IV = Overlapping(Left.Items, Right_IV)
                    end if
                end loop

                if Right_IV not null :
                    # Add Right_IV
                    Left.Items <|= Right_IV
                end if

            end loop
        end if
    end defop "<|="

    defop "|="(var Left : Countable_Set; Right : Countable_Set) :
	# Pass the buck to the "<|=" operation
	var Right_Copy for Left = Right
	Left <|= Right_Copy
    end defop "|="

    defop "-"(Left, Right : Countable_Set) -> Result : Countable_Set :
      # Set difference
	Result = Left
	Result -= Right
    end defop "-"

    defop "-"(Left : Countable_Set; Right : Element_Type)
      -> Result : Countable_Set :
      # Remove one element
        Result = Left
        Result -= Right
    end defop "-"
        
    defop "-="(var S : Countable_Set; Elem : Element_Type) :
      # Remove the given element from the set, if present
	const IV = Overlapping(S.Items, (Low => Elem, High => Elem))
	  # Get interval, if any, which overlaps given element

	if IV not null :
	    # Delete interval and put back after removing Elem
	    Delete(S.Items, IV)
	    if IV.High > IV.Low :
		# We need to put something back
		if IV.Low == Elem :
		    S.Items |= (Low => IV.Low + 1, High => IV.High)
		elsif IV.High == Elem :
		    S.Items |= (Low => IV.Low, High => IV.High-1)
		else :
		    # Elem is in the middle, put back intervals
		    # on either side.
		    S.Items |= (Low => IV.Low, High => Elem-1)
		    S.Items |= (Low => Elem+1, High => IV.High)
		end if
	    end if
	end if

    end defop "-="

    defop "-="(var Left : Countable_Set; Right : Countable_Set) :
      # Remove all elements of Right from Left, if present
	for Elem in Right :
	    Left -= Elem
	end loop
    end defop "-="

    defop "and"(Left, Right : Countable_Set) -> Result : Countable_Set :
	# Intersection
	Result = []
	for Elem in Right :
	    if Elem in Left :
		Result += Elem
	    end if
	end loop
    end defop "and"

    defop "and="(var Left : Countable_Set; Right : Countable_Set) :
	# Intersection
	for Elem in Left :
	    if Elem not in Right :
		Left -= Elem
	    end if
	end loop
    end defop "and="

    defop "xor"(Left, Right : Countable_Set) -> Result : Countable_Set :
	# Symmetric difference
	Result = Left
	Result xor= Right
    end defop "xor"

    defop "xor="(var Left : Countable_Set; Right : Countable_Set) :
	# Symmetric difference
	# Want elements that are only in one of the two inputs
	for Elem in Right :
	    if Elem in Left :
		Left -= Elem
	    else :
		Left += Elem
	    end if
	end loop
    end defop "xor="

    defop "in"(Left : Element_Type; Right : Countable_Set) -> Boolean :
        return Overlapping(Right.Items, (Low => Left, High => Left)) not null
    end defop "in"

    defop "=?"(Left, Right : Countable_Set) -> Ordering :
	# Return #equal if Left and Right have the same elements
	# Return #less if Left is a proper subset of Right
	# Return #greater if Left is a proper superset of Right
	# Return #unordered otherwise
        var Overlaps = 0
        var Missing = 0
        for Elem in Left :
            if Elem not in Right :
                Missing += 1
            else :
                Overlaps += 1
            end if
        end loop

        if Missing > 0 :
            # Can't be equal, but Left might be a proper superset
            if Overlaps < Count(Right) :
                return #unordered
            else :
                # Left is a superset
                return #greater
            end if
        else :
            # Might be equal or Left might be a proper subset
            if Overlaps < Count(Right) :
                # Left is a proper subset of Right
                return #less
            else :
                return #equal
            end if
        end if
    end defop "=?"

    def Count(S : Countable_Set) -> Result : Univ_Integer :
        # Return count of items in set

        Result = 0

        # Copy items and then iterate through them to build up count
        var Items = S.Items
        var Next_IV = Remove_Any(Items)
        while Next_IV not null :
            Result += Next_IV.High - Next_IV.Low + 1
            Next_IV = Remove_Any(Items)
        end loop
    end def Count

    def Is_Empty(S : Countable_Set) -> Boolean :
	return Is_Empty(S.Items)
    end def Is_Empty

    def First(S : Countable_Set) -> optional Element_Type :
        const First_IV = First(S.Items)
        if First_IV is null :
            return null
        else :
            return First_IV.Low
        end if
    end def First

    def Last(S : Countable_Set) -> optional Element_Type :
        const Last_IV = Last(S.Items)
        if Last_IV is null :
            return null
        else :
            return Last_IV.High
        end if
    end def Last

    def Remove_First(var S : Countable_Set) 
      -> Result : optional Element_Type :
        # Return first element of set

        # Get first interval in tree
        var First_IV = Remove_First(S.Items)
        if First_IV is null :
            # Tree is empty
            return null
        end if

        # See whether interval has more than one value in it
        if First_IV.High > First_IV.Low :
            # Need to put back the remainder
            S.Items |= (Low => First_IV.Low+1, High => First_IV.High)
        end if

        # Return first item
        return First_IV.Low
    end def Remove_First

    def Remove_Last(var S : Countable_Set) -> Result : optional Element_Type :
        # Remove last element of set

        # Get Last interval in tree
        var Last_IV = Remove_Last(S.Items)
        if Last_IV is null :
            # Tree is empty
            return null
        end if

        # See whether interval has more than one value in it
        if Last_IV.High > Last_IV.Low :
            # Need to put back the remainder
            S.Items |= (Low => Last_IV.Low, High => Last_IV.High-1)
        end if

        # Return Last item
        return Last_IV.High
    end def Remove_Last

    def Remove_Any(var S : Countable_Set) -> optional Element_Type :
        # Remove any element of set

        # Get any interval in tree
        var Any_IV = Remove_Any(S.Items)
        if Any_IV is null :
            # Tree is empty
            return null
        end if

        # See whether interval has more than one value in it
        if Any_IV.High > Any_IV.Low :
            # Need to remove one to return and put back the remainder
            if (Any_IV.High - Any_IV.Low) mod 2 == 0 :
                # Return high bound when high-low is even
                # NOTE: We do this to avoid having algorithms become
                #       dependent on always getting values in ascending 
                #       or descending order.
                S.Items |= (Low => Any_IV.Low, High => Any_IV.High-1)
                return Any_IV.High
            else :
                # Return low bound when high-low is odd
                S.Items |= (Low => Any_IV.Low+1, High => Any_IV.High)
                return Any_IV.Low
            end if
        else :
            # Return only item in interval
            return Any_IV.Low
        end if

    end def Remove_Any

end class PSL::Containers::Countable_Set

def PSL::Test::Test_Countable_Set(A, X, Y, Z : Integer) :
    var S : Countable_Set<Integer> = X | Y..Z
    Println(A | " in " | X | "|" | Y | ".." | Z | "=" | ( A in S ))

    const Agg : Countable_Set<Integer> = [X, Y, Z]
    const Or : Countable_Set<Integer> = Y | Z | X

    Println(" [X, Y, Z] =? (Y | Z | X) --> " | ( Agg =? Or ))

    for J in S :
        Println("Remove_Any(S) = " | J)
    end loop

    S = []

    for I in 1..10 forward loop
        S += I
        Println("Adding " | I | " to S, Count = " | Count(S))
    end loop

    for K in S :
        Println("Remove_Any(S) = " | K)
    end loop

    Println("Count(S) = " | Count(S))
    S -= 7
    Println("After S -= 7, Count(S) = " | Count(S))

    var Xor = S xor [2, 5, 12, 15]
    Print("S xor [2, 5, 12, 15] = ")
    for J in Xor forward loop
	Print(J | " ")
    end loop
    Print('\n')
end def PSL::Test::Test_Countable_Set

class interface PSL::Containers::Ordered_Set<Element_Type is Comparable<>> :
  # A set abstraction that supports efficiently storing potentially
  # large ranges of values
    defop "[]"() -> Ordered_Set

    def Singleton(Elem : Element_Type) -> Ordered_Set
	# Return a set consisting of a single element

    defop ".."(Left, Right : Element_Type) -> Ordered_Set
	# Closed interval of values
    defop "<.."(Left, Right : Element_Type) -> Ordered_Set
	# Open-Closed interval of values
    defop "..<"(Left, Right : Element_Type) -> Ordered_Set
	# Closed-Open interval of values
    defop "<..<"(Left, Right : Element_Type) -> Ordered_Set
	# Open interval of values

    defop "|"(Left, Right : Element_Type) -> Ordered_Set
    defop "|"(Left : Ordered_Set; Right : Element_Type) -> Ordered_Set
    defop "|"(Left : Element_Type; Right : Ordered_Set) -> Ordered_Set
    defop "|"(Left : Ordered_Set; Right : Ordered_Set) -> Ordered_Set

    defop "|="(var Left : Ordered_Set; Right : Element_Type)
    defop "|="(var Left : Ordered_Set; Right : Ordered_Set)

    defop "<|="(var Left : Ordered_Set; var Right : optional Element_Type)
        # Move element into set, leaving Right null afterward.

    defop "<|="(var Left : Ordered_Set; var Right : Ordered_Set)
	# Move all elements of Right into Left, leaving Right empty.

    defop "-"(Left, Right : Ordered_Set) -> Ordered_Set
      # Set difference
    defop "-="(var S : Ordered_Set; Elem : Element_Type)
      # Remove the given element from the set, if present
    defop "-="(var Left : Ordered_Set; Right : Ordered_Set)
      # Remove all elements of Right from Left, if present

    defop "or"(Left : Ordered_Set; Right : Ordered_Set) 
      -> Ordered_Set is "|"   # union
    defop "or="(var Left : Ordered_Set; Right : Ordered_Set) is "|="

    defop "+"(Left : Ordered_Set; Right : Ordered_Set) 
      -> Ordered_Set is "|"   # Union
    defop "+="(var Left : Ordered_Set; Right : Ordered_Set) is "|="
    defop "+="(var Left : Ordered_Set; Right : Element_Type) is "|="
   
    defop "and"(Left, Right : Ordered_Set) -> Ordered_Set
	# Intersection
    defop "and="(var Left : Ordered_Set; Right : Ordered_Set)

    defop "xor"(Left, Right : Ordered_Set) -> Ordered_Set
	# Symmetric difference
    defop "xor="(var Left : Ordered_Set; Right : Ordered_Set)

    defop "in"(Left : Element_Type; Right : Ordered_Set) -> Boolean

    defop "=?"(Left, Right : Ordered_Set) -> Ordering
	# Return #equal if Left and Right have the same elements
	# Return #less if Left is a proper subset of Right
	# Return #greater if Left is a proper superset of Right
	# Return #unordered otherwise

    def Is_Empty(S : Ordered_Set) -> Boolean

    def Lower_Bound(S : Ordered_Set) -> optional Element_Type
	# Lower bound of set
    def Lower_Bound_Is_Open(S : Ordered_Set) -> Boolean
	# Whether lower bound is "open" or "closed"

    def Upper_Bound(S : Ordered_Set) -> optional Element_Type
	# Upper bound of set
    def Upper_Bound_Is_Open(S : Ordered_Set) -> Boolean
	# Whether upper bound is "open" or "closed"

    def Remove_First(var S : Ordered_Set) -> optional Interval<Element_Type>
	# Remove first interval of set (lowest low bound)

    def Remove_Last(var S : Ordered_Set) -> optional Interval<Element_Type>
	# Remove last interval of set (highest high bound)

    def Remove_Any(var S : Ordered_Set) -> optional Interval<Element_Type>
	# Remove an arbitrary interval of set

end interface PSL::Containers::Ordered_Set

class PSL::Containers::Ordered_Set :

    type Element_Interval is Interval<Element_Type>

    var Items : optional AA_Tree<Element_Interval>

    def Remove_Interval(var S : Ordered_Set; Remove_IV : Element_Interval) :
      # Remove the given interval from the set, if present
	if Remove_IV.Low > Remove_IV.High :
	    # Nothing to remove
	    return
	end if

	  # Find an interval within the AA_Tree that overlaps
	for Remaining_IV = Remove_IV while Remaining_IV not null :
	    # Get overlap, if any
	    var IV = Overlapping(S.Items, Remaining_IV)

	    if IV is null :
		# No overlap, nothing left to remove
		exit loop
	    end if
	
	    # Found an overlapping interval; delete it and add back what's left
	    Delete(S.Items, IV)
	    if Is_Strictly_Within(Remaining_IV, IV) :
		# Add back left and right remnants
		# Remaining_IV is fully subsumed.
		S.Items |= (Low => IV.Low, Low_Is_Open => IV.Low_Is_Open, 
		  High => Remaining_IV.Low, 
		  High_Is_Open => not Remaining_IV.Low_Is_Open)
		S.Items |= (Low => Remaining_IV.High, 
		  Low_Is_Open => not Remaining_IV.High_Is_Open,
		  High => IV.High, 
		  High_Is_Open => IV.High_Is_Open)
		# we are all done now
		exit loop
	    end if

	    # May be something left
	    const Overlap = IV and Remaining_IV
	    IV -= Overlap

	    if not Is_Empty(IV) :
		# Worth putting the interval back
		S.Items |= IV
	    end if

	    continue loop with Remaining_IV => Remaining_IV - Overlap

	end loop

    end def Remove_Interval

    def Is_Subset(Left, Right : Ordered_Set) -> Boolean :
	# Return True if Left is a subset of Right
	for Left_IV in Left :
	    for Remaining_IV = Left_IV while not Is_Empty(Remaining_IV) :
		const Right_IV = Overlapping(Right.Items, Remaining_IV)
		if Right_IV is null :
		    # Found some values that are not in Right
		    return #false
		end if

		# Loop around with what is left
		continue loop with Remaining_IV => Remaining_IV - Right_IV
	    end loop
	end loop
	# Everything in Left was found in Right
	return #true
    end def Is_Subset

  exports :
    defop "[]"() -> Ordered_Set :
        return (Items => [])
    end defop "[]"

    def Singleton(Elem : Element_Type) -> Result : Ordered_Set :
	# Return a set consisting of a single element
	Result = []
	Result.Items |= Singleton(Elem)
    end def Singleton

    defop ".."(Left, Right : Element_Type) -> Result : Ordered_Set :
        Result = []
        if Left <= Right :
            Result.Items |= (Low => Left, Low_Is_Open => #false, 
	      High => Right, High_Is_Open => #false)
        end if
    end defop ".."
    
    defop "<.."(Left, Right : Element_Type) -> Result : Ordered_Set :
        Result = []
        if Left < Right :
            Result.Items |= (Low => Left, Low_Is_Open => #true,
	      High => Right, High_Is_Open => #false)
        end if
    end defop "<.."
    
    defop "<..<"(Left, Right : Element_Type) -> Result : Ordered_Set :
        Result = []
        if Left < Right :
            Result.Items |= (Low => Left, Low_Is_Open => #true,
	      High => Right, High_Is_Open => #true)
        end if
    end defop "<..<"
    
    defop "..<"(Left, Right : Element_Type) -> Result : Ordered_Set :
        Result = []
        if Left < Right :
            Result.Items |= (Low => Left, Low_Is_Open => #false,
	      High => Right, High_Is_Open => #true)
        end if
    end defop "..<"
    
    defop "|"(Left, Right : Element_Type) -> Result : Ordered_Set :
        Result = []
	Result |= Left
	if Left != Right :
	    Result |= Right
        end if
    end defop "|"

    defop "|"(Left : Ordered_Set; Right : Element_Type) 
      -> Result : Ordered_Set :
        Result = Left
        Result |= Right
    end defop "|"

    defop "|"(Left : Element_Type; Right : Ordered_Set) -> Ordered_Set :
        return Right | Left
    end defop "|"

    defop "|"(Left : Ordered_Set; Right : Ordered_Set) 
      -> Result : Ordered_Set :
        Result = Left
        Result |= Right
    end defop "|"

    defop "|="(var Left : Ordered_Set; Right : Element_Type) :
        const Right_IV : Element_Interval = Singleton(Right)
        const Left_IV = Overlapping(Left.Items, Right_IV)
        if Left_IV is null :
            # Nothing overlaps, need to add it (might want to merge someday)
            Left.Items |= Right_IV
        end if
    end defop "|="

    defop "<|="(var Left : Ordered_Set; var Right : optional Element_Type) :
        # Move element into set, leaving Right null afterward.
	# TBD: No copy minimization done at the moment
	Left |= Right
	Right = null
    end defop "<|="

    defop "|="(var Left : Ordered_Set; Right : Ordered_Set) :
        if Count(Left.Items) == 0 :
            Left = Right
        else :
            # Make a copy of the Right set
            var Right_Copy for Left = Right

	    # Merge all of its intervals into Left
	    Left <|= Right_Copy
	end if
    end defop "|="

    defop "<|="(var Left : Ordered_Set; var Right : Ordered_Set) :
	# Move all elements of Right into Left, leaving Right empty.
        if Count(Left.Items) == 0 :
            Left.Items <== Right.Items
        else :
            # Iterate through the tree
	    loop
		# Extract interval from Right
		var Right_IV for Left = Remove_Any(Right.Items)
		if Right_IV is null :
		    return
		end if

                # See whether it overlaps with an existing interval
                # in Left tree
                var Left_IV for Left = Overlapping(Left.Items, Right_IV)
                while Left_IV not null :
		    # Incorporate left interval into Right_IV
		    Right_IV |= Left_IV

		    if Right_IV == Left_IV :
			# Right_IV fully subsumed by existing interval
			Right_IV = null
			exit loop
		    end if

		    # Need to delete Left_IV 
		    Delete(Left.Items, Left_IV)

		    # Now see if there is anything still overlapping
		    Left_IV = Overlapping(Left.Items, Right_IV)
                end loop

                if not Is_Empty(Right_IV) :
                    # Add Right_IV
                    Left.Items <|= Right_IV
                end if

            end loop
        end if
    end defop "<|="

    defop "-"(Left, Right : Ordered_Set) -> Result : Ordered_Set :
      # Set difference
	Result = Left
	Result -= Right
    end defop "-"

    defop "-="(var S : Ordered_Set; Elem : Element_Type) :
      # Remove the given element from the set, if present
	Remove_Interval(S, Singleton(Elem))
    end defop "-="

    defop "-="(var Left : Ordered_Set; Right : Ordered_Set) :
      # Remove all intervals of Right from Left, if present
	for IV in Right :
	    Remove_Interval(Left, IV)
	end loop
    end defop "-="

    defop "and"(Left, Right : Ordered_Set) -> Result : Ordered_Set :
	# Intersection

	# Add elements that are in both Right and Left into result
	Result = []
	for Right_IV in Right :
	    for Remaining_IV = Right_IV while Remaining_IV not null :
		var Left_IV = Overlapping(Left.Items, Remaining_IV)

		if Left_IV is null :
		    exit loop
		end if

		# Compute overlap, add into result, and then :
		# around after removing it from Remaining_IV
		const Overlap = Remaining_IV and Left_IV

		Result.Items |= Overlap

		continue loop with Remaining_IV => Remaining_IV - Overlap
	    end loop
	end loop
    end defop "and"

    defop "and="(var Left : Ordered_Set; Right : Ordered_Set) :
	# Intersection
	Left = Left and Right
    end defop "and="

    defop "xor"(Left, Right : Ordered_Set) -> Ordered_Set :
	# Symmetric difference

	return (Left - Right) or (Right - Left)
    end defop "xor"

    defop "xor="(var Left : Ordered_Set; Right : Ordered_Set) :
	# Symmetric difference
	# Want elements that are only in one of the two inputs
	const Only_In_Right = Right - Left
	Left -= Right
	Left += Only_In_Right
    end defop "xor="

    defop "in"(Left : Element_Type; Right : Ordered_Set) -> Boolean :
        return Overlapping(Right.Items, Singleton(Left)) not null
    end defop "in"

    defop "=?"(Left, Right : Ordered_Set) -> Ordering :
	# Return #equal if Left and Right have the same elements
	# Return #less if Left is a proper subset of Right
	# Return #greater if Left is a proper superset of Right
	# Return #unordered otherwise
	if Is_Subset(Left, Right) :
	    if Is_Subset(Right, Left) :
		return #equal
	    else :
		return #less
	    end if
	elsif Is_Subset(Right, Left) :
	    return #greater
	else :
	    return #unordered
	end if
    end defop "=?"

    def Is_Empty(S : Ordered_Set) -> Boolean :
        # Return count of items in set
	return Count(S.Items) == 0
    end def Is_Empty

    def Lower_Bound(S : Ordered_Set) -> optional Element_Type :
        const First_IV = First(S.Items)
        if First_IV is null :
            return null
        else :
            return First_IV.Low
        end if
    end def Lower_Bound

    def Lower_Bound_Is_Open(S : Ordered_Set) -> Boolean :
        const First_IV = First(S.Items)
        if First_IV is null :
            return #true   # TBD: or null?
        else :
            return First_IV.Low_Is_Open
        end if
    end def Lower_Bound_Is_Open

    def Upper_Bound(S : Ordered_Set) -> optional Element_Type :
        const Last_IV = Last(S.Items)
        if Last_IV is null :
            return null
        else :
            return Last_IV.High
        end if
    end def Upper_Bound

    def Upper_Bound_Is_Open(S : Ordered_Set) -> Boolean :
        const Last_IV = Last(S.Items)
        if Last_IV is null :
            return #true   # TBD: or null?
        else :
            return Last_IV.High_Is_Open
        end if
    end def Upper_Bound_Is_Open

    def Remove_First(var S : Ordered_Set) 
      -> optional Interval<Element_Type> :
	# Remove first interval of set (lowest low bound)
	return Remove_First(S.Items)
    end def Remove_First

    def Remove_Last(var S : Ordered_Set) 
      -> optional Interval<Element_Type> :
	# Remove last interval of set (highest high bound)
	return Remove_Last(S.Items)
    end def Remove_Last

    def Remove_Any(var S : Ordered_Set) 
      -> optional Interval<Element_Type> :
	# Remove an arbitrary interval of set
	return Remove_Any(S.Items)
    end def Remove_Any

end class PSL::Containers::Ordered_Set

def PSL::Test::Test_Ordered_Set(A, X, Y, Z : Univ_Real) :
    var S : Ordered_Set<Univ_Real> = X | Y..Z
    Println(A | " in " | X | "|" | Y | ".." | Z | "=" | ( A in S ))

    const Agg : Ordered_Set<Univ_Real> = [X, Y, Z]
    const Or : Ordered_Set<Univ_Real> = Y | Z | X
    const Open_Ind : Map<Boolean, Univ_String> = [#false : "", #true : "<"]

    Println(" [X, Y, Z] =? (Y | Z | X) --> " | ( Agg =? Or ))

    S = []

    var R = 0.0
    for I in 1..10 forward loop
	R += 1.0
        S += R
        Println("Adding " | R | " to S, Is_Empty(S) = " | Is_Empty(S))
    end loop

    var Xor = S xor [2.0, 5.0, 12.0, 15.0]
    Print("S xor [2.0, 5.0, 12.0, 15.0] = ")
    for IV in Xor forward loop
	if IV.Low != IV.High :
	    Print(IV.Low | Open_Ind[IV.Low_Is_Open] | ".." | 
	      Open_Ind[IV.High_Is_Open] | IV.High | " ")
	else :
            Print(IV.Low | " ")
	end if
    end loop
    Print('\n')
end def PSL::Test::Test_Ordered_Set

class interface PSL::Core::Univ_Real<> :
    defop "+"(Right : Univ_Real) -> Univ_Real
      is import(#identity)

    defop "-"(Right : Univ_Real) -> Univ_Real
      is import(#real_negate)

    defop "abs"(Right : Univ_Real) -> Univ_Real
      is import(#real_abs)

    defop "magnitude"(Univ_Real) -> Univ_Real is "abs"

    defop "+"(Left, Right : Univ_Real) -> Result : Univ_Real 
      is import(#real_add)

    defop "-"(Left, Right : Univ_Real) -> Result : Univ_Real
      is import(#real_subtract)

    defop "*"(Left, Right : Univ_Real) -> Result : Univ_Real 
      is import(#real_multiply)

    defop "*"(Left : Univ_Real; Right : Univ_Integer) -> Univ_Real
      is import(#real_int_multiply)

    defop "*"(Left : Univ_Integer; Right : Univ_Real) -> Univ_Real

    defop "/"(Left, Right : Univ_Real) -> Result : Univ_Real
      is import(#real_divide)

    defop "/"(Left : Univ_Real; Right : Univ_Integer) -> Univ_Real
      is import(#real_int_divide)

    defop "**"(Left : Univ_Real; Right : Univ_Integer) -> Univ_Real
      is import(#real_exp)

    defop "+="(var Left : Univ_Real; Right : Univ_Real) 
      is import(#real_assign_add)

    defop "-="(var Left : Univ_Real; Right : Univ_Real) 
      is import(#real_assign_subtract)

    defop "*="(var Left : Univ_Real; Right : Univ_Real) 
      is import(#real_assign_multiply)

    defop "/="(var Left : Univ_Real; Right : Univ_Real) 
      is import(#real_assign_divide)

    defop "**="(var Left : Univ_Real; Right : Univ_Integer) 
      is import(#real_assign_exp)


    defop "=?"(Left, Right : Univ_Real) -> Ordering
      is import(#real_compare)

    def Min(Left, Right : optional Univ_Real) -> optional Univ_Real
      is import(#real_min)
    def Max(Left, Right : optional Univ_Real) -> optional Univ_Real
      is import(#real_max)

    def Sqrt(Val : Univ_Real {> Val >= 0.0 <}) -> Univ_Real

    def Hash(Val : Univ_Real) -> Univ_Integer
      is import(#identity)

    def Print(X : Univ_Real) is import(#print_real)

    def To_String(Val : Univ_Real) -> Univ_String
      is import(#to_string_real)

    def From_String(Str : Univ_String) -> optional Univ_Real
      is import(#from_string_real)

    def Round_To_Int(Real : Univ_Real) -> Univ_Integer
      is import(#round_to_int)

    def Int_To_Real(Int : Univ_Integer) -> Univ_Real
      is import(#int_to_real)

    defop "in"(Left : Univ_Real; Right : Ordered_Set<Univ_Real>) -> Boolean
      is in Ordered_Set<Univ_Real>

end interface PSL::Core::Univ_Real

class PSL::Core::Univ_Real :
  exports :
    defop "*"(Left : Univ_Integer; Right : Univ_Real) -> Univ_Real :
        # Hand off to built-in real * int defop
        return Right * Left
    end defop "*"

    def Sqrt(Val : Univ_Real {> Val >= 0.0 <}) -> Result : Univ_Real :
	{> Val >= 0.0 <}
	Result = Val / 2.0
	while Result > 0.0 and then Result * Result / Val 
	  not in 0.9999999999999 .. 1.00000000000001 :
	    Result = (Val / Result + Result)/2.0
	end loop
    end def Sqrt

end class PSL::Core::Univ_Real
    
def PSL::Test::Test_Real() :
    var X = 3.5
    var Y = 5.22
    var Z = X + Y

    Println("X = " | X | ", Y = " | Y | ", X + Y = " | Z)
    var Teeny = 0.000023
    Println("Teeny = " | Teeny)
    var Small = 0.005
    Println("Small = " | Small)
    var Medium = 235.123
    Println("Medium = " | Medium)
    var Big = 11.0**5
    Println("Big = " | Big)
    var Huge = 13.0**15
    Println("Huge = " | Huge)

    Println("Y in 1.0 .. 6.0 = " | ( Y in 1.0 .. 6.0 ))
    
    Println("Y in 1.0 ..< 5.22 = " | ( Y in 1.0 ..< 5.22 ))

    Println("Sqrt(2.0) = " | Sqrt(2.0))

    var Neg = -2.0
    Println("Sqrt(-2.0) = " | Sqrt(Neg))
end def PSL::Test::Test_Real

interface PSL::Core::Indexable
  <Elem_Type is Assignable<>; Index_Type is Countable<>> :
    # An indexable container
    defop "indexing"(ref A : Indexable; Index : Index_Type) -> ref Elem_Type
    def Length(A : Indexable) -> Univ_Integer
    defop "magnitude"(Indexable) -> Univ_Integer is Length
    defop "index_set"(A : Indexable) -> Countable_Set<Index_Type>
    def Bounds(A : Indexable) -> Countable_Set<Index_Type> is "index_set"
end interface PSL::Core::Indexable

class interface PSL::Containers::Basic_Array<Element_Type is Assignable<>> :
  # Builtin array type, not extendable, indexed by Univ_Integer, 1..Length
    def Create(Length : Univ_Integer<>; Val : optional Element_Type) 
      -> Basic_Array 
      is import(#basic_array_create)
    defop "indexing"(ref V : Basic_Array; Index : Univ_Integer<>) -> 
      ref Element_Type is import(#basic_array_indexing)
    defop "var_indexing"(ref var V : Basic_Array; Index : Univ_Integer<>) -> 
      ref var Element_Type is import(#basic_array_indexing)
    def Length(V : Basic_Array) -> Univ_Integer<>
      is import(#basic_array_length)
    defop "magnitude"(Basic_Array) -> Univ_Integer<> is Length
    defop "|"(Left, Right : Basic_Array) -> Basic_Array
    defop "index_set"(A : Basic_Array) -> Countable_Set<Univ_Integer>
end interface PSL::Containers::Basic_Array

class PSL::Containers::Basic_Array :
  # Builtin array type, not extendable, indexed by Univ_Integer, 1..Length
  exports :
    defop "|"(Left, Right : Basic_Array) -> Result : Basic_Array :
        const Left_Len = Length(Left)

        if Left_Len == 0 :
            return Right
        end if

        Result = Create(Left_Len + Length(Right), null)

        # Copy the Left elements
        for I in 1..Left_Len concurrent :
            Result[I] = Left[I]
        end loop

        # Copy the Right elements
        
        for J in 1..Length(Right) concurrent :
            Result[Left_Len + J] = Right[J]
        end loop
    end defop "|"
            
    defop "index_set"(A : Basic_Array) -> Countable_Set<Univ_Integer> :
	return 1..Length(A)
    end defop "index_set"
end class PSL::Containers::Basic_Array

def PSL::Test::Test_Basic_Array() :
    var A : Basic_Array<Univ_Integer<>> = Create(3, 7)
    A[1] = 42
    A[2] += 6
    A[3] += A[2]
    Print("The answer is: " | A[1] + A[2] + A[3] | "\n")
    Print("Length(A) = " | Length(A) | "\n")

    var B = A | A
    Print("Length(B) = " | Length(B) | ", B[5] = " | B[5] | "\n")
    for I in 1..Length(B) forward loop
        Print("B[" | I | "] = " | B[I] | ", ")
    end loop
    Print("\n")
end def PSL::Test::Test_Basic_Array
    
class interface PSL::Core::Vector<Element_Type is Assignable<>> 
  implements Indexable<Element_Type, Univ_Integer> :
  # Extendable vector, indexed by Univ_Integer, 1..Length
    defop "[]"() -> Vector
    def Create(Length : Univ_Integer; Value : Element_Type) -> Vector
    defop "indexing"(ref V : Vector; Index : Univ_Integer) -> 
      ref Element_Type
    defop "slicing"(V : Vector; Index_Set : Countable_Range<Univ_Integer>)
      -> Vector  # a "read-only" slice
    defop "index_set"(V : Vector) -> Countable_Set<Univ_Integer>
    defop "|="(var V : Vector; Elem : Element_Type)
    defop "|="(var V : Vector; Right : Vector)
    defop "<|="(var V : Vector; var Elem : optional Element_Type)
    defop "|"(Left, Right : Vector) -> Vector
    defop "|"(Left : Vector; Right : Element_Type) -> Vector
    def Length(V : Vector) -> Univ_Integer
    defop "magnitude"(Vector) -> Univ_Integer is Length
end interface PSL::Core::Vector
    
class PSL::Core::Vector :
  # Extendable vector, indexed by Univ_Integer, 1..Length
    const Initial_Size = 4
    var Count : Univ_Integer = 0
    var Data : optional Basic_Array<optional Element_Type>

    def Expand_By_One(var V : Vector) :
        # Expand size by one
        if V.Data is null :
            V.Data = Create(Initial_Size, null)
        elsif V.Count == Length(V.Data) :
            # Double the size
            var Old_Data <== V.Data
            V.Data = Create(2*V.Count, null)
            for I in 1..V.Count :
                V.Data[I] <== Old_Data[I]
            end loop
            Old_Data = null
        end if
        V.Count += 1
    end def Expand_By_One

    def Expand_Count(var V : Vector; New_Count : Univ_Integer) :
        # Expand count to new count, putting nulls in new slots
        if V.Data is null :
            V.Data = Create(Max(New_Count, Initial_Size), null)
        elsif Length(V.Data) < New_Count :
            var Old_Data <== V.Data
            V.Data = Create(Max(New_Count, 2*Length(Old_Data)), null)
            for I in 1..V.Count :
                V.Data[I] = Old_Data[I]
            end loop
            Old_Data = null
        end if
        V.Count = New_Count
    end def Expand_Count

  exports :
    defop "[]"() -> Vector :
        return (Count => 0, Data => null)
    end defop "[]"

    def Create(Length : Univ_Integer; Value : Element_Type) -> Vector :
        return (Count => Length, Data => Create (Length, Value))
    end def Create

    def Length(V : Vector) -> Univ_Integer :
        return V.Count
    end def Length

    defop "indexing"(ref V : Vector; Index : Univ_Integer) -> 
      ref Element_Type :
        {> *vector_index_out_of_bounds* Index in 1..V.Count <}
        return V.Data[Index]
    end defop "indexing"

    defop "slicing"(V : Vector; Index_Set : Countable_Range<Univ_Integer>)
      -> Vector :
      # a "read-only" slice
        return [V[I] for I in Index_Set]
    end defop "slicing"

    defop "index_set"(V : Vector) -> Countable_Set<Univ_Integer> :
        return 1..Length(V)
    end defop "index_set"

    defop "<|="(var V : Vector; var Elem : optional Element_Type) :
        Expand_By_One(V)
        V.Data[V.Count] <== Elem
    end defop "<|="

    defop "|="(var V : Vector; Elem : Element_Type) :
        Expand_By_One(V)
        V.Data[V.Count] = Elem
    end defop "|="
    
    defop "|="(var V : Vector; Right : Vector) :
        const Orig_Count = V.Count
        if Orig_Count == 0 :
            # Just copy right
            V = Right;
        else :
            # Grow V, and then copy in Right
            Expand_Count(V, V.Count + Right.Count)
            for I in 1..Right.Count :
                V[I+Orig_Count] = Right[I]
            end loop
        end if
    end defop "|="

    defop "|"(Left, Right : Vector) -> Result : Vector :
        # Copy left into result, and then add in the Right elements
        if Left.Count == 0 :
            # Left is null vector; result is determined by Right
            Result = Right
        else :
            # Start with Left
            Result = Left
            Result |= Right
        end if
    end defop "|"

    defop "|"(Left : Vector; Right : Element_Type) -> Result : Vector :
        Result = Left
        Result |= Right
    end defop "|"

end class PSL::Core::Vector

def PSL::Test::Test_Vector() :
    var V : Vector<Univ_Integer> = []
    var U : Vector<Univ_Integer> = []
    V |= 23
    U |= 24
    V = V | U
    Println("V = " | V[1] | ", " | V[2])
    for I in 1..10 :
        V |= I
    end loop
    for I in 1..Length(V) forward loop
        Println("V[" | I | "] = " | V[I])
    end loop
    Println("\"indexing\"(V, 12) = 33;")
    "indexing"(V, 12) = 33
    for I in 1..Length(V) forward loop
        Println("V[" | I | "] = " | V[I])
    end loop
    const Slice = V[3..7]
    Println("Slice = V[3..7]")
    for I in 1..Length(Slice) forward loop
        Println("Slice[" | I | "] = " | Slice[I])
    end loop
end def PSL::Test::Test_Vector

class interface PSL::Core::ZVector<Element_Type is Assignable<>> 
  implements Indexable<Element_Type, Univ_Integer> :
  # Extendable vector, indexed by Univ_Integer, 0..Length-1
    defop "[]"() -> ZVector
    def Create(Length : Univ_Integer; Value : Element_Type) -> ZVector
    defop "indexing"(ref V : ZVector; Index : Univ_Integer) -> 
      ref Element_Type
    defop "slicing"(V : ZVector; Index_Set : Countable_Range<Univ_Integer>)
      -> ZVector
      # a "read-only" slice
    defop "index_set"(V : ZVector) -> Countable_Set<Univ_Integer>
    defop "|="(var V : ZVector; Elem : Element_Type)
    defop "<|="(var V : ZVector; var Elem : optional Element_Type)
    defop "|"(Left, Right : ZVector) -> ZVector
    def Length(V : ZVector) -> Univ_Integer
    defop "magnitude"(ZVector) -> Univ_Integer is Length
end interface PSL::Core::ZVector
    
class PSL::Core::ZVector :
  # Extendable ZVector, indexed by Univ_Integer, 0..Length-1
    var Vec : Vector<Element_Type>

  exports :
    defop "[]"() -> ZVector :
        return (Vec => [])
    end defop "[]"

    def Create(Length : Univ_Integer; Value : Element_Type) -> ZVector :
        return (Vec => Create(Length, Value))
    end def Create

    def Length(V : ZVector) -> Univ_Integer :
        return Length(V.Vec)
    end def Length

    defop "indexing"(ref V : ZVector; Index : Univ_Integer) -> 
      ref Element_Type :
        {> Index in 0..<Length(V.Vec) <}  # Index out of bounds
        return V.Vec[Index+1]
    end defop "indexing"

    defop "slicing"(V : ZVector; Index_Set : Countable_Range<Univ_Integer>)
      -> ZVector :
      # a "read-only" slice
        return [V[I] for I in Index_Set]
    end defop "slicing"

    defop "index_set"(V : ZVector) -> Countable_Set<Univ_Integer> :
        return 0..<Length(V.Vec)
    end defop "index_set"

    defop "<|="(var V : ZVector; var Elem : optional Element_Type) :
        V.Vec <|= Elem
    end defop "<|="

    defop "|="(var V : ZVector; Elem : Element_Type) :
        V.Vec |= Elem
    end defop "|="
    
    defop "|"(Left, Right : ZVector) -> Result : ZVector :
        return (Vec => Left.Vec | Right.Vec)
    end defop "|"

end class PSL::Core::ZVector

def PSL::Test::Test_ZVector() :
    var V : ZVector<Univ_Integer> = []
    var U : ZVector<Univ_Integer> = []
    V |= 23
    U |= 24
    V = V | U
    Println("V = " | V[0] | ", " | V[1])
    for I in 1..10 :
        V |= I
    end loop
    for I in 0..<Length(V) forward loop
        Println("V[" | I | "] = " | V[I])
    end loop
    Println("\"indexing\"(V, 11) = 33;")
    "indexing"(V, 11) = 33
    for I in 0..<Length(V) forward loop
        Println("V[" | I | "] = " | V[I])
    end loop
    const Slice = V[3..7]
    Println("Slice = V[3..7]")
    for I in 0..<Length(Slice) forward loop
        Println("Slice[" | I | "] = " | Slice[I])
    end loop
end def PSL::Test::Test_ZVector

class interface PSL::Core::ZString<> :
  # A universal string with characters indexed starting at 0
    defop "from_univ"(Univ : Univ_String) -> ZString
      is import(#identity)
    defop "to_univ"(ZStr : ZString) -> Univ_String
      is import(#identity)

    def Print(ZString) is import(#print_string)
    def Println(ZString) is import(#println_string)
    def Readln() -> optional ZString is import(#read_string)

    defop "*"(Left : Univ_Integer; Right : ZString) -> ZString
	# Produce specified number of "Right" strings in a row
    defop "*"(Left : ZString; Right : Univ_Integer) -> ZString
	# Produce specified number of "Left" strings in a row

    defop "|"(Left, Right : ZString) -> ZString 
      is import(#concat_string)

    defop "=?"(Left, Right : ZString) -> Ordering
      is import(#string_compare)

    defop "|="(var Left : ZString; Right : ZString)
      is import(#assign_concat_string)

    defop "indexing"(Str : ZString; Index : Univ_Integer<>) -> Univ_Character
	# a "read-only" element, indexed 0..<Length(Str)

    defop "index_set"(Str : ZString) -> Countable_Set<Univ_Integer>
        # Return set of indices for string

    defop "slicing"(Str : ZString;
      Index_Set : Countable_Range<Univ_Integer>) 
      -> ZString
	# a "read-only" slice

    def Length(Str : ZString) -> Univ_Integer
      is import(#string_length)

    defop "magnitude"(ZString) -> Univ_Integer is Length

    def Hash(Val : ZString) -> Univ_Integer
      is import(#identity)

    defop "|"(Left : ZString; Right : Right_Type is Imageable<>) 
      -> ZString

    defop "|"(Left : Left_Type is Imageable<>; Right : ZString)
      -> ZString

    defop "|="(var Left : ZString; Right : Right_Type is Imageable<>)

    # Operations to convert to/from a ZVector of Univ_Character's
    def To_ZVector(Str : ZString) -> ZVector<Univ_Character>
    def From_ZVector(Vec : ZVector<Univ_Character>) -> ZString

end interface PSL::Core::ZString
    
class PSL::Core::ZString :
  # A universal string with characters indexed starting at 0
    var U_Str : Univ_String

  exports :
    defop "indexing"(Str : ZString; Index : Univ_Integer<>) -> Univ_Character :
	# a "read-only" element, indexed 0..<Length(Str)
        return Str.U_Str[Index+1]
    end defop "indexing"

    defop "index_set"(Str : ZString) -> Countable_Set<Univ_Integer> :
        # Return set of indices for string
        return 0 ..< |Str.U_Str|
    end defop "index_set"

    defop "slicing"(Str : ZString;
      Index_Set : Countable_Range<Univ_Integer>) 
      -> ZString :
	# a "read-only" slice
        return (U_Str => Str.U_Str[Index_Set.First <.. Index_Set.Last+1])
    end defop "slicing"

    defop "*"(Left : Univ_Integer; Right : ZString) -> ZString :
	# Produce specified number of "Right" strings in a row
        return (U_Str => Left * Right.U_Str)
    end defop "*"

    defop "*"(Left : ZString; Right : Univ_Integer) -> ZString :
	# Produce specified number of "Left" strings in a row
	return Right * Left   # Just pass the buck to other "*"
    end defop "*"

    defop "|"(Left : ZString; Right : Right_Type is Imageable<>) 
      -> ZString :
        if Right is null :
            return (U_Str => Left.U_Str | "null")
        else :
            return (U_Str => Left.U_Str | Right_Type::To_String(Right))
        end if
    end defop "|"

    defop "|"(Left : Left_Type is Imageable<>; Right : ZString)
      -> ZString :
        if Left is null :
            return (U_Str => "null" | Right.U_Str)
        else :
            return (U_Str => Left_Type::To_String(Left) | Right.U_Str)
        end if
    end defop "|"

    defop "|="(var Left : ZString; Right : Right_Type is Imageable<>) :
        if Right is null :
            Left.U_Str |= "null"
        else :
            Left.U_Str |= Right_Type::To_String(Right)
        end if
    end defop "|="

    # Operations to convert to/from a ZVector of Univ_Character's
    def To_ZVector(Str : ZString) -> ZVector<Univ_Character> :
        return [Str[I] for I in 0..<Length(Str)]
    end def To_ZVector

    def From_ZVector(Vec : ZVector<Univ_Character>) -> ZString :
        return (for each C of Vec forward => <""> | C)
end class PSL::Core::ZString

def PSL::Test::Test_ZString() :
    const U : ZString = "tab\t"
    const T = U[3]
    Println("U = " | U | ", Length(U) = " | Length(U) | ", U[4] = '" | T | "'")
    const X : ZString = "this is a string"
    const Y = X[2]
    {> Y == 'i' <} # string indexing problem
    const Z = X[5..6]
    {> Z == "is" <} # string slicing problem, expected 'is', found Z 
    Println("X = " | X | ", X[2] = " | Y | ", X[5..6] = " | Z)

    Println("About to indent 4 and then print 'hello'")
    Println(4 * " " | "hello")

    const Seven_Xs :ZString = "x" * 7
    {> Seven_Xs == "xxxxxxx" <} # Char multiplication check

    Println("Here are seven x's: " | Seven_Xs)
    
    Print("Here are 6 y's: ")
    Println('y' * 6)
end def PSL::Test::Test_ZString

class interface PSL::Containers::Slice<Array_Type is Indexable<>> :
  # A sliceable array.
    const First : Array_Type::Index_Type
    const Last : Array_Type::Index_Type

    def Length(S : Slice) -> Univ_Integer
        # Return length of slice (i.e. Last - First + 1)

    defop "magnitude"(Slice) -> Univ_Integer is Length
        # Allows use of |...| operator

    defop "slicing"(ref A : Array_Type) -> ref Slice
        # Convert an array into a slice covering 1..Length(A)

    defop "slicing"(ref A : Array_Type;
      Bounds : Countable_Range<Array_Type::Index_Type>)
      -> ref Slice
        # Slice of an array

    defop "slicing"(ref S : Slice;
      Bounds : Countable_Range<Array_Type::Index_Type>)
      -> ref Slice
        # Slice of a slice

    defop "indexing"(ref S : Slice; Index : Array_Type::Index_Type) 
      -> ref Array_Type::Elem_Type
        # Index into a slice

    defop "index_set"(S : Slice) -> Countable_Set<Array_Type::Index_Type>
end interface PSL::Containers::Slice

class PSL::Containers::Slice :
    ref Arr : Array_Type
  exports :
    def Length(S : Slice) -> Univ_Integer :
        # Return length of slice (i.e. Last - First + 1)
        return S.Last - S.First + 1
    end def Length

    defop "slicing"(ref A : Array_Type) -> ref Slice :
        # Convert an array into a slice covering whole array
        const A_Bounds = "index_set"(A)
        return (First => First(A_Bounds), Last => Last(A_Bounds), Arr => A)
    end defop "slicing"

    defop "slicing"(ref A : Array_Type;
      Bounds : Countable_Range<Array_Type::Index_Type>)
      -> ref Slice :
        # Slice of an array
        const A_Bounds = "index_set"(A)
        {> Bounds.First > Bounds.Last or else
          (Bounds.First >= First(A_Bounds) and then
            Bounds.Last <= Last(A_Bounds)) <} # slice out of bounds
        return (First => Bounds.First, Last => Bounds.Last, Arr => A)
    end defop "slicing"

    defop "slicing"(ref S : Slice;
      Bounds : Countable_Range<Array_Type::Index_Type>)
      -> ref Slice :
        # Slice of a slice
        {> Bounds.First > Bounds.Last or else
          (Bounds.First >= S.First and then Bounds.Last <= S.Last) <}
          # slice out of bounds
        return (First => Bounds.First, Last => Bounds.Last, Arr => S.Arr)
    end defop "slicing"

    defop "indexing"(ref S : Slice; Index : Array_Type::Index_Type) 
      -> ref Array_Type::Elem_Type :
        # Index into a slice
        {> Index >= S.First and then Index <= S.Last <}
          # array index out of bounds
        return S.Arr[Index]
    end defop "indexing"
    
    defop "index_set"(S : Slice) -> Countable_Set<Array_Type::Index_Type> :
        return S.First .. S.Last
    end defop "index_set"
end class PSL::Containers::Slice

class interface PSL::Core::Enum<Literals : Vector<Univ_Enumeration>> :
    defop "from_univ"(Univ : Univ_Enumeration) 
      {> (for some Lit of Literals => Lit == Univ) <}
      -> Enum
    defop "to_univ"(Val : optional Enum) -> Result : optional Univ_Enumeration
      {> Result is null or else (for some Lit of Literals => Lit == Result) <}

    defop "=?"(Left, Right : Enum) -> Ordering
      is import("=?")

    # Functions for Imageable
    def To_String(Val : Enum) -> Univ_String
    def From_String(Str : Univ_String) -> optional Enum

    # Operators for Countable
    defop "+"(Left : Univ_Integer; Right : Enum) -> Enum 
      is import("+")
    defop "+"(Left : Enum; Right : Univ_Integer) -> Enum 
      is import("+")
    defop "-"(Left, Right : Enum) -> Univ_Integer 
      is import("-")
    defop "-"(Left : Enum; Right : Univ_Integer) -> Enum 
      is import("-")

    # TBD: These should be properties some day (e.g. Color#first).
    def First()->Enum
    def Last()->Enum
    def Range()->Countable_Range<Enum> is "[..]" in Countable_Range<Enum>

    defop "[..]"() -> Countable_Range<Enum> is in Countable_Range<Enum>

    def Hash(Val : Enum) -> Univ_Integer
      is import(#identity)

    defop ".."(Left, Right : Enum) -> Countable_Set<Enum>
      is in Countable_Set<Enum>
    defop "<.."(Left, Right : Enum) -> Countable_Set<Enum>
      is in Countable_Set<Enum>
    defop "..<"(Left, Right : Enum) -> Countable_Set<Enum>
      is in Countable_Set<Enum>
    defop "<..<"(Left, Right : Enum) -> Countable_Set<Enum>
      is in Countable_Set<Enum>
    defop "|"(Left, Right : Enum) -> Countable_Set<Enum>
      is in Countable_Set<Enum>

    def Min(Left, Right : optional Enum) -> optional Enum
      is import(#min)
    def Max(Left, Right : optional Enum) -> optional Enum
      is import(#max)
end interface PSL::Core::Enum

class PSL::Core::Enum :
    const Value : Univ_Integer
  exports :
    defop "from_univ"(Univ : Univ_Enumeration) 
      {> (for some Lit of Literals => Lit == Univ) <}
      -> Enum :
        for I in 1 .. |Literals| :
            if Literals[I] == Univ :
                return (Value => I-1)
            end if
        end loop
	{> *bad_enum_literal* #false <}
        return null
    end defop "from_univ"

    defop "to_univ"(Val : optional Enum) -> Result : optional Univ_Enumeration
      {> Result is null or else (for some Lit of Literals => Lit == Result) <} :
	if Val is null :
	    return null
	else :
	    return Literals[Val.Value+1]
	end if
    end defop "to_univ"

    # Functions for Imageable
    def To_String(Val : Enum) -> Univ_String :
	return Univ_Enumeration::To_String([[Val]])
    end def To_String

    def From_String(Str : Univ_String) -> optional Enum :
	return Univ_Enumeration::From_String(Str)
    end def From_String

    def First()->Enum :
        return (Value => 0)
    end def First

    def Last()->Enum :
        return (Value => |Literals| - 1)
    end def Last

end class PSL::Core::Enum

def PSL::Test::Test_Enum() :
    type Color is Enum< [#red, #green, #blue] >
    type Day_Of_Week is
       Enum< [#Monday, #Tuesday, #Wednesday, #Thursday, 
        #Friday, #Saturday, #Sunday] >

    for C : Color in Color::First() .. Color::Last() forward :
        Println("Next color = " | C)
    end loop

    for C2 in Color reverse :
        Println("Prev color = " | C2)
    end loop

    for D3 in Day_Of_Week :
        Println("Random day of week = " | D3)
    end loop

    {> #Monday in Day_Of_Week <}

end def PSL::Test::Test_Enum

class interface PSL::Containers::Map
  <Key_Type is Hashable<>; Value_Type is Assignable<>> :
  # A hashed-map module
    type Pair is Key_Value<Key_Type, Value_Type>

    defop "[]"() -> Map

    defop "|="(var Left : Map; Right : Pair)
	# Add Key=>Value to Map, replacing pre-existing mapping
	# for Key, if any.

    defop "|"(Left : Map; Right : Pair) -> Map
	# Add Key=>Value to Map, replacing pre-existing mapping
	# for Key, if any in result.

    defop "<|="(var Left : Map; var Right : optional Pair)
	# Move Key/Value pair into map, leaving Right null

    defop "<|="(var Left : Map; var Right : Map)
	# Move key/value pairs from Right into Left, leaving Right empty,
	# replacing any pre-existing entries with the same key.

    defop "+="(var Left : Map; Right : Pair) is "|="
	# A synonym for adding a key=>value pair

    defop "in"(Left : Key_Type; Right : Map) -> Boolean
	# Return True if given key has a mapping in the Map

    defop "-="(var M : Map; Key : Key_Type)   # aka Exclude
	# Remove mapping for Right, if present

    defop "index_set"(M : Map) -> Set<Key_Type>
	# Return set of keys with mappings

    def Keys(M : Map) -> Set<Key_Type> is "index_set"

    defop "indexing"(ref M : Map; Key : Key_Type) {> Key in M <} -> ref Value_Type
	# Used for references to M[Key]; requires the Key to be in M.

    defop "var_indexing"(ref var M : Map; Key : Key_Type) 
      -> ref var optional Value_Type
	# Used for assignments to M[Key]; Key is added to M if not present

    def Remove_Any(var M : Map) -> optional Pair
	# Remove one mapping from the Map.  Return null if Map is empty

    def Count(M : Map) -> Univ_Integer
	# Number of mappings in the table

    defop "magnitude"(Map) -> Univ_Integer is Count

    def Is_Empty(M : Map) -> Boolean
	# Return True if no mappings in the table

    def Dump_Statistics(M : Map)
      # A debugging routine to show bucket sizes of Map

end interface PSL::Containers::Map

class PSL::Containers::Map :
  # A hashed-map module

    var Data : Basic_Map<Key_Value<Key_Type, Value_Type>>

  exports :
    defop "[]"() -> Map :
        return (Data => [])
    end defop "[]"

    defop "|"(Left : Map; Right : Pair) -> Result : Map :
	# Add Key=>Value to Map, replacing pre-existing mapping
	# for Key, if any in result.
        Result = Left
        Result.Data |= Right
    end defop "|"

    defop "|="(var Left : Map; Right : Pair) :
	# Add Key=>Value to Map, replacing pre-existing mapping
	# for Key, if any.
	Left.Data |= Right
    end defop "|="

    defop "<|="(var Left : Map; var Right : optional Pair) :
	# Move Key/Value pair into map, leaving Right null
	Left.Data <|= Right
    end defop "<|="

    defop "<|="(var Left : Map; var Right : Map) :
	# Move key/value pairs from Right into Left, leaving Right empty,
	# replacing any pre-existing entries with the same key.
	loop
	    var Right_Elem for Left = Remove_Any(Right)
	    if Right_Elem is null :
		return   # All done
	    end if

	    Left.Data <|= Right_Elem
	end loop
    end defop "<|="

    defop "in"(Left : Key_Type; Right : Map) -> Boolean :
	return Left in Right.Data
    end defop "in"

    defop "-="(var M : Map; Key : Key_Type) :
      # Remove the given key from the Map, if present
	M.Data -= Key
    end defop "-="
   
    defop "index_set"(M : Map) -> Result : Set<Key_Type> :
	# Return set of keys with non-null mappings
        if M.Data is null :
            return []
        else :
           return "index_set"(M.Data)
        end if
    end defop "index_set"

    defop "indexing"(ref M : Map; Key : Key_Type) {> Key in M <} -> ref Value_Type :
	# Used for references to M[Key]; requires the Key to be in M.
	return M.Data[Key].Value
    end defop "indexing"

    defop "var_indexing"(ref var M : Map; Key : Key_Type) 
      -> ref var optional Value_Type :
	# Used for assignments to M[Key]; Key is added to M if not present
	return "var_indexing"(M.Data, Key).Value
    end defop "var_indexing"

    def Count(M : Map) -> Univ_Integer :
	return Count(M.Data)
    end def Count

    def Is_Empty(M : Map) -> Boolean :
	# Return True if no mappings in the table
	return Is_Empty(M.Data)
    end def Is_Empty

    def Remove_Any(var M : Map) -> Result : optional Pair :
	return Remove_Any(M.Data)
    end def Remove_Any

    def Dump_Statistics(M : Map) :
      # A debugging routine to show bucket sizes of Map
	Dump_Statistics(M.Data)
    end def Dump_Statistics

end class PSL::Containers::Map

def PSL::Test::Test_Map(X : Univ_Enumeration; Y : Univ_String;
  A : Univ_Enumeration; B : Univ_String) :
    type Enum_String_Map is Map<Univ_Enumeration, Univ_String>

    var M : Enum_String_Map = [X : Y, A : B]

    Println("Count = " | Count(M))

    for each [K => V] of M :
	Println("Mapping " | K | " => " | V)
	Println(K | " in M = " | (K in M))
    end loop

    Println("#xy in M = " | (#xy in M))

    M |= [X : null]

    Println("Count after deletion = " | Count(M))

    for each [K => V] of M :
	Println("Mapping " | K | " => " | V)
    end loop

    M |= [X : "a new value"]

    Println("Count after addition = " | Count(M))

    for each [K => V] of M :
	Println("Mapping " | K | " => " | V)
    end loop

    M |= [X : "a third value"]

    Println("Count after replacement = " | Count(M))

    for each [K => V] of M :
	Println("Mapping " | K | " => " | V)
    end loop

    M -= A

    Println("Count after deletion = " | Count(M))

    for each [K => V] of M :
	Println("Mapping " | K | " => " | V)
    end loop

    var Ran = Random::Start(Hash(A))
    var MUI : Map<Univ_Integer, Univ_Integer> = []

    Println("Adding 100 random pairings to Map.")
    for I in 1..100 :
	const Key = Next(Ran) mod 100
	const Value = Next(Ran) mod 100
	MUI[Key] = Value
    end loop
    Println("Map is now of count = " | Count(MUI))

    for each [K => V] of MUI :
	Println("Mapping " | K | " => " | V)
	Println("MUI[" | K | "] = " | MUI[K])
    end loop

    for (Pair in MUI; I in 1..60 forward) :
	Print(" [" | Pair.Key | " => " | Pair.Value | "]")
	if I mod 5 == 0 :
	    Print('\n')
	end if
    end loop

    if Count(MUI) > 60 :
	Println(" ...")
    elsif Count(MUI) mod 5 != 0 :
	Print('\n')
    end if

end def PSL::Test::Test_Map

class interface PSL::Core::Float<Digits : Univ_Integer = 15> :
  # A floating point type that provides at least the
  # given number of digits of precision.

    defop "from_univ"(Univ : Univ_Real) -> Float
      is import(#identity)

    defop "to_univ"(Val : Float) -> Univ_Real
      is import(#identity)

    defop "+"(Right : Float) -> Float
      is import(#identity)

    defop "-"(Right : Float) -> Float
      is import(#real_negate)

    defop "abs"(Right : Float) -> Float
      is import(#real_abs)

    defop "magnitude"(Float) -> Float is "abs"

    defop "+"(Left, Right : Float) -> Result : Float 
      is import(#real_add)

    defop "-"(Left, Right : Float) -> Result : Float
      is import(#real_subtract)

    defop "*"(Left, Right : Float) -> Result : Float 
      is import(#real_multiply)

    defop "*"(Left : Float; Right : Univ_Integer) -> Result : Float 
      is import(#real_int_multiply)

    defop "*"(Left : Univ_Integer; Right : Float) -> Result : Float

    defop "/"(Left, Right : Float) -> Result : Float
      is import(#real_divide)

    defop "/"(Left : Float; Right : Univ_Integer) -> Result : Float
      is import(#real_int_divide)

    defop "**"(Left : Float; Right : Univ_Integer) -> Float
      is import(#real_exp)

    defop "+="(var Left : Float; Right : Float) 
      is import(#real_assign_add)

    defop "-="(var Left : Float; Right : Float) 
      is import(#real_assign_subtract)

    defop "*="(var Left : Float; Right : Float) 
      is import(#real_assign_multiply)

    defop "/="(var Left : Float; Right : Float) 
      is import(#real_assign_divide)

    defop "**="(var Left : Float; Right : Univ_Integer) 
      is import(#real_assign_exp)


    defop "=?"(Left, Right : Float) -> Ordering
      is import(#real_compare)

    def Min(Left, Right : optional Float) -> optional Float
      is import(#real_min)
    def Max(Left, Right : optional Float) -> optional Float
      is import(#real_max)

    def Hash(Val : Float) -> Univ_Integer
      is import(#identity)

    def Round_To_Int(Val : Float) -> Univ_Integer
      is import(#round_to_int)

    def Int_To_Float(Int : Univ_Integer) -> Float
      is import(#int_to_real)

    def Print(X : Float) is import(#print_real)

    def To_String(Val : Float) -> Univ_String
      is import(#to_string_real)

    def From_String(Str : Univ_String) -> optional Float
      is import(#from_string_real)

    defop "in"(Left : Float; Right : Ordered_Set<Float>) -> Boolean
      is in Ordered_Set<Float>
	# "in" defined in Ordered_Set

    
end interface PSL::Core::Float

class PSL::Core::Float :
    const Value : Univ_Real
  exports :
    defop "*"(Left : Univ_Integer; Right : Float) -> Result : Float :
        # Hand off to built-in real*int defop
        return Right * Left
    end defop "*"
end class PSL::Core::Float
    
def PSL::Test::Test_Float() :
    type My_Float is Float<Digits => 8>

    var X : My_Float = 3.5
    var Y : My_Float = 5.22
    var Z : My_Float = X + Y

    Println("X = " | X | ", Y = " | Y | ", X + Y = " | Z)
    var Teeny = 0.000023
    Println("Teeny = " | Teeny)
    var Small = 0.005
    Println("Small = " | Small)
    var Medium = 235.123
    Println("Medium = " | Medium)
    var Big = 11.0**5
    Println("Big = " | Big)
    var Huge = 13.0**15
    Println("Huge = " | Huge)

    var X2 : My_Float = 3.5 * 2

    Println("X * 2 = " | X * 2 | ", 2 * X = " | 2 * X | ", X2 = " | X2)
    Println("X / 2 = " | X / 2 )

    if X in 1.5 ..< 3.6 :
	Println("X *is* in 1.5 ..< 3.6")
    else :
	Println("X *not* in 1.5 ..< 3.6??")
    end if

    Println("X in 1.5 ..< 3.5 = " | (X in 1.5 ..< 3.5) )
end def PSL::Test::Test_Float

class interface PSL::Core::Fixed<Delta : Univ_Real> :
  # Fixed point types, represented as an integer
  # multiple of the given Delta
    defop "from_univ"(Univ : Univ_Real) -> Fixed
    defop "to_univ"(Val : optional Fixed) -> optional Univ_Real

    defop "+"(Right : Fixed) -> Fixed
      is import(#identity)

    defop "-"(Right : Fixed) -> Fixed
      is import(#negate)

    defop "abs"(Right : Fixed) -> Fixed
      is import("abs")

    defop "magnitude"(Fixed) -> Fixed is "abs"

    defop "+"(Left, Right : Fixed) -> Fixed
      is import("+")
    defop "-"(Left, Right : Fixed) -> Fixed
      is import("-")
    defop "*"(Left : Fixed; Right : Univ_Real) -> Fixed
      is import(#fixed_real_mul)
    defop "*"(Left : Univ_Real; Right : Fixed) -> Fixed
      is import(#real_fixed_mul)
    defop "*"(Left : Fixed; Right : Univ_Integer) -> Fixed
      is import("*")
    defop "*"(Left : Univ_Integer; Right : Fixed) -> Fixed
      is import("*")
    defop "/"(Left : Fixed; Right : Univ_Real) -> Fixed
      is import(#fixed_real_div)
    defop "/"(Left : Fixed; Right : Univ_Integer) -> Fixed
      is import("/")

    defop "+="(var Left : Fixed; Right : Fixed) 
      is import("+=")

    defop "-="(var Left : Fixed; Right : Fixed) 
      is import("-=")

    defop "*="(var Left : Fixed; Right : Fixed) 
      is import("*=")

    defop "/="(var Left : Fixed; Right : Univ_Integer) 
      is import("/=")

    defop "/="(var Left : Fixed; Right : Univ_Real)

    defop "=?"(Left, Right : Fixed) -> Ordering
      is import("=?")

    def Min(Left, Right : optional Fixed) -> optional Fixed
      is import(#min)
    def Max(Left, Right : optional Fixed) -> optional Fixed
      is import(#max)

    def Hash(Val : Fixed) -> Univ_Integer
      is import(#identity)

    def Round_To_Int(Val : Fixed) -> Univ_Integer

    def Print(X : Fixed)

    def To_String(Val : Fixed) -> Univ_String

    def From_String(Str : Univ_String) -> optional Fixed

    defop "in"(Left : Fixed; Right : Ordered_Set<Fixed>) 
      -> Boolean is in Ordered_Set<Fixed>
    
end interface PSL::Core::Fixed

class PSL::Core::Fixed :
    const Value : Univ_Integer

  exports :
    defop "from_univ"(Univ : Univ_Real) -> Fixed :
	return (Value => Univ_Real::Round_To_Int(Univ/Delta))
    end defop "from_univ"

    defop "to_univ"(Val : optional Fixed) -> optional Univ_Real :
	if Val is null :
	    return null
	else :
	    return Delta * Int_To_Real(Val.Value)
	end if
    end defop "to_univ"

    defop "/="(var Left : Fixed; Right : Univ_Real) :
	Left = Left / Right
    end defop "/="

    def Print(X : Fixed) :
	var Val : Univ_Real = [[X]]
	Print(Val)
    end def Print

    def To_String(Val : Fixed) -> Univ_String :
	return Univ_Real::To_String([[Val]])
    end def To_String

    def From_String(Str : Univ_String) -> optional Fixed :
	return Univ_Real::From_String(Str)
    end def From_String

    def Round_To_Int(Val : Fixed) -> Univ_Integer :
	return Univ_Real::Round_To_Int([[Val]])
    end def Round_To_Int

end class PSL::Core::Fixed

def PSL::Test::Test_Fixed(X : Univ_Real) :
    type My_Fixed is Fixed<Delta => 1.0E-6>

    var XTI : My_Fixed = X
    const Minute : My_Fixed = 60.0
    Println("X = " | XTI)
    Println("X * 2 = " | (XTI*2))
    Println("X + X = " | (XTI + XTI))
    Println("X - X = " | (XTI - XTI))
    Println("X / 2 = " | (XTI / 2))
    Println("X / 2.0 = " | (XTI / 2.0))
    Println("X * 3.0 = " | (XTI * 3.0))
    Println("5.0 * X = " | (5.0 * XTI))
    Println("-5.0 * X = " | (-5.0 * XTI))
    Println("-5.0 = " | -5.0)
    Println("Minute = " | Minute)
    Println("-5.0*Minute = " | -5.0*Minute)

    Println("Round_To_Int(X) = " | Round_To_Int(XTI))

    const Rng : Ordered_Set<My_Fixed> = 1.0 ..< 5.0

    Print("Rng = ")
    const Open_Ind : Map<Boolean, Univ_String> = [#false : "", #true : "<"]
    for IV in Rng forward loop
	if IV.Low != IV.High :
	    Print(IV.Low | Open_Ind[IV.Low_Is_Open] | ".." | 
	      Open_Ind[IV.High_Is_Open] | IV.High | " ")
	else :
            Print(IV.Low | " ")
	end if
    end loop
    Print('\n')

    Println("X = " | XTI)
    Println("X in Rng = " | (XTI in Rng))

    if XTI in 1.0 ..< 5.0 :
	Println("X in 1.0 ..< 5.0")
    else :
	Println("X not in 1.0 ..< 5.0")
    end if

    Println("X in 1.0 ..< 5.0 = " | (XTI in 1.0 ..< 5.0))

end def PSL::Test::Test_Fixed

class interface PSL::Containers::Array
  <Elem_Type is Assignable<>; Indexed_By is Countable<>> 
  implements Indexable<Elem_Type, Indexed_By> :
  # An array abstraction where the index type is not necessarily
  # Univ_Integer, but instead can be any Countable type (e.g. Enum<...>)

    def Create(Bounds : Countable_Range<Indexed_By>;
      Initial_Value : optional Elem_Type)
      -> Array

    def Length(Arr : Array) -> Univ_Integer
    defop "magnitude"(Array) -> Univ_Integer is Length

    def Bounds(Arr : Array) -> Countable_Range<Indexed_By>

    defop "indexing"(ref Arr : Array; Index : Indexed_By) -> ref Elem_Type
    defop "var_indexing"(ref var Arr : Array; Index : Indexed_By) 
      -> ref var Elem_Type
    defop "index_set"(Arr : Array) -> Countable_Set<Indexed_By>
    defop "<|="(var Left : Array; var Right : optional Elem_Type)
    defop "|"(Left : Array; Right : Key_Value<Indexed_By, Elem_Type>) -> Array
    defop "[]"() -> Array
end interface PSL::Containers::Array

class PSL::Containers::Array :
  # An array abstraction where the index type is not necessarily
  # Univ_Integer, but instead can be any Countable type (e.g. Enum<...>)
    type Elem_Array is Basic_Array<Elem_Type>
    var Data : Elem_Array
    const Bounds : Countable_Range<Indexed_By>
  exports :

    def Create(Bounds : Countable_Range<Indexed_By>;
      Initial_Value : optional Elem_Type)
      -> Array :
	return (Data => Create(Length(Bounds), Initial_Value),
	  Bounds => Bounds)
    end def Create

    def Length(Arr : Array) -> Univ_Integer :
	return Length(Arr.Data)
    end def Length

    def Bounds(Arr : Array) -> Countable_Range<Indexed_By> :
	return Arr.Bounds
    end def Bounds

    defop "indexing"(ref Arr : Array; Index : Indexed_By) -> ref Elem_Type :
	return Arr.Data[ Index - Arr.Bounds.First + 1 ]
    end defop "indexing"

    defop "var_indexing"(ref var Arr : Array; Index : Indexed_By) 
      -> ref var Elem_Type :
	if Length(Arr) == 0 :
	    # Create a one-element array
	    Arr = (Data => Create(1, null),
	      Bounds => Index .. Index)
	elsif Index < Arr.Bounds.First :
	    # Extend below
	    Arr = (Data => 
	      Elem_Array::Create(Arr.Bounds.First - Index, null) | Arr.Data,
	      Bounds => Index .. Arr.Bounds.Last)
	elsif Index > Arr.Bounds.Last :
	    # Extend above
	    Arr = (Data => 
	      Arr.Data | Elem_Array::Create(Index - Arr.Bounds.Last, null),
	      Bounds => Arr.Bounds.First .. Index)
	end if
	return Arr.Data[ Index - Arr.Bounds.First + 1 ]
    end defop "var_indexing"

    defop "index_set"(Arr : Array) -> Countable_Set<Indexed_By> :
        return Arr.Bounds.First .. Arr.Bounds.Last
    end defop "index_set"

    defop "<|="(var Left : Array; var Right : optional Elem_Type) :
	# Add Right onto Array
	if Length(Left) == 0 :
	    Left = (Data => Create(1, null),
	      Bounds => Indexed_By::First() .. Indexed_By::First())
	    Left[Indexed_By::First()] <== Right
	else :
	    Left[Left.Bounds.Last + 1] <== Right
	end if
    end defop "<|="
    
    defop "|"(Left : Array; Right : Key_Value<Indexed_By, Elem_Type>) 
      -> Result : Array :
	Result = Left
	Result[Right.Key] = Right.Value
    end defop "|"

    defop "[]"() -> Result : Array :
	return (Data => Create(0, null),
	  Bounds => Indexed_By::First()+1 .. Indexed_By::First())
	    # Bounds not meaningful when empty
    end defop "[]"

end class PSL::Containers::Array

def PSL::Test::Test_Array() :
    type Color is Enum< [#red, #green, #blue] >
    type RCB is Array<Integer<Range => 0..0xFF>, Indexed_By => Color>

    const Red : RCB = [#red : 0x33, #green : 0, #blue : 0]
    const Blue : RCB = [#red : 0, #green : 0, #blue : 0x33]
    const Yellow : RCB = [0, 0x33, 0x33]
    const White : RCB = [0xFF, 0xFF, 0xFF]
    
    const RCBs : Vector<RCB> = [Red, Blue, Yellow, White]

    for each C of RCBs :
	Println("#red => " | C[#red] | ", #green => " | C[#green] |
	  ", #blue => " | C[#blue])
    end loop
end def PSL::Test::Test_Array

class interface PSL::Containers::Index_Pair
  <Index1_Type is Countable<>; Index2_Type is Countable<>> :
  # Two countable indices which can be combined to form an 2D array index
    const Index1 : Index1_Type
    const Index2 : Index2_Type

    def Nth_Pair
      (Bounds1 : Countable_Range<Index1_Type>;
       Bounds2 : Countable_Range<Index2_Type>;
       N : Univ_Integer) -> optional Index_Pair
      # Return Nth Pair given bounds for each of 2 index types
      # when N goes from 1 to Length(Bounds1) * Length(Bounds2)
      # presuming second index varies fastest.
      # Return null if N out of range.

    def Pair_Position
      (Bounds1 : Countable_Range<Index1_Type>;
       Bounds2 : Countable_Range<Index2_Type>;
       Pair : Index_Pair) -> optional Univ_Integer
      # Return position of Pair within bounds
      # presuming Pair.Index1 in Bounds1 and Pair.Index2 in Bounds2
      # and second index varies fastest.
      # Return null if Pair out of range.

end interface PSL::Containers::Index_Pair

class PSL::Containers::Index_Pair :
  exports :
    def Nth_Pair
      (Bounds1 : Countable_Range<Index1_Type>;
       Bounds2 : Countable_Range<Index2_Type>;
       N : Univ_Integer) -> optional Index_Pair :
      # Return Nth Pair given bounds for each of 2 index types
      # when N goes from 1 to Length(Bounds1) * Length(Bounds2).
      # Return null if N out of range.
        const Len2 = Length (Bounds2)
        if N <= 0 or else N > Length(Bounds1) * Len2 :
            return null
        else :
            const I1 = (N-1) / Len2
            const I2 = (N-1) rem Len2
            return (Index1 => Bounds1.First + I1,
                    Index2 => Bounds2.First + I2)
        end if
    end def Nth_Pair

    def Pair_Position
      (Bounds1 : Countable_Range<Index1_Type>;
       Bounds2 : Countable_Range<Index2_Type>;
       Pair : Index_Pair) -> optional Univ_Integer :
      # Return position of Pair within bounds (starting at 1)
      # presuming Pair.Index1 in Bounds1 and Pair.Index2 in Bounds2
      # and second index varies fastest.
      # Return null if Pair out of range.
        if Pair.Index1 not in Bounds1 or else Pair.Index2 not in Bounds2 :
            return null
        else :
            return (Pair.Index1-Bounds1.First) * Length(Bounds2) + 
              (Pair.Index2-Bounds2.First) + 1
        end if
    end def Pair_Position

end class PSL::Containers::Index_Pair

class interface PSL::Containers::Index_Pair_Set<Pair is Index_Pair<>> :
  # Set of index pairs
    def Create(Bounds1 : Countable_Range<Pair::Index1_Type>;
                Bounds2 : Countable_Range<Pair::Index2_Type>)
      -> Index_Pair_Set

    defop "[]"() -> Index_Pair_Set

    def Count(Index_Pair_Set) -> Univ_Integer

    def Is_Empty(Index_Pair_Set) -> Boolean

    def First(Index_Pair_Set)
      -> optional Pair
    def Last(Index_Pair_Set)
      -> optional Pair
    
    def Remove_First(var Index_Pair_Set)
      -> optional Pair
    def Remove_Last(var Index_Pair_Set)
      -> optional Pair
    def Remove_Any(var Index_Pair_Set)
      -> optional Pair
end interface PSL::Containers::Index_Pair_Set

class PSL::Containers::Index_Pair_Set :
  # Set of index pairs
    const Bounds1 : Countable_Range<Pair::Index1_Type>
    const Bounds2 : Countable_Range<Pair::Index2_Type>
    var First : Univ_Integer
    var Last : Univ_Integer

    def Nth_Pair(Index_Pair_Set; N : Univ_Integer) -> Pair :
      # Return Nth Pair of original Index_Pair_Set
      # when N goes from 1 to Length(Bounds1) * Length(Bounds2).
      # Return null if N out of range.
        return Nth_Pair(Index_Pair_Set.Bounds1, Index_Pair_Set.Bounds2, N)
    end def Nth_Pair

  exports :
    def Create(Bounds1 : Countable_Range<Pair::Index1_Type>;
                Bounds2 : Countable_Range<Pair::Index2_Type>)
      -> Index_Pair_Set :
        return (Bounds1 => Bounds1, Bounds2 => Bounds2,
                First => 1, Last => Length(Bounds1) * Length(Bounds2))
    end def Create

    defop "[]"() -> Index_Pair_Set :
        return
          (Bounds1 =>
             Pair::Index1_Type::First()+1 .. Pair::Index1_Type::First(),
           Bounds2 =>
             Pair::Index2_Type::First()+1 .. Pair::Index2_Type::First(),
           First => 1,
           Last => 0)
    end defop "[]"

    def Count(Index_Pair_Set) -> Univ_Integer :
        return Index_Pair_Set.Last - Index_Pair_Set.First + 1
    end def Count

    def Is_Empty(Index_Pair_Set) -> Boolean :
        return Index_Pair_Set.First > Index_Pair_Set.Last
    end def Is_Empty

    def First(Index_Pair_Set)
      -> optional Pair :
        return Nth_Pair(Index_Pair_Set, Index_Pair_Set.First)
    end def First

    def Last(Index_Pair_Set)
      -> optional Pair :
        return Nth_Pair(Index_Pair_Set, Index_Pair_Set.Last)
    end def Last

    def Remove_First(var Index_Pair_Set)
      -> Result : optional Pair :
        if Is_Empty(Index_Pair_Set) :
            Result = null
        else :
            Result = Nth_Pair(Index_Pair_Set, Index_Pair_Set.First)
            Index_Pair_Set.First += 1
        end if
    end def Remove_First

    def Remove_Last(var Index_Pair_Set)
      -> Result : optional Pair :
        if Is_Empty(Index_Pair_Set) :
            Result = null
        else :
            Result = Nth_Pair(Index_Pair_Set, Index_Pair_Set.Last)
            Index_Pair_Set.Last -= 1
        end if
    end def Remove_Last

    def Remove_Any(var Index_Pair_Set)
      -> optional Pair :
        if Count(Index_Pair_Set) mod 2 == 1 :
            return Remove_First(Index_Pair_Set)
        else :
            return Remove_Last(Index_Pair_Set)
        end if
    end def Remove_Any

end class PSL::Containers::Index_Pair_Set

class interface PSL::Containers::Array2D
  <Elem_Type is Assignable<>; Index1 is Countable<>; Index2 is Countable<>> :
  # A 2-D array abstraction where the index types are not necessarily
  # Univ_Integer, but instead can be any Countable type (e.g. Enum<...>)

    def Create(Bounds1 : Countable_Range<Index1>;
      Bounds2 : Countable_Range<Index2>;
      Initial_Value : optional Elem_Type)
      -> Array2D

    type Indices is Index_Pair<Index1, Index2>
    type Set_Of_Indices is Index_Pair_Set<Indices>

    def Length1(Arr : Array2D) -> Univ_Integer
    def Length2(Arr : Array2D) -> Univ_Integer

    def Bounds1(Arr : Array2D) -> Countable_Range<Index1>
    def Bounds2(Arr : Array2D) -> Countable_Range<Index2>

    # Indexing operations using an index pair
    defop "indexing"(ref Arr : Array2D; Indices) -> ref Elem_Type
    defop "var_indexing"(ref var Arr : Array2D; Indices)
      -> ref var Elem_Type

    # Indexing operations using two individual indices
    defop "indexing"(ref Arr : Array2D; Index1; Index2) -> ref Elem_Type
    defop "var_indexing"(ref var Arr : Array2D; Index1; Index2)
      -> ref var Elem_Type

    # Set of index pairs that cover the 2D array.
    defop "index_set"(Arr : Array2D) -> Set_Of_Indices

    defop "[]"() -> Array2D
end interface PSL::Containers::Array2D

class PSL::Containers::Array2D :
  # An array abstraction where the index type is not necessarily
  # Univ_Integer, but instead can be any Countable type (e.g. Enum<...>)
    type Elem_Array is Basic_Array<optional Elem_Type>
    var Data : Elem_Array
    const Bounds1 : Countable_Range<Index1>
    const Bounds2 : Countable_Range<Index2>
  exports :

    def Create(Bounds1 : Countable_Range<Index1>;
      Bounds2 : Countable_Range<Index2>;
      Initial_Value : optional Elem_Type)
      -> Array2D :
	return
          (Data => Create(Length(Bounds1) * Length(Bounds2), Initial_Value),
	   Bounds1 => Bounds1, Bounds2 => Bounds2)
    end def Create

    def Length1(Arr : Array2D) -> Univ_Integer :
	return Length(Arr.Bounds1)
    end def Length1

    def Length2(Arr : Array2D) -> Univ_Integer :
	return Length(Arr.Bounds2)
    end def Length2

    def Bounds1(Arr : Array2D) -> Countable_Range<Index1> is (Arr.Bounds1)
    def Bounds2(Arr : Array2D) -> Countable_Range<Index2> is (Arr.Bounds2)

    # Indexing operations using an index pair
    defop "indexing"(ref Arr : Array2D; Indices) -> ref Elem_Type :
        {> Indices.Index1 in Arr.Bounds1; Indices.Index2 in Arr.Bounds2 <}
	return Arr.Data[Pair_Position(Arr.Bounds1, Arr.Bounds2, Indices)]
    end defop "indexing"

    defop "var_indexing"(ref var Arr : Array2D; Indices)
      -> ref var Elem_Type :
        return "var_indexing"(Arr, Indices.Index1, Indices.Index2)
    end defop "var_indexing"

    # Indexing operations using two individual indices
    defop "indexing"(ref Arr : Array2D; Index1; Index2) -> ref Elem_Type :
        {> Index1 in Arr.Bounds1; Index2 in Arr.Bounds2 <}
	return Arr.Data
          [Indices::Pair_Position(Arr.Bounds1, Arr.Bounds2, (Index1, Index2))]
    end defop "indexing"

    defop "var_indexing"(ref var Arr : Array2D; Index1; Index2)
      -> ref var Elem_Type :
        if Index1 not in Arr.Bounds1 or else Index2 not in Arr.Bounds2 :
            # Extend array to include Index1 and Index2
            var Old_Arr <== Arr
            Arr = Create
              (Bounds1 => Min(Old_Arr.Bounds1.First, Index1) ..
                 Max(Old_Arr.Bounds1.Last, Index1),
               Bounds2 => Min(Old_Arr.Bounds2.First, Index2) ..
                 Max(Old_Arr.Bounds2.Last, Index2),
               Initial_Value => null)

            # Move over old data
            for I in Old_Arr.Bounds1 :
                for J in Old_Arr.Bounds2 :
                    # Explicitly use "indexing" to avoid infinite recursion
                    "indexing"(Arr, I, J) <== Old_Arr[I, J]
                end loop
            end loop

            # Reclaim old data space
            Old_Arr = null
        end if

        #  Now return ref to appropriate element
	return "indexing"(Arr, Index1, Index2)
    end defop "var_indexing"

    # Set of index pairs that cover the 2D array.
    defop "index_set"(Arr : Array2D) -> Set_Of_Indices :
        return Create(Arr.Bounds1, Arr.Bounds2)
    end defop "index_set"

    defop "[]"() -> Array2D :
	return (Data => Create(0, null),
	  Bounds1 => Index1::First()+1 .. Index1::First(),
          Bounds2 => Index2::First()+1 .. Index2::First())
	    # Bounds not meaningful when empty
    end defop "[]"

end class PSL::Containers::Array2D

def PSL::Test::Test_Array2D() :
    type Shade is Enum< [#white, #black] >
    type Chess_Board is Array2D<Shade,
      Index1 => Integer<1..8>, Index2 => Integer<1..8>>

    const Board_Indices : Chess_Board::Set_Of_Indices = Create(1..8, 1..8)
    var CB : Chess_Board =
      [(if (Ix.Index1 - Ix.Index2) mod 2 == 0 then #white else #black)
         for Ix in Board_Indices]

    for each [Ix => C] of CB forward loop
	Println("CB[" | Ix.Index1 | ", " | Ix.Index2 | "] = " | C)

        {> CB[Ix.Index1, Ix.Index2] == C <}
    end loop
end def PSL::Test::Test_Array2D

class interface PSL::Core::Time<> :
  # Time in nanoseconds since Jan 1, 1970

    type Time_Interval is new
      Fixed<Delta => 1.0E-9>

    const Second : Time_Interval = 1.0
    const Millisecond : Time_Interval = 1.0/1000
    const Microsecond : Time_Interval = 1.0/10**6
    const Nanosecond : Time_Interval = 1.0/10**9

    const Minute : Time_Interval = 60.0
    const Hour : Time_Interval = 60 * Minute
    const Day : Time_Interval = 24 * Hour

    type Month_Enum is Enum< 
       [#Jan, #Feb, #Mar, #Apr, #May, #Jun,
        #Jul, #Aug, #Sep, #Oct, #Nov, #Dec] >

    def Create(Year : Univ_Integer; Month : Month_Enum;
      Day_Of_Month : Univ_Integer;
      Time_Of_Day : Time_Interval = 0.0;
      Time_Zone : Time_Interval = 0.0)
      {> Year in 1970 .. 2399; Day_Of_Month in 1..31 <}
      {> Time_Zone in -12.0*Hour .. +12.0*Hour; Time_Of_Day in 0.0 .. 86400.0 <}
      -> Time

    def Zero() -> Time

    defop "+"(Left : Time; Right : Time_Interval) -> Time
      is import("+")
    defop "+"(Left : Time_Interval; Right : Time) -> Time
      is import("+")
    defop "-"(Left : Time; Right : Time_Interval) -> Time
      is import("-")
    defop "-"(Left, Right : Time) -> Time_Interval
      is import("-")

    defop "=?"(Left, Right : Time) -> Ordering
      is import("=?")

    def Min(Left, Right : optional Time) -> optional Time
      is import(#min)
    def Max(Left, Right : optional Time) -> optional Time
      is import(#max)

    def Seconds_Since_1970(Time) -> Time_Interval
      is import(#identity)

end interface PSL::Core::Time

class PSL::Core::Time :
    const Since_1970 : Time_Interval

    type Year_Kind is Enum< [#normal, #leap] >

    const Month_Base : Array<Array<Univ_Integer, Indexed_By => Year_Kind>,
      Indexed_By => Month_Enum> = 
	[#Jan : [0, 0], #Feb : [31, 31], #Mar : [59, 60], #Apr : [90, 91],
         #May : [120, 121], #Jun : [151, 152], #Jul : [181, 182],
         #Aug : [212, 213], #Sep : [243, 244], #Oct : [273, 274],
         #Nov : [304, 305], #Dec : [334, 335]]

    def Kind_Of_Year(Year : Univ_Integer) -> Year_Kind :
	if Year mod 4 == 0 and then
	  (Year mod 100 != 0 or else Year mod 400 == 0) :
	    # Every 100 years leap year is omitted, 
	    # except for multiples of 400 which *are* leap years.
	    return #leap
	else :
	    return #normal
	end if
    end def Kind_Of_Year

    def Leaps_Before(Year : Univ_Integer) -> Univ_Integer :
	# Number of leap years before this year, starting from 1970
	if Year < 2101 :
	    return (Year-1969)/4
	else :
	    # Every 100 years leap year is omitted, 
	    # except for multiples of 400 which *are* leap years.
	    return (Year-1969)/4 - (Year-2001)/100
	end if
    end def Leaps_Before

  exports :
    def Create(Year : Univ_Integer; Month : Month_Enum;
      Day_Of_Month : Univ_Integer;
      Time_Of_Day : Time_Interval = 0.0;
      Time_Zone : Time_Interval = 0.0)
      {> Year in 1970 .. 2399; Day_Of_Month in 1..31 <}
      {> Time_Zone in -12.0*Hour .. +12.0*Hour; Time_Of_Day in 0.0 .. 86400.0 <}
      -> Time :
	# Determine number of nanos since Jan 1, 1970
	const Kind = Kind_Of_Year(1970)
	const Base = Month_Base[Month][Kind]

	return (Since_1970 => 
	  (Year-1970) * (365*Day) + 
	  (Leaps_Before(Year) + Base + Day_Of_Month)*Day +
	  Time_Of_Day - Time_Zone)
    end def Create

    def Zero() -> Time :
	return (Since_1970 => 0.0)
    end def Zero

end class PSL::Core::Time

def PSL::Test::Test_Time(X : Univ_Real) :
    var XTI : Time::Time_Interval = X
    const Local_Minute : Time::Time_Interval = 60.0
    Println("X = " | XTI)
    Println("X * 2 = " | XTI*2)
    Println("X + X = " | (XTI + XTI))
    Println("X - X = " | (XTI - XTI))
    Println("X / 2 = " | (XTI / 2))
    Println("X / 2.0 = " | (XTI / 2.0))
    Println("X * 3.0 = " | (XTI * 3.0))
    Println("5.0 * X = " | (5.0 * XTI))
    Println("-5.0 * X = " | (-5.0 * XTI))
    Println("-5.0 = " | -5.0)
    Println("Local_Minute = " | Local_Minute)
    Println("-5.0*Local_Minute = " | (-5.0*Local_Minute))
    Println("Time::Minute = " | Time::Minute)
    Println("-5.0*Time::Minute = " | (-5.0*Time::Minute))
    Println("Time::Hour = " | Time::Hour )
    Println("-5.0*Time::Hour = " | (-5.0*Time::Hour))

    var T : Time = Create(Year => 2011, Month => #Oct, Day_Of_Month => 18,
      Time_Of_Day => 10*Time::Hour + 5*Time::Minute,
      Time_Zone => -5.0*Time::Hour)

    Println("18-Oct-2011 = " | ( T - Zero() ))
    Println("X = " | XTI )
    Println("18-Oct-2011 - X = " | ( (T - XTI) - Zero() ) )

end def PSL::Test::Test_Time
     
concurrent class interface PSL::Core::Clock<> :
    # A module that provides access to a wall clock and
    # to other timers, and an ability to delay the caller
    # for a given amount of time, or until a specified time
    # in the future.
    
    def Create() -> Clock
	# Create a clock, which corresponds to the current wall clock time

    def Create(Now : Time) -> Clock
	# Create a clock, where Now() returns the given time when created

    def Now(locked C : Clock) -> Time
	# Return current reading of clock

    def Now() -> Time
	# Return current reading of wall clock

    queued def Delay(C : Clock; Until : Time)
	# Wait until the given clock reads >= Until

    queued def Delay(Until : Time)
	# Wait until the wall clock reads >= Until

    queued def Delay(C : Clock; For : Time::Time_Interval)
	# Wait until the given clock reads "Now(C) + For"

    queued def Delay(For : Time::Time_Interval)
	# Wait until the wall clock reads "Now() + For"

    def Set(locked var C : Clock; To : Time)
	# Update clock so it reads the given time
end interface PSL::Core::Clock

concurrent class PSL::Core::Clock :

    var Timer_Delta : Time::Time_Interval
	# Amount to be subtracted from Current Time
	# to produce value of timer

    def Current_Time(locked C : Clock) -> Time 
      is import(#clock_current_time)
	# NOTE: Clock parameter is ignored; returns wall clock

    def Wait_For_Delay(queued C : Clock; Until : Time)
      is import(#clock_delay)
	# Wait until the wall clock reads >= Until
	# NOTE: Clock parameter is ignored

  exports :
    def Create() -> Clock :
	# Create a clock, which corresponds to the current wall clock time
	# Println("Create Clock with Delta 0.0")
	return (Timer_Delta => 0.0)
    end def Create

    def Create(Now : Time) -> Result : Clock :
	# Create a clock, where Now() returns the given time when created
	const Delta = Current_Time(Create()) - Now
	# Println("Create Clock with Delta = " | Delta)
	return (Timer_Delta => Current_Time(Create()) - Now)
    end def Create

    def Now(locked C : Clock) -> Result : Time :
	# Return current reading of clock
	Result = Current_Time(C) - C.Timer_Delta
	# Println("Now returning " | Result-Zero())
    end def Now

    def Now() -> Result : Time :
	# Return current reading of wall clock
        var C : Clock = Create()
	Result = Current_Time(C)
	# Println("Now returning " | Result-Zero())
    end def Now

    queued def Delay(C : Clock; Until : Time) :
	# Wait until the given clock reads >= Until
	Wait_For_Delay(C, Until + C.Timer_Delta)
    end def Delay

    queued def Delay(Until : Time) :
	# Wait until the wall clock reads >= Until
        var C : Clock = Create()
	Wait_For_Delay(C, Until)
    end def Delay

    queued def Delay(C : Clock; For : Time::Time_Interval) :
	# Wait until the given clock reads "Now(C) + For"
	# NOTE: Timer_Delta is irrelevant for this one
	# Println("Wait for delay of " | For)
	const Time_Now = Current_Time(C)
	const Until = Time_Now + For
	# Println("Time_Now + " | For | " - Time_Now = " | Until - Time_Now)
	Wait_For_Delay(C, Until)
    end def Delay

    queued def Delay(For : Time::Time_Interval) :
	# Wait until the wall clock reads "Now() + For"
	# NOTE: Timer_Delta is irrelevant for this one
	# Println("Wait for delay of " | For)
        var C : Clock = Create()
	const Time_Now = Current_Time(C)
	const Until = Time_Now + For
	# Println("Time_Now + " | For | " - Time_Now = " | Until - Time_Now)
	Wait_For_Delay(C, Until)
    end def Delay

    def Set(locked var C : Clock; To : Time) :
	# Update clock so it reads the given time
	C.Timer_Delta = Current_Time(C) - To
    end def Set

end class PSL::Core::Clock

def PSL::Test::Test_Clock() :
    var C = Clock::Create()
    const Start = C.Now()
    var Ran = Random::Start (Round_To_Int (Seconds_Since_1970 (Clock::Now())))
    Println("First random number = " | Next(Ran));
    Println("Second random number = " | Next(Ran));
    Println("Third random number = " | Next(Ran));

  then :
    Println("About to delay 4.0 seconds")
    C.Delay(4.0)
    Println("Done with delay of 4.0")
  ||
    Println("About to delay 2.0 seconds")
    C.Delay(2.0)
    Println("Done with delay of 2.0")
||    # check that indent of 0 works
    for I in 1..7 :
	Println("In loop delaying for 1.0")
	C.Delay(1.0)
	Println("Done with delay " | I | " of 1.0")
    end loop
then : # check that indent of 0 works
    Println("Elapsed time: " | (C.Now() - Start))
end def PSL::Test::Test_Clock

concurrent class interface PSL::Core::Atomic<Content_Type is Comparable<>> :
    def Create(Initial_Val : Content_Type) -> Atomic
      # Create an atomic object with given initial value

    def Set_Value(locked var A : Atomic; Val : Content_Type)
      # Atomically set the new value
      # TBD: Use atomic hardware instructions eventually

    def Value(locked A : Atomic) -> Content_Type
      # Return the current value
      # TBD: Use atomic hardware instructions eventually

    def Test_And_Set(locked var A : Atomic; New_Val : Content_Type) ->
      Content_Type
      # Set Value(A) to New_Val; Return the prior value.

    def Compare_And_Swap(locked var A : Atomic;
      Expected_Val, New_Val : Content_Type) -> Content_Type
      # If Value(A) == Expected_Val, then set Value(A) to New_Val.
      # In any case, return the prior value.
end interface PSL::Core::Atomic

concurrent class PSL::Core::Atomic :
    var Value : Content_Type
  exports :
    def Create(Initial_Val : Content_Type) -> Atomic :
      # Create an atomic object with given initial value
	return (Value => Initial_Val)
    end def Create

    def Set_Value(locked var A : Atomic; Val : Content_Type) :
      # Atomically set the new value
      # TBD: Use atomic hardware instructions eventually
	A.Value = Val
    end def Set_Value

    def Value(locked A : Atomic) -> Content_Type :
      # Return the current value
      # TBD: Use atomic hardware instructions eventually
	return A.Value
    end def Value

    def Test_And_Set(locked var A : Atomic; New_Val : Content_Type) ->
      Result : Content_Type :
      # Set Value(A) to New_Val; Return the prior value.
	Result = A.Value
	A.Value = New_Val
    end def Test_And_Set

    def Compare_And_Swap(locked var A : Atomic;
      Expected_Val, New_Val : Content_Type) -> Result : Content_Type :
      # If Value(A) == Expected_Val, then set Value(A) to New_Val.
      # In any case, return the prior value.
	Result = A.Value
	if Result == Expected_Val :
	    A.Value = New_Val
	end if
    end def Compare_And_Swap
end class PSL::Core::Atomic

def PSL::Test::Test_Atomic(X, Y : Univ_Integer) :
    type Atomic_Univ is Atomic<Univ_Integer>
    var AX = Atomic_Univ::Create(X)

    Println("Value(X) = " | Value(AX))

    Set_Value(AX, X+1)
    Println("After Set_Value(" | X+1 | "), Value(X) = " | Value(AX))

    const Z = Test_And_Set(AX, Y)
    Println("Test_And_Set(X, " | Y | ") = " | Z)

    const CAS = Compare_And_Swap(AX, Expected_Val => X-1, New_Val => Y-1)
    Println("Compare_And_Swap(X, " | X-1 | ", " | Y-1 | 
      ") = " | CAS | ", Value(X) = " | Value(AX))

    const CAS2 = Compare_And_Swap(AX, Y, Y-2)
    Println("Compare_And_Swap(X, " | Y | ", " | Y-2 | 
      ") = " | CAS2 | ", Value(X) = " | Value(AX))

end def PSL::Test::Test_Atomic

class interface PSL::Core::Modular<Modulus : Univ_Integer> :
  # This is an unsigned type whose arithmetic is "modulo" the modulus
    defop "from_univ"(Lit : Univ_Integer) -> Modular 
      is import(#integer_from_univ)

    defop "to_univ"(Val : Modular) -> Univ_Integer 
      is import(#integer_to_univ)

    defop "+"(Right : Modular) -> Modular
      is import(#identity)

    defop "-"(Right : Modular) -> Modular

    defop "abs"(Right : Modular) -> Modular
      is import(#identity)

    defop "magnitude"(Modular) -> Modular is "abs"

    defop "+"(Left, Right : Modular) -> Result : Modular

    defop "-"(Left, Right : Modular) -> Result : Modular

    defop "*"(Left, Right : Modular) -> Result : Modular

    defop "/"(Left, Right : Modular) -> Result : Modular
      is import("/")

    defop "mod"(Left, Right : Modular) -> Modular
      is import("mod")

    defop "rem"(Left, Right : Modular) -> Modular
      is import("rem")

    defop "**"(Left : Modular; Right : Univ_Integer) -> Result : Modular

    defop "+="(var Left : Modular; Right : Modular)

    defop "-="(var Left : Modular; Right : Modular)

    defop "*="(var Left : Modular; Right : Modular)

    defop "/="(var Left : Modular; Right : Modular) 
      is import("/=")

    defop "**="(var Left : Modular; Right : Univ_Integer)

    defop "=?"(Left, Right : Modular) -> Ordering
      is import("=?")

    defop ">>"(Modular; Modular) -> Modular is import(">>")

    defop "<<"(Modular; Modular) -> Modular is import("<<")

    defop "and"(Left, Right : Modular) -> Modular is import(#bit_and)
    defop "or"(Left, Right : Modular) -> Modular
    defop "xor"(Left, Right : Modular) -> Modular
    defop "not"(M : Modular) -> Modular

    def Min(Left, Right : optional Modular) -> optional Modular
      is import(#min)
    def Max(Left, Right : optional Modular) -> optional Modular
      is import(#max)

    def Hash(Val : Modular) -> Univ_Integer
      is import(#identity)

    def Print(X : Modular) is import(#print_int)

    def To_String(Val : Modular) -> Univ_String
      is import(#to_string_int)

    def From_String(Str : Univ_String) -> optional Modular

    def First() -> Modular

    def Last() -> Modular

    defop "[..]"()->Countable_Range<Modular> is in Countable_Range<Modular>

    defop ".."(Left, Right : Modular) -> Countable_Set<Modular>
      is in Countable_Set<Modular>
    defop "<.."(Left, Right : Modular) -> Countable_Set<Modular>
      is in Countable_Set<Modular>
    defop "..<"(Left, Right : Modular) -> Countable_Set<Modular>
      is in Countable_Set<Modular>
    defop "<..<"(Left, Right : Modular) -> Countable_Set<Modular>
      is in Countable_Set<Modular>
    defop "|"(Left, Right : Modular) -> Countable_Set<Modular>
      is in Countable_Set<Modular>
  implements for Countable
    # These operations are needed so Modular satifies
    # requirements of "Countable" interface, but these
    # operations are not directly callable (if they were callable,
    # we would have ambiguity when adding a Modular to an int-literal).

    defop "+"(Left : Modular; Right : Univ_Integer) -> Result : Modular

    defop "+"(Left : Univ_Integer; Right : Modular) -> Result : Modular

    defop "-"(Left : Modular; Right : Univ_Integer) -> Result : Modular

    defop "-"(Left, Right : Modular) -> Result : Univ_Integer

end interface PSL::Core::Modular

class PSL::Core::Modular :
    const Content : Univ_Integer;  # So this ends up as a wrapper

    def Bit_Or(Left, Right : Univ_Integer) -> Univ_Integer 
      is import(#bit_or)

    def Bit_Xor(Left, Right : Univ_Integer) -> Univ_Integer 
      is import(#bit_xor)
  exports :
    defop "-"(Right : Modular) -> Modular :
	return (Content => Modulus - Right.Content)
    end defop "-"

    defop "+"(Left, Right : Modular) -> Result : Modular :
	return (Content => (Left.Content + Right.Content) mod Modulus)
    end defop "+"

    defop "-"(Left, Right : Modular) -> Result : Modular :
	return (Content => (Left.Content - Right.Content) mod Modulus)
    end defop "-"

    defop "*"(Left, Right : Modular) -> Result : Modular :
	return (Content => (Left.Content * Right.Content) mod Modulus)
    end defop "*"

    defop "**"(Left : Modular; Right : Univ_Integer) -> Result : Modular :
	return (Content => (Left.Content ** Right) mod Modulus)
    end defop "**"

    defop "+="(var Left : Modular; Right : Modular) :
	Left = (Content => (Left.Content + Right.Content) mod Modulus)
    end defop "+="

    defop "-="(var Left : Modular; Right : Modular) :
	Left = (Content => (Left.Content - Right.Content) mod Modulus)
    end defop "-="

    defop "*="(var Left : Modular; Right : Modular) :
	Left = (Content => (Left.Content * Right.Content) mod Modulus)
    end defop "*="

    defop "**="(var Left : Modular; Right : Univ_Integer) :
	Left = (Content => (Left.Content ** Right) mod Modulus)
    end defop "**="

    def From_String(Str : Univ_String) -> optional Modular :
	const Val : Univ_Integer = From_String(Str)
	if Val is null or else Val in 0..<Modulus :
	    return (Content => Val)
	else :
	    return null
	end if
    end def From_String

    defop "or"(Left, Right : Modular) -> Modular :
	return (Content => Bit_Or(Left.Content, Right.Content) mod Modulus)
    end defop "or"

    defop "xor"(Left, Right : Modular) -> Modular :
	return (Content => Bit_Xor(Left.Content, Right.Content) mod Modulus)
    end defop "xor"

    defop "not"(M : Modular) -> Modular :
	return (Content => Modulus - 1 - M.Content)
    end defop "not"

    def First() -> Modular :
	return (Content => 0)
    end def First

    def Last() -> Modular :
	return (Content => Modulus-1)
    end def Last

  # implements
  # for Countable
    # These operations are needed so Modular satifies
    # requirements of "Countable" interface, but these
    # operations are not directly callable (if they were callable,
    # we would have ambiguity when adding an Modular to an int-literal).

    defop "+"(Left : Modular; Right : Univ_Integer) -> Result : Modular :
	return (Content => (Left.Content + Right) mod Modulus)
    end defop "+"

    defop "+"(Left : Univ_Integer; Right : Modular) -> Result : Modular :
	return (Content => (Left + Right.Content) mod Modulus)
    end defop "+"

    defop "-"(Left : Modular; Right : Univ_Integer) -> Result : Modular :
	return (Content => (Left.Content - Right) mod Modulus)
    end defop "-"

    defop "-"(Left, Right : Modular) -> Result : Univ_Integer :
	return (Left.Content - Right.Content) mod Modulus
    end defop "-"
end class PSL::Core::Modular
    
def PSL::Test::Test_Modular() :
    type Mod16 is Modular<16>

    Println("Testing mod 16")
    Println("0xA + 0xA = " | Mod16::0xA + Mod16::0xA)
    Println("0xA or 0x1 = " | (Mod16::0xA or Mod16::0x1))
    Println("0xA xor 0x2 = " | (Mod16::0xA xor Mod16::0x2))
    Println("not 0xA = " | (not Mod16::0xA))
end def PSL::Test::Test_Modular

def Test_Enum() :
    PSL::Test::Test_Enum()
end def Test_Enum

class interface PSL::Containers::Discrete_Ordered_Set<Element_Type is Comparable<>> :
  # A set over individual but ordered elements
    defop "[]"() -> Discrete_Ordered_Set

    def Singleton(Elem : Element_Type) -> Discrete_Ordered_Set

    defop "|"(Left, Right : Element_Type) -> Discrete_Ordered_Set
    defop "|"(Left : Discrete_Ordered_Set; Right : Element_Type) 
      -> Discrete_Ordered_Set
    defop "|"(Left : Element_Type; Right : Discrete_Ordered_Set) 
      -> Discrete_Ordered_Set
    defop "|"(Left : Discrete_Ordered_Set; Right : Discrete_Ordered_Set) 
      -> Discrete_Ordered_Set

    defop "|="(var Left : Discrete_Ordered_Set; Right : Element_Type)
    defop "|="(var Left : Discrete_Ordered_Set; Right : Discrete_Ordered_Set)

    defop "<|="(var Left : Discrete_Ordered_Set; var Right : optional Element_Type)
        # Move element into set, leaving Right null afterward.

    defop "<|="(var Left : Discrete_Ordered_Set; var Right : Discrete_Ordered_Set)
	# Move all elements of Right into Left, leaving Right empty.

    defop "-"(Left, Right : Discrete_Ordered_Set) -> Discrete_Ordered_Set
      # Set difference
    defop "-"(Left : Discrete_Ordered_Set; Right : Element_Type) 
      -> Discrete_Ordered_Set
      # Remove one element
    defop "-="(var S : Discrete_Ordered_Set; Elem : Element_Type)
      # Remove the given element from the set, if present
    defop "-="(var Left : Discrete_Ordered_Set; Right : Discrete_Ordered_Set)
      # Remove all elements of Right from Left, if present

    defop "or"(Left : Discrete_Ordered_Set; Right : Discrete_Ordered_Set) 
      -> Discrete_Ordered_Set is "|"   # union
    defop "or="(var Left : Discrete_Ordered_Set; Right : Discrete_Ordered_Set)
      is "|="

    defop "+"(Left : Discrete_Ordered_Set; Right : Discrete_Ordered_Set) 
      -> Discrete_Ordered_Set is "|"   # Union
    defop "+="(var Left : Discrete_Ordered_Set; Right : Discrete_Ordered_Set)
      is "|="
    defop "+="(var Left : Discrete_Ordered_Set; Right : Element_Type) is "|="
   
    defop "and"(Left, Right : Discrete_Ordered_Set) -> Discrete_Ordered_Set
	# Intersection
    defop "and="(var Left : Discrete_Ordered_Set; Right : Discrete_Ordered_Set)

    defop "xor"(Left, Right : Discrete_Ordered_Set) -> Discrete_Ordered_Set
	# Symmetric difference
    defop "xor="(var Left : Discrete_Ordered_Set; Right : Discrete_Ordered_Set)

    defop "in"(Left : Element_Type; Right : Discrete_Ordered_Set) -> Boolean

    defop "=?"(Left, Right : Discrete_Ordered_Set) -> Ordering
	# Return #equal if Left and Right have the same elements
	# Return #less if Left is a proper subset of Right
	# Return #greater if Left is a proper superset of Right
	# Return #unordered otherwise

    def Count(S : Discrete_Ordered_Set) -> Univ_Integer

    defop "magnitude"(Discrete_Ordered_Set) -> Univ_Integer is Count

    def Is_Empty(S : Discrete_Ordered_Set) -> Boolean

    def First(S : Discrete_Ordered_Set) -> optional Element_Type
    def Last(S : Discrete_Ordered_Set) -> optional Element_Type

    def Remove_First(var S : Discrete_Ordered_Set) -> optional Element_Type
	# Remove first element of set (lowest value)

    def Remove_Last(var S : Discrete_Ordered_Set) -> optional Element_Type
	# Remove last element of set (highest value)

    def Remove_Any(var S : Discrete_Ordered_Set) -> optional Element_Type
	# Remove an arbitrary element of set

end interface PSL::Containers::Discrete_Ordered_Set

class PSL::Containers::Discrete_Ordered_Set :

    var Items : optional AA_Tree<Element_Type>

  exports :
    defop "[]"() -> Discrete_Ordered_Set :
        return (Items => [])
    end defop "[]"

    def Singleton(Elem : Element_Type) -> Result : Discrete_Ordered_Set :
	Result = []
	Result.Items |= Elem
    end def Singleton

    defop "|"(Left, Right : Element_Type) -> Result : Discrete_Ordered_Set :
        Result = [];
        Result.Items |= Left;
        Result.Items |= Right;
    end defop "|"

    defop "|"(Left : Discrete_Ordered_Set; Right : Element_Type) 
      -> Result : Discrete_Ordered_Set :
        Result = Left
        Result.Items |= Right
    end defop "|"

    defop "|"(Left : Element_Type; Right : Discrete_Ordered_Set) 
      -> Discrete_Ordered_Set :
        return Right | Left
    end defop "|"

    defop "|"(Left : Discrete_Ordered_Set; Right : Discrete_Ordered_Set) 
      -> Result : Discrete_Ordered_Set :
        Result = Left
        Result |= Right
    end defop "|"

    defop "|="(var Left : Discrete_Ordered_Set; Right : Element_Type) :
        Left.Items |= Right;
    end defop "|="

    defop "<|="(var Left : Discrete_Ordered_Set; var Right : optional Element_Type)
      :
        # Move element into set, leaving Right null afterward.
	Left.Items <|= Right
    end defop "<|="

    defop "<|="(var Left : Discrete_Ordered_Set; var Right : Discrete_Ordered_Set)
      :
	# Move all elements of Right into Left, leaving Right empty.
        if Count(Left.Items) == 0 :
            Left.Items <== Right.Items
        else :
            # Iterate through the tree
	    loop
		# Extract element from right
		var Elem for Left = Remove_Any(Right.Items)

		if Elem is null :
		    return   # All done
		end if

                Left.Items <|= Elem

            end loop
        end if
    end defop "<|="

    defop "|="(var Left : Discrete_Ordered_Set; Right : Discrete_Ordered_Set) :
	# Pass the buck to the "<|=" operation
	var Right_Copy for Left = Right
	Left <|= Right_Copy
    end defop "|="

    defop "-"(Left, Right : Discrete_Ordered_Set) 
      -> Result : Discrete_Ordered_Set :
      # Set difference
	Result = Left
	Result -= Right
    end defop "-"

    defop "-"(Left : Discrete_Ordered_Set; Right : Element_Type)
      -> Result : Discrete_Ordered_Set :
      # Remove one element
        Result = Left
        Result -= Right
    end defop "-"
        
    defop "-="(var S : Discrete_Ordered_Set; Elem : Element_Type) :
      # Remove the given element from the set, if present
	Delete(S.Items, Elem);
    end defop "-="

    defop "-="(var Left : Discrete_Ordered_Set; Right : Discrete_Ordered_Set) :
      # Remove all elements of Right from Left, if present
	for Elem in Right :
	    Left -= Elem
	end loop
    end defop "-="

    defop "and"(Left, Right : Discrete_Ordered_Set)
      -> Result : Discrete_Ordered_Set :
	# Intersection
	Result = []
	for Elem in Right :
	    if Elem in Left :
		Result += Elem
	    end if
	end loop
    end defop "and"

    defop "and="(var Left : Discrete_Ordered_Set; Right : Discrete_Ordered_Set) :
	# Intersection
	for Elem in Left :
	    if Elem not in Right :
		Left -= Elem
	    end if
	end loop
    end defop "and="

    defop "xor"(Left, Right : Discrete_Ordered_Set) 
      -> Result : Discrete_Ordered_Set :
	# Symmetric difference
	Result = Left
	Result xor= Right
    end defop "xor"

    defop "xor="(var Left : Discrete_Ordered_Set; Right : Discrete_Ordered_Set) :
	# Symmetric difference
	# Want elements that are only in one of the two inputs
	for Elem in Right :
	    if Elem in Left :
		Left -= Elem
	    else :
		Left += Elem
	    end if
	end loop
    end defop "xor="

    defop "in"(Left : Element_Type; Right : Discrete_Ordered_Set) -> Boolean :
        return Overlapping(Right.Items, Left) not null
    end defop "in"

    defop "=?"(Left, Right : Discrete_Ordered_Set) -> Ordering :
	# Return #equal if Left and Right have the same elements
	# Return #less if Left is a proper subset of Right
	# Return #greater if Left is a proper superset of Right
	# Return #unordered otherwise
        var Overlaps = 0
        var Missing = 0
        for Elem in Left :
            if Elem not in Right :
                Missing += 1
            else :
                Overlaps += 1
            end if
        end loop

        if Missing > 0 :
            # Can't be equal, but Left might be a proper superset
            if Overlaps < Count(Right) :
                return #unordered
            else :
                # Left is a superset
                return #greater
            end if
        else :
            # Might be equal or Left might be a proper subset
            if Overlaps < Count(Right) :
                # Left is a proper subset of Right
                return #less
            else :
                return #equal
            end if
        end if
    end defop "=?"

    def Count(S : Discrete_Ordered_Set) -> Result : Univ_Integer :
        # Return count of items in set
        return Count(S.Items);
    end def Count

    def Is_Empty(S : Discrete_Ordered_Set) -> Boolean :
	return Is_Empty(S.Items)
    end def Is_Empty

    def First(S : Discrete_Ordered_Set) -> optional Element_Type :
        return First(S.Items);
    end def First

    def Last(S : Discrete_Ordered_Set) -> optional Element_Type :
        return Last(S.Items);
    end def Last

    def Remove_First(var S : Discrete_Ordered_Set) 
      -> Result : optional Element_Type :
        # Return first element of set
        return Remove_First(S.Items)
    end def Remove_First

    def Remove_Last(var S : Discrete_Ordered_Set) 
      -> Result : optional Element_Type :
        # Remove last element of set
        return Remove_Last(S.Items);
    end def Remove_Last

    def Remove_Any(var S : Discrete_Ordered_Set) -> optional Element_Type :
        # Remove any element of set
        return Remove_Any(S.Items);
    end def Remove_Any

end class PSL::Containers::Discrete_Ordered_Set

class interface PSL::Core::Enum_With_Rep
  <Rep_Type is Imageable<>; Rep_Map : Map<Univ_Enumeration, Rep_Type>> :
  # An enumeration type specified using a map from literal to value
  # of an underlying representation type.
    defop "from_univ"(Univ : Univ_Enumeration) 
      {> (for some [Lit => Val] of Rep_Map => Lit == Univ) <}
      -> Enum_With_Rep
    defop "to_univ"(Val : optional Enum_With_Rep)
      -> Result : optional Univ_Enumeration
      {> Result is null or else
        (for some [Lit => V] of Rep_Map => Lit == Result) <}

    # Functions to convert from/to rep
    def From_Rep (Rep : optional Rep_Type) -> optional Enum_With_Rep
    def To_Rep (Val : optional Enum_With_Rep) -> optional Rep_Type

    defop "[..]"() -> Discrete_Ordered_Set<Enum_With_Rep>

    defop "=?"(Left, Right : Enum_With_Rep) -> Ordering

    # Functions for Imageable
    def To_String(Val : Enum_With_Rep) -> Univ_String
    def From_String(Str : Univ_String) -> optional Enum_With_Rep

    def Hash(Val : Enum_With_Rep) -> Univ_Integer

    def Min(Left, Right : optional Enum_With_Rep) -> optional Enum_With_Rep
    def Max(Left, Right : optional Enum_With_Rep) -> optional Enum_With_Rep

end interface PSL::Core::Enum_With_Rep

class PSL::Core::Enum_With_Rep :
    const Rep : Rep_Type
  exports :
    defop "from_univ"(Univ : Univ_Enumeration) 
      -> Enum_With_Rep :
        return (Rep => Rep_Map[Univ]);
    end defop "from_univ"

    defop "to_univ"(Val : optional Enum_With_Rep) 
      -> Result : optional Univ_Enumeration :
	if Val is null :
	    return null
	else :
            for each [Lit => Rep] of Rep_Map :
                if Val.Rep == Rep :
                    return Lit
                end if
            end loop
	    return null
	end if
    end defop "to_univ"

    def From_Rep (Rep : optional Rep_Type) -> optional Enum_With_Rep :
        if Rep is null :
            return null
        else :
            return (Rep => Rep)
        end if
    end def From_Rep

    def To_Rep (Val : optional Enum_With_Rep) -> optional Rep_Type :
        if Val is null :
            return null
        else :
            return Val.Rep
        end if
    end def To_Rep

    # Functions for Imageable
    def To_String(Val : Enum_With_Rep) -> Univ_String :
        const Enum : optional Univ_Enumeration = [[Val]];
        if Enum not null :
            # use enum image
            return Univ_Enumeration::To_String(Enum);
        else :
            # use rep-type To_String
            return Rep_Type::To_String(Val.Rep)
        end if
    end def To_String

    def From_String(Str : Univ_String) -> optional Enum_With_Rep :
        if Str[1] == '#' :
            # Presume is an enum
            return Univ_Enumeration::From_String(Str)
        else :
            # Use rep-type From_String
            return (Rep => Rep_Type::From_String(Str))
        end if
    end def From_String

    defop "[..]"() -> Discrete_Ordered_Set<Enum_With_Rep> :
        return [From_Rep(Rep) for each Rep of Rep_Map]
    end defop "[..]";

    defop "=?"(Left, Right : Enum_With_Rep) -> Ordering :
        return Left.Rep =? Right.Rep
    end defop "=?"

    def Hash(Val : Enum_With_Rep) -> Univ_Integer :
        return Hash (Val.Rep)
    end def Hash

    def Min(Left, Right : optional Enum_With_Rep) -> optional Enum_With_Rep :
        if Left is null :
            return Right
        elsif Right is null or else Right.Rep > Left.Rep :
            return Left
        else :
            return Right
        end if
    end def Min

    def Max(Left, Right : optional Enum_With_Rep) -> optional Enum_With_Rep :
        if Left is null :
            return Right
        elsif Right is null or else Right.Rep < Left.Rep :
            return Left
        else :
            return Right
        end if
    end def Max

end class PSL::Core::Enum_With_Rep

def PSL::Test::Test_Enum_With_Rep() :
    type Color is Enum_With_Rep<Univ_Integer,
               [#red : 1, #green : 3, #blue : 5]>;

    type Day_Of_Week is
       Enum_With_Rep<Modular<2**7>,
          [#Monday : 1<<1, #Tuesday : 1<<2, #Wednesday : 1<<3,
           #Thursday : 1<<4, #Friday : 1<<5,
            #Saturday : 1<<6, #Sunday : 1<<7]>

    for C in Color forward :
        Println("Next color = " | C)
    end loop

    for C2 in Color reverse :
        Println("Prev color = " | C2)
    end loop

    for D3 in Day_Of_Week :
        Println("Random day of week = " | D3)
    end loop

    {> #Monday in Day_Of_Week <}

    for D4 in Day_Of_Week reverse :
        Println("Prev day of week = " | D4)
    end loop

    for D5 in Day_Of_Week forward :
        Println("To_Rep(" | D5 | ") = " | To_Rep(D5))
    end loop

    for I in 1..10 forward :
        Println("Day_Of_Week::From_Rep(" | I | ") = " |
          Day_Of_Week::From_Rep(I))
    end loop

end def PSL::Test::Test_Enum_With_Rep

concurrent class interface PSL::Core::IO<>:
  # Locked versions of the Print, Println and Readln operations
    def Get_IO() -> IO  # Get handle on IO subsystem

    def Println(locked var IO; Univ_String)
    def Print(locked var IO; Univ_String)
    def Print(locked var IO; Univ_Character)
    def Print(locked var IO; Univ_Integer)
    def Print(locked var IO; Univ_Real)
    def Print(locked var IO; Univ_Enumeration) 

    def Readln(locked var IO) -> optional Univ_String
end interface PSL::Core::IO

concurrent class PSL::Core::IO:
  exports :
    def Get_IO() -> IO:
        return ()

    def Println(locked var IO; Univ_String):
        Println (Univ_String)

    def Print(locked var IO; Univ_String):
        Print (Univ_String)

    def Print(locked var IO; Univ_Character):
        Print (Univ_Character)

    def Print(locked var IO; Univ_Integer):
        Print (Univ_Integer)

    def Print(locked var IO; Univ_Real):
        Print (Univ_Real)

    def Print(locked var IO; Univ_Enumeration):
        Print (Univ_Enumeration)

    def Readln(locked var IO) -> optional Univ_String:
        return Readln()

end class PSL::Core::IO

def PSL::Test::Test_IO (X, Y : Univ_String; Z : Univ_Integer):
    var IO = IO::Get_IO()
    const Pi = 3.141592653589793
    const True = Boolean::#true

    IO.Print ("Pi = ")
    IO.Print (Pi)
    IO.Print (", True prints as ")
    IO.Print (True)
    IO.Print ('\n')
    IO.Print ("X = \"" | X | '"')
    IO.Print (", Y = \"" | Y | '"')
    IO.Println (", Z = " | Z )
    IO.Print ("Give me somethin: ")
    var Input = IO.Readln()
    IO.Println ("You gave me: \"" | Input | '"')

end def PSL::Test::Test_IO
    
import PSL::Test::Test_IO

def Test_IO (X, Y : Univ_String; Z : Univ_Integer):
    PSL::Test::Test_IO (X, Y, Z)
end def Test_IO

class interface PSL::Core::Output_Stream<> :
  # Output stream interface; all operations
  # expressed in terms of Univ_String print and close
    abstract def Print(var Output_Stream; Univ_String)
    abstract def Close(var optional Output_Stream)

    def Println(var Output_Stream+; Univ_String)

    def Print(var Output_Stream+; Univ_Character)
    def Print(var Output_Stream+; Univ_Integer)
    def Print(var Output_Stream+; Univ_Real)
    def Print(var Output_Stream+; Univ_Enumeration) 
end interface PSL::Core::Output_Stream

class PSL::Core::Output_Stream :
  # Output stream interface; all operations
  # expressed in terms of Univ_String print
  exports :
    def Println(var Output_Stream+; Univ_String):
        Output_Stream.Print(Univ_String)
        Output_Stream.Print("\n")

    def Print(var Output_Stream+; Univ_Character):
        Output_Stream.Print(To_String(Univ_Character))

    def Print(var Output_Stream+; Univ_Integer):
        Output_Stream.Print(To_String(Univ_Integer))

    def Print(var Output_Stream+; Univ_Real):
        Output_Stream.Print(To_String(Univ_Real))

    def Print(var Output_Stream+; Univ_Enumeration):
        Output_Stream.Print(To_String(Univ_Enumeration))
end class PSL::Core::Output_Stream

interface PSL::Core::Input_Stream<> :
    def Readln(var Input_Stream) -> optional Univ_String
    def Close(var optional Input_Stream)
end interface PSL::Core::Input_Stream

class interface PSL::Core::File_Output_Stream<> extends Output_Stream<> :
    def Create(var IO; Name : Univ_String) -> optional File_Output_Stream
      is import(#create_output_file)
    def Append(var IO; Name : Univ_String) -> optional File_Output_Stream
      is import(#append_output_file)
    def Close(var optional File_Output_Stream)
      is import(#close_output_file)
    def Print(var File_Output_Stream; Univ_String)
      is import(#print_to_file)
end interface PSL::Core::File_Output_Stream

class PSL::Core::File_Output_Stream :
    const Name : Univ_String
    const Index : Univ_Integer
  exports :
end class PSL::Core::File_Output_Stream

class interface PSL::Core::File_Input_Stream<> extends Input_Stream :
    def Open(var IO; Name : Univ_String) -> optional File_Input_Stream
      is import(#open_input_file)
    def Close(var optional File_Input_Stream)
      is import(#close_input_file)
    def Readln(var File_Input_Stream) -> optional Univ_String
      is import(#read_from_file)
end interface PSL::Core::File_Input_Stream

class PSL::Core::File_Input_Stream :
    const Name : Univ_String;
    const Index : Univ_Integer
  exports :
end class PSL::Core::File_Input_Stream

def PSL::Test::Test_File (Name, X, Y : Univ_String; Z : Univ_Integer):
    var IO = IO::Get_IO()
    var File = File_Output_Stream::Create(IO, Name);
    const Pi = 3.14159265389793
    const True = Boolean::#true

    IO.Println ("Writing to file: " | Name)

    File.Print ("Pi = ")
    Output_Stream::Print (File, Pi)
    File.Print (", True prints as ")
    Print (File, True)
    Print (File, '\n')
    File.Print ("X = \"" | X | '"')
    Println (File, ", Y = \"" | Y | '"')
    IO.Print ("Give me somethin: ")
    var Inp = IO.Readln()
    IO.Println ("You gave me: \"" | Inp | '"')
    File.Println ("User input = \"" | Inp | '"')
    File.Close();

    var Inp_File = File_Input_Stream::Open(IO, Name)
    IO.Println ("Contents of file " | Name)
    loop :
        const Line = Inp_File.Readln()
        if Line is null :
            exit loop
        end if
        IO.Println(Line)
    end loop

    IO.Println("All done")

end def PSL::Test::Test_File
