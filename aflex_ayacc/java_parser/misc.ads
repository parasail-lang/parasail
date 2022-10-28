with Utils.Spellings;
with Utils.Strings;
use Utils.Strings;
with Utils.Hash_Mappings;
with Utils.Messages;
with Lists;
with BE.BE_Messages.Message_Input_Output;
 use BE.BE_Messages.Message_Input_Output;
with java_lexer_dfa;

package Misc is
    use Utils.Spellings;

    -- arg can be input only, output only or both
    -- input output
    type Argkind_enum is (
      None, Input_Obj, Output_Obj, Inout );

    type Param_rec;
    type Param_Ptr is access Param_rec;
      -- Reprsents a parameter - used in parser
    type Param_rec is record
      Param_Type : Utils.Spellings.Spelling;
      Param_Name : Utils.Spellings.Spelling;
      Has_Final  : Boolean := False;
      Next : Param_Ptr := null;
    end record;

     -- Represents an argument in a method
    type Arg_rec is record
      Param_Type : Utils.Spellings.Spelling;
      Param_Name : Utils.Spellings.Spelling;
      Param_Kind : Argkind_enum := None;
    end record;
    type Arg_Ptr is access Arg_rec;
    package Argument_Lists is new Lists(ItemType => Arg_Ptr, Equal => "=");

    type Node_rec;
    type Node_Ptr is access Node_rec;
      -- Represents a spelling /token
    type Node_rec is record
      Str : Utils.Spellings.Spelling;
      Next : Node_Ptr := null;
    end record;

      -- Used in parser for TypeSepcifier rule
    type TypeSpec_rec is record
      Type_Name : Utils.Spellings.Spelling;
      Dims      : Natural;
      Has_Args  : Boolean := False;
     end record;
     type TypeSpec_Ptr is access TypeSpec_rec;

      -- method / procedure
     type Method_rec is record
       Method_Name : Utils.Spellings.Spelling;
       Return_Type : Utils.Spellings.Spelling;
       ParamList   : Argument_Lists.List;
       Class_Name : Utils.Spellings.Spelling;
         -- can be class or interface, belongs to this class
     end record;

     type Method_Ptr is access Method_rec;
     package Method_Lists is new Lists(ItemType => Method_Ptr, Equal => "=");

     package Method_Name_To_MethodInfo is new Utils.Hash_Mappings
        ("Method_Name_To_MethodInfo_Mappings",
        Key_Type => Spelling,
        Hash => Utils.Spellings.Hash_Spelling,
        Element => Method_Ptr);
        -- Mapping from method name to Method
      type Method_Name_To_MethodInfo_Ptr is access
        Method_Name_To_MethodInfo.Mapping;

     -- Class / Module
     type Class_rec is record
       Class_Name : Utils.Spellings.Spelling;
         -- can be class or interface
       Package_Name : Utils.Spellings.Spelling;
         -- class belongs to this package - can be default
--       MethodList   : Method_Lists.List;
       MethodMap    : Method_Name_To_MethodInfo_Ptr;
         -- methods for this class
     end record;
     type Class_Ptr is access Class_rec;
     package Class_Lists is new Lists(ItemType => Class_Ptr, Equal => "=");

      -- Represents the parsing data for the source file
      type File_rec is record
        File_Name    : Utils.Spellings.Spelling;
        Package_Name : Utils.Spellings.Spelling;
          -- if no package name, then default package
        ClassList    : Class_Lists.List;
          -- class => module
      end record;
      type File_Ptr is access File_rec;

      package File_Name_To_FileInfo is new Utils.Hash_Mappings
        ("File_Name_To_FileInfo_Mappings",
        Key_Type => Spelling,
        Hash => Utils.Spellings.Hash_Spelling,
        Element => File_Ptr);
        -- Mapping from source fiilename to File_Info
      type File_Name_To_FileInfo_Ptr is access
        File_Name_To_FileInfo.Mapping;

--    function Arg_Equal (X, Y : TypeSpec_rec) return Boolean;

--    package Arg_Lists is new Lists(ItemType =>TypeSpec_rec,
--      Equal => Arg_Equal);

    function Create_Type_Specifier (
       Name : Utils.Spellings.Spelling;
       Dims : Natural;
       Has_Args : Boolean) return TypeSpec_Ptr;

    function Create_Param (
      Ptype : Utils.Spellings.Spelling;
      Name : Utils.Spellings.Spelling;
      Is_Final : Boolean)
      return Param_Ptr;

     -- Insert parameter into the parameter list
    procedure Insert_Param(List : in out Param_Ptr;
      Block : Param_Ptr);

     -- Insert token /spelling into the list
    procedure Insert_Node(List : in out Node_Ptr;
      Block : Node_Ptr);

      -- Traverse the parameter list
    procedure Traverse_List(List : in Param_Ptr);

     -- Returns _in for Input arguments, _out for output args and
     -- _inout for argument being both input and output type
    function Get_Arg_Type(Filename : Utils.Spellings.Spelling;
       Classname : Utils.Spellings.Spelling;
       Methodname : Utils.Spellings.Spelling;
       Argname : Utils.Spellings.Spelling;
       Srcpos  : Utils.Messages.Source_Position) return String;

    procedure Allocate_Filename_FileInfo_Map ;

     -- Process Messages for the given source file and build the data structures
     -- to generate SAL annotations - will be used by the parser
    procedure Process_File_Messages (
      in_file_name : String;
      File_Info : Messages_Per_File_Ptr);

    -- Print the data for all the source files
    procedure Print_FileInfo_Map ;

    -- Print the File_Info for the given file
    procedure Print_One_FileInfo(Filename : String);

    -- Free the FileInfo_Map
    procedure Free_FileInfo_Map;

    procedure Set_Input_Buffer(Str : Access_Constant_String);

    procedure YY_INPUT(buf : out java_lexer_dfa.unbounded_character_array;
       result : out Integer; max_size: in Integer);

end Misc;

