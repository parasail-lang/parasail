with Std_Output; use Std_Output;
with Utils.Spellings; use Utils.Spellings;
with Lists;

with Utils.Storage_Management.Varying_Pools;
 use Utils.Storage_Management.Varying_Pools;
with Utils.Storage_Management.Subpools;
 use Utils.Storage_Management.Subpools;
  pragma Elaborate_All(Utils.Storage_Management.Subpools);
with Utils.Storage_Management.GC_Pools;
with Utils.Storage_Tracking.Dump_Statistics;

with Utils.Strings;
 use Utils.Strings;
with Utils.Messages;
 use Utils.Messages;
 use Utils.Messages.Message_Seqs;
with Utils.Program_Exit; pragma Elaborate_All(Utils.Program_Exit);
with Utils.Program_Exit.Errors; use Utils.Program_Exit.Errors;

with BE.BE_Messages;
 use BE.BE_Messages;
with BE.Message_Kinds;
 use BE.Message_Kinds;
with BE.BE_Messages.Message_Input_Output;
 with BE.BE_Messages.Message_Input_Output;

with java_lexer_io;
with java_lexer_dfa;
with Text_IO;

package body  Misc is

      -- Remembers where to read from for the next time
      -- simulates file pointer
     read_from : Integer  := 0;
     Source_Contents : Access_Constant_String;
     -- holds the Filename to FileInfo mapping for SAL annotations
     Filename_FileInfo_Map : File_Name_To_FileInfo_Ptr;

     Sal_Pool_Name : aliased constant String := "Sal_Pool";
     Sal_Pool : aliased Subpool(Sal_Pool_Name'Unchecked_Access);

--    function Arg_Equal(X,Y : TypeSpec_rec) return Boolean is
--    begin
--      return Case_Sensitive_Equal(X.Type_Name, Y.Type_Name);
--    end Arg_Equal;

    function Create_Param(
      Ptype : Utils.Spellings.Spelling;
      Name : Utils.Spellings.Spelling;
      Is_Final : Boolean) return Param_Ptr is
      Block : Param_Ptr;
    begin

        Block := new Param_rec;
        Block.Param_Type := Ptype;
        Block.Param_Name := Name;
        Block.Has_Final := Is_Final;
        return Block;

    end Create_Param;

    function Create_Type_Specifier (
       Name : Utils.Spellings.Spelling;
       Dims : Natural;
       Has_Args : Boolean) return TypeSpec_Ptr is
       Ptype : TypeSpec_Ptr;
    begin

        Ptype := new TypeSpec_rec;
        Ptype.Type_Name := Name;
        Ptype.Dims := Dims;
        Ptype.Has_Args := Has_Args;
        return Ptype;
    end Create_Type_Specifier;

    -- Insert the Block in the given list at the end
    procedure Insert_Param(List : in out Param_Ptr;
      Block : Param_Ptr) is
    begin
        if List = null then
          -- add to the front of the list
          Block.Next := List;
          List := Block;
        else
          declare
            -- previous node to the current node
            Prev : Param_Ptr := List;
            -- designates the current node in the list
            Curr : Param_Ptr := List.Next;
           begin
               while Curr /= null loop
                 Prev := Curr;
                 Curr := Curr.Next;
               end loop;
               if Curr = null then
                 -- end of the list
                 Prev.Next := Block;
               end if;
          end;
         end if;
    end Insert_Param;

    procedure Traverse_List (List : in Param_Ptr) is
        Curr : Param_Ptr;
    begin
        Put_Line("Traversing List");
        Curr := List;
        while Curr /= null loop
          Put(To_String(Curr.Param_Type));
          Put(" ");
          Put(To_String(Curr.Param_Name));
          Put_Line(" ");
          Curr := Curr.Next;
        end loop;
       Put_Line(" ");

    end Traverse_List;

      -- Insert a token/spelling in the list at the front
    procedure Insert_Node(List : in out Node_Ptr;
      Block : Node_Ptr) is
    begin
        if List = null then
          -- add to the front of the list
          Block.Next := List;
          List := Block;
        else
          declare
            -- previous node to the current node
            Prev : Node_Ptr := List;
            -- designates the current node in the list
            Curr : Node_Ptr := List.Next;
           begin
               while Curr /= null loop
                 Prev := Curr;
                 Curr := Curr.Next;
               end loop;
               if Curr = null then
                 -- end of the list
                 Prev.Next := Block;
               end if;
          end;
         end if;
    end Insert_Node;

    -- Given the procedure name  e.g. P(I)V, return the procedure
    -- name as P .
    function Remove_Signature (Proc_Name : String) return String is
    begin
        if Contains (Proc_Name, '(') then
            for J in Proc_Name'Range loop
                if Proc_Name (J) = '(' then
                    return Proc_Name (Proc_Name'First .. J - 1);
                end if;
            end loop;
        end if;
        return Proc_Name;
    end Remove_Signature;

    -- Given the class name  e.g. Options$optionModified, return
    -- the inner classname after the $ sign e.g. optionModified
    function Remove_Outer_Name (Class_Name : String) return String is
    begin
        if Contains (Class_Name, '$') then
            for J in Class_Name'Range loop
                if Class_Name (J) = '$' then
                    return Class_Name (J + 1 .. Class_Name'Last);
                end if;
            end loop;
        end if;
        return Class_Name;
    end Remove_Outer_Name;

      -- Find the method for the given class for the specified file
    function Find_Method_For_Class( pFile : File_Ptr;
      Classname : Utils.Spellings.Spelling;
      Methodname : Utils.Spellings.Spelling
      ) return Method_Ptr is
      ClassIter : Class_Lists.Listiter;
      pClass : Class_Ptr;
      pMethod : Method_Ptr;
    begin
        ClassIter :=  Class_Lists.Makelistiter(pFile.ClassList);
        while Class_Lists.More(ClassIter) loop
            Class_Lists.Next(ClassIter, pClass);
            if Case_Sensitive_Equal(pClass.Class_Name, Classname)  then
                -- class found, now find method
               if Method_Name_To_MethodInfo.Is_Present
                 (pClass.MethodMap.all, Methodname) then
                   pMethod := Method_Name_To_MethodInfo.Lookup
                     (pClass.MethodMap.all, Methodname) ;
                    return pMethod;
                else
                -- method not found
                Put_Line_Err("Didn't find method " &
                  To_String(Methodname)
                  & " for class " & To_String(Classname));
                return null;
                end if;
             end if;
         end loop;
         -- class not found
        Put_Line_Err("Didn't find class " & To_String(Classname)
          & " for file " & To_String(pFile.File_Name) );
         return null;
    end Find_Method_For_Class;

      -- Find the given class for the given file
    function Find_Class(pFile : File_Ptr;
      Classname : Utils.Spellings.Spelling
      ) return Class_Ptr is
      pClass : Class_Ptr := null;
      ClassIter : Class_Lists.Listiter;
    begin
        ClassIter :=  Class_Lists.Makelistiter(pFile.ClassList);
        while Class_Lists.More(ClassIter) loop
            Class_Lists.Next(ClassIter, pClass);
                if Case_Sensitive_Equal(pClass.Class_Name, Classname)  then
                    return pClass;
                end if;
        end loop;
        Put_Line_Err("Didn't find class " & To_String(Classname)
          & " for file " & To_String(pFile.File_Name) );
        return null;
    end Find_Class;

    -- Given the argument name(e.g. BodyRef) and the parametr list,
    -- return True if argument name (e.g. BodyRef.) has an output
    -- annotation.
    function Get_Method_Arg_Ref(Arg_Name : Utils.Spellings.Spelling;
       pList : Argument_Lists.List) return Boolean is
      ParamIter : Argument_Lists.Listiter;
      pArg      : Arg_Ptr;
      Arg_Ref_Name : constant String := To_String(Arg_Name) & "." ;
    begin
      -- If there is an argument which is modified inside
      -- the function, look for output_annotation for argname.
      -- public void commonName(Body bodyRef) {
      --  bodyRef.name = "Mercury"; }
      -- bodyRef is modified inside the function, so there
      -- will be output annotation for bodyRef.name, so look for
      -- bodyRef. when trying to get the argument type for bodyRef
      ParamIter := Argument_Lists.Makelistiter(pList);
      while Argument_Lists.More(ParamIter) loop
          Argument_Lists.Next(ParamIter, pArg);
          if Is_Prefix(Arg_Ref_Name, To_String(pArg.Param_Name))  then
               return True;
          end if;
      end loop;
      return False;  -- not in list
    end Get_Method_Arg_Ref;

    -- Given the argument name and the parametr list, return the
    -- pointer to the Arg.
    function Get_Method_Arg(Arg_Name : Utils.Spellings.Spelling;
       pList : Argument_Lists.List) return Arg_Ptr is
      ParamIter : Argument_Lists.Listiter;
      pArg      : Arg_Ptr;
    begin
      ParamIter := Argument_Lists.Makelistiter(pList);
      while Argument_Lists.More(ParamIter) loop
          Argument_Lists.Next(ParamIter, pArg);
          if Case_Sensitive_Equal(pArg.Param_Name, Arg_Name)  then
               return pArg;
          end if;
      end loop;
      return null;  -- not in list
    end Get_Method_Arg;

    -- Given the source filename, classname, methodname and the
    -- argument name :-
    -- Returns _in for Input arguments, _out for output args
    -- and _inout for argument being both input && output
    function Get_Arg_Type(Filename : Utils.Spellings.Spelling;
      Classname : Utils.Spellings.Spelling;
      Methodname : Utils.Spellings.Spelling;
      Argname : Utils.Spellings.Spelling;
      Srcpos : Utils.Messages.Source_Position ) return String is
      pFile : File_Ptr;
      pClass :  Class_Ptr;
      pMethod :  Method_Ptr;
      pArg    :  Arg_Ptr;
      ClassIter : Class_Lists.Listiter;
      Argkind   : Argkind_enum;
    begin

        -- Validate arguments from the parser
        if Filename = No_Spelling then
            Put_Line_Err("Filename missing "  );
            return (" ");
        elsif Classname = No_Spelling  then
            Put_Line_Err("Class name missing "  );
            return (" ");
        elsif Methodname = No_Spelling then
            Put_Line_Err("Method name missing "  );
            return (" ");
        elsif Argname = No_Spelling then
            Put_Line_Err("Argument name missing "  );
            return (" ");
        end if;

if False then
        Put_Line("Get_Arg_Type " & To_String(Filename) & " "
         & To_String(Classname) & " " & To_String(Methodname) &
         " " & To_String(Argname) & " " & Srcpos_Image(Srcpos));
end if;

        if Filename_FileInfo_Map /= null and then
          File_Name_To_FileInfo.Is_Present
          (Filename_FileInfo_Map.all, Filename) then
            pFile := File_Name_To_FileInfo.Lookup
             (Filename_FileInfo_Map.all, Filename) ;

--            Put_Line("Num of classes " &
--              Integer'Image(Class_Lists.Length(pFile.ClassList)));
            ClassIter :=  Class_Lists.Makelistiter(pFile.ClassList);
            while Class_Lists.More(ClassIter) loop
                Class_Lists.Next(ClassIter, pClass);
--                Put_Line("Class " & To_String(pClass.Class_Name));
--      if Case_Sensitive_Equal(pClass.Class_Name, Classname)  then
                if Equal_Ignoring_Whitespace(To_String(pClass.Class_Name),
                   To_String(Classname) )  then
                     -- class found, now find method
                    if Method_Name_To_MethodInfo.Is_Present
                      (pClass.MethodMap.all, Methodname) then
                         pMethod := Method_Name_To_MethodInfo.Lookup
                     (pClass.MethodMap.all, Methodname) ;
                         -- Method found, now find argument
                        pArg := Get_Method_Arg(Argname, pMethod.ParamList);
                        if pArg /= null then
                            Argkind := pArg.Param_Kind;
                            case Argkind is
                              when None =>
                                return " ";
                              when Input_Obj =>
                                -- check for argname. (e.g. bodyRef.)
                                -- and if it has output annotation, output
                                -- out. This is an approximation to
                                -- detect cases *pbuff which will be modified
                                -- inside the function.
                                if Get_Method_Arg_Ref(Argname,
                                  pMethod.ParamList)  then
                                  return "__out ";
                                end if;
                                return " __in ";
                              when Output_Obj =>
                                return " __out ";
                              when Inout =>
                                return " __inout ";
                              end case;
                        else
                            -- arg not found - not an error can be
                            -- cases when arg is not referenced in the function
                            return " ";  -- Error
                        end if;
                     else
                         -- method not found - can be abstract methods for
                         -- abstract classes
--                        Put_Line_Err("Cannot find method " &
--                         To_String(Methodname));
--                        Utils.Program_Exit.Update_Error_Status
--                          (Status => Fatal_Internal_Error);
--                        Utils.Program_Exit.Exit_Program ;
                         return " ";  -- Error
                     end if ;
                   end if;   -- class found
             end loop;  -- while loop
                   -- class not found, error
                   Put_Line_Err("Cannot find class " & To_String(Classname)
                     & " in file " & To_String(Filename));
--                   Utils.Program_Exit.Update_Error_Status
--                    (Status => Fatal_Internal_Error);
--                   Utils.Program_Exit.Exit_Program ;
       elsif Filename_FileInfo_Map /= null then
          -- File not found
          Put_Line_Err("Cannot find file " & To_String(Filename));
--          Utils.Program_Exit.Update_Error_Status
--           (Status => Fatal_Internal_Error);
--          Utils.Program_Exit.Exit_Program ;
        end if;

        return " ";  -- Error

    end Get_Arg_Type;


    procedure Allocate_Filename_FileInfo_Map is
    begin

        Filename_FileInfo_Map := new File_Name_To_FileInfo.Mapping;
    end Allocate_Filename_FileInfo_Map;

    -- Process Messages for the given source file and build the data
    -- structures to generate SAL annotations - will be used by the Java parser
    procedure Process_File_Messages
      ( in_file_name : String;
       File_Info     : Messages_Per_File_Ptr) is
      pFile    :  File_Ptr;
      pClass   :  Class_Ptr;
      pMethod  :  Method_Ptr;
      pArg     :  Arg_Ptr;
      MethodName     : Utils.Spellings.Spelling;
      ClassName     : Utils.Spellings.Spelling;

    begin
      pFile := new File_rec;
      pFile.File_Name := Intern(in_file_name);
      pFile.Package_Name := File_Info.all.Package_Name;
      pFile.ClassList := Class_Lists.Create;
      Put_Line("Process_File_Messages: " & in_file_name);

      declare
          Within_Sal_Pool : Within_Pool (Sal_Pool'Access);
          pragma Unreferenced (Within_Sal_Pool);
      begin
          File_Name_To_FileInfo.Insert(Filename_FileInfo_Map.all,
            pFile.File_Name, pFile);
      end;

--        Put_Line("Package name " & To_String(File_Info.all.Package_Name));
--        Put_Line("Nick name " & To_String(File_Info.all.Nick_Name));
        for I in 1..Length(File_Info.Messages.Messages) loop
          declare
            M: Message renames Ptr_Nth(
              File_Info.Messages.Messages, I).all;
          begin

            declare
                Subkind : BE.Message_Kinds.BE_Message_Subkind;
                Text: constant String :=
                 Message_Text(M, Ignore_Key => True);
                BE_Msg : BE.BE_Messages.BE_Message_Kind renames
                  BE.BE_Messages.BE_Message_Kind(M.Kind.all);
            begin
--                Put_Line("Filename " & To_String(M.File_Name));
-- Put(" Kind : " &
-- BE.Message_Kinds.BE_Message_Subkind'Image( BE_Msg.Subkind ));
--                Put(" Key " & To_String(M.Message_Key));
--                Put_Line(" Msg : " & Text);
                Subkind := BE_Msg.Subkind;
                case Subkind is
                  when Module_Annotation =>
--                 Put_Line("Class " & To_String(BE_Msg.Module_Name));
                     pClass := new Class_rec;
                     -- Remove the pathname and $ sign to get the inner class
                     pClass.Class_Name := Intern(Remove_Outer_Name(
                       Bogus_Get_File_Name(
                       To_String(BE_Msg.Module_Name))));
                     pClass.Package_Name := File_Info.all.Package_Name;
                     pClass.MethodMap := new Method_Name_To_MethodInfo.Mapping;
                     Class_Lists.Attach(pClass, pFile.ClassList);
--                     Put_Line("Inserting class " &
--                      To_String(pClass.Class_Name));
                  when Procedure_Annotation =>
                     declare
                       Within_Sal_Pool : Within_Pool (Sal_Pool'Access);
                        pragma Unreferenced (Within_Sal_Pool);
                     begin
                         -- Remove the pathname and ( and then $
                         -- e.g. net/neurotech/quotes/QuoteFactory$QuoteImpl(
                         -- ......;)
                         MethodName := Intern(Remove_Outer_Name(
                          Bogus_Get_File_Name(
                           Remove_Signature(To_String(
                           BE_Msg.Procedure_Name)))));
                         -- Remove the pathname and $ sign to get the
                         -- inner class
                         ClassName := Intern(Remove_Outer_Name(
                          Bogus_Get_File_Name(
                          To_String(BE_Msg.Module_Name))));
                         pClass := Find_Class(pFile, ClassName);
                         if (pClass /= null)  and then not
                           Method_Name_To_MethodInfo.Is_Present
                           (pClass.MethodMap.all, MethodName) then
                             pMethod := new Method_rec;
                             pMethod.Method_Name := MethodName;
                             pMethod.Class_Name := pClass.Class_Name;
                             pMethod.ParamList := Argument_Lists.Create;
                             Method_Name_To_MethodInfo.Insert(
                               pClass.MethodMap.all,
                               pMethod.Method_Name, pMethod);
--                             Put_Line("Inserting Method " &
--                               To_String(MethodName)
--                              & " for class " &To_String( pClass.Class_Name));
                         end if;
                     end;
                  when End_Module_Annotation =>
                      pClass := null;
                  when End_Procedure_Annotation =>
                      pMethod := null;
                  when Input_Annotation =>
--                       Put_Line("Input " & To_String(M.Message_Key));
                       -- ignore internal arguments not seen in source file
                         -- Remove the pathname and (
                         MethodName := Intern(Remove_Outer_Name(
                          Bogus_Get_File_Name(
                           Remove_Signature(To_String(
                           BE_Msg.Procedure_Name)))));
                         -- Remove the pathname and $ sign to get the
                         -- inner class
                         ClassName := Intern(Remove_Outer_Name(
                          Bogus_Get_File_Name(
                          To_String(BE_Msg.Module_Name))));
                         pMethod := Find_Method_For_Class(pFile,
                           ClassName, MethodName);
                       if pMethod /= null then
                           pArg := Get_Method_Arg(M.Message_Key,
                            pMethod.ParamList);
                           if pArg = null then
                               pArg := new Arg_rec;
                               pArg.Param_Name := M.Message_Key;
                               pArg.Param_Kind := Input_Obj;
                               Argument_Lists.Attach(pArg,pMethod.ParamList);
                           else
                                -- already in list
                               if pArg.Param_Kind /= Input_Obj then
                                    pArg.Param_Kind := Inout;
                                end if;
                           end if;
                       end if;
                  when Output_Annotation =>
--                       Put_Line("Output " & To_String(M.Message_Key));
                       -- ignore internal arguments not seen in source file
                         -- Remove the pathname and (
                         MethodName := Intern(Remove_Outer_Name(
                           Bogus_Get_File_Name(
                           Remove_Signature(To_String(
                           BE_Msg.Procedure_Name)))));
                         -- Remove the pathname and $ sign to get the
                         -- inner class
                         ClassName := Intern(Remove_Outer_Name(
                          Bogus_Get_File_Name(
                          To_String(BE_Msg.Module_Name))));
                         pMethod := Find_Method_For_Class(pFile,
                           ClassName, MethodName);
                       if pMethod /= null then
                           pArg := Get_Method_Arg(M.Message_Key,
                             pMethod.ParamList);
                           if pArg = null then
                               pArg := new Arg_rec;
                               pArg.Param_Name := M.Message_Key;
                               pArg.Param_Kind := Output_Obj;
                               Argument_Lists.Attach(pArg,pMethod.ParamList);
                           else
                              -- already in list
                              if pArg.Param_Kind /= Output_Obj then
                                  pArg.Param_Kind := Inout;
                              end if;
                           end if;
                       end if;

                  when others => null;
                  end case;
            end;
          end;
        end loop;

    end Process_File_Messages;

    procedure Print_Arg(pArg : Arg_Ptr) is
      Argkind   : Argkind_enum;
    begin
        Put("Arg " & To_String(pArg.Param_Name));
        Argkind := pArg.Param_Kind;
        case Argkind is
          when None =>
            Put_Line("  none ");
          when Input_Obj =>
            Put_Line("  _in");
          when Output_Obj =>
           Put_Line("  _out");
          when Inout =>
            Put_Line( "  _inout");
         end case;
    end Print_Arg;

    procedure Print_Method(pMethod : Method_Ptr) is
      ParamIter : Argument_Lists.Listiter;
      pArg      : Arg_Ptr;
    begin
        Put("Method " & To_String(pMethod.Method_Name));
        Put_Line("  Class " & To_String(pMethod.Class_Name));
        ParamIter := Argument_Lists.Makelistiter(pMethod.ParamList);
        while Argument_Lists.More(ParamIter) loop
            Argument_Lists.Next(ParamIter, pArg);
            Print_Arg(pArg);
        end loop;
    end Print_Method;

    procedure Print_Method_Map(pMethodMap : Method_Name_To_MethodInfo_Ptr)  is
        procedure Print_One (Methodname :Spelling;
               Method_Info : Method_Ptr) is
        begin
          Put_Line("Method " & To_String(Methodname));
          Print_Method(Method_Info);

        end Print_One;
        procedure Print_All is new
          Method_Name_To_MethodInfo.Iterate (Print_One);
    begin
        Print_All ( pMethodMap.all);

    end  Print_Method_Map;

    procedure Print_Class(pClass : Class_Ptr) is
    begin
        Put("Class " & To_String(pClass.Class_Name));
        Put_Line("  Package " & To_String(pClass.Package_Name));
        Print_Method_Map(pClass.MethodMap);

    end Print_Class;

    procedure Print_FileInfo(pFile : File_Ptr) is
      ClassIter : Class_Lists.Listiter;
      pClass    : Class_Ptr;
    begin
        Put("Filename " & To_String(pFile.File_Name));
        Put_Line("  Package_Name " & To_String(pFile.Package_Name));
        ClassIter :=  Class_Lists.Makelistiter(pFile.ClassList);
        while Class_Lists.More(ClassIter) loop
            Class_Lists.Next(ClassIter, pClass);
--            Put_Line("Class " & To_String(pClass.Class_Name));
            Print_Class(pClass);
        end loop;
    end Print_FileInfo ;

    procedure Print_One_FileInfo(Filename : String) is
      pFile : File_Ptr;
    begin
        if File_Name_To_FileInfo.Is_Present
          (Filename_FileInfo_Map.all, Intern(Filename)) then
            pFile := File_Name_To_FileInfo.Lookup
             (Filename_FileInfo_Map.all, Intern(Filename)) ;
             Print_FileInfo(pFile);
         end if;

    end Print_One_FileInfo;

    procedure Print_FileInfo_Map  is
        procedure Print_One (Filename :Spelling;
               File_Info : File_Ptr) is
        begin
--          Put_Line("Filename " & To_String(Filename));
          Print_FileInfo(File_Info);

        end Print_One;
        procedure Print_All is new File_Name_To_FileInfo.Iterate (Print_One);
    begin
        Print_All ( Filename_FileInfo_Map.all);

    end  Print_FileInfo_Map;

    procedure Free_FileInfo(pFile : File_Ptr) is
      ClassIter : Class_Lists.Listiter;
      pClass    : Class_Ptr;
    begin
--        Put("Filename " & To_String(pFile.File_Name));
--        Put_Line("  Package_Name " & To_String(pFile.Package_Name));
        ClassIter :=  Class_Lists.Makelistiter(pFile.ClassList);
        while Class_Lists.More(ClassIter) loop
            Class_Lists.Next(ClassIter, pClass);
--            Free_Class(pClass);
        end loop;
        Class_Lists.Destroy(pFile.ClassList);
    end Free_FileInfo ;

    procedure Free_FileInfo_Map  is
        procedure Free_One (Filename :Spelling;
               File_Info : File_Ptr) is
        begin
--          Put_Line("Filename " & To_String(Filename));
          Free_FileInfo(File_Info);

        end Free_One;
        procedure Free_All is new File_Name_To_FileInfo.Iterate (Free_One);
    begin
        Free_All ( Filename_FileInfo_Map.all);

    end  Free_FileInfo_Map;

    procedure Set_Input_Buffer(Str : Access_Constant_String) is
    begin
        java_lexer_dfa.yy_init := True;
        Source_Contents := Str;
    end Set_Input_Buffer;

    procedure YY_INPUT(buf: out java_lexer_dfa.unbounded_character_array;
       result: out Integer; max_size: in Integer) is
      c : Character;
      i : Integer := 1;
      loc : Integer := buf'First;
      offset : Integer;  -- where to read from
      use Text_IO;
    begin

    if (Source_Contents /= null) then
        -- read from the last place you left off
        offset := read_from;
        while (i <= max_size and i + offset <= Source_Contents'Last) loop
          c := Source_Contents(i + offset);
--          Std_Output.Put(c);
          buf(loc) := c;
          loc := loc + 1;
          i := i + 1;
        end loop;
        if i + offset > Source_Contents'Last then
          -- when we hit EOF we need to set yy_eof_has_been_seen
          java_lexer_io.yy_eof_has_been_seen := True;
          read_from := 0;
        else
          read_from := i - 1 + offset;
        end if;
        result := i - 1;
    else
        java_lexer_io.YY_INPUT_default(buf, result, max_size);
        return;
    end if;

    exception
        when End_Error => result := i - 1;
         -- when we hit EOF we need to set yy_eof_has_been_seen
         java_lexer_io.yy_eof_has_been_seen := True;
         read_from := 0;
    end YY_INPUT;


    function New_Get_Arg_Type(Filename : Utils.Spellings.Spelling;
      Classname : Utils.Spellings.Spelling;
      Methodname : Utils.Spellings.Spelling;
      Argname : Utils.Spellings.Spelling;
      Srcpos : Utils.Messages.Source_Position ) return String is
      pFile : File_Ptr;
      pClass :  Class_Ptr;
      pMethod :  Method_Ptr;
      pArg    :  Arg_Ptr;
      ClassIter : Class_Lists.Listiter;
      Argkind   : Argkind_enum;
    begin

        -- Validate arguments from the parser
        if Filename = No_Spelling then
            Put_Line_Err("Filename missing "  );
            return (" ");
        elsif Classname = No_Spelling  then
            Put_Line_Err("Class name missing "  );
            return (" ");
        elsif Methodname = No_Spelling then
            Put_Line_Err("Method name missing "  );
            return (" ");
        elsif Argname = No_Spelling then
            Put_Line_Err("Argument name missing "  );
            return (" ");
        end if;

         return " ";

    end New_Get_Arg_Type;


end  Misc;
