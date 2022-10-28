-- $Source: /home/projects/ada/cvs-repository/rts/not_mccabe/ada.text_io.bdy,v $
-- $Revision: 5689 $ -- $Date: 2009-07-23 15:31:34 -0400 (Thu, 23 Jul 2009) $ -- $Author: stt $
--        Copyright (C) 1995 by Intermetrics, Inc.

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with System.RTS.Lexer;
    pragma Elaborate_All(System.RTS.Lexer);
with System.RTS.String_Utilities;   use System.RTS.String_Utilities;
with System.RTS.TGT.IO_Constants;
    pragma Elaborate_All(System.RTS.TGT.IO_Constants);
with System.RTS.Float_IO_Support;
with System.RTS.Integer_State_Tables;  -- int lex tables
with System.RTS.Integer_Value_Pkg;
    pragma Elaborate_All(System.RTS.Integer_Value_Pkg);
with Ada.Streams;
with Ada.Strings;use Ada.Strings;
with Interfaces;
with Interfaces.C;
with Interfaces.C.Stdio;
use Interfaces.C;

pragma ada_child;
package body Ada.Text_IO is

    pragma Suppress( Elaboration_Check );

    package IC   renames Interfaces.C;
    package FIO  renames System.RTS.Float_IO_Support;
    package TGT  renames System.RTS.TGT;
    package AS   renames Ada.Streams;

    NYI : exception;

    Record_Oriented: constant Boolean :=
      System.RTS.TGT.IO_Constants.Record_Oriented;

    Buffer_Length : constant integer := 1;

    subtype Buffer_Count is integer range 0 .. Buffer_Length;
    
    type Name_Type is access String;
    subtype Mode_Type is String(1..2);     --   Used for mode;

    type Call_Type is (Open_Type, Create_Type);

    type String_Access is access String;

    type File_Record is new AS.Root_Stream_Type with record
        Mode           : File_Mode;        --| Mode of file
        Line_Length    : Count;            --| Maximum line length allowed
        Page_Length    : Count;            --| Maximum page length allowed
        Current_Column : Positive_Count;
        Current_Line   : Positive_Count;
        Current_Page   : Positive_Count;
        Is_Special     : boolean;          --| True if file is the current
                                           --| default input file or the
                                           --| current default output file
        File_Name      : Name_Type;
        CFile          : Stdio.File_Ptr;         -- corresponds to the File in C
	Pushed_Back    : String(1..2);  -- Ungetc only guarantees one push-back
	Num_Pushed_Back : Natural := 0;
        Self           : aliased File_Type;
    end record;

    procedure Read (File : in out File_Record;
                    Item : out AS.Stream_Element_Array;
                    Last : out AS.Stream_Element_Offset);
    -- Read used when Stream_IO file is treated directly as Stream

    procedure Write (File : in out File_Record;
                     Item : in AS.Stream_Element_Array);
    -- Write used when Stream_IO file is treated directly as Stream

Line_Terminator : constant character := ASCII.LF;
Vertical_Tab    : constant character := ASCII.VT;
Page_Terminator : constant character := ASCII.FF;

Standard_Input_File,
Standard_Output_File,
Standard_Error_File : aliased File_Type;
    --| Files associated with standard input and output

Current_Input_File,
Current_Output_File,
Current_Error_File : aliased File_Type;
    --| File associated with current default input and output

Current_Input_Initialized,
Current_Output_Initialized,
Current_Error_Initialized : boolean := false;
    --| False until first reference to current input or output, then
    --| the file is initialized and the boolean becomes true

Internal_Error : exception;    --| Raised if internal inconsistency
                               --| detected

procedure Free_File is new
  Ada.Unchecked_Deallocation(File_Record, File_Type);

-- Local subprograms

procedure Init_File (  --| Initializes all fields of file to
  File : in File_Type; --| their default values (except Char_IO_File),
  Mode : in File_Mode  --| and sets mode of file to Mode
);

procedure Close_Special_File (  --| Closes current input/output
  File : in File_Type           --| if associated file closed.
);

function Get_Standard_Input return File_Type;
    --| Return standard input file

function Get_Standard_Output return File_Type;
    --| Return standard output file

function Get_Standard_Error return File_Type;
    --| Return standard error file

procedure Local_Put (
   File : in File_Type;
   Item : in string
);

procedure Local_Put_Line (
  File : in File_Type;
  Item : in string
);

procedure Local_New_Line(
  File : in File_Type;
  Spacing : in Positive_Count
);

procedure Local_New_Page( File : in File_Type );

procedure Local_Get (
  File : in File_Type;
  Item : out string
);

procedure Local_Get_Line (
  File : in File_Type;
  Item : out string;
  Last : out Natural
);

type Int_Ptr is access all Integer;

function c_fopen (
    Name : Interfaces.C.char_array;
    Mode : Interfaces.C.char_array;
    Excep : Int_Ptr
) return Stdio.File_Ptr;
pragma Import (c, c_fopen, link_name => "rts_c_fopen");

function c_tempnam (Dir: Interfaces.C.Char_Array;
  Pfx : Interfaces.C.Char_Array) return Interfaces.C.Char_Array;
pragma Import(c, c_tempnam, link_name => "tempnam");

function c_fclose (File_Pointer : Stdio.File_Ptr) return Integer;
pragma Import (c, c_fclose, "fclose");

function c_fflush ( Fileid : Stdio.File_Ptr) return Integer;
pragma Import (c, c_fflush, "fflush");

procedure c_rewind ( Fileid : Stdio.File_Ptr);
pragma Import (c, c_rewind, "rewind");

function c_remove ( Name : Interfaces.C.char_array) return Integer;
pragma Import (c, c_remove, "remove");

function c_fgetc ( Fileid : Stdio.File_Ptr) return Integer;
pragma Import (c, c_fgetc, "fgetc");

function c_ungetc (
   Ch : Integer;   
   Fileid : Stdio.File_Ptr) return Integer;
pragma Import (c, c_ungetc, "ungetc");

function c_fputc ( Ch : Integer;
  Fileid : Stdio.File_Ptr ) return Integer;
pragma Import (c, c_fputc, "fputc");

function c_fputs ( S : Interfaces.C.char_array;
  Fileid : Stdio.File_Ptr ) return Integer;
pragma Import (c, c_fputs, "fputs");

function c_fread(
  S : System.Address;
  Size : Natural;
  Nobj : Natural;
  Fileid : Stdio.File_Ptr) return Natural;
pragma Import(Intrinsic, c_fread, "fread");

function c_fwrite(
  S : System.Address;
  Size : Natural;
  Nobj : Natural;
  Fileid : Stdio.File_Ptr) return Natural;
pragma Import(Intrinsic, c_fwrite, "fwrite");

function c_get_stdin return Stdio.File_Ptr;
pragma Import (c, c_get_stdin, link_name => "rts_c_get_stdin");

function c_get_stdout return Stdio.File_Ptr;
pragma Import (c, c_get_stdout, link_name => "rts_c_get_stdout");

function c_get_stderr return Stdio.File_Ptr;
pragma Import (c, c_get_stderr, link_name => "rts_c_get_stderr");

function c_feof(FileID : Stdio.File_Ptr) return Integer;
pragma Import (Intrinsic, c_feof, link_name => "feof");

-- Local subprograms

procedure Raise_Excep(Excep : Integer) is
  -- Raise appropriate exception as indicated by Excep
begin
    case Excep is
      when 1 => raise Name_Error;
      when 2 => raise Status_Error;
      when 3 => raise Use_Error;
      when others => null;
    end case;
end Raise_Excep;

procedure Check_Mode_In(File : in File_Type) is
begin
    if File = null then
        raise STATUS_ERROR;
    elsif File.Mode /= In_File then
        raise MODE_ERROR;
    end if;
end Check_Mode_In;

procedure Check_Mode_Out_Or_Append(File : in File_Type) is
begin
    if File = null then
        raise STATUS_ERROR;
    elsif File.Mode = In_File then
        raise Mode_Error;
    end if;
end Check_Mode_Out_Or_Append;

procedure Check_Current_Input is
begin
    -- Check to make sure current_input hasn't been closed
    if Current_Input_Initialized and then
        Current_Input_File = null then
            raise STATUS_ERROR;
    end if;
end Check_Current_Input;

procedure Check_Current_Output is
begin
    if Current_Output_Initialized and then
        Current_Output_File = null then
          -- The current output file has been closed
          raise STATUS_ERROR;
    end if;
end Check_Current_Output;

procedure Next_Page ( File : in File_Type ) is
begin
    File.Current_Page   := File.Current_Page + 1;
    File.Current_Line   := 1;
    File.Current_Column := 1;
end Next_Page;

procedure Next_Line ( File : in File_Type ) is
begin
    File.Current_Line := File.Current_Line + 1;
    File.Current_Column := 1;
end Next_Line;

function Get_Next_Char (File : in File_Type) return Integer is
  -- Wrapper for f_getc, but worries about pushed-back chars
begin
    if File.Num_Pushed_Back > 0 then
	-- We have a pushed-back char
	File.Num_Pushed_Back := File.Num_Pushed_Back - 1;
	if File.Num_Pushed_Back > 0 then
	    -- We had 2 or more pushed back chars.  These are in our own
	    -- stack.
	    return Character'Pos(File.Pushed_Back(File.Num_Pushed_Back));
	end if;
    end if;
    return c_fgetc(File.Cfile);
end Get_Next_Char;

function Push_Back_Char(Ch : in Integer; File : in File_Type) return Integer is
  -- Wrapper for c_ungetc, but worries about push-back failures
  -- We try to use ungetc so other readers of the file see everything.
begin
    if Ch < 0 then
	-- We were pushing back an EOF
	return Ch;
    end if;

    if File.Num_Pushed_Back = 0 then
	-- First char of push back uses ungetc
      declare
	Result : constant Integer := c_ungetc(Ch, File.Cfile);
      begin
	pragma Assert(Result = Ch); null;
      end;
    else
	-- No more than one push-back is reliable for ungetc.
	-- Need to use our own push-back buffer.
	File.Pushed_Back(File.Num_Pushed_Back) := Character'Val(Ch);
    end if;
    File.Num_Pushed_Back := File.Num_Pushed_Back + 1;
    return Ch;
end Push_Back_Char;

-- Visible subprogram bodies

-- File Management

function To_Mode(Mode : in File_Mode;
  Mode_Type : in Call_Type) return
  String is
begin
    case Mode is
      when Out_File =>
        if Mode_Type = Open_Type then
            return "r+" & ASCII.NUL;
        end if;
        return "w" & ASCII.NUL;
      when Append_File =>
        return "a" & ASCII.NUL;
      when In_File =>
        if Mode_Type = Create_Type then
            raise Use_Error;
        end if;
        return "r+" & ASCII.NUL;
     end case;

end To_Mode;

procedure Create (File : in out File_Type;
                  Mode : in File_Mode := Out_File;
                  Name : in string := "";
                  Form : in string := "") is
    use type Stdio.File_Ptr;
    Excep : aliased Integer := 0;
begin
    if File /= null then
        raise STATUS_ERROR;
    else
    -- Allocate new file, fill in fields as needed
        File := new File_Record;
        Init_File(File, Mode);
        if (Name'Length = 0) then
            File.File_Name := new String'(Interfaces.C.To_Ada(
	      c_tempnam(Interfaces.C.To_C("."), Interfaces.C.To_C("tmp_"))));
        else
            File.File_Name := new String'(Name);
        end if;
        File.CFile := c_fopen(Interfaces.C.To_C(File.File_Name.all),
          Interfaces.C.To_C(To_Mode(Mode, Create_Type)),
	  Excep'Unchecked_Access);

	if Excep /= 0 then
	    Raise_Excep(Excep);
	end if;
	
	if File.CFile = null then
	    raise Name_Error;
	end if;

    end if;
end Create;

procedure Open (File : in out File_Type;
                Mode : in File_Mode;
                Name : in string;
                Form : in string := "") is
    use type Stdio.File_Ptr;
    Excep : aliased Integer := 0;
begin
    if File /= null then
        raise STATUS_ERROR;
    else
        File := new File_Record;
        Init_File(File, Mode);
        File.File_Name := new String'(Name);
        File.CFile := c_fopen(Interfaces.C.To_C(Name), 
          Interfaces.C.To_C(To_Mode(Mode, Open_Type)),
	  Excep'Unchecked_Access); 

	if Excep /= 0 then
	    Raise_Excep(Excep);
	end if;
	
	if File.CFile = null then
	    raise Name_Error;
	end if;

    end if;
end Open;


procedure Close (File : in out File_Type) is
    Result : Integer;
begin
    if File = null then
        raise STATUS_ERROR;
    else
        if File.Is_Special then
            Close_Special_File(File);
        end if;
        if File.Current_Column /= 1 and then
           (File.Mode = Out_File or else File.Mode = Append_File) then
            Local_New_Line(File, 1);
        end if;
         Result := c_fclose(File.CFile);
        Free_File(File);
    end if;
end Close;


procedure Delete (File : in out File_Type) is
    Result : Integer;
begin
    if File = null then
        raise STATUS_ERROR;
    else
        if File.Is_Special then
            Close_Special_File(File);
        end if;
        Result := c_remove(Interfaces.C.To_C(File.File_Name.all));
        Free_File(File);
    end if;
end Delete;


procedure Reset (File : in out File_Type;
                 Mode : in File_Mode) is
    Result : Integer;
    use type Stdio.File_Ptr;
    Excep : aliased Integer := 0;
begin
    if File = null then
        raise STATUS_ERROR;
    elsif File.Is_Special and then File.Mode /= Mode then
        -- File is either the current default input file or the
        -- current default output file, and it's mode cannot be
        -- changed.
        raise MODE_ERROR;
    else
        if File.Current_Column /= 1 and then
           (File.Mode = Out_File or else File.Mode = Append_File) then
            -- Last line in file unterminated, so terminate it
            Local_New_Line(File, 1);
        end if;
       if Mode /= File.Mode then
          -- Mode of the file changes
           Result := c_fclose(File.CFile);
           if Result /= 0 then
               raise STATUS_ERROR;
           end if;
           File.CFile := c_fopen(Interfaces.C.To_C(File.File_Name.all),
             Interfaces.C.To_C(To_Mode(Mode, Open_Type)),
	     Excep'Unchecked_Access);

	    if Excep /= 0 then
		Raise_Excep(Excep);
	    end if;
	
	   if File.CFile = null then
	       raise Use_Error;
	   end if;

       else
           c_rewind(File.CFile);
       end if;
       Init_File(File, Mode);
    end if;
end Reset;

procedure Reset (File : in out File_Type) is
begin
    if File /= null then
        Reset (File, File.Mode);
    else
        raise STATUS_ERROR;
    end if;
end Reset;

function Mode (File : in File_Type) return File_Mode is
begin
    if File = null then
        raise STATUS_ERROR;
    else
        return File.Mode;
    end if;
end Mode;

function Name (File : in File_Type) return string is
begin
    if File = null then
        raise STATUS_ERROR;
    else
        return File.File_Name.all;
    end if;
end Name;

function Form (File : in File_Type) return String is
begin
    if File = null then
        raise STATUS_ERROR;
    else
        -- return empty string
        return " ";
    end if;
end Form;


function Is_Open (File : in File_Type) return boolean is
begin
    return File /= null;
end Is_Open;


-- Default Input and Output files

procedure Set_Input (File : in File_Type) is
begin
    Check_Mode_In(File);
    if Current_Input_File /= null then
        -- Old file no longer associated with current input
        Current_Input_File.Is_Special := false;
    end if;
    Current_Input_File := File;
    Current_Input_File.Is_Special := true;
    Current_Input_Initialized := true;
end Set_Input;

procedure Set_Output (File : in File_Type) is
begin
    Check_Mode_Out_Or_Append(File);
    if Current_Output_File /= null then
        -- Old file no longer associated with current output
        Current_Output_File.Is_Special := false;
    end if;
    Current_Output_File := File;
    Current_Output_File.Is_Special := true;
    Current_Output_Initialized := true;
end Set_Output;

procedure Set_Error( File : in File_Type ) is
begin
    Check_Mode_Out_Or_Append(File);
    if Current_Error_File /= null then
        Current_Error_File.Is_Special := false;
    end if;
    Current_Error_File := File;
    Current_Error_File.Is_Special := true;
    Current_Error_Initialized := true;
end Set_ERROR;

function Standard_Input return File_Type is
begin
    return Standard_Input.all;
end Standard_Input;

function Standard_Input return File_Access is
begin
    if Standard_Input_File = null then
        Standard_Input_File := Get_Standard_Input;
    end if;
    return Standard_Input_File'Access;
end Standard_Input;

function Standard_Output return File_Type is
begin
    return Standard_Output.all;
end Standard_Output;

function Standard_Output return File_Access is
begin
    if Standard_Output_File = null then
        Standard_Output_File := Get_Standard_Output;
    end if;
    return Standard_Output_File'Access;
end Standard_Output;

function Standard_Error return File_Type is
begin
    return Standard_Error.all;
end Standard_Error;

function Standard_Error return File_Access is
begin
    if Standard_Error_File = null then
        Standard_Error_File := Get_Standard_Error;
    end if;
    return Standard_Error_File'Access;
end Standard_Error;

function Current_Input return File_Type is
begin
    return Current_Input.all;
end Current_Input;

function Current_Input return File_Access is
begin
    if not Current_Input_Initialized then
        Current_Input_File := Standard_Input;
        Current_Input_File.Is_Special := true;
        Current_Input_Initialized := true;
    end if;
    return Current_Input_File.Self'Access;
end Current_Input;

function Current_Output return File_Type is
begin
    return Current_Output.all;
end Current_Output;

function Current_Output return File_Access is
begin
    if not Current_Output_Initialized then
        Current_Output_File := Standard_Output;
        Current_Output_File.Is_Special := true;
        Current_Output_Initialized := true;
    end if;
    return Current_Output_File.Self'Access;
end Current_Output;

function Current_Error return File_Type is
begin
    return Current_Error.all;
end Current_Error;

function Current_Error return File_Access is
begin
    if not Current_Error_Initialized then
        Current_Error_File := Standard_Error;
        Current_Error_File.Is_Special := true;
        Current_Error_Initialized := true;
    end if;
    return Current_Error_File.Self'Access;
end Current_Error;

procedure Flush (File : in File_Type) is
    Result : Integer;
begin
    Check_Mode_Out_Or_Append(File);
    Result := c_fflush(File.CFile);
end FLUSH;

procedure Flush is
    FT : File_Type;
begin
    Check_Current_Output;
    FT := Current_Output;
    Flush(FT);
end FLUSH;

-- Specification of Line and Page Lengths
procedure Set_Line_Length (File : in File_Type;
                           To   : in Count) is
begin
    Check_Mode_Out_Or_Append(File);
    File.Line_Length := To;
end Set_Line_Length;

procedure Set_Line_Length (To : in Count) is
begin
    Check_Current_Output;
    Set_Line_Length(Current_Output, To);
end Set_Line_Length;

procedure Set_Page_Length (File : in File_Type;
                           To   : in Count) is
begin
    Check_Mode_Out_Or_Append(File);
    File.Page_Length := To;
end Set_Page_Length;


procedure Set_Page_Length (To : in Count) is
begin
    Check_Current_Output;
    Set_Page_Length(Current_Output, To);
end Set_Page_Length;

function Line_Length (File : in File_Type) return Count is
begin
    Check_Mode_Out_Or_Append(File);
    return File.Line_Length;
end Line_Length;

function Line_Length return Count is
begin
    Check_Current_Output;
    return Line_Length(Current_Output);
end Line_Length;

function Page_Length (File : in File_Type) return Count is
begin
    Check_Mode_Out_Or_Append(File);
    return File.Page_Length;
end Page_Length;

function Page_Length return Count is
begin
    Check_Current_Output;
    return Page_Length(Current_Output);
end Page_Length;

-- Operations on Columns, Lines, and Pages

procedure New_Line (File : in File_Type;
                    Spacing : in Positive_Count := 1) is
begin
    Check_Mode_Out_Or_Append(File);
    Local_New_Line (File, Spacing);
end New_Line;

procedure New_Line (Spacing :  in Positive_Count := 1) is
begin
    Check_Current_Output;
    Local_New_Line(Current_Output, Spacing);
end New_Line;

procedure Skip_Line (File : in File_Type;
                     Spacing : in Positive_Count := 1) is
    -- For a Spacing of one: Reads and discards all characters until a 
    -- line terminator has been read, and then sets the current column 
    -- number to one. If the line terminator is not immediately followed 
    -- by a page terminator, the current line number is incremented by one. 
    -- Otherwise, if the line terminator is immediately followed by a page 
    -- terminator, then the page terminator is skipped, the current page 
    -- number is incremented by one, and the current line number is set to one.
    -- 
    -- For a Spacing greater than one, the above actions are performed 
    -- Spacing times.
    --
    -- The exception Mode_Error is propagated if the mode is not In_File. 
    -- The exception End_Error is propagated if an attempt is made to read 
    -- a file terminator.

    Char : character;
    ch : Integer;
    Result : Integer;
begin
    Check_Mode_In(File);

    for i in 1 .. Spacing loop
        loop
            -- Find out where we are
            ch := Get_Next_Char(File);
            if ch < 0 then
                -- EOF detected
		exit when i = Spacing and then File.Current_Column > 1;

		-- EOF at beginning of line
		raise END_ERROR;
            end if;
            Char := Character'Val(ch);
	    File.Current_Column := File.Current_Column + 1;
            exit when Char = ASCII.LF or else Char = ASCII.FF;

        end loop;

        -- We have seen the EOL
        Next_Line(File);

	if Char = ASCII.FF then
	    -- Line was actually terminated by a page terminator
	    Next_Page(File);
	else
	    -- Check if the next char is Page terminator
	    ch := Get_Next_Char(File);
	    if ch < 0 then
		-- EOF detected
		if i = Spacing then
                    -- EOL followed by EOF
                    Next_Page(File);
		    return;
		else
		    raise END_ERROR;
		end if;
	    end if;
	    Char := Character'Val(ch);
	    if Char = ASCII.FF then
		-- Skip the page terminator too
		Next_Page(File); 
	    else
		-- Not followed by a page terminator
		-- so put back the character.
		Result := Push_Back_Char(ch, File);
	    end if;
	end if;

    end loop;

end Skip_Line;

procedure Skip_Line (Spacing : in Positive_Count := 1) is

begin
    Check_Current_Input;
    Skip_Line(Current_Input, Spacing);
end Skip_Line;

function End_of_Line (File : in File_Type) return boolean is
    Char : Character;
    ch : Integer;
    Result : Integer;
begin
    Check_Mode_In(File);

    -- Returns True if a line terminator or a File Terminator is next
    ch := Get_Next_Char(File);
    if ch < 0 then
	-- End of file always has an implicit line terminator
	return True;
    end if;

    Char := Character'Val(ch);
    -- Push back first character
    Result := Push_Back_Char(ch, File);
    return Char  = ASCII.LF;
end End_Of_Line;


function End_Of_Line return boolean is
begin
    Check_Current_Input;
    return End_Of_Line(Current_Input);
end End_Of_Line;

procedure New_Page (File : in File_Type) is
begin
    Check_Mode_Out_Or_Append(File);
    Local_New_Page(File);
end New_Page;

procedure New_Page is
begin
    Check_Current_Output;
    Local_New_Page(Current_Output);
end New_Page;

procedure Skip_Page (File : in File_Type) is
    Char : character;
    ch : Integer;
    Result : Integer;
    Skipped_At_Least_One_Character : Boolean := False;
begin
    Check_Mode_In(File);
    loop
        ch := Get_Next_Char(File);
        if ch < 0 then
            -- EOF detected
	    if Skipped_At_Least_One_Character then
	        Next_Page(File);
		return;
	    else
                -- if EOF is the first character you see
	        raise END_ERROR;
	    end if;
        end if;
	Skipped_At_Least_One_Character := True;
        Char := Character'Val(ch);
        File.Current_Column := File.Current_Column + 1;
        if Char = ASCII.LF then
            Next_Line(File);
        end if;
        exit when Char = ASCII.FF ;

    end loop;
    -- page terminator has been gobbled up before exiting the loop
    Next_Page(File);
end Skip_Page;

procedure Skip_Page is
begin
    Check_Current_Input;
    Skip_Page(Current_Input);
end Skip_Page;

function End_Of_Page (File : in File_Type) return boolean is
    Item : character;
    ch : Integer;
    chnext : Integer;
    Result : Integer;
begin
    Check_Mode_In(File);

    -- Returns true if the combination of a line terminator and
    -- a page terminator is next or if a file terminator is next

    ch := Get_Next_Char(File);
    if ch < 0 then
	-- End of file always has an implicit page terminator
	return True;
    end if;

    Item := Character'Val(ch);
    if Item = ASCII.LF then
	chnext := Get_Next_Char(File);
	if chnext < 0 then
	    -- End of file
	    Result := Push_Back_Char(ch, File);
	    return True;
	end if;
	-- Push back second character
	Result := Push_Back_Char(chnext, File);

	-- Now decide whether at end of page
        Item := Character'Val(chnext);
    end if;
    -- Push back first character
    Result := Push_Back_Char(ch, File);
	
    return Item = ASCII.FF;

end End_Of_Page;

function End_Of_Page return boolean is
begin
    Check_Current_Input;
    return End_Of_Page(Current_Input);
end End_Of_Page;

function End_Of_File (File : in File_Type) return boolean is
    Char : character;
    ch : Integer;
    chnext : Integer := -1;
    chlast : Integer;
    Item : character;
    Result : Integer;
begin
    Check_Mode_In(File);

    -- Returns true if a file terminator is next or if the combination of
    -- a line, a page  and a file terminator is next; otherwise returns
    -- False
    ch := Get_Next_Char(File);
    if ch < 0 then
	-- End of file 
	return True;
    end if;

    Item := Character'Val(ch);
    if Item = ASCII.LF then
	chnext := Get_Next_Char(File);
	if chnext < 0 then
	    -- End of file
	    Result := Push_Back_Char(ch, File);
	    return True;
	end if;

	-- Now decide whether at end of page
        Item := Character'Val(chnext);
    end if;

    if Item = ASCII.FF then
	chlast := Get_Next_Char(File);
	if chlast < 0 then
	    -- End of file
            -- push back in reverse order
	    Result := Push_Back_Char(chnext, File);
	    Result := Push_Back_Char(ch, File);
	    return True;
	end if;
	-- Push back third character
	Result := Push_Back_Char(chlast, File);
    end if;

    Result := Push_Back_Char(chnext, File);
    Result := Push_Back_Char(ch, File);
    return False;
end End_Of_File;

function End_Of_File return boolean is
begin
    Check_Current_Input;
    return End_Of_File(Current_Input);
end End_Of_File;

procedure Set_Col (File : in File_Type;
                   To   : in Positive_Count) is
    Char : character;
    Item : character;
    Itemnext : character;
    ch   : Integer;
    Result : Integer;
    chnext : Integer;
begin
    if File = null then
        raise STATUS_ERROR;
    else
        case File.Mode is
            when In_File =>
                if To = File.Current_Column then
                    return;
                end if;
                loop
                    ch := Get_Next_Char(File);
		    if ch < 0 then
		        -- EOF detected
			raise END_ERROR;
		    end if;
                    Item := Character'Val(ch);
	            File.Current_Column := File.Current_Column + 1;
                    exit when (File.Current_Column - 1)  = To  and 
                     Item /= ASCII.LF and Item /= ASCII.FF ;

		    if Item = ASCII.LF then
                        -- EOL detected
                        Next_Line(File);
                    elsif Item = ASCII.FF then
                        -- EOP detected 
                        Next_Page(File);
		    else
                        -- Non terminator - 
                        null;
		    end if;
                end loop;

                -- Put back the non terminator character
		Result := Push_Back_Char(ch, File);
                File.Current_Column := File.Current_Column - 1;

            when Out_File | Append_File =>
                if To = File.Current_Column then
                    return;
                end if;
                if File.Line_Length /= Unbounded and then
                    To > File.Line_Length then
                      raise LAYOUT_ERROR;
                end if;
                if To < File.Current_Column then
                    Local_New_Line(File, 1);
                end if;
                for i in File.Current_Column .. To - 1 loop
                    Put(File, ' ');
                end loop;
            when others =>
                null;
        end case;
    end if;
end Set_Col;

procedure Set_Col(To : in Positive_Count) is
begin
    Check_Current_Output;
    Set_Col(Current_Output, To);
end Set_Col;

procedure Set_Line(File : in File_Type;
                   To   : in Positive_Count) is
    Char : character;
    ch   : Integer;
begin
    if File = null then
        raise STATUS_ERROR;
    else
        case File.Mode is
            when In_File =>
                loop
                     if File.Current_Line < To then
                         Skip_Line(File);
                         exit when File.Current_Line = To;
                     elsif File.Current_Line > To then
                         Skip_Page(File);
                     else
                         exit;
                     end if;
                 end loop;

            when Out_File | Append_File =>
                if File.Page_Length /= Unbounded and then
                    To > File.Page_Length then
                      raise LAYOUT_ERROR;
                end if;
                if To < File.Current_Line then
                    Local_New_Page(File);
                    Local_New_Line(File, To - 1);
                    return;
                end if;
                if To > File.Current_Line then
                    loop
                        Local_New_Line(File, 1);
                        exit when File.Current_Line = To;
                    end loop;
                end if;
            when others =>
                null;
        end case;
    end if;
end Set_Line;

procedure Set_Line(To : in Positive_Count) is
begin
    Check_Current_Output;
    Set_Line(Current_Output, To);
end Set_Line;

function Col (File : in File_Type) return Positive_Count is
begin
    if File = null then
        raise STATUS_ERROR;
    else
        return File.Current_Column;
    end if;
end Col;

function Col return Positive_Count is
begin
    Check_Current_Output;
    return Col(Current_Output);
end Col;

function Line(File : in File_Type) return Positive_Count is
    Char : character;
begin
    if File = null then
        raise STATUS_ERROR;
    end if;
    return File.Current_Line;
end Line;

function Line return Positive_Count is
begin
    Check_Current_Output;
    return Line(Current_Output);
end Line;

function Page (File : in File_Type) return Positive_Count is
    Char : character;
begin
    if File = null then
        raise STATUS_ERROR;
    end if;

    return File.Current_Page;
end Page;

function Page return Positive_Count is
begin
    Check_Current_Output;
    return Page(Current_Output);
end Page;

procedure Get (File : in File_Type;
               Item : out character) is
    ch : Integer;
    Result : Integer;
begin
    -- After skipping any line terminator and any page terminator,
    -- reads the next character from the file 
    Check_Mode_In(File);

    ch := Get_Next_Char(File);
    if ch < 0 then
        -- EOF detected
	raise END_ERROR;
    end if;
    Item := Character'Val(ch);
    File.Current_Column := File.Current_Column + 1;

    loop
        if Item = ASCII.LF then
        -- EOL detected, read next char
            ch := Get_Next_Char(File);
            if ch < 0 then
                -- EOF detected
                raise END_ERROR;
            end if;
            Item := Character'Val(ch);
            Next_Line(File);
        elsif Item = ASCII.FF then
        -- EOP detected, read next char
            ch := Get_Next_Char(File);
            if ch < 0 then
                -- EOF detected
                raise END_ERROR;
            end if;
            Item := Character'Val(ch);
            Next_Page(File);
        elsif ch < 0 then
            -- EOF detected
                raise END_ERROR;
        else
            -- Non terminator
            return;
        end if;
    end loop;
end Get;

procedure Get (Item : out character) is
begin
    Check_Current_Input;
    Get(Current_Input, Item);
end Get;

procedure Put (File : in File_Type;
               Item : in character) is
    ch : Integer;
begin

    Check_Mode_Out_Or_Append(File);
    -- Check for output of terminators
    case Item is
        when Line_Terminator =>
            Local_New_Line(File, 1);
        when Page_Terminator =>
            Local_New_Page(File);
        when others =>
            -- Not a terminator, just do the usual
            if File.Line_Length /= Unbounded and then
                File.Current_Column > File.Line_Length then
                  Local_New_Line(File, 1);
            end if;
            ch := c_fputc(Character'Pos(Item), File.CFile);
            File.Current_Column := File.Current_Column + 1;
    end case;
end Put;

procedure Put (Item : in character) is
    ch : Integer;
begin
    Check_Current_Output;
    Put(Current_Output, Item);
end Put;

procedure Look_Ahead( File : in File_Type;
                      Item : out Character;
                      End_Of_Line : out Boolean ) is
     ch : Integer;
begin
    Check_Mode_In(File);
    ch := Get_Next_Char(File);
    if ch < 0 then
        -- EOF detected
        Item := ASCII.NUL;
        End_Of_Line := true;
        return;
    end if;
    Item := Character'Val(ch);
    if Item = ASCII.LF or Item = ASCII.FF then
        Item := ASCII.NUL;
        End_Of_Line := true;
    else
        End_Of_Line := false;
    end if;
    if ch  > 0 then
        ch := Push_Back_Char(ch, File);
    end if;
end Look_Ahead;

procedure Look_Ahead( Item : Out Character;
                      End_Of_Line : out Boolean ) is
begin
    Check_Current_Input;
    Look_Ahead(Current_Input, Item, End_Of_Line);
end Look_Ahead;

procedure Get_Immediate( File : in File_Type;
                         Wait : in Boolean;
                         Item : out Character;
                         Available : out Boolean )  is
    ch : Integer;
begin
    Check_Mode_In(File);
    ch := Get_Next_Char(File);
    if ch < 0 then
        -- EOF detected
	raise END_ERROR;
        return;
    end if;
    Item := Character'Val(ch);
    Available := True;
    -- TBD when does Available become false
    -- Change the mode of terminal so that we it reads
    -- 1 char at a time later on

end Get_Immediate;

procedure Get_Immediate( File : in File_Type;
                         Item : out Character )  is
    Ignore : Boolean;
begin
    Get_Immediate(File      => File,
                  Wait      => True,
                  Item      => Item,
                  Available => Ignore);
end Get_Immediate;

procedure Get_Immediate( Item : out Character )   is
begin
    Check_Current_Input;
    Get_Immediate(Current_Input, Item);
end Get_Immediate;

procedure Get_Immediate( File : In File_Type;
                         Item : out Character;
                         Available : out Boolean )   is
begin
    Get_Immediate(File      => File,
                  Wait      => False,
                  Item      => Item,
                  Available => Available);
end Get_Immediate;

procedure Get_Immediate( Item      : out Character;
                         Available : out Boolean )   is
begin
    Check_Current_Input;
    Get_Immediate(Current_Input, Item, Available);
end Get_Immediate;

procedure Get (File : in File_Type;
               Item : out string) is
begin
    Check_Mode_In(File);
    Local_Get(File, Item);
end Get;

procedure Get (Item : out string) is
begin
    Check_Current_Input;
    Local_Get(Current_Input, Item);

end Get;

procedure Put (File : in File_Type;
               Item : in string) is
    ch : Integer;
begin
    Check_Mode_Out_Or_Append(File);
    Local_Put(File, Item);
end Put;

procedure Put(Item : in string) is
    ch : Integer;
begin
    Check_Current_Output;
    Local_Put(Current_Output, Item);
end Put;

procedure Get_Line (File : in File_Type;
                    Item : out string;
                    Last : out natural) is
begin
    Check_Mode_In(File);
    Local_Get_Line(File, Item, Last);
end Get_Line;

procedure Get_Line(Item : out string;
                   Last : out Natural) is
begin
    Check_Current_Input;
    Local_Get_Line(Current_Input, Item, Last);
end Get_Line;

procedure Put_Line(File : in File_Type;
                   Item : in string) is
begin
    Check_Mode_Out_Or_Append(File);
    Local_Put_Line(File, Item);
end Put_Line;

procedure Put_Line(Item : in string) is
begin
    Check_Current_Output;
    Local_Put_Line(Current_Output, Item);
end Put_Line;

--| Put out word, padded to WIDTH
--| Evoke NEW_LINE if would reach line length
--| Raise LAYOUT_ERROR if exceeds line length
--| Raise MODE_ERROR if mode(file) /= out_file
--| Raise STATUS_ERROR if file not open


-------------------------------------------------------------
--    LOCAL SUBPROGRAMS USED BY SEVERAL BODIES
-------------------------------------------------------------

procedure RTS_LEX_FILE(FILE : FILE_TYPE;
            STATE_TABLE: System.RTS.Lexer.STATE_TRANSITIONS;
            CHAR_TO_TOKEN_TABLE: System.RTS.Lexer.CHAR_TO_TOKEN_MAPPING;
            WIDTH: FIELD;
            RESULT: out STRING; -- resulting lexical unit withOUT
                -- leading or trailing blanks from file
                -- value in result(result'first .. LAST)
            LAST  : out NATURAL  ------------------/\
            );
--+-----------------------------------------------------
--| Effect     RTS_LEX_FILE
--|   Lexes the next token in FILE according to STATE_TABLE and
--|   CHAR_TO_TOKEN_TABLE.  If the token is Accept according to the
--|   tables, then then token without leading or trailing blanks
--|   is returned in RESULT(RESULT'first .. LAST).  Otherwise,
--|   DATA_ERROR or END_ERROR are raised.
--|
--|   The effect is as specified for GET(file) procedures in enum_io,
--|   integer_io, fixed_io, and float_io: as to effect of WIDTH, when
--|   DATA_ERROR and END_ERROR are raised, and which character will be
--|   the next one read from the file.
--|   RESULT is modified even if an exception is raised (though LAST isn't
--|   due to semantics of OUT Scalar Parms.)

procedure RTS_LEX_FILE (FILE : FILE_TYPE;
                    STATE_TABLE: System.RTS.Lexer.STATE_TRANSITIONS;
                    CHAR_TO_TOKEN_TABLE: System.RTS.Lexer.CHAR_TO_TOKEN_MAPPING;
                    WIDTH: FIELD;
                    RESULT: out STRING; -- resulting lexical unit withOUT
                        -- trailing blanks from file
                        -- value in result(result'first .. LAST)
                    LAST  : out NATURAL  ------------------/\
                    )
is
    --+-----------------------------------------------------
    --| Overview
    --|   Lexes the next token in FILE according to STATE_TABLE and
    --|   CHAR_TO_TOKEN_TABLE.  If the token is Accept according to the
    --|   tables, then the token without leading or trailing blanks
    --|   is returned in RESULT(RESULT'first .. LAST).  Otherwise,
    --|   DATA_ERROR or END_ERROR are raised.
    --|
    --| Effect
    --|   The effect is as specified for GET(file) procedures in enum_io,
    --|   integer_io, fixed_io, and float_io: as to effect of WIDTH, when
    --|   DATA_ERROR and END_ERROR are raised, and which character will be
    --|   the next one read from the file.
    --|
    --| Algorithm
    --|   1. SKIP_BLANKS is called to skip spaces and tabs (and when width=0,
    --|      line and page terminators), such that a call to NEXT will return
    --|      the first non-blank character.
    --|   2. LEXER is called to "lex" the desired token, using NEXT.
    --|   3. NEXT returns a single "lookahead" character to LEXER each time it
    --|      is called, and also "consumes" the previous lookahead character
    --|      removing it from the file and storing it into RESULT.
    --|   4. If WIDTH/=0, fewer than WIDTH chars have been read, and the
    --|      file is not at end-of-line, then (a) the syntax is bad, and
    --|      (b) we need to read those remaining characters, or up to EOL.
    --|   5. DATA_ERROR is raised if the final state isn't OK; else LAST
    --|      is given its appropriate value.
    --|
    --| Requires
    --|   RESULT is a non-null string, big enough to hold the lexical item
    --|   when stripped of leading/trailing blanks. The FSA specified by
    --|   TABLEs are such that all accept transitions are made whenever a
    --|   charcater which can't be a legal part of the lexical element is
    --|   read. (NEXT returns a space when it reaches EOL or reads WIDTH
    --|   characters.)

    use System.RTS.Lexer;
--  dbg : constant boolean := false;
      -- add "constant" to entirely eliminate all debugging code.

    Num_Chars_Read : Natural := 0;
    Last_Index : Natural := RESULT'first - 1;  -- Index into result string
    Last_Char_To_Push_Back : Integer := -1;
    result_state : state;   -- result of LEXER

    function NEXT return character;

    function MY_LEXER is new LEXER (next => NEXT);

    procedure SKIP_BLANKS
    --+-----------------------------------------------------
    --| Overview
    --| Skips leading blanks (= ' ' or horizontal_tab), and if
    --|    width = 0, also line and page terminators.
    --| On completion, a call to NEXT will return the first non-blank
    --|
    --| Exceptions:
    --|   END_ERROR for width = 0 and only blanks and terminators remain
    --|     in file.
    --|   END_ERROR for width /= 0 only if positioned at end-of-file
    --|
    --|   DATA_ERROR for width /= 0 and all blanks WIDTH position, or up to
    --|        first EOL.
    is

        CHAR : Character;
        ch : Integer;
    begin
        loop
            ch := Get_Next_Char(FILE);
	    if ch < 0  then 
		-- EOF detected
		raise END_ERROR;
	    end if;
            File.Current_Column := File.Current_Column + 1;
            CHAR := Character'Val(ch);
            -- what about TABS
	    if CHAR /= ' ' and then CHAR /= ASCII.HT then
		-- Not a blank

		exit when CHAR /= ASCII.LF and then CHAR /= ASCII.FF;
		    -- Exit now, we have a non-blank, non-terminator

		-- Is a line/page terminator; skip it if Width = 0;
		-- raise DATA_ERROR otherwise since there is only
		-- white space.
		    
		if Width > 0 then
		    -- Put back the terminator and raise DATA_ERROR
		    ch := Push_Back_Char(ch, File);
		    raise DATA_ERROR;
		end if;
		-- Keep going 
	    end if;

	    -- Keep track of total characters read to compare against Width
            Num_Chars_Read := Num_Chars_Read + 1;
            if Num_Chars_Read = Width then
		-- Error, no non-blanks found
                raise DATA_ERROR;
            end if;

        end loop;

	-- Put back the non-blank character
	ch := Push_Back_Char(ch, File);

    end SKIP_BLANKS;

    function Next return character is
    --+-----------------------------------------------------
    --| Overview
    --|  "CONSUMES" the previous lookahead character and returns
    --|  the next lookahead character, incrementing Num_Chars_Read
    --|  each time a character is consumed and putting it into the
    --|  next position of RESULT
    --|
    --| Requires
    --|   SKIP_BLANKS is called before Next is called the first time.
    --|
    --|  Modifies
    --|    Num_Chars_Read, RESULT, and Last_Index
    --|    Last_Char_To_Push_Back if >= 0 on return should be
    --|     "pushed back" (i.e. passed to ungetc).

       Item : Character;

    begin

        if Width /= 0 and then Num_Chars_Read >= Width then
            -- End of width: Don't read any more, but return space
            -- so Lexer can ACCEPT, if string is OK
	    Last_Char_To_Push_Back := -1;  -- Nothing to push back
	    return ' ';
	end if;

	Last_Char_To_Push_Back := Get_Next_Char(File);
	if Last_Char_To_Push_Back < 0 then
	    -- EOF detected
	    if Num_Chars_Read > 0 then
		-- Some characters were read.
		-- Return a space as though there were a line terminator.
		return ' ';
	    end if;
	    -- Immediate EOF, this is an End_Error
	    raise END_ERROR;
	end if;
	Item := Character'Val(Last_Char_To_Push_Back);
	if Item = ASCII.LF or Item = ASCII.FF then
	    -- Line/page terminator, return a space.
	    Last_Char_To_Push_Back := Push_Back_Char(Last_Char_To_Push_Back,
	      File);
	    Last_Char_To_Push_Back := -1;
	    return ' ';
	end if;

        -- "Normal" character
	Num_Chars_Read := Num_Chars_Read + 1;
	Last_Index := Last_Index + 1;
	RESULT (Last_Index) := Item;

        return (Item);
    end Next;
   
    ch : Integer;
    Char : Character;

begin
        if Mode(File)     -- will raise STATUS_ERROR if FILE not open
           /= In_File then
            raise MODE_ERROR;
        end if;

        SKIP_BLANKS;

        Result_State := MY_LEXER (state_table, char_to_token_table);
	if Last_Char_To_Push_Back >= 0 then
	    -- Put back character which caused completion
	    ch := Push_Back_Char(Last_Char_To_Push_Back, File);

	    -- Reduce index and count
	    Last_Index := Last_Index - 1;
	    Num_Chars_Read := Num_Chars_Read - 1;
	end if;

--      if dbg then --dbg_on
--        text_io.new_line;
--        text_io.put_line("lex_file: after calling lexer. width, num_char," &
--          "last_index, result_state");
--        text_io.put(integer'image(width));
--        text_io.put(integer'image(num_chars_read));
--        text_io.put(integer'image(last_index));
--        text_io.put_line(state'image(result_state));
--      end if;     --dbg_off

          -- For the case of width /= 0, check that WIDTH chars were
          -- read (or that EOL) was hit.  If not then READ remaining
          -- width chars up to EOL, and then raise DATA_ERROR
          -- because it is required that the ENTIRE width number of
          -- characters satisfy the syntax table: if some chars are left
          -- then there must be trailing blanks and other characters

        if width > Num_Chars_Read then
            for i in Num_Chars_Read + 1 .. WIDTH loop
	        ch := Get_Next_Char(File);
	        exit when ch < 0; -- EOF detected
                Char := Character'Val(ch);
                if Char = ASCII.LF or else Char = ASCII.FF then
		    -- Push back the terminator
		    ch := Push_Back_Char(ch, File);

		    exit;
		end if;
		-- Unless first char we encounter is a terminator, we have
		-- a data error.
		Result_State := System.RTS.Lexer.Error;
            end loop;
        end if;

        -- Now check on final state of LEXER
        if Result_State = Accepted then
            LAST := Last_Index;
        else
            raise DATA_ERROR;
        end if;

end RTS_LEX_FILE;


procedure PUT_WORD( FILE  : in FILE_TYPE;
                    WORD  : in STRING;
                    WIDTH : in FIELD    ) is
    LENGTH : constant Integer := WORD'Length;
begin
    --| Algorithm
    --| If line is bounded, and output would go
    --| past end-of-line, then break line here.
    --| In any case, output the word and the padding spaces (if any).

    Check_Mode_Out_Or_Append(File);

    if FILE.Line_Length /= UNBOUNDED then
        -- Line is bounded,Check for LAYOUT_ERROR
        if WIDTH > INTEGER(FILE.Line_Length) or else
            LENGTH > INTEGER(FILE.Line_Length) then
            raise LAYOUT_ERROR;
        end if;
         -- Calculate room left
        declare
            Room_Left : INTEGER := INTEGER(
                            FILE.Line_Length - FILE.Current_Column + 1);
        begin
            if WIDTH > Room_Left or else LENGTH > Room_Left then
                -- Output is too fat, break the line here
                Local_New_Line(FILE, 1);
            end if;
        end; -- declare
    end if;

    -- put out the word
    PUT(FILE, WORD);

    -- pad with spaces
    for I in LENGTH+1 .. WIDTH loop
        PUT(FILE, ' ');
    end loop;
end PUT_WORD;


procedure Read (File : in out File_Record;
                Item : out AS.Stream_Element_Array;
                Last : out AS.Stream_Element_Offset) is
    Amount_Read : Natural;
    use type AS.Stream_Element_Offset;
begin
    Check_Mode_In(File.Self);
 
    if c_feof(File.CFile) = 1 then
        Last := Item'First-1;
        return;              --| End-of-file already encountered
    end if;
    for i in Item'RANGE loop
        -- Read used when Stream_IO file is treated directly as Stream
        begin
            Amount_Read := c_fread(Item(i)'Address, 1, 1, File.CFile);
               
        exception
                when End_Error => Amount_Read := 0;
        end;

        if Amount_Read = 0 then
            Last := I-1;
            return;
        end if;
    end loop;
    Last := Item'last;
end Read;

procedure Write (File : in out File_Record;
                 Item : in AS.Stream_Element_Array) is
    Amount_Written : Natural;
begin
    Check_Mode_Out_Or_Append(File.Self);
    for i in Item'RANGE loop
        -- Write used when Stream_IO file is treated directly as Stream
        Amount_Written := c_fwrite (Item(i)'Address, 1, 1, File.CFile);
        if Amount_Written /= 1 then
            raise Internal_Error;
        end if;
    end loop;
end Write;

-------------------------------------------------------------
--        INTEGER_IO_SUPPORT     SPEC
-------------------------------------------------------------

package INTEGER_IO_SUPPORT is

--| Overview
--|   INTEGER_IO_SUPPORT is the same as an instantiation of
--| INTEGER_IO for type INTEGER, except GET/PUT using default
--| files are missing.

-------------------------------------------------------------
    procedure GET(FILE  : in FILE_TYPE;
                  ITEM  : out TGT.LONGEST_INTEGER;
                  WIDTH : in FIELD);
-------------------------------------------------------------
    procedure PUT(FILE : in FILE_TYPE;
                  ITEM : in TGT.LONGEST_INTEGER;
                  WIDTH : in FIELD;
                  BASE  : in NUMBER_BASE);
-------------------------------------------------------------

    procedure GET(FROM : in STRING;
                  ITEM : out TGT.LONGEST_INTEGER;
                  LAST : out POSITIVE);
-------------------------------------------------------------

    procedure PUT(TO   : out STRING;
                  ITEM : in TGT.LONGEST_INTEGER;
                  BASE  : in NUMBER_BASE);

end INTEGER_IO_SUPPORT;


-------------------------------------------------------------
--        INTEGER_IO_SUPPORT     BODY
-------------------------------------------------------------


package body INTEGER_IO_SUPPORT is

-------------------------------------------------------------
--           LOCAL DECLARATIONS
-------------------------------------------------------------


function INTEGER_IMAGE ( --| returns integer image
                         ITEM : TGT.LONGEST_INTEGER;
                         WIDTH : INTEGER;
                         BASE  : INTEGER)
    return STRING;

int_to_char : constant string (1 .. 16) := "0123456789ABCDEF";
  -- int_to_char(i+1) is the hex_char for i
  -- used in INTEGER_IMAGE

--

-------------------------------------------------------------
    procedure GET(FILE  : in FILE_TYPE;
                  ITEM  : out TGT.LONGEST_INTEGER;
                  WIDTH : in FIELD)
    --| Effect
    --|   Lexes the next token from FILE using the syntax of
    --| integer literals, and converts it to an integer,
    --| putting the value into ITEM.  If WIDTH /= 0, then
    --| exactly that number of characters (or up to end-
    --| of-line) are used for the token; otherwise only
    --| those characters necessary to make up an integer
    --| are read.
    --|
    --| Algorithm
    --|   Use RTS_LEX_FILE to LEX the next integer token
    --| via Integer transition tables into BUFFER(1..last).
    --|   Generate the integer value via INTEGER'VALUE,
    --| converting any constraint error into DATA_ERROR.

    is
        buffer : string (1 .. 256); -- big enough for non-blank part of
                                    -- any integer
        last : natural;
    begin
        RTS_LEX_FILE (FILE,
                      System.RTS.Integer_State_Tables.Transitions,
                      System.RTS.Integer_State_Tables.Mapping,
                      WIDTH,
                      buffer,
                      last);
        --  Protect against constraint_error when converting to int, and
        -- change it to DATA_ERROR
        begin
            -- Since ITEM already lexed and legal, use a direct conversion
            -- with no check. ('value does check)    Offer
            ITEM := TGT.Longest_Integer(
                      System.RTS.Integer_Value_Pkg.RTS_Integer_Value(
                        BUFFER(BUFFER'first .. last)));
        exception
            when CONSTRAINT_ERROR =>
                raise DATA_ERROR;
        end;
    end GET;


--|
-------------------------------------------------------------
    procedure PUT(FILE : in FILE_TYPE;
                  ITEM : in TGT.LONGEST_INTEGER;
                  WIDTH : in FIELD;
                  BASE  : in NUMBER_BASE)
    is
    --+-----------------------------------------------------
    --| Overview
    --|   Uses INTEGER_IMAGE to generate a string containing the
    --| necessary WIDTH and BASE.  PUT_WORD then outputs
    --| the string to the file, performing NEW_LINE, if necessary

    begin

        PUT_WORD (file => FILE,
                  word => INTEGER_IMAGE (item, width, base),
                  width => 0 ); -- no padding needed

    end PUT;  -- file

    --|
    -------------------------------------------------------------

    procedure GET(FROM : in STRING;
                  ITEM : out TGT.LONGEST_INTEGER;
                  LAST : out POSITIVE)
    --| Effect
    --|   Lexes the first token from FROM using the syntax of integer
    --|   literals, skips leading spaces, then converts it to an integer,
    --|   putting the value into ITEM. LAST is set to the index of the
    --|   last character used.
    --|
    --| Algorithm
    --|   Use RTS_LEX_STRING to LEX the next integer token via
    --|   Integer-Transition tables, setting My_Last. Skipping leading
    --|   spaces and then converting to integer using RTS_Integer_Value.
    --|   Convert any constraint error into DATA_ERROR.
    is
        My_Last   : natural;  -- Need local so the return value from
                              -- lex_string can be used in slice.
        My_First  : natural := From'First;  -- The first non-space character.
    begin
        System.RTS.Lexer.RTS_LEX_STRING(FROM,
                       System.RTS.Integer_State_Tables.Transitions,
                       System.RTS.Integer_State_Tables.Mapping,
                       MY_Last);
        LAST := My_Last;
        begin
            for i in FROM'first..My_Last loop
                -- Skip leading spaces
                My_First := i;
                exit when (FROM(i) /= ' ' and
                           FROM(i) /= ASCII.HT);
            end loop;

            -- Since ITEM already lexed and legal, use a direct conversion
            -- with no check. ('value does check)    Offer
            ITEM := TGT.Longest_Integer(
                      System.RTS.Integer_Value_Pkg.RTS_Integer_Value(
                        FROM(My_first .. My_Last)));
        exception
            when CONSTRAINT_ERROR =>
                raise DATA_ERROR;
        end;
    end GET;  -- string
--|
-------------------------------------------------------------

    procedure PUT(TO   : out STRING;
                  ITEM : in TGT.LONGEST_INTEGER;
                  BASE  : in NUMBER_BASE)
    is
    --+-----------------------------------------------------
    --| Overview
    --| INTEGER_IMAGE is employed to generate a string from ITEM,
    --| using TO'length as the WIDTH. However, INTEGER_IMAGE is required
    --| to override WIDTH if more room is needed.  Thus it is necessary
    --| to see if the returned string is too big before assigning it into
    --| TO.

   put_integer_string_internal_error : exception;
     -- raised if somehow the item_image is SMALLER than TO,
     -- implying integer_image didn't pad with blanks

       item_image : constant string :=
         INTEGER_IMAGE (item, +TO'length, base);

       diff : constant integer := ITEM_IMAGE'length - TO'length;

   begin
       -- Check that ITEM_IMAGE fits in TO

       if diff = 0 then        -- Same Size
           TO := ITEM_IMAGE;

       elsif diff > 0 then     -- Item_Image is bigger.
           raise LAYOUT_ERROR;

       else  -- TO is longer than ITEM_IMAGE. But then
             -- INTEGER_IMAGE didn't pad with enough blanks!
             raise put_integer_string_internal_error;
       end if;
    end PUT; -- string
--

-------------------------------------------------------------
-- Source: /usr9/mdavis/io/RCS/int_imb.bdy,v
-- Revision: 1.1  -- Date: 85/08/20 10:48:38  -- Author: mdavis

--------------------------------------------------------------
function INTEGER_IMAGE ( --| returns integer image
                         ITEM : TGT.LONGEST_INTEGER;
                         WIDTH : INTEGER;
                         BASE  : INTEGER)
    return STRING
is

--| Effect
--|   Returns a string with the IMAGE of ITEM.  If BASED /= 10, then
--| it has the syntax of a based number, with 1 or 2 digits for the
--| base.  There is a '-' sign if ITEM is negative, otherwise no
--| character position is reserved for a positive number.
--|   If the so-defined number consumes fewer than WIDTH positions, then
--| it is padded with leading spaces.  If WIDTH is too small, then WIDTH
--| is ignored.
--|
--| Algorithm
--|   Accumulate the result string BACKWARDS.  X holds the NEGATIVE magnitude
--| value remaining to be output. (Negative is used because it can handle
--| integer'first, which cannot be done using positive values for twos
--| complement machines.)
--|
--| Requires
--|   BASE is in range 2 .. 16.  Since this routine is a "helping" routine,
--|   PUT(file) and PUT(string) have already checked that base is correct.

    subtype index_subtype is positive range 1 .. 75;
        -- big enough for 64-bit integer in base 2

    index : index_subtype := index_subtype'last;  -- current position
      -- in BUF where next char should be put.

    buf : string (index_subtype);

    X : TGT.Longest_Integer := ITEM;

    use type TGT.Longest_Integer;

    procedure ADD (C : character)
    --+-----------------------------------------------------
    --| Overview
    --|   Adds C to buf(index), and decrements index by 1
    --| Note BUF is filled from back to front!
    is
    begin
        buf(index) := c;
        index := index - 1;
    end;

begin
    if X > 0 then X := - X; end if;  -- keep X non-positive

    if base /= 10 then
        add (ASCII.Sharp);
    end if;

    for i in reverse index_subtype range buf'first .. index loop

        buf(i) := int_to_char(integer(1 - (X rem TGT.Longest_Integer(BASE))));
          -- X is negative, so we want
          -- to negate the result, because: (-X) rem BASE == -(X rem BASE)

        X := X / TGT.Longest_Integer(BASE);

        if X >= 0 then  -- (we do test here and use an EXIT instead of
                        -- a WHILE loop in order to handle the case of
                        -- ITEM = 0.)
            index := i - 1; -- set index to next avail pos in buf
            exit;
        end if;
    end loop;

    if base /= 10 then
        add (ASCII.Sharp);
        add (int_to_char (1 + (BASE rem 10)));
            -- "ones" place for BASE's value
        if base > 10 then
            add ('1');
        end if;  -- "tens" place
    end if;

    if ITEM < 0 then
        add ('-');
    end if;

    -- Result is in buf (index + 1 .. buf'last)
    -- Tack on (width - (buf'last - index)) leading spaces, if necessary

    return (1 .. (width - (buf'last - index)) => ' ') &
           buf (index + 1 .. buf'last);

end INTEGER_IMAGE;

end INTEGER_IO_SUPPORT;


-------------------------------------------------------------
--             INTEGER_IO : BODY
-------------------------------------------------------------

package body INTEGER_IO is

subtype NUM_RANGE is     --range of NUM in type integer
TGT.Longest_Integer range TGT.Longest_Integer(NUM'first) ..
                           TGT.Longest_Integer(NUM'last);
                     --used in range check of resulting integer


procedure GET( FILE : in FILE_TYPE;
               ITEM : out NUM;
               WIDTH : in FIELD := 0)  is
    INTEGER_ITEM : TGT.Longest_Integer;
begin
    INTEGER_IO_SUPPORT.GET (FILE, INTEGER_ITEM, WIDTH);

    if INTEGER_ITEM in NUM_RANGE then
        ITEM := NUM (INTEGER_ITEM);
    else
    -- If the result is not within the instantiated subtype, then
    -- DATA_ERROR must be raised.  If the value is within NUM,
    -- but outside range of ITEM's subtype, then CONSTRAINT_ERROR
    -- will be raised on return.
        raise DATA_ERROR;
    end if;
end GET;


procedure GET( ITEM : out NUM;
               WIDTH : in FIELD := 0 ) is
begin
    Check_Current_Input;
    Get(CURRENT_INPUT, ITEM, WIDTH);
end GET;


procedure PUT( FILE  : in FILE_TYPE;
               ITEM  : in NUM;
               WIDTH : in FIELD := default_width;
               BASE  : in NUMBER_BASE := default_base ) is
begin
    INTEGER_IO_SUPPORT.PUT (FILE, TGT.LONGEST_INTEGER (ITEM), WIDTH, BASE);
end PUT;


procedure PUT( ITEM : in NUM;
               WIDTH : in FIELD := default_width;
               BASE : in NUMBER_BASE := default_base ) is
begin
    Check_Current_Output;
    PUT( CURRENT_OUTPUT, ITEM, WIDTH, BASE);
end PUT;


procedure GET( FROM : in STRING;
               ITEM : out NUM;
               LAST : out POSITIVE ) is
    INTEGER_ITEM : TGT.Longest_integer;
begin
    INTEGER_IO_SUPPORT.GET (FROM, INTEGER_ITEM, LAST);

    if INTEGER_ITEM in NUM_RANGE then
        ITEM := NUM (INTEGER_ITEM);
    else
    -- If the result is not within the instantiated subtype, then
    -- DATA_ERROR must be raised.  If the value is within NUM,
    -- but outside range of ITEM's subtype, then CONSTRAINT_ERROR
    -- will be raised on return.
        raise DATA_ERROR;
    end if;
end GET;


procedure PUT( TO : out STRING;
               ITEM : in NUM;
               BASE : in NUMBER_BASE := default_base ) is
begin
   INTEGER_IO_SUPPORT.PUT (TO, TGT.LONGEST_INTEGER (ITEM), BASE);
end PUT;

end INTEGER_IO;

-------------------------------------------------------------
--        MODULAR_IO_SUPPORT     SPEC
-------------------------------------------------------------

package MODULAR_IO_SUPPORT is

--| Overview
--|   MODULAR_IO_SUPPORT is the same as an instantiation of
--| MODULAR_IO for MODULAR types, except GET/PUT using default
--| files are missing.

-------------------------------------------------------------
    procedure GET(FILE  : in FILE_TYPE;
                  ITEM  : out TGT.Longest_Modular;
                  WIDTH : in FIELD);
-------------------------------------------------------------
    procedure PUT(FILE : in FILE_TYPE;
                  ITEM : in TGT.Longest_Modular;
                  WIDTH : in FIELD;
                  BASE  : in NUMBER_BASE);
-------------------------------------------------------------

    procedure GET(FROM : in STRING;
                  ITEM : out TGT.Longest_Modular;
                  LAST : out POSITIVE);
-------------------------------------------------------------

    procedure PUT(TO   : out STRING;
                  ITEM : in TGT.Longest_Modular;
                  BASE  : in NUMBER_BASE);

end MODULAR_IO_SUPPORT;


-------------------------------------------------------------
--        MODULAR_IO_SUPPORT     BODY
-------------------------------------------------------------


package body MODULAR_IO_SUPPORT is

-------------------------------------------------------------
--           LOCAL DECLARATIONS
-------------------------------------------------------------


function MODULAR_IMAGE ( --| returns image
                         ITEM : TGT.Longest_Modular;
                         WIDTH : INTEGER;
                         BASE  : INTEGER)
    return STRING;

int_to_char : constant string (1 .. 16) := "0123456789ABCDEF";
  -- int_to_char(i+1) is the hex_char for i
  -- used in MODULAR_IMAGE

--

-------------------------------------------------------------
    procedure GET(FILE  : in FILE_TYPE;
                  ITEM  : out TGT.Longest_Modular;
                  WIDTH : in FIELD)
    --| Effect
    --|   Lexes the next token from FILE using the syntax of
    --| modular type literals, and converts it to a modular type,
    --| putting the value into ITEM.  If WIDTH /= 0, then
    --| exactly that number of characters (or up to end-
    --| of-line) are used for the token; otherwise only
    --| those characters necessary to make up a modular type
    --| are read.
    --|
    --| Algorithm
    --|   Use RTS_LEX_FILE to LEX the next modular token
    --| via modular transition tables into BUFFER(1..last).
    --|   Generate the modular value via RTS_Unsigned_Value,
    --| converting any constraint error into DATA_ERROR.

    is
        buffer : string (1 .. 256); -- big enough for non-blank part of
                                    -- any integer
        last : natural;
    begin
        RTS_LEX_FILE (FILE,
                      System.RTS.Integer_State_Tables.Transitions,
                      System.RTS.Integer_State_Tables.Unsigned_Mapping,
                      WIDTH,
                      buffer,
                      last);
        --  Protect against constraint_error when converting to int, and
        -- change it to DATA_ERROR
        begin
            -- Since ITEM already lexed and legal, use a direct conversion
            -- with no check. ('value does check)    Offer
            ITEM := System.RTS.Integer_Value_Pkg.RTS_Unsigned_Value(
                        BUFFER(BUFFER'first .. last));
        exception
            when CONSTRAINT_ERROR =>
                raise DATA_ERROR;
        end;
    end GET;


--|
-------------------------------------------------------------
    procedure PUT(FILE : in FILE_TYPE;
                  ITEM : in TGT.Longest_Modular;
                  WIDTH : in FIELD;
                  BASE  : in NUMBER_BASE)
    is
    --+-----------------------------------------------------
    --| Overview
    --|   Uses MODULAR_IMAGE to generate a string containing the
    --| necessary WIDTH and BASE.  PUT_WORD then outputs
    --| the string to the file, performing NEW_LINE, if necessary

    begin

        PUT_WORD (file => FILE,
                  word => MODULAR_IMAGE (item, width, base),
                  width => 0 ); -- no padding needed

    end PUT;  -- file

    --|
    -------------------------------------------------------------

    procedure GET(FROM : in STRING;
                  ITEM : out TGT.Longest_Modular;
                  LAST : out POSITIVE)
    --| Effect
    --|   Lexes the first token from FROM using the syntax of integer
    --|   literals, skips leading spaces, then converts it to an integer,
    --|   putting the value into ITEM. LAST is set to the index of the
    --|   last character used.
    --|
    --| Algorithm
    --|   Use RTS_LEX_STRING to LEX the next integer token via
    --|   Integer-Transition tables, setting My_Last. Skipping leading
    --|   spaces and then converting to integer using RTS_Unsigned_Value.
    --|   Convert any constraint error into DATA_ERROR.
    is
        My_Last   : natural;  -- Need local so the return value from
                              -- lex_string can be used in slice.
        My_First  : natural := From'First;  -- The first non-space character.
    begin
        System.RTS.Lexer.RTS_LEX_STRING(FROM,
                       System.RTS.Integer_State_Tables.Transitions,
                       System.RTS.Integer_State_Tables.Unsigned_Mapping,
                       MY_Last);
        LAST := My_Last;
        begin
            for i in FROM'first..My_Last loop
                -- Skip leading spaces
                My_First := i;
                exit when (FROM(i) /= ' ' and
                           FROM(i) /= ASCII.HT);
            end loop;

            -- Since ITEM already lexed and legal, use a direct conversion
            -- with no check. ('value does check)    Offer
            ITEM := System.RTS.Integer_Value_Pkg.RTS_Unsigned_Value(
                        FROM(My_first .. My_Last));
        exception
            when CONSTRAINT_ERROR =>
                raise DATA_ERROR;
        end;
    end GET;  -- string
--|
-------------------------------------------------------------

    procedure PUT(TO   : out STRING;
                  ITEM : in TGT.Longest_Modular;
                  BASE  : in NUMBER_BASE)
    is
    --+-----------------------------------------------------
    --| Overview
    --| MODULAR_IMAGE is employed to generate a string from ITEM,
    --| using TO'length as the WIDTH. However, MODULAR_IMAGE is required
    --| to override WIDTH if more room is needed.  Thus it is necessary
    --| to see if the returned string is too big before assigning it into
    --| TO.

   put_integer_string_internal_error : exception;
     -- raised if somehow the item_image is SMALLER than TO,
     -- implying integer_image didn't pad with blanks

       item_image : constant string :=
         MODULAR_IMAGE (item, +TO'length, base);

       diff : constant integer := ITEM_IMAGE'length - TO'length;

   begin
       -- Check that ITEM_IMAGE fits in TO

       if diff = 0 then        -- Same Size
           TO := ITEM_IMAGE;

       elsif diff > 0 then     -- Item_Image is bigger.
           raise LAYOUT_ERROR;

       else  -- TO is longer than ITEM_IMAGE. But then
             -- MODULAR_IMAGE didn't pad with enough blanks!
             raise put_integer_string_internal_error;
       end if;
    end PUT; -- string

--------------------------------------------------------------

function MODULAR_IMAGE ( --| returns image
                         ITEM  : TGT.Longest_Modular;
                         WIDTH : INTEGER;
                         BASE  : INTEGER)
    return STRING
is

--| Effect
--|   Returns a string with the IMAGE of ITEM.  If BASED /= 10, then
--| it has the syntax of a based number, with 1 or 2 digits for the
--| base.  There is a '-' sign if ITEM is negative, otherwise no
--| character position is reserved for a positive number.
--|   If the so-defined number consumes fewer than WIDTH positions, then
--| it is padded with leading spaces.  If WIDTH is too small, then WIDTH
--| is ignored.
--|
--| Algorithm
--|   Accumulate the result string BACKWARDS.  X holds the NEGATIVE magnitude
--| value remaining to be output. (Negative is used because it can handle
--| integer'first, which cannot be done using positive values for twos
--| complement machines.)
--|
--| Requires
--|   BASE is in range 2 .. 16.  Since this routine is a "helping" routine,
--|   PUT(file) and PUT(string) have already checked that base is correct.

    subtype index_subtype is positive range 1 .. 75;
        -- big enough for 64-bit integer in base 2

    index : index_subtype := index_subtype'last;  -- current position
      -- in BUF where next char should be put.

    buf : string (index_subtype);

    X : TGT.Longest_Modular := ITEM;

    use type TGT.Longest_Modular;

    procedure ADD (C : character)
    --+-----------------------------------------------------
    --| Overview
    --|   Adds C to buf(index), and decrements index by 1
    --| Note BUF is filled from back to front!
    is
    begin
        buf(index) := c;
        index := index - 1;
    end;

begin

    if base /= 10 then
        add (ASCII.Sharp);
    end if;

    for i in reverse index_subtype range buf'first .. index loop

        buf(i) := int_to_char(
                     integer(TGT."rem"(X, TGT.Longest_Modular(BASE))) + 1);

        if TGT."<"(X, TGT.Longest_Modular(BASE)) then
            index := i - 1; -- set index to next avail pos in buf
            exit;
        end if;

        X := TGT."/"(X, TGT.Longest_Modular(BASE));
    end loop;

    if base /= 10 then
        add (ASCII.Sharp);
        add (int_to_char (1 + (BASE rem 10)));
            -- "ones" place for BASE's value
        if base > 10 then
            add ('1');
        end if;  -- "tens" place
    end if;

    -- Result is in buf (index + 1 .. buf'last)
    -- Tack on (width - (buf'last - index)) leading spaces, if necessary

    return (1 .. (width - (buf'last - index)) => ' ') &
           buf (index + 1 .. buf'last);

end MODULAR_IMAGE;

end MODULAR_IO_SUPPORT;

package body Modular_IO is

--range of NUM used in range check of resulting modular type
subtype NUM_RANGE is TGT.Longest_Modular range
        TGT.Longest_Modular(NUM'first) ..  TGT.Longest_Modular(NUM'last);

MODULAR_ITEM : TGT.Longest_Modular;

procedure GET( FILE : in  File_Type;
               ITEM : out NUM;
               WIDTH : in FIELD := 0 ) is

begin
    MODULAR_IO_SUPPORT.GET (FILE, MODULAR_ITEM, WIDTH);
    if MODULAR_ITEM in NUM_RANGE then
        ITEM := NUM (MODULAR_ITEM);
    else
        raise DATA_ERROR;
    end if;
end GET;

procedure GET( ITEM: out NUM;
               WIDTH : in FIELD := 0 ) is
begin
    Check_Current_Input;
    Get(Current_Input, ITEM, WIDTH);
end GET;

procedure PUT( FILE  : in File_Type;
               ITEM  : in NUM;
               WIDTH : in FIELD := DEFAULT_WIDTH;
               BASE  : in NUMBER_BASE := DEFAULT_BASE ) is
begin
    MODULAR_IO_SUPPORT.PUT (FILE, TGT.Longest_Modular(ITEM), WIDTH, BASE);
end PUT;

procedure PUT( ITEM  : in NUM;
               WIDTH : in FIELD := DEFAULT_WIDTH;
               BASE  : in NUMBER_BASE := DEFAULT_BASE ) is
begin
    Check_Current_Output;
    PUT( CURRENT_OUTPUT, ITEM, WIDTH, BASE);
end PUT;

procedure GET( FROM : in String;
               ITEM : out NUM;
               LAST : out Positive ) is
begin
    MODULAR_IO_SUPPORT.GET (FROM, MODULAR_ITEM, LAST);
    if MODULAR_ITEM in NUM_RANGE then
        ITEM := NUM (MODULAR_ITEM);
    else
        raise DATA_ERROR;
    end if;
end GET;

procedure PUT( TO    : out String;
               ITEM  : in NUM;
               BASE  : in NUMBER_BASE := DEFAULT_BASE ) is
begin
    MODULAR_IO_SUPPORT.PUT (TO, TGT.Longest_Modular(ITEM), BASE);
end PUT;

end Modular_IO;


-------------------------------------------------------------
--        FLOAT_IO_SUPPORT
-------------------------------------------------------------

package FLOAT_IO_SUPPORT is

--| Overview
--|   FLOAT_IO_SUPPORT is like an instantiation of FLOAT_IO
--| for type FLOAT.  (The only missing routines are GET/PUT
--| with default files.)  We use LONG_FLOAT on RISCAE, so 32 bit
--| float is converted to 64 bit float before processing.

-------------------------------------------------------------

    procedure GET(FILE  : in FILE_TYPE;
                  ITEM  : out TGT.LONGEST_FLOAT;
                  WIDTH : in FIELD);

-------------------------------------------------------------
    procedure PUT (FILE : in FILE_TYPE;
                   ITEM : in TGT.LONGEST_FLOAT;
                   FORE : in FIELD;
                   AFT  : in FIELD;
                   EXP  : in FIELD);

-------------------------------------------------------------

    procedure GET(FROM : in STRING;
                  ITEM : out TGT.LONGEST_FLOAT;
                  LAST : out POSITIVE);

-------------------------------------------------------------

    procedure PUT (TO   : out STRING;
                   ITEM : in TGT.LONGEST_FLOAT;
                   AFT  : in FIELD;
                   EXP  : in FIELD);

end FLOAT_IO_SUPPORT;


-------------------------------------------------------------
--        FLOAT_IO_SUPPORT     BODY
-------------------------------------------------------------

package body FLOAT_IO_SUPPORT is

-------------------------------------------------------------

    procedure GET(FILE  : in FILE_TYPE;
                  ITEM  : out TGT.LONGEST_FLOAT;
                  WIDTH : in FIELD)

    --| Effect
    --|   Lexes the next token from FILE using the syntax of
    --| real literals, and converts it to a float,
    --| putting the value into ITEM.  If WIDTH /= 0, then
    --| exactly that number of characters (or up to end-
    --| of-line) are used for the token; otherwise only
    --| those characters necessary to make up a real
    --| are read.
    --|
    --| Algorithm
    --|   Use RTS_LEX_FILE to LEX the next real token
    --|   via REAL transition tables into BUFFER(1..last).
    --|   Generate the integer value via FLOAT_VALUE,
    --|   converting any constraint error into DATA_ERROR.

    is
        buffer : string (1 .. 512); -- big enough for non-blank part of
                                    -- any real
        last : natural;
    begin
        RTS_LEX_FILE (FILE,
                      FIO.FLOAT_TRANSIT.Transitions,
                      FIO.FLOAT_TRANSIT.Mapping,
                      WIDTH,
                      buffer,
                      last);
        --  Protect against constraint_error when converting to int, and
        -- change it to DATA_ERROR
        begin
            ITEM := FIO.FLOAT_VALUE (BUFFER(BUFFER'first .. last));
        exception
            when CONSTRAINT_ERROR =>
                raise DATA_ERROR;
        end;
    end GET;  -- file

--


-------------------------------------------------------------
    procedure PUT (FILE : in FILE_TYPE;
                   ITEM : in TGT.LONGEST_FLOAT;
                   FORE : in FIELD;
                   AFT  : in FIELD;
                   EXP  : in FIELD)
    is
    --+-----------------------------------------------------
    --| Overview
    --|   Uses FLOAT_IMAGE to generate a string containing the
    --| necessary FORE, AFT, EXP fields.  PUT_WORD then outputs
    --| the string to the file, performing NEW_LINE, if necessary

    begin

        PUT_WORD (file => FILE,
                  word => FIO.FLOAT_IMAGE (item, fore, aft, exp),
                  width => 0 ); -- no padding needed

    end PUT;  -- file

    -------------------------------------------------------------

    procedure GET(FROM : in STRING;
                  ITEM : out TGT.LONGEST_FLOAT;
                  LAST : out POSITIVE)
    --| Effect
    --|   Lexes the first token from FROM using the syntax of
    --| REAL literals, and converts it to a float,
    --| putting the value into ITEM. LAST is set to the
    --| index of the last character used.
    --|
    --| Algorithm
    --|   Use RTS_LEX_STRING to LEX the next real token
    --|   via Real transition tables, setting My_Last.
    --|   Generate the float value via FLOAT_VALUE,
    --|   converting any constraint error into DATA_ERROR.
    is
        My_Last   : natural;  -- Need local so the return value from
                              -- lex_string can be used in slice.
    begin
        System.RTS.Lexer.RTS_LEX_STRING(FROM,
                       FIO.FLOAT_TRANSIT.Transitions,
                       FIO.FLOAT_TRANSIT.Mapping,
                       My_Last);
        LAST := My_Last;
        begin
            ITEM := FIO.FLOAT_VALUE (FROM(FROM'first .. My_Last));
        exception
            when CONSTRAINT_ERROR =>
                raise DATA_ERROR;
        end;
    end GET;  -- string

    -------------------------------------------------------------

    procedure PUT (TO   : out STRING;
                   ITEM : in TGT.LONGEST_FLOAT;
                   AFT  : in FIELD;
                   EXP  : in FIELD)
    is
    --+-----------------------------------------------------
    --| Algorithm
    --| FLOAT_IMAGE is employed to generate a string from ITEM.
    --| However, first a value for FORE should be computed.
    --| This isn't strictly necessary, since float_image is required
    --| to override sizes of FORE and EXP if more room is needed.
    --| However, guessing "right" means the result can be put directly
    --| into TO; otherwise it is necessary to check that a tail portion
    --| of the string can be assigned into TO such that the head portion is
    --| ONLY spaces. This could happen in the following case:
    --|   exp=1, and a value of 20 was computed for FORE.
    --|   You always need at least 2 for exp, so that is
    --|   1 unexpected character needed, and the returned
    --|   string will be LONGER than TO.  However, FORE
    --|   was very generous (since if exp/=0, at most 2
    --|   positions are needed for FORE), so there are
    --|   at least 18 leading blanks that can be eliminated.

        FORE : integer;
        PUT_AFT : integer;
        PUT_EXP : integer;

         put_float_string_internal_error : exception;
           -- raised if somehow the item_image is SMALLER than TO,
           -- implying this computation of FORE is bad.

     begin

         if AFT = 0 then
             PUT_AFT := 1;
         else
             PUT_AFT := AFT;
         end if;

         if EXP = 1 then
             PUT_EXP := 2;
         else
             PUT_EXP := EXP;
         end if;

         -- FORE = string_length - specified_portions
         FORE := TO'length -
                 ( 1              -- '.'
                 + PUT_AFT        -- fraction
                 + boolean'pos(PUT_EXP /= 0) -- 'E' plus sign
                                             -- if there will be an exponent
                 + PUT_EXP );           -- sign and digits of exponent

         -- TO may not have been big enough, so FORE might be negative,
         -- it might not belong to subtype FIELD, so LAYOUT_ERROR should
         -- be raised

         if FORE not in FIELD then
             raise LAYOUT_ERROR;
         end if;

         declare
             -- All parms are prepared, so convert item to a string

             item_image : constant string :=
               FIO.FLOAT_IMAGE (item, fore, put_aft, put_exp);

             diff : constant integer := ITEM_IMAGE'length - TO'length;

         begin
             -- Check that ITEM_IMAGE fits in TO, or that all omitted
             -- leading characters are blanks

             if diff = 0 then
                 -- Same Size
                 TO := ITEM_IMAGE;

             elsif diff > 0 then
                 -- Item_Image is bigger.  Check that first diff
                 -- characters are blanks

                 if ITEM_IMAGE(ITEM_IMAGE'first ..
                               ITEM_IMAGE'first + diff -1) =
                    (1 .. diff-1 => ' ') then
                     -- It's OK, so assign a slice to TO
                     TO := ITEM_IMAGE (ITEM_IMAGE'first + diff ..
                                       ITEM_IMAGE'last);
                 else
                     -- Nope, doesn't fit!
                     raise LAYOUT_ERROR;
                 end if;

             else  -- TO is longer than ITEM_IMAGE. But we computed a
                   -- value such that this couldn't happen!
                   raise put_float_string_internal_error;
             end if;
         end;  -- declare

         return;
    end PUT; -- string

end FLOAT_IO_SUPPORT;


-------------------------------------------------------------
--          FLOAT_IO : BODY
-------------------------------------------------------------

package body FLOAT_IO is

subtype NUM_RANGE is    -- NUM range in type float
--sem FLOAT range float(NUM'first) .. float(NUM'last);
--sem dies on -------------| and --------------|
        TGT.LONGEST_FLOAT;  --sem --


procedure GET( FILE : in FILE_TYPE;
               ITEM : out NUM;
               WIDTH : in FIELD := 0) is
    FLOAT_ITEM : TGT.Longest_Float;
begin
    FLOAT_IO_SUPPORT.GET (FILE, FLOAT_ITEM, WIDTH);

    if FLOAT_ITEM in NUM_RANGE then
        ITEM := NUM (FLOAT_ITEM);
    else
    -- If the result is not within the instantiated subtype, then
    -- DATA_ERROR must be raised.  If the value is within NUM,
    -- but outside range of ITEM's subtype, then CONSTRAINT_ERROR
    -- will be raised on return.
        raise DATA_ERROR;
    end if;
exception  --sem
    when constraint_error =>  --sem  happens when convert float to num
         raise DATA_ERROR; --sem
end GET;


procedure GET( ITEM : out NUM;
               WIDTH : in FIELD := 0) is
begin
    Check_Current_Input;
    Get(CURRENT_INPUT, ITEM, WIDTH);
end GET;


procedure PUT( FILE : in FILE_TYPE;
               ITEM : in NUM;
               FORE : in FIELD := default_fore;
               AFT  : in FIELD := default_aft;
               EXP  : in FIELD := default_exp ) is
begin
    FLOAT_IO_SUPPORT.PUT (FILE, TGT.LONGEST_FLOAT(ITEM), FORE, AFT, EXP);
end PUT;


procedure PUT( ITEM : in NUM;
               FORE : in FIELD := default_fore;
               AFT  : in FIELD := default_aft;
               EXP  : in FIELD := default_exp) is
begin
    Check_Current_Output;
    PUT( CURRENT_OUTPUT, ITEM, FORE, AFT, EXP);
end PUT;


procedure GET( FROM : in STRING;
               ITEM : out NUM;
               LAST : out POSITIVE ) is
    FLOAT_ITEM : TGT.Longest_Float;
begin
    FLOAT_IO_SUPPORT.GET (FROM, FLOAT_ITEM, LAST);

    if FLOAT_ITEM in NUM_RANGE then
        ITEM := NUM (FLOAT_ITEM);
    else
    -- If the result is not within the instantiated subtype, then
    -- DATA_ERROR must be raised.  If the value is within NUM,
    -- but outside range of ITEM's subtype, then CONSTRAINT_ERROR
    -- will be raised on return.
        raise DATA_ERROR;
    end if;
exception  --sem
    when constraint_error =>  --sem  happens when convert float to num
         raise DATA_ERROR; --sem
end GET;


procedure PUT( TO   : out STRING;
               ITEM : in NUM;
               AFT  : in FIELD := default_aft;
               EXP  : in FIELD := default_exp ) is
begin
    FLOAT_IO_SUPPORT.PUT (TO, TGT.LONGEST_FLOAT(ITEM), AFT, EXP);
end PUT;

end FLOAT_IO;


-------------------------------------------------------------
--           FIXED IO : BODY
-------------------------------------------------------------

package body FIXED_IO is

--+-----------------------------------------------------
--| Overview
--| FIXED_IO is performed by converting to/from FLOAT, and
--| using FLOAT_IO_SUPPORT routines.  This will only
--| cause problems for Fixed values that are outside the
--| range of FLOAT, e.g., they have extremely large or small
--| values for 'SMALL.

procedure GET( FILE : in FILE_TYPE;
               ITEM : out NUM;
               WIDTH : in FIELD := 0 ) is
    FLOAT_ITEM : TGT.Longest_Float;
       subtype NUM_RANGE is TGT.LONGEST_FLOAT
    --sem   range float(NUM'first) .. float(NUM'last);
    --sem doesn't like num'first as an expression
       ; --sem
begin
    FLOAT_IO_SUPPORT.GET (FILE, FLOAT_ITEM, WIDTH);

    if FLOAT_ITEM in NUM_RANGE then
        ITEM := NUM (FLOAT_ITEM);
    else
    -- If the result is not within the instantiated subtype, then
    -- DATA_ERROR must be raised.  If the value is within NUM,
    -- but outside range of ITEM's subtype, then CONSTRAINT_ERROR
    -- will be raised on return.
        raise DATA_ERROR;
    end if;
exception  --sem
    when constraint_error =>  --sem  happens when convert float to num
        raise DATA_ERROR; --sem
end GET;


procedure GET( ITEM : out NUM;
               WIDTH : in FIELD := 0 ) is
begin
    Check_Current_Input;
    GET(CURRENT_INPUT, ITEM, WIDTH);
end GET;

procedure PUT( FILE : in FILE_TYPE;
               ITEM : in NUM;
               FORE : in FIELD := default_fore;
               AFT  : in FIELD := default_aft;
               EXP  : in FIELD := default_exp ) is
begin
    FLOAT_IO_SUPPORT.PUT (FILE, TGT.LONGEST_FLOAT(ITEM), FORE, AFT, EXP);
end PUT;

procedure PUT( ITEM : in NUM;
               FORE : in FIELD := default_fore;
               AFT  : in FIELD := default_aft;
               EXP  : in FIELD := default_exp ) is
begin
    Check_Current_Output;
    PUT( CURRENT_OUTPUT, ITEM, FORE, AFT, EXP);
end PUT;


procedure GET( FROM : in STRING;
               ITEM : out NUM;
               LAST : out POSITIVE ) is
    FLOAT_ITEM : TGT.Longest_Float;
    subtype NUM_RANGE is TGT.LONGEST_FLOAT
    --sem   range float(NUM'first) .. float(NUM'last);
    --sem doesn't like num'first as an expression
       ; --sem
begin
    FLOAT_IO_SUPPORT.GET (FROM, FLOAT_ITEM, LAST);

    if FLOAT_ITEM in NUM_RANGE then
        ITEM := NUM (FLOAT_ITEM);
    else
    -- If the result is not within the instantiated subtype, then
    -- DATA_ERROR must be raised.  If the value is within NUM,
    -- but outside range of ITEM's subtype, then CONSTRAINT_ERROR
    -- will be raised on return.
        raise DATA_ERROR;
    end if;
exception  --sem
    when constraint_error =>  --sem  happens when convert float to num
         raise DATA_ERROR; --sem
end GET;


procedure PUT( TO : out STRING;
               ITEM : in NUM;
               AFT : in FIELD := default_aft;
               EXP : in FIELD := default_exp ) is
begin
    FLOAT_IO_SUPPORT.PUT (TO, TGT.LONGEST_FLOAT(ITEM), AFT, EXP);
end PUT;

end FIXED_IO;


-------------------------------------------------------------
--           DECIMAL_IO
-------------------------------------------------------------

package body Decimal_IO is

procedure GET( FILE  : in File_Type;
               ITEM  : out NUM;
               WIDTH : in FIELD := 0 ) is
    FLOAT_ITEM : TGT.Longest_Float;
    subtype NUM_RANGE is TGT.LONGEST_FLOAT;
begin
    FLOAT_IO_SUPPORT.GET (FILE, FLOAT_ITEM, WIDTH);

    if FLOAT_ITEM in NUM_RANGE then
        ITEM := NUM (FLOAT_ITEM);
    else
        raise DATA_ERROR;
    end if;
exception
    when constraint_error =>
        raise DATA_ERROR;
end GET;

procedure GET( ITEM : out NUM;
               WIDTH : in FIELD := 0 ) is
begin
    Check_Current_Input;
    GET(CURRENT_INPUT, ITEM, WIDTH);
end GET;

procedure PUT( FILE : in File_Type;
               ITEM : in NUM;
               FORE : in FIELD := DEFAULT_FORE;
               AFT  : in FIELD := DEFAULT_AFT;
               EXP  : in FIELD := DEFAULT_EXP ) is
begin
    FLOAT_IO_SUPPORT.PUT (FILE, TGT.LONGEST_FLOAT(ITEM), FORE, AFT, EXP);
end PUT;

procedure PUT( ITEM : in NUM;
               FORE : in FIELD := DEFAULT_FORE;
               AFT  : in FIELD := DEFAULT_AFT;
               EXP  : in FIELD := DEFAULT_EXP ) is
begin
    Check_Current_Output;
    PUT( CURRENT_OUTPUT, ITEM, FORE, AFT, EXP);
end PUT;

procedure GET( FROM : in String;
               ITEM : out NUM;
               LAST : out Positive ) is
    FLOAT_ITEM : TGT.Longest_Float;
    subtype NUM_RANGE is TGT.LONGEST_FLOAT;
begin
    FLOAT_IO_SUPPORT.GET (FROM, FLOAT_ITEM, LAST);

    if FLOAT_ITEM in NUM_RANGE then
        ITEM := NUM (FLOAT_ITEM);
    else
        raise DATA_ERROR;
    end if;
exception
    when constraint_error =>
         raise DATA_ERROR;
end GET;

procedure PUT( TO   : out String;
               ITEM : in NUM;
               AFT  : in FIELD := DEFAULT_AFT;
               EXP  : in FIELD := DEFAULT_EXP ) is
begin
    FLOAT_IO_SUPPORT.PUT (TO, TGT.LONGEST_FLOAT(ITEM), AFT, EXP);
end PUT;

end Decimal_IO;

-------------------------------------------------------------
--          ENUMERATION_IO: LOCAL DECLARATIONS
-------------------------------------------------------------
--| mapping and transitions for Enmumerations
package ENUM_TRANSIT is
use System.RTS.Lexer;

Mapping: Char_To_Token_Mapping := Char_To_Token_Mapping'
 ('0'..'9'                   => Digit,
  'A'..'Z' | 'a'..'z'        => Letter,
  ASCII.Underline            => UnderScore,
  -- For character literal enumerations
  '''                        => Quote,
  -- Space can be in char literal enum...
  ' '                        => System.RTS.Lexer.Space,
  -- ... but tab cannot!
  ASCII.Ht                   => Htab,
    -- This part uses knowledge of ASCII ordering from LRM C(13)
    -- to define other graphic characters as certain ranges.
  -- (Omit single quote)
  ASCII.Exclam .. ASCII.Ampersand       |
  '(' .. '/'                            |
  -- up to start of digits
  ASCII.Colon .. ASCII.At_Sign          |
  -- to start of letters
  ASCII.Back_Slash           => Other_Printing,
  ASCII.R_Bracket            => Other_Printing,
  ASCII.Circumflex           => Other_Printing,
  ASCII.Grave                => Other_Printing,
  ASCII.L_Brace              => Other_Printing,
  ASCII.Bar                  => Other_Printing,
  ASCII.R_Brace              => Other_Printing,
  ASCII.Tilde                => Other_Printing,
    -- These other_printing chars may be character literal enumerals
  others                     => Delim);

    -- Further state definitions for Enumerations
Expecting_Letter_Digit_Underscore_or_End  : constant state := 3;
Expecting_Letter_or_Digit                 : constant state := 4;
Expecting_Graphic_Char                    : constant state := 5;
Expecting_Quote                           : constant state := 6;
Expecting_End_After_Quote                 : constant state := 7;

  -- subtypes needed to define transitions constant:
subtype ST2 is State_transitions (Start..Expecting_End_After_Quote);
  -- ST2'last MUST be largest State constant for enumeration IO

    -- The following state transitions are what drive the
    -- Finite State Automaton (FSA).
Transitions: constant ST2 := ST2'(
  Start => (
    System.RTS.Lexer.Space | Htab   => Start, 
      -- Skip_Blanks() should prevent this
    Letter  => Expecting_Letter_Digit_Underscore_or_End,
    Quote   => Expecting_Graphic_Char,
    others  => System.RTS.Lexer.Error),
  Expecting_Letter_Digit_Underscore_or_End => (
    Underscore      => Expecting_Letter_or_Digit,
    Letter | Digit  => Expecting_Letter_Digit_Underscore_or_End,
    others => Accepted),   -- End
  Expecting_Letter_or_Digit => (
    Letter | Digit  => Expecting_Letter_Digit_Underscore_or_End,
    others  => System.RTS.Lexer.Error),

    -- transitions for character literal enumerations
  Expecting_Graphic_Char => (
    Letter | Digit | Quote | Underscore
    | System.RTS.Lexer.Space | Other_Printing => Expecting_Quote,
    others  => System.RTS.Lexer.Error),
  Expecting_Quote => (
    Quote   => Expecting_End_After_Quote,
    others  => System.RTS.Lexer.Error),
  Expecting_End_After_Quote => (
    others  => Accepted),
  others => (       -- Did we forget anything?
    others => System.RTS.Lexer.Error));
end Enum_transit;


function ENUM_IMAGE (  --| performs requested conversion lower case and
                   --| padding with trailing blanks to WIDTH
                   IMAGE : STRING;
                   WIDTH : FIELD;
                   SET   : TYPE_SET) return STRING
--+-----------------------------------------------------
--| Effect
--|   Builds a string of length max(width, image'length);
--| If SET=lower_case, AND this isn't a char literal, then
--| convert the characters to lower_case
--|
--| Requires
--|   IMAGE is a legal enum value, i.e., non-null, either is a char literal
--| or is in upper case already.
--|
--| Algorithm
--|   If size is OK, and no case conversion is needed, just return IMAGE.
--|   Else, build a BUFFER of the required size, inited to spaces.
--|   Move IMAGE into the front of it.  If case conversion is needed,
--|   change each uppercase char to lower case.
is
length : natural := IMAGE'length;
begin
if length >= width then
    -- No need to pad with blanks, if don't have to
    -- convert to lower case either, then just return
    if SET = UPPER_CASE or else
        -- No conversion on char lits
        IMAGE(IMAGE'first) = '''
      then
        return IMAGE;
    end if;
else
    -- need more space for spaces
    length := width;
end if;
declare
    subtype index_subtype is positive range 1 .. length;
    buffer : string (index_subtype) := (others => ' ');
begin
    buffer (buffer'first .. IMAGE'length) := IMAGE;
    if SET = LOWER_CASE and then
       buffer(buffer'first) /= '''  then
        -- Have to convert to lower case
       for index in index_subtype range buffer'first..IMAGE'length
       loop
            buffer(index) := lower_case(buffer(index));
       end loop;
    end if;
    return buffer;
end;
end ENUM_IMAGE;

-------------------------------------------------------------
--          ENUMERATION_IO : BODY
-------------------------------------------------------------
-- Source: /usr9/mdavis/io/RCS/enum_iob.bdy,v
-- Revision: 1.4  -- Date: 85/08/21 17:34:12  -- Author: mdavis


package body ENUMERATION_IO is

   subtype ENUM_Width is positive range 1 .. Enum'WIDTH+1;

procedure GET (FILE : in FILE_TYPE;
               ITEM : out ENUM)
--| Effect
--|   Lexes the next token from FILE using the syntax of
--| Enumeration literals, and converts it to an ENUM,
--| putting the value into ITEM.
--|
--| Algorithm
--|   Use RTS_LEX_FILE to LEX the next ENUM token
--| via Enumeration transition tables into BUFFER(1..last).
--| Use WIDTH=0 which means to pick up the next token, stopping
--| as soon as the syntax is correct or in error.
--|   Generate the ENUM value via ENUM'VALUE,
--| converting any constraint error into DATA_ERROR.

is
    buffer : string (ENUM_Width); -- big enough for non-blank part of
                                -- any ENUM plus a trailing blank
    last : natural;
    first : natural := buffer'FIRST;
begin
        Check_Mode_In(File);  -- check for closed file/wrong mode
        RTS_LEX_FILE (FILE,
                      ENUM_TRANSIT.Transitions,
                      ENUM_TRANSIT.Mapping,
                      0,  -- width
                      buffer,
                      last);
    --  Protect against constraint_error when converting to enum, and
    -- change it to DATA_ERROR
    begin
        while first in BUFFER'first..last loop
            exit when (BUFFER(first) /= ' ') and then
              (BUFFER(first) /= ASCII.HT);
            first := first + 1;
        end loop;
        ITEM := ENUM'value (BUFFER(first .. last));
    exception
        when CONSTRAINT_ERROR =>
            raise DATA_ERROR;
    end;
end GET;  -- file


procedure GET( ITEM : out ENUM ) is
begin
    Check_Current_Input;
    GET (CURRENT_INPUT, ITEM);
end GET;


procedure PUT( FILE : in FILE_TYPE;
               ITEM : in ENUM;
               WIDTH : in FIELD := default_width ;
               SET : TYPE_SET := default_setting ) is
begin
    -- check for closed file/wrong mode
    Check_Mode_Out_Or_Append(File);
    PUT_WORD (file => FILE,
              word => ENUM_IMAGE (Enum'IMAGE(Item), 1, set),
              width => WIDTH);
end PUT;  -- file


procedure PUT( ITEM : in ENUM;
               WIDTH : in FIELD := default_width ;
               SET : in TYPE_SET := default_setting ) is
begin
    Check_Current_Output;
    PUT (CURRENT_OUTPUT, ITEM, WIDTH, SET);
end PUT;

procedure GET( FROM : in STRING;
               ITEM : out ENUM;
               LAST : out POSITIVE ) is
--| Effect
--|   Lexes the first token from FROM using the syntax of
--| enumeration literals, and converts it to an ENUM,
--| putting the value into ITEM. LAST is set to the
--| index of the last character used.
--|
--| Algorithm
--|   Use RTS_LEX_STRING to LEX the next enum token
--| via Enumeration transition tables, setting My_Last.
--|   Generate the enum value via ENUM'VALUE,
--| converting any constraint error into DATA_ERROR.
    My_First  : natural := FROM'first; -- for stripping lead blanks
    My_Last   : natural;  -- Need local so the return value from
                          -- lex_string can be used in slice.
begin
    System.RTS.Lexer.RTS_LEX_STRING(FROM,
                   ENUM_TRANSIT.Transitions,
                   ENUM_TRANSIT.Mapping,
                   MY_Last);
    LAST := My_Last;
    begin
        while My_First in FROM'first..My_Last loop
            exit when (FROM(My_First) /= ' ') and then
              (FROM(My_First) /= ASCII.HT);
            My_First := My_First + 1;
        end loop;
        ITEM := ENUM'value (FROM(My_First .. My_Last));
    exception
        when CONSTRAINT_ERROR =>
            raise DATA_ERROR;
    end;
end GET;  -- string


procedure PUT( TO : out STRING;
               ITEM : in ENUM;
               SET : in TYPE_SET := default_setting ) is
   item_image : constant string :=
           enum_image(Enum'IMAGE(Item), To'LENGTH, Set);

   diff : constant integer := ITEM_IMAGE'length - TO'length;

begin
   -- Check that ITEM_IMAGE fits in TO

   if diff = 0 then        -- Same Size
       TO := ITEM_IMAGE;

   elsif diff > 0 then     -- Item_Image is bigger.
       raise LAYOUT_ERROR;

   else  -- TO is longer than ITEM_IMAGE. But then
         -- ENUM_IMAGE didn't pad with enough blanks!
         raise Internal_Error;
   end if;
end PUT; -- string

end ENUMERATION_IO;

-- Local subprogram bodies


procedure Close_Special_File ( File : in File_Type ) is
begin
    case File.Mode is
        when In_File =>
            Current_Input_File := null;
        when Out_File =>
            Current_Output_File := null;
        when Append_File =>
            Current_Output_File := null;
    end case;
end Close_Special_File;

procedure Init_File ( File : in File_Type;
                      Mode : in File_Mode ) is
begin
    File.Mode           := Mode;
    File.Current_Page   := 1;
    File.Current_Line   := 1;
    File.Current_Column := 1;
    File.Is_Special     := false;
    File.Self           := File;
    File.Line_Length := 0;
    File.Page_Length := 0;
    if Mode = Out_File or else Mode = Append_File then
        File.Line_Length := Unbounded;
        File.Page_Length := Unbounded;
    end if;
end Init_File;

function Get_Standard_Input return File_Type is
    F : File_Type;
begin
    F := new File_Record;
    Init_File(F, In_File);
    F.CFile := c_get_stdin;
    F.File_Name := new String'("Standard Input");
    return F;
end Get_Standard_Input;

function Get_Standard_Output return File_Type is
    F : File_Type;
begin
    F := new File_Record;
    Init_File(F, Out_File);
    F.CFile := c_get_stdout;
    F.File_Name := new String'("Standard Output");
    return F;
end Get_Standard_Output;

function Get_Standard_Error return File_Type is
    F : File_Type;
begin
    F := new File_Record;
    Init_File(F, Out_File);
    F.CFile :=  c_get_stderr;
    return F;
end Get_Standard_Error;

procedure Local_Put ( File : in File_Type;
                      Item : in String ) is
    ch : Integer;
begin
    if File.Line_Length = Unbounded then
        -- Write out entire string at once, and update column number
        -- Note: this will cause the line/page number to be "incorrect"
        -- if the string contains line or page terminators, but this
        -- behavior is implementation dependent anyway.
        ch := c_fputs(Interfaces.C.To_C(Item), File.CFile);
        File.Current_Column := File.Current_Column + Item'LENGTH;
    else
        -- Line length is bounded, so carefully put out characters
        -- one at a time, keeping track of line/page numbers
        for i in Item'RANGE loop
            Put(File, Item(i));
        end loop;
    end if;
end Local_Put;

procedure Local_Put_Line ( File : in File_Type;
                           Item : in string ) is
begin
    Local_Put(File, Item);
    Local_New_Line(File, 1);
end Local_Put_Line;

procedure Local_New_Line ( File : in File_Type;
                           Spacing : in Positive_Count ) is
    Result : Integer;
begin
    for I in 1 .. Spacing loop
        Result := c_fputc(Character'Pos(ASCII.LF), File.CFile);
        Next_Line(File);
        if File.Page_Length /= Unbounded and then
            File.Current_Line > File.Page_Length then
              Local_New_Page(File);
        end if;
    end loop;

    Result := c_fflush(File.CFile);
    -- Work around bug in ADI runtimes
    -- (see PR/8026 Dropped characters).
    -- If we flush after every line, the buffer is unlikely to fill up and
    -- cause the bug.
end Local_New_Line;

procedure Local_New_Page( File : in File_Type ) is
    Result : Integer;
begin
    if File.Current_Column /= 1 or else File.Current_Line = 1 then
        -- Line not terminated or page is empty, so terminate line
        Result := c_fputc(Character'Pos(ASCII.LF), File.CFile );
    end if;
    Result :=  c_fputc(Character'Pos(ASCII.FF), File.CFile );
    if Record_Oriented then
        -- If record-oriented, line terminator follows page terminator
        Result := c_fputc(Character'Pos(ASCII.LF), File.CFile);
    end if;
    Next_Page(File);
end Local_New_Page;

procedure Local_Get (
  File : in File_Type;
  Item : out string
) is
    Char : character;
    ch : Integer;
begin
    for i in Item'RANGE loop
	loop
	    -- Skip any leading line/page-terminators
            ch := Get_Next_Char(File);
            if ch < 0 then
	        -- EOF detected
                raise END_ERROR;
	    end if;
	    Char := Character'Val(ch);
            if Char = ASCII.LF then
                Next_Line(File);
            elsif Char = ASCII.FF then
                Next_Page(File);
            else
		exit;
	    end if;
        end loop; 

	-- Now actually save the character
	Item(i) := Char;
	File.Current_Column := File.Current_Column + 1;
    end loop;
end Local_Get;

procedure Local_Get_Line (
  File : in File_Type;
  Item : out string;
  Last : out natural
) is
    ch : Integer;
    Skipped_At_Least_One_Character : Boolean := False;
begin
    -- Reads successive characters from the specified input file and assigns them
    -- to successive characters of the specified string. Reading stops if the end
    -- of the string is met. Reading also stops  if the EOL is met before meeting
    -- the end of the string; in this case Skip_Line is called with a spacing of 1.
    -- The values of characters not assigned are not specified. If chars are read, returns
    -- in Last the index value such that Item(Last) is the last char assigned. If no chars
    -- are read, returns in Last an index value that is one less than Item'First. The exception
    -- End_Error is propagated if an attempt is made to skip a File terminator.

    if Item'LAST < Item'FIRST then
        -- String can't hold ANY characters!
        Last := Item'FIRST - 1;
        return;
    end if;

    -- There can be EOL but the string hasnot ended
    for i in Item'FIRST .. Item'LAST loop
        ch := Get_Next_Char(File);
        Last := i;
        if ch < 0 then
        -- EOF seen
	    if Skipped_At_Least_One_Character then
                Last := Last - 1;
                exit;
	    else
                -- if EOF is the first character you see
	        raise END_ERROR;
	    end if;
        end if;
        Item(i) := Character'Val(ch);
        Skipped_At_Least_One_Character := True;
        if Item(i) = ASCII.LF then
            -- consume EOL
            Last := Last - 1;
            exit;
        end if;
    end loop;

    if Last /= Item'LAST then
        -- We have EOL before end of string
        Next_Line(File);
    else
       -- We have end of string but not EOL
        -- Update column to indicate how far we advanced
        File.Current_Column := File.Current_Column +
          Count(Last - Item'FIRST + 1);
    end if;
end Local_Get_Line;

end Ada.Text_IO;

-- From MonoCM file ada.text_io.bdy,v.
