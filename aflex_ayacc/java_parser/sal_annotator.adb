-- $Revision: 22024 $ $Date: 2010-06-04 12:41:57 -0400 (Fri, 04 Jun 2010) $

pragma Style_Checks(Off);
with Java_Parser, Java_Lexer_io, Java_Lexer ;
use Java_Parser ;

with Java_Lexer_dfa;
with Misc; use Misc;

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Calendar;

with DB; use DB;
with DB.XML;
    use DB.XML;
with DB.SQLite;

with DB.Source_Files;

with Naming_Config; use Naming_Config;

with ST.Expected_Statistics;
    use ST.Expected_Statistics;
with ST.Logging;
with ST.Languages ;
 use ST.Languages ;
with ST.Lister_Utils;

with Std_Output; use Std_Output;
with DB.Command_Arguments;
 use DB.Command_Arguments;
with DB.Source_Files;

with Utils.Program_Exit; pragma Elaborate_All(Utils.Program_Exit);
with Utils.Program_Exit.Errors; use Utils.Program_Exit.Errors;

with Utils.Statistics; use type Utils.Statistics.Statistics_Verbosity_Level;
with Utils.Command_Line; use Utils.Command_Line;
with Utils.Files; use Utils.Files;
with Utils.Zip_Files;
 use Utils.Zip_Files;
with Utils.Output.Streams.Files;
with Utils.OS_Dep;
with Utils.Input.Streams.Storage;
with Utils.Storage_Management.Varying_Pools;
 use Utils.Storage_Management.Varying_Pools;
with Utils.Storage_Management.Subpools;
 use Utils.Storage_Management.Subpools;
  pragma Elaborate_All(Utils.Storage_Management.Subpools);
with Utils.Storage_Management.GC_Pools;
with Utils.Storage_Tracking.Dump_Statistics;
with Utils.Timing;

with Utils.Spellings;
 use Utils.Spellings;
with Utils.Spellings.Seqs;
 use Utils.Spellings.Seqs;
with Utils.Strings;
 use Utils.Strings;
with Utils.Messages;
 use Utils.Messages;
 use Utils.Messages.Message_Seqs;
with Utils.License_Key_Support;

with Versions;


with ST; use ST;
with ST.Lister_Utils;

with ST.Message_Leveling_And_Suppression;
with ST.Html_Listings;
with ST.Message_Tables;
with ST.Inspection_Configuration;
    use ST.Inspection_Configuration;
with ST.Annotations.Html;
with ST.Race_Conditions.Xml_To_Html;
  use ST.Race_Conditions.Xml_To_Html;
with ST.Race_Post_Html;

with BE.BE_Messages;
 use BE.BE_Messages;
with BE.Message_Kinds;
 use BE.Message_Kinds;
with BE.BE_Messages.Message_Input_Output;
 use BE.BE_Messages.Message_Input_Output;

procedure Sal_Annotator is

    use Utils;
    use type Utils.Program_Exit.Badness_Enum;

    Debug_Storage_Usage : constant Boolean :=
      DB.Command_Arguments.Debug_Flag_Is_On ("storage_usage");

    Debug_Patterns : constant Boolean :=
      DB.Command_Arguments.Debug_Flag_Is_On ("patterns");

    Generate_Text_Listings : constant Boolean :=
      DB.Command_Arguments.Debug_Flag_Is_On ("text-listing", True);

    New_XML_Database_Handle : aliased DB.XML.XML_DB_Handle;
    New_SQLite_Handle       : aliased DB.SQLite.SQLite_DB_Handle;

    -- holds the handle for the entire inspection (or db_lister)
    Database_Handle : DB.DB_Handle_Ptr;

    function List_Dir_Name(
      Package_Name: Spelling) return String is
        Dir_String : constant String := "list";
        Output_Directory : Spelling renames Inspection.Output_Directory;
    begin
        if Package_Name = No_Spelling or else
          Package_Name = Intern("") then
            declare Result : constant String :=
              String_Ref(Output_Directory).S & "/" &
              Dir_String;
            begin
                return Result ;
            end;
        else
            declare Result : constant String :=
              String_Ref(Output_Directory).S & "/" &
              Dir_String & "/" &
              To_String(Package_Name );
            begin
                return Result ;
            end;
        end if;
    end List_Dir_Name ;

     -- Parse Java source using lex and yacc
    procedure Parse_Java_Source(in_file_name : String;
       Sal_Listing_Name : Spelling ) is
      last : natural;
      Source_Contents_Ptr : Access_Constant_String;
    begin
        Put_Line("Parsing " & in_file_name);
if True then
        -- Has to read source file inside a zip
        Source_Contents_Ptr := ST.Lister_Utils.Read_Source_File(
          FN => To_String
           (Source_Files.File_On_Disk(Intern(in_file_name))));
        Misc.set_input_buffer(Source_Contents_Ptr);
--        for i in Source_Contents_Ptr'Range loop
--            Put(Source_Contents_Ptr(i));
--        end loop;
        if Contains(Source_Contents_Ptr.all, "No source file available") = True then
           Put_Line("Source for file " & in_file_name & " not available");
           return ;
        end if;
        Java_Lexer_IO.YY_INPUT := Misc.YY_INPUT'Access;
end if;

--        Java_Lexer_io.open_input(in_file_name);

        -- Remember current file name
        Java_Lexer.Cur_File :=
          Utils.Spellings.Intern(in_file_name);
--        Put_Line(" ");
--        Put_Line("Parsing " & in_file_name);
        Java_Lexer.Start_New_File;
        Java_Lexer.lines := 1;
        Java_Lexer.linenum;
        Java_Parser.Num_Annonymous_Classes := 0;
        -- Create listing file with Sal annotations from the source
        Utils.Output.Streams.Files.Create(Java_Parser.Listing,
         To_String(Sal_Listing_Name));
        yyparse;
        Utils.Output.Streams.Files.Close(Java_Parser.Listing);

--        Java_Lexer_io.close_input;

--        put_line("---- Finished parse ----");
--        new_line;
--        put(integer'image(number_of_errors));
--        put_line(" errors found");
        number_of_errors := 0;

    end Parse_Java_Source;

    -- the following is copied from lister.2.ada, now obsolete
    procedure Gen_One_Text_Listing(
       Source_File_Name : String;
       Sal_Listing_Name : Spelling;
       Orig_Lang: Source_Language;
       File_Info : Messages_Per_File_Ptr ) is

        function Init_Comment return String is
        begin
            case Orig_Lang is
                when Ada_Language => return "--#";
                when Java_Like_Language => return "//#";
                when C_Language => return "//#";
            end case;
        end Init_Comment;

        Comment: constant String := Init_Comment;
        B_Comment: constant String := "    " & Comment;

        function Maybe_Srcpos(M: Message) return String is
            use BE.Message_Kinds;
        begin
            if M.Kind.all in BE_Message_Kind'Class then
                if not Is_Method_Annotation(BE_Message_Kind(M.Kind.all).Subkind)
                then
                    return Full_Srcpos_Image(
                      FN => To_String(File_Info.Nick_Name),
                      Srcpos => M.Srcpos) & ": " ;
                end if;
            end if;
            return "";
        end Maybe_Srcpos;

        Pin: Utils.Storage_Management.GC_Pools.Pinned
          (File_Info.Messages'Access);
        pragma Unreferenced (Pin);

    begin
        -- Rather than "0 errors, 0 messages", say that the file
        -- was skipped during the inspection.
-- TBD mel, move this after the count has been calculated
--        if (Num_Messages(File_Info.PVP_Messages) +
--            Num_Messages(File_Info.Race_Messages) = 0) then
--            Format
--              (Listing,
--               Template => "%1 *%2*%n%1%n",
--               Arg1 => Comment,
--               Arg2 => "File was skipped during inspection");
--        else
            Format
              (Listing,
               Template => "%1 %2 errors, %3 messages%n%4%n",
               Arg1 => Comment,
               Arg2 => Img(File_Info.Messages.Error_Count),
               Arg3 => Img(File_Info.Messages.Message_Count),
               Arg4 => Comment);
--        end if;

        Utils.Storage_Management.GC_Pools.Resume_Construction
                (File_Info.Messages);
        Freeze_Messages(File_Info.Messages);
        Utils.Storage_Management.GC_Pools.Pause_Construction
                (File_Info.Messages);
        -- Finish_Construction(File_Info.Messages);
        Misc.Process_File_Messages(Source_File_Name, File_Info);
--        Misc.Print_One_FileInfo(Source_File_Name);
        Parse_Java_Source(Source_File_Name,Sal_Listing_Name);

    end Gen_One_Text_Listing;

    procedure Gen_Text_Listing
      (Source_File_Name        : String;
       File_Info               : Messages_Per_File_Ptr;
       Exists_On_Disk          : out Boolean;
       Num_Source_Lines        : out Line_Number'Base;
       Approx_Num_Source_Lines : out Line_Number'Base;
       Check_Counts            : out BE.BE_Messages.Check_Count_Array)
    is
        pragma Unreferenced (Check_Counts);

        -- Generates:
        List_Dir : constant String := List_Dir_Name
          (Package_Name => File_Info.Package_Name);
          -- "listing/" & To_String( File_Info.Package_Name );
          -- Dir_Name (Listing_Dir_Kind, Output_Dir, File.Package_Name);

        Sal_Listing_Name: aliased constant String
          := List_Dir & "/" & To_String(File_Info.Nick_Name)  & ".sal"
             & ".txt";

        -- Make sure we don't try to generate listings from .class files:
        Lang: constant Source_Language :=
          Source_Language'Value(To_String(File_Info.Language_Name));
        -- pragma Assert(Lang /= Intern("JBC"));
        -- pragma Assert(Lang = Intern("Ada") or Lang = Intern("Text"));

        -- TBD TBD
        --Orig_Lang: constant Source_Language := Original_Source_Language(File);

        Source_Pool_Name : aliased constant String := Source_File_Name;
        Source_Pool : aliased Subpool (Source_Pool_Name'Unchecked_Access);
        Within      : Within_Pool (Source_Pool'Access);
        pragma Unreferenced (Within);

        Null_Contents : aliased constant String :=
          " " & Product_Name
          & ": No source file available. " & ASCII.LF & ASCII.NUL;

    begin -- Gen_Text_Listing

        -- pragma Assert(Lang /= Intern("JBC")); TBD TBD

        Make_Directory_If_Nonexistent(List_Dir);

        ST.Languages.Set_Current_Language(
          Get_File_Ext( To_String(File_Info.Nick_Name )));

         Gen_One_Text_Listing(Source_File_Name,
           Intern(Sal_Listing_Name),
           Orig_Lang => Lang,
           File_Info => File_Info);

    end Gen_Text_Listing;

    procedure Lister_One_File(
      File_Name : Spelling;
      File_Info : Messages_Per_File_Ptr) ;
    procedure Lister_All_Files is new
        File_Name_To_Messages_Mappings.Iterate( Lister_One_File );

    procedure Lister_One_File(
      File_Name : Spelling;
      File_Info : Messages_Per_File_Ptr) is
        Exists_On_Disk: Boolean;
        Num_Source_Lines, Approx_Num_Source_Lines: Line_Number'Base;
        Check_Counts: BE.BE_Messages.Check_Count_Array;
    begin
        Gen_Text_Listing (
          Source_File_Name => To_String( File_Name ),
          File_Info => File_Info,
          Exists_On_Disk => Exists_On_Disk,
          Num_Source_Lines => Num_Source_Lines,
          Approx_Num_Source_Lines => Approx_Num_Source_Lines,
          Check_Counts => Check_Counts);
    end Lister_One_File;

------------------- end of subps copied from lister.2.ada

    procedure Add_One_File_Info_To_DB (Handle: in out DB_Handle'Class;
      File_Info : File_Name_To_Messages_Mapping_Ptr) is


        Source_File_Id : Source_File_Id_Type;
        Source_Loc_Id: Source_Locator_Id_Type;

        procedure Add_One_File(
            File_Name : Spelling;
            File_Info : Messages_Per_File_Ptr) ;
        procedure Add_All_Files is new
            File_Name_To_Messages_Mappings.Iterate( Add_One_File );

        procedure Add_One_File(
         File_Name : Spelling;
         File_Info : Messages_Per_File_Ptr) is
            --Exists_On_Disk: Boolean;
            --Num_Source_Lines, Approx_Num_Source_Lines: Line_Number'Base;
            --Check_Counts: BE.BE_Messages.Check_Count_Array;
            --Pin: Utils.Storage_Management.GC_Pools.Pinned (
                --File_Info.Messages'Access);
            use Spelling_Seqs;
        begin
            Add_File (Handle, File_Name,  File_Info.Package_Name,
                   File_Info.Language_Name, No_Spelling,
                   Source_File_Id,
                   Source_Loc_Id);
            for Idx in 1 .. Length(File_Info.Generated_Files) loop
                Add_Generated_Files (Handle, File_Name,
                    Nth (File_Info.Generated_Files, Idx));
            end loop;
        end Add_One_File;

        procedure Add_One_File_Messages
          (File_Name : Spelling;
           File_Info : Messages_Per_File_Ptr)
        is
            Pin : Utils.Storage_Management.GC_Pools.Pinned
              (File_Info.Messages'Access);
        begin
            Add_Messages (Handle, File_Info.Messages);
            -- clean the message_pool, so that memory can be reclaimed
             Utils.Storage_Management.GC_Pools.Resume_Construction
                (File_Info.Messages);
           declare
             Within: Within_Pool(File_Info.Messages'Access);
           begin
             Message_Sets.Remove_All
               (Message_Sets.Set (File_Info.Messages.Already_Seen));
             Clean_Pool_Method (File_Info.Messages,
               Other_Reason);
           end;
           Utils.Storage_Management.GC_Pools.Finish_Construction
                (File_Info.Messages);

        end Add_One_File_Messages;

        procedure Add_All_File_Messages is new
            File_Name_To_Messages_Mappings.Iterate( Add_One_File_Messages );

    begin
        Add_All_Files( File_Info.all );
        Add_All_File_Messages( File_Info.all );
    end Add_One_File_Info_To_DB;

    procedure Init_Message_Patterns  is
      -- Init message patterns (from xml file)
      -- We copied the applicable pattern info from the
      -- inspection into the inspection results file
      --use ST;
    begin
        Message_Leveling_And_Suppression.
          Read_Message_Pattern_Configuration_Files(
          To_String(Inspection.Message_Patterns_Path));
    exception
    when others =>
        Put_Line_Err("Cannot open Message Patterns file: " &
        To_String(Inspection.Message_Patterns_Path) &
        " or malformed XML.");
        Program_Exit.Update_Error_Status
            (Status => Fatal_Internal_Error);
        Program_Exit.Exit_Program ;
    end Init_Message_Patterns;

    procedure Get_Project_Env_Var is
      -- Get the value of the SOFCHECK_PROJECTS environment variable.
      -- "User_Env_Var" is the value that is actually set; it may be
      -- null, if the user didn't set it.  "Env_Var" is the value that
      -- the Inspector will actually use; it may be the "default" value,
      -- if the user did not specifically set it.  "Use_Default_Env" is
      -- True if Env_Var is the default value.

        Env_Var_Str : constant String :=
          Utils.OS_Dep.Get_Env_Variable("SOFCHECK_PROJECTS");
    begin
        if Env_Var_Str /= "" then
           Inspection.Project_Env_Var_Spelling := Intern (Env_Var_Str);
        end if;
    end Get_Project_Env_Var;

    function Has_Zip_Extension(Dir_Name: String) return Boolean is
      -- Return True if given name has one of the extensions that indicate a
      -- Zip archive (case insensitively).
        Ext: constant String := To_Lower(Get_File_Ext(Dir_Name));
        Result : constant Boolean :=
          Ext = ".zip" or Ext = ".jar" or Ext = ".ear" or
          Ext = ".war" or Ext = ".sar";
    begin
        return Result;
    end Has_Zip_Extension;


begin
    declare

        -- possible usage:
        -- lister.exe xml_files
        Lister_Pool_Name : aliased constant String := "Lister_Pool";
        Lister_Pool : aliased Subpool(Lister_Pool_Name'Unchecked_Access);
        Within_Lister_Pool : Within_Pool (Lister_Pool'Access);
        pragma Unreferenced (Within_Lister_Pool);

        use Lister_Args;

        Show_Version: constant Boolean := Get_Opt(Result, Version_Option);
        Diffable_Output: constant Access_Constant_Boolean :=
            Get_Opt(Result, Dbg_Diff_Option);

        Output_Only: constant Boolean := Get_Opt(Result, Output_Only_Option);
        Message_Pattern_Path: constant Access_Constant_String :=
            Get_Opt(Result, Message_Patterns_File_Option);
        Cutoff_Id : constant Access_Constant_Natural :=
            Get_Opt(Result, Cutoff_Option);
        Is_Picky : constant Access_Constant_Boolean :=
            Get_Opt(Result, Picky_Option);
        Message_Quantity : constant Access_Constant_String :=
            Get_Opt(Result, Messages_Option);
        Generate_Annotation_Report: constant Boolean :=
            Get_Opt(Result, Annotations_Option);
        Eclipse_Mode: constant Boolean :=
            Get_Opt(Result, Eclipse_Mode_Option);

        Error_Count : Natural;

        --use Utils.Strings ;
        --Xml_File_List : constant String_Seq :=
          --Lister_Args.Get_Plain_Args(Result);
        Xml_File_List : Spelling_Seq;
        Msg_Dir : constant String_Seq :=
            Lister_Args.Get_Plain_Args(Result);

        Source_Paths : constant String_Seq :=
            Lister_Args.Get_Opt (Result, Source_Path_Option);
        Zip_Source_Paths : constant String_Seq :=
            Lister_Args.Get_Opt (Result, Zip_Source_Path_Option);
        Source_File_Seq : Spellings.Seqs.Var_Spelling_Seq;
        Zip_Source_File_Seq : Spellings.Seqs.Var_Spelling_Seq;

        Accum_Num_Class_Included: Natural := 0;
        Accum_File_Skips: Natural := 0;
        Accum_Proc_Skips: Natural := 0;
        Accum_Mem_Use: Natural := 0;
        Accum_Num_Errors: Natural := 0;
        Accum_Num_Source_Lines: Natural := 0;
        Accum_Elapsed_Time: Natural := 0;

        Has_Global_Race_Conditions : Boolean := False;

        Partition_Count : Natural := 0;
        First_Good_Partition : Natural := 0;

        -- boolean so we know if it is safe to run the annotation report
        Database_Successfully_Updated : Boolean := False;
    begin   -- sal_annotator
        Program_Exit.Reset_Status ;
        Inspection.Split_Output_Context := True;
        Utils.Timing.Timing_Enabled := False;
        Debug_SQL := DB.Command_Arguments.Debug_Flag_Is_On("sql");
        if Length(Msg_Dir) /= 1 then
            if Show_Version then
              Put_Line(Versions.RCS_Revision);
              Program_Exit.Exit_Program ;
            else
              Put_Line_Err
                 ("Usage: sal_annotator  [-version]" &
                  " [-message_patterns_file file]  msg_dir" &
                  " [ {-source_path dir} ... ]" &
                  " [{ -zip_source_path zipfile} ...]");
              Program_Exit.Update_Error_Status
                (Status => Fatal_User_Error);
              Program_Exit.Exit_Program ;
            end if;
        end if;
        if Show_Version then
            Put_Line(Versions.RCS_Revision);
        end if;

        declare
           Dir : constant String := Nth(Msg_Dir,1) ;
        begin
            -- look for message files
            Check_Is_Directory (Dir);
            Xml_File_List := Expand_Wildcard
                (Dir, "*msgs.xml", Include_Dir => True);
        exception
            when File_Not_Found =>
                Put_Line_Err("Cannot open inspector msgs files.");
                Program_Exit.Update_Error_Status
                    (Status => Fatal_Internal_Error);
                Program_Exit.Exit_Program ;
        end;
        if Length(Xml_File_List)=0 then
            -- in case the directory did not exist?
            Put_Line_Err("Cannot open inspector msgs files.");
            Program_Exit.Update_Error_Status
                (Status => Fatal_Internal_Error);
            Program_Exit.Exit_Program ;
        end if;

        -- read the Inspection.xml file to initialize the Inspection_Record
        declare
            Dir        : constant String := Nth (Msg_Dir, 1);
            Timer_Name : aliased constant String :=
                          "Time to read inspection file";
            Dummy      : Timing.Timer (Timer_Name'Unchecked_Access);
            pragma Unreferenced (Dummy);

        begin
            Read_Inspection_Info
                (File_Name=> Dir &  "/Inspection_Info.xml");
        exception
            when Exc: others =>
                  -- we could not read the inspection record
                  -- not much we can do
              Program_Exit.Update_Error_Status
                  (Status => Fatal_Internal_Error);
              if Inspection.Logging_Output then
                   ST.Logging.Add_To_Log_File
                     ("Error reading inspection_info.xml file " &
                        Exception_Information(Exc));
              else
                Put_Line_Err (
                    "Fatal error reading inspection_info.xml file." );
              end if;
        end;
        Partition_Count := Inspection.Partition_Count;

        -- the lister log switch takes precedence

        if Partition_Count <= 1 and then
            Inspection.Num_Class_Files_Included = 0 then
            -- no class file, nothing to do
                Put_Line("No output generated.");
                Program_Exit.Exit_Program ;
        end if;
        -- command line
        if Inspection.Logging_Output then
          declare
            use Ada.Strings.Unbounded;
            Result : Unbounded_String := Null_Unbounded_String;
          begin
            for arg in 1 .. Ada.Command_Line.Argument_Count loop
                    Append (Result, Ada.Command_Line.Argument (arg) & " ");
            end loop;
            -- command line
            Logging.Add_To_Log_File ("Lister arguments : ");
            Logging.Add_To_Log_File(To_String(Result));
          end;
        end if;
        -- make sure the output directories match. When invoked directly, the
        -- lister could be run from a different place, and we want the lister to
        -- find the log file.
        declare
           Dir : constant String := Nth(Msg_Dir,1) ;
        begin
          if To_String(Inspection.Output_Directory) /= Dir then
              -- TBD: should make sure it is the same
            Inspection.Output_Directory := Intern(Dir);
          end if;
        end;
        -- the -log on the lister takes precedence
        -- if the flag was not set in the inspector
        if not Inspection.Logging_Output and then
            Get_Opt(Result, Log_Option) then
            Inspection.Logging_Output := True;
            Logging.Set_Log_File (New_File=> True);
        end if;


          if Inspection.Logging_Output then
            declare
                pragma Assert (Is_Proper_Spelling(Inspection.Output_Directory));
                Output_Dir : constant String :=
                    To_String(Inspection.Output_Directory) ;
                Log_File_Name : constant String :=
                    Output_Dir & "/Inspection.log";
            begin
                pragma Assert (File_Exists (Log_File_Name));
                ST.Logging.Set_Log_File (False);
                if Is_Proper_Spelling(Inspection.Database_Path) then
                  ST.Logging.Add_To_Log_File
                    ("Generating Output and Updating Database at " &
                     Timing.Image(Ada.Calendar.Clock) &".");
                else
                  ST.Logging.Add_To_Log_File
                    ("Generating Output at " &
                     Timing.Image(Ada.Calendar.Clock) &".");
                end if;
                ST.Logging.Close_Log_File;
            end;
          end if;

          Get_Project_Env_Var;

          if Debug_Patterns then
            Inspection.Debug_Patterns := True;
          end if;

          Inspection.Database_Kind := No_Database;
          Inspection.Database_Path := No_Spelling;
          Inspection.Shared_Database := No_Spelling;

          --MAYBE Database_Handle := New_XML_Database_Handle'Unchecked_Access;

          if Inspection.Partition_Count > Partition_Count then
                Partition_Count := Inspection.Partition_Count;
          else
              if Inspection.Partition_Count > 0 and then
                  Natural(Length(Xml_File_List)) > Partition_Count then
                  Put_Line_Err("More inspector msgs files than" &
                      " partitions. Cleanup old files.");
                  Program_Exit.Update_Error_Status
                      (Status => Fatal_Internal_Error);
                  Program_Exit.Exit_Program ;
              end if;
          end if;

          -- find all the possible source files
          if not Is_Null_Seq (Source_Paths) and then
            Length (Source_Paths)> 0 then
              -- non null for split mode only
              Inspection.Main_Routine := Inspector_BE;
              for Src_Dir in 1 .. Length (Source_Paths) loop
                  -- java files
                  declare
                      Dir : constant String := Nth(Source_Paths,Src_Dir) ;
                      File_List : Spelling_Seq;
                   begin
                       Check_Is_Directory (Dir);
                       File_List := Expand_Wildcard
                           (Dir, "*.java", Include_Dir => True,
                           Walk_Subdirectories => True);
                       -- for F in 1 .. Length (File_List) loop
                           -- Put_Line (To_String (Nth (File_List, F)));
                       -- end loop;
                       Append (Source_File_Seq, File_List);
                   exception
                     when File_Not_Found =>
                       null;
                 end;
                  -- ada files
                  declare
                      Dir : constant String := Nth(Source_Paths,Src_Dir) ;
                      File_List : Spelling_Seq;
                   begin
                       Check_Is_Directory (Dir);
                       File_List := Expand_Wildcard
                           (Dir, "*.ad*", Include_Dir => True,
                           Walk_Subdirectories => True);
                       -- for F in 1 .. Length (File_List) loop
                           -- Put_Line (To_String (Nth (File_List, F)));
                       -- end loop;
                       Append (Source_File_Seq, File_List);
                   exception
                     when File_Not_Found =>
                       null;
                  end;
                  -- ada files
                  declare
                      Dir : constant String := Nth(Source_Paths,Src_Dir) ;
                      File_List : Spelling_Seq;
                   begin
                       Check_Is_Directory (Dir);
                       File_List := Expand_Wildcard
                           (Dir, "*.ads", Include_Dir => True,
                           Walk_Subdirectories => True);
                       -- for F in 1 .. Length (File_List) loop
                           -- Put_Line (To_String (Nth (File_List, F)));
                       -- end loop;
                       Append (Source_File_Seq, File_List);
                   exception
                     when File_Not_Found =>
                       null;
                 end;
                  -- ada files
                  declare
                      Dir : constant String := Nth(Source_Paths,Src_Dir) ;
                      File_List : Spelling_Seq;
                   begin
                       Check_Is_Directory (Dir);
                       File_List := Expand_Wildcard
                           (Dir, "*.spc", Include_Dir => True,
                           Walk_Subdirectories => True);
                       -- for F in 1 .. Length (File_List) loop
                           -- Put_Line (To_String (Nth (File_List, F)));
                       -- end loop;
                       Append (Source_File_Seq, File_List);
                   exception
                     when File_Not_Found =>
                       null;
                 end;
                  -- ada files
                  declare
                      Dir : constant String := Nth(Source_Paths,Src_Dir) ;
                      File_List : Spelling_Seq;
                   begin
                       Check_Is_Directory (Dir);
                       File_List := Expand_Wildcard
                           (Dir, "*.bdy", Include_Dir => True,
                           Walk_Subdirectories => True);
                       -- for F in 1 .. Length (File_List) loop
                           -- Put_Line (To_String (Nth (File_List, F)));
                       -- end loop;
                       Append (Source_File_Seq, File_List);
                   exception
                     when File_Not_Found =>
                       null;
                 end;
                 -- TBD: other ada extensions?
              end loop;
          else
              Inspection.Main_Routine := Inspector;
          end if;


          -- find all the possible source files in the zipfiles
          if not Is_Null_Seq (Zip_Source_Paths) and then
            Length (Zip_Source_Paths)> 0 then
              -- non null for split mode only
              Inspection.Main_Routine := Inspector_BE;
              for Src_Dir in 1 .. Length (Zip_Source_Paths) loop
                  -- java files
                  declare
                      Dir : constant String := Nth(Zip_Source_Paths,Src_Dir) ;
                      File_List : Var_Spelling_Seq;
                      use Spellings.Seqs;
                   begin
                       if Has_Zip_Extension(Dir) then
                            Zip_Files.Expand_Wildcard
                              (Zip_Files.Get_Zip_File(Intern(Dir)).all,
                               Pattern => "*.java",
                               Walk_Subdirectories => True,
                               Include_Dir => True,
                               Sort_Them => False,
                               Result => File_List);

                           Spellings.Seqs.Append (Zip_Source_File_Seq,
                             Freeze (File_List));
                       end if;

                   exception
                     when File_Not_Found =>
                       null;
                 end;
                  -- ada files
                  declare
                      Dir : constant String := Nth(Zip_Source_Paths,Src_Dir) ;
                      File_List : Var_Spelling_Seq;
                   begin
                       if Has_Zip_Extension(Dir) then
                            Zip_Files.Expand_Wildcard
                              (Zip_Files.Get_Zip_File(Intern(Dir)).all,
                               Pattern => "*.ada",
                               Walk_Subdirectories => True,
                               Include_Dir => True,
                               Sort_Them => False,
                               Result => File_List);

                           Spellings.Seqs.Append (Zip_Source_File_Seq,
                             Freeze(File_List));
                       end if;
                   exception
                     when File_Not_Found =>
                       null;
                 end;
                  -- ada files
                  declare
                      Dir : constant String := Nth(Zip_Source_Paths,Src_Dir) ;
                      File_List : Var_Spelling_Seq;
                   begin
                       if Has_Zip_Extension(Dir) then
                            Zip_Files.Expand_Wildcard
                              (Zip_Files.Get_Zip_File(Intern(Dir)).all,
                               Pattern => "*.ads",
                               Walk_Subdirectories => True,
                               Include_Dir => True,
                               Sort_Them => False,
                               Result => File_List);

                           Spellings.Seqs.Append (Zip_Source_File_Seq,
                             Freeze(File_List));
                       end if;
                   exception
                     when File_Not_Found =>
                       null;
                 end;
                  -- ada files
                  declare
                      Dir : constant String := Nth(Zip_Source_Paths,Src_Dir) ;
                      File_List : Var_Spelling_Seq;
                   begin
                       if Has_Zip_Extension(Dir) then
                            Zip_Files.Expand_Wildcard
                              (Zip_Files.Get_Zip_File(Intern(Dir)).all,
                               Pattern => "*.spc",
                               Walk_Subdirectories => True,
                               Include_Dir => True,
                               Sort_Them => False,
                               Result => File_List);

                           Spellings.Seqs.Append (Zip_Source_File_Seq,
                             Freeze(File_List));
                       end if;
                   exception
                     when File_Not_Found =>
                       null;
                 end;
                  -- ada files
                  declare
                      Dir : constant String := Nth(Zip_Source_Paths,Src_Dir) ;
                      File_List : Var_Spelling_Seq;
                   begin
                       if Has_Zip_Extension(Dir) then
                            Zip_Files.Expand_Wildcard
                              (Zip_Files.Get_Zip_File(Intern(Dir)).all,
                               Pattern => "*.bdy",
                               Walk_Subdirectories => True,
                               Include_Dir => True,
                               Sort_Them => False,
                               Result => File_List);

                           Spellings.Seqs.Append (Zip_Source_File_Seq,
                              Freeze(File_List));
                       end if;
                   exception
                     when File_Not_Found =>
                       null;
                 end;
                 -- TBD: other ada extensions?
              end loop;
          else
              Inspection.Main_Routine := Inspector;
          end if;

          -- Init message patterns in all cases, as we may have to
          -- display old messages
          Init_Message_Patterns;
          -- Check license key
          Utils.License_Key_Support.Set_Degraded_Mode
              (Inspection.Degraded_Mode);

          for Part_Num in 1..Length(Xml_File_List) loop
            declare
                One_Partition_File_Name : constant String :=
                    To_String(Nth( Xml_File_List, Part_Num));
                use ST.Race_Post_Html;
                Pool_Name : aliased constant String := "Current_File_Pool";
                Messages : aliased Message_Pool(Pool_Name'Unchecked_Access);
                Read_Result : Inspection_Info_Record ;
            begin


                  declare
                    Timer_Name : aliased constant String :=
                            "Time to read xml_file";
                    -- this timer is already in the xml reader
                    --Dummy      : Timing.Timer (Timer_Name'Unchecked_Access);
                    --pragma Unreferenced (Dummy);

                  begin
                      Read_Result := Read_All_Messages(
                        File_Name=> One_Partition_File_Name,
                        Messages => Messages'Unchecked_Access);
                  end;

                  if First_Good_Partition = 0 then
                    First_Good_Partition := Integer(Part_Num);
                  end if;

                  if Message_Pattern_Path /= null then
                      -- overwrite the message_pattern file from the inspection
                      -- record.
                      Inspection.Message_Patterns_Path := Intern
                        (Message_Pattern_Path.all);
                  end if;
                  if Is_Picky /= null then
                      -- overwrite the message-quantity from the inspection
                      -- record.
                      -- TBD: For backward compatibility
                      if Is_Picky.all then
                          Inspection.Message_Quantity := Max;
                      else
                          Inspection.Message_Quantity := Normal;
                      end if;
                  end if;
                  if Message_Quantity /= null then
                      -- overwrite the message-quantity from the inspection
                      -- record.
                      begin
                          Inspection.Message_Quantity :=
                            Message_Quantity_Enum'Value(Message_Quantity.all);
                      exception
                          when others =>
                              null;
                      end;
                  end if;

                  if not  File_Name_To_Messages_Mappings.Is_Empty
                      (Read_Result.All_Messages.all) then
                    -- html listings are generated on the last good partition
                    -- even if there were no class file in that partition
                    if Read_Result.Default_Lang /=
                        ST.Languages.Undefined_Language then
                      ST.Languages.Set_Current_Language(
                          Read_Result.Default_Lang);
                    end if;

                    -- if there is a search paths for source files, match the
                    -- source given in the xml with the known source-paths
                    if not Is_Null_Seq (Source_Paths) then
                        declare
                            procedure Match_One_File(
                              File_Name : Spelling;
                              File_Info : Messages_Per_File_Ptr)  is

                              use DB.Source_Files, Spelling_Seqs;
                            begin
                                for File in 1 .. Length (Source_File_Seq)
                                loop
                                  declare
                                      Long_File_Name : constant Spelling :=
                                          Nth (Source_File_Seq, File);
                                  begin
                                      -- convert to lower, as the spellings in
                                      -- source_file_seq have all been converted
                                    if Is_Suffix
                                      (To_String (Long_File_Name),
                                       To_Lower (To_String (File_Name))) then
                                       -- we found a correspondance
                                       Insert_Source
                                         (File_Name, Long_File_Name);
                                       -- Put_Line ("Found correspondance");
                                       return;
                                    end if;
                                  end;

                                end loop;
                            end Match_One_File;

                            procedure Match_All_Files is new
                                File_Name_To_Messages_Mappings.Iterate(
                                    Match_One_File );
                        begin
                            Match_All_Files (Read_Result.All_Messages.all);
                        end;
                    end if;

                    -- if there is a search paths for source files in zip files,
                    -- match the source given in the xml with the known
                    -- source-paths for zip files
                    if not Is_Null_Seq (Zip_Source_Paths) then
                        declare
                            procedure Match_One_File(
                              File_Name : Spelling;
                              File_Info : Messages_Per_File_Ptr)  is

                              use DB.Source_Files, Spelling_Seqs;
                            begin
                                for File in 1 .. Length (Zip_Source_File_Seq)
                                loop
                                  declare
                                      Long_File_Name : constant Spelling :=
                                          Nth (Zip_Source_File_Seq, File);
                                  begin
                                    if Is_Suffix
                                      (To_String (Long_File_Name),
                                       To_String (File_Name)) then
                                       -- we found a correspondance
                                       Insert_Source
                                         (File_Name, Long_File_Name);
                                       -- Put_Line ("Found correspondance");
                                       return;
                                    end if;
                                  end;

                                end loop;
                            end Match_One_File;

                            procedure Match_All_Files is new
                                File_Name_To_Messages_Mappings.Iterate(
                                    Match_One_File );
                        begin
                            Match_All_Files (Read_Result.All_Messages.all);
                        end;
                    end if;

                    if Generate_Text_Listings then
                      if Inspection.Logging_Output then
                        if not Is_Partitioned_Inspection then
                          ST.Logging.Add_To_Log_File
                             ("Generating text listings");
                        else
                          ST.Logging.Add_To_Log_File
                             ("Generating text listings for partition "&
                             Image(Integer(Inspection.Partition_Number)) &
                              " at " &
                             Timing.Image(Ada.Calendar.Clock) &".");
                        end if;
                        ST.Logging.Close_Log_File;
                      end if;

                      Allocate_Filename_FileInfo_Map;
                      Lister_All_Files (Read_Result.All_Messages.all);
                      Free_FileInfo_Map;
                    end if;

                  end if;
            exception
                when Exc: others =>
                  Program_Exit.Update_Error_Status
                      (Status => Recoverable_Internal_Error);
                  if Inspection.Logging_Output then
                    if Is_Partitioned_Inspection then
                       ST.Logging.Add_To_Log_File
                         ("Problem in partition " &
                            Image(Integer(Part_Num)) &
                            " : " & Exception_Information(Exc));
                    else
                       ST.Logging.Add_To_Log_File
                         ("Problem generating output: " &
                            Exception_Information(Exc));
                    end if;
                  else
                    if Is_Partitioned_Inspection then
                       Put_Line_Err
                         ("Error generating output for partition " &
                            Image(Integer(Part_Num)) & ".");
                    else
                        Put_Line_Err ("Error generating output." );
                    end if;
                  end if;
            end;
          end loop; -- for each partition

--        end;
        if Inspection.Logging_Output then
            ST.Logging.Add_To_Log_File
                  ("Done at "& Timing.Image(Ada.Calendar.Clock));
            ST.Logging.Close_Log_File;
        end if;
        if not Inspection.Minimize_Screen_Output then
            -- Overwrite current line with spaces
            Put(ASCII.CR);
            Put("                                        ");
            Put("                                       ");
            Put(ASCII.CR);
        end if;

        Program_Exit.Exit_Program;
    end;
exception
    when Exc: others =>
        if Inspection.Logging_Output then
            ST.Logging.Add_To_Log_File
                ("Fatal internal error in sal_annotator: " &
                   Exception_Information(Exc));
            ST.Logging.Close_Log_File;
        end if;
        Put_Line_Err ("sal_annotator stopped");
        if Debug_Storage_Usage then
            Storage_Tracking.Dump_Statistics;
        end if;
        Program_Exit.Update_Error_Status (Status => Fatal_Internal_Error);
end Sal_Annotator;
