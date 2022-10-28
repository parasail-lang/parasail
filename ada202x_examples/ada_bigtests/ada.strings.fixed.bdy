-- $Source: /home/projects/ada/cvs-repository/rts/not_mccabe/ada.strings.fixed.bdy,v $
-- $Revision: 245875 $ $Date: 2009-07-23 15:31:34 -0400 (Thu, 23 Jul 2009) $ $Author: stt $
--        Copyright (C) 1995 by Intermetrics, Inc.

with Ada.Strings.Maps;
with System.RTS.Strings_Search;

package body Ada.Strings.Fixed is

    -- Note well: All functions returning String must
    -- return a result with 'First = 1.  In many cases, we
    -- convert to a subtype called Slide to slide the bounds
    -- to 1..whatever.

    pragma Suppress( Elaboration_Check );

    package ASM renames Ada.Strings.Maps;
    package RSS renames System.RTS.Strings_Search;

    procedure Move (Source  : in  String;
                    Target  : out String;
                    Drop    : in  Truncation := Error;
                    Justify : in  Alignment  := Left;
                    Pad     : in  Character  := Space) is
        SLen : constant Integer := Source'Length;
        TLen : constant Integer := Target'Length;
        TSDiff : constant Integer := TLen - SLen;
        TSPad  : constant String(1 .. TSDiff) := (others => Pad);
        STDiff : constant Integer := SLen - TLen;
        STPad  : constant String(1 .. STDiff) := (others => Pad);
    begin
        if SLen = TLen then
          Target := Source;
        elsif SLen < TLen then
          case Justify is
            when Left =>
              Target(Target'First .. Target'First + (SLen - 1)) := Source;
              Target(Target'First + SLen .. Target'Last) := TSPad;
            when Right =>
              Target(Target'Last - (SLen - 1) .. Target'Last) := Source;
              Target(Target'First .. Target'Last - (SLen - 1) - 1) := TSPad;
            when Center =>
              Target(Target'First .. Target'First + (TSDiff / 2) - 1) :=
                  (others => Pad);
              Target(Target'First + (TSDiff / 2) ..
                     Target'First + (TSDiff / 2) + (SLen - 1)) := Source;
              Target(Target'Last - ((TSDiff + 1)/2) + 1 .. Target'Last) :=
                  (others => Pad);
          end case;
        else -- SLen > TLen then
          case Drop is
            when Left =>
              Target := Source(Source'Last - (Tlen - 1) .. Source'Last);
            when Right =>
              Target := Source(Source'First .. Source'First + (Tlen - 1));
            when Error =>
              case Justify is
                when Left =>
                  if Source(Source'Last - (STDiff-1) .. Source'Last) /= STPad
                  then
                     raise Length_Error;
                  end if;
                  Target := Source(Source'First .. Source'First + (TLen - 1));
                when Right =>
                  if Source(Source'First .. Source'First + (STDiff-1)) /= STPad
                  then
                     raise Length_Error;
                  end if;
                  Target := Source(Source'Last -  (TLen - 1) .. Source'Last);
                when others =>
                  raise Length_Error;
              end case;
          end case;
        end if;
    end Move;

    function Index (Source   : in String;
                    Pattern  : in String;
                    Going    : in Direction := Forward;
                    Mapping  : in Maps.Character_Mapping :=
                                  Maps.Identity)
       return Natural
    renames RSS.Index;

    function Index (Source   : in String;
                    Pattern  : in String;
                    Going    : in Direction := Forward;
                    Mapping  : in Maps.Character_Mapping_Function)
       return Natural
    renames RSS.Index;

    function Index (Source : in String;
                    Set    : in Maps.Character_Set;
                    Test   : in Membership := Inside;
                    Going  : in Direction  := Forward)
       return Natural
    renames RSS.Index;

    function Index_Non_Blank (Source : in String;
                              Going  : in Direction := Forward)
       return Natural
    renames RSS.Index_Non_Blank;

    function Count (Source   : in String;
                    Pattern  : in String;
                    Mapping  : in Maps.Character_Mapping :=
                                  Maps.Identity)
       return Natural
    renames RSS.Count;

    function Count (Source   : in String;
                    Pattern  : in String;
                    Mapping  : in Maps.Character_Mapping_Function)
       return Natural
    renames RSS.Count;

    function Count (Source   : in String;
                    Set      : in Maps.Character_Set)
       return Natural
    renames RSS.Count;

    procedure Find_Token (Source : in String;
                          Set    : in Maps.Character_Set;
                          Test   : in Membership;
                          First  : out Positive;
                          Last   : out Natural)
    renames RSS.Find_Token;

    function Translate (Source  : in String;
                        Mapping : in Maps.Character_Mapping)
       return String is
        Result : String (1 .. Source'Length);
    begin
        for Index in Result'First .. Result'Last loop
            Result(Index) :=
              ASM.Value(Mapping, Source(Source'First + Index - 1));
        end loop;
        return Result;
    end Translate;

    function Translate (Source  : in String;
                        Mapping : in Maps.Character_Mapping_Function)
       return String is
        Result : String (1 .. Source'Length);
    begin
        for Index in Result'First .. Result'Last loop
            Result(Index) := Mapping.all(Source(Source'First + Index - 1));
        end loop;
        return Result;
    end Translate;

    procedure Translate (Source  : in out String;
                         Mapping : in Maps.Character_Mapping) is
    begin
        Source := Translate(Source, Mapping);
    end Translate;

    procedure Translate (Source  : in out String;
                         Mapping : in Maps.Character_Mapping_Function)
       is
    begin
        Source := Translate(Source, Mapping);
    end Translate;

    function Replace_Slice (Source   : in String;
                            Low      : in Positive;
                            High     : in Natural;
                            By       : in String)
       return String is
    begin
        if Low > Source'Last + 1 or else High < Source'First - 1 then
            raise Index_Error;
        end if;
        if High >= Low then
            declare
                subtype Slide is String
                  (1 .. Source'Length - Source(Low..High)'Length + By'Length);
            begin
                return Slide(Source(Source'First .. Low - 1) &
                             By &
                             Source(High + 1 .. Source'Last));
            end;
        else
            return Insert(Source   => Source,
                          Before   => Low,
                          New_Item => By);
        end if;
    end Replace_Slice;

    procedure Replace_Slice (Source   : in out String;
                             Low      : in Positive;
                             High     : in Natural;
                             By       : in String;
                             Drop     : in Truncation := Error;
                             Justify  : in Alignment  := Left;
                             Pad      : in Character  := Space) is
    begin
        Move (Replace_Slice(Source, Low, High, By), Source, Drop, Justify, Pad);
    end Replace_Slice;

    function Insert (Source   : in String;
                     Before   : in Positive;
                     New_Item : in String)
       return String is
        Result : String(1 .. Source'Length + New_Item'Length);
    begin
        if Before not in  Source'First .. Source'Last + 1 then
            raise Index_Error;
        end if;
        Result := Source(Source'First .. Before - 1) &
                  New_Item &
                  Source(Before .. Source'Last);
        return Result;
    end Insert;

    procedure Insert (Source   : in out String;
                      Before   : in Positive;
                      New_Item : in String;
                      Drop     : in Truncation := Error) is
    begin
        Move (Source => Insert (Source, Before, New_Item),
              Target => Source,
              Drop   => Drop);
    end Insert;

    function Overwrite (Source   : in String;
                        Position : in Positive;
                        New_Item : in String)
            return String is
    begin
        if Position not in Source'First .. Source'Last + 1 then
            raise Index_Error;
        end if;
        declare
            Result: constant String :=
              Source(Source'First .. Position - 1) &
              New_Item &
              Source(Position + New_Item'Length .. Source'Last);
              subtype Slide is String(1 .. Result'Length);
        begin
            return Slide(Result);
        end;
    end Overwrite;

    procedure Overwrite (Source   : in out String;
                         Position : in Positive;
                         New_Item : in String;
                         Drop     : in Truncation := Right) is
    begin
        Move (Source => Overwrite (Source, Position, New_Item),
              Target => Source,
              Drop   => Drop);
    end Overwrite;

    function Delete (Source  : in String;
                     From    : in Positive;
                     Through : in Natural)
       return String is
        subtype Slide is String(1 .. Source'Length);
    begin
        if From not in Source'Range or else Through > Source'Last then
            raise Index_Error;
        end if;
        if From <= Through then
            return Replace_Slice(Source, From, Through, "");
        else
            return Slide(Source);
        end if;
    end Delete;

    procedure Delete (Source  : in out String;
                      From    : in Positive;
                      Through : in Natural;
                      Justify : in Alignment := Left;
                      Pad     : in Character := Space) is
    begin
        Move (Source  => Delete (Source, From, Through),
              Target  => Source,
              Justify => Justify,
              Pad     => Pad);
    end Delete;

    function Trim
      (Source : in String;
       Side   : in Trim_End)
       return String
    is
        Low : Integer := Source'First;
        High : Integer := Source'Last;
    begin
        if Side /= Right then
            Low := Index_Non_Blank(Source, Forward);
            if Low = 0 then -- All blanks?
                return "";
            end if;
        end if;

        if Side /= Left then
            High := Index_Non_Blank (Source, Backward);
        end if;

        declare
            subtype Slide is String(1 .. High - Low + 1);
        begin
            return Slide(Source(Low .. High));
        end;
    end Trim;

    procedure Trim (Source  : in out String;
                    Side    : in Trim_End;
                    Justify : in Alignment := Left;
                    Pad     : in Character := Space) is
    begin
        Move (Trim(Source, Side),
              Source,
              Justify => Justify,
              Pad => Pad);
    end Trim;

    function Trim
      (Source : in String;
       Left   : in Maps.Character_Set;
       Right  : in Maps.Character_Set)
       return String
    is
       High, Low : Integer;
    begin
       Low := Index (Source, Set => Left, Test  => Outside, Going => Forward);

       --  Case where source comprises only characters in Left

       if Low = 0 then
          return "";
       end if;

       High :=
         Index (Source, Set => Right, Test  => Outside, Going => Backward);

       --  Case where source comprises only characters in Right

       if High = 0 then
          return "";
       end if;

       declare
           subtype Slide is String(1 .. High - Low + 1);
       begin
           return Slide(Source(Low .. High));
       end;
    end Trim;

    procedure Trim (Source  : in out String;
                    Left    : in Maps.Character_Set;
                    Right   : in Maps.Character_Set;
                    Justify : in Alignment := Strings.Left;
                    Pad     : in Character := Space) is
    begin
        Move(Trim(Source, Left, Right), Source, Justify => Justify, Pad => Pad);
    end Trim;

    function Head (Source : in String;
                   Count  : in Natural;
                   Pad    : in Character := Space)
       return String is
        Padding : String(1 .. Count - Source'Length) := (others => Pad);
        subtype Slide is String(1 .. Count);
    begin
        if Count <= Source'Length then
            return Slide(Source(Source'First .. Source'First + Count - 1));
        else
            return Slide(Source & Padding);
        end if;
    end Head;

    procedure Head (Source  : in out String;
                    Count   : in Natural;
                    Justify : in Alignment := Left;
                    Pad     : in Character := Space) is
    begin
        Move(Source  => Head(Source, Count, Pad),
             Target  => Source,
             Drop    => Error,
             Justify => Justify,
             Pad     => Pad);
    end Head;

    function Tail (Source : in String;
                   Count  : in Natural;
                   Pad    : in Character := Space)
       return String is
        Padding : String(1 .. Count - Source'Length) := (others => Pad);
        subtype Slide is String(1 .. Count);
    begin
        if Count <= Source'Length then
            return Slide(Source(Source'Last - Count + 1 .. Source'Last));
        else
            return Slide(Padding & Source);
        end if;
    end Tail;

    procedure Tail (Source  : in out String;
                    Count   : in Natural;
                    Justify : in Alignment := Left;
                    Pad     : in Character := Space) is
    begin
        Move(Source  => Tail(Source, Count, Pad),
             Target  => Source,
             Drop    => Error,
             Justify => Justify,
             Pad     => Pad);
    end Tail;

    function "*" (Left  : in Natural;
                  Right : in Character)
        return String
    is
        Result : String(1 .. Left) := (others => Right);
    begin
        return Result;
    end "*";

    function "*" (Left  : in Natural;
                  Right : in String)
       return String
    is
        Len    : constant Integer := Right'Length;
        Result : String(1 .. Left * Len);
        From   : Integer := 1;
    begin
        for Index in 1 .. Left loop
            Result (From .. From + Len - 1) := Right(Right'First .. Right'Last);
            From := From + Len;
        end loop;
        return Result;
    end "*";

end Ada.Strings.Fixed;

-- From MonoCM file ada.strings.fixed.bdy,v.
