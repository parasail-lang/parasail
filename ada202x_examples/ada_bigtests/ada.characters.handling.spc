-- $Source: /home/projects/ada/cvs-repository/rts/portable/ada.characters.handling.spc,v $
-- $Revision: 245876 $ $Date: 2009-07-23 15:34:40 -0400 (Thu, 23 Jul 2009) $ $Author: stt $
--        Copyright (C) 1995 by Intermetrics, Inc.

-- ARM(5;0) C.3.2

-- Note: This file is used in the compiler.

package Ada.Characters.Handling is

    pragma Pure(Handling);

    pragma Suppress( Elaboration_Check );


--Character classification functions

  function Is_Control           (Item : in Character) return Boolean;
  function Is_Graphic           (Item : in Character) return Boolean;
  function Is_Letter            (Item : in Character) return Boolean;
  function Is_Lower             (Item : in Character) return Boolean;
  function Is_Upper             (Item : in Character) return Boolean;
  function Is_Basic             (Item : in Character) return Boolean;
  function Is_Digit             (Item : in Character) return Boolean;
  function Is_Decimal_Digit     (Item : in Character) return Boolean renames
    Is_Digit;
  function Is_Hexadecimal_Digit (Item : in Character) return Boolean;
  function Is_Alphanumeric      (Item : in Character) return Boolean;
  function Is_Special           (Item : in Character) return Boolean;


--Conversion functions for Character and String

  function To_Lower (Item : in Character) return Character;
  function To_Upper (Item : in Character) return Character;
  function To_Basic (Item : in Character) return Character;

  function To_Lower (Item : in String) return String;
  function To_Upper (Item : in String) return String;
  function To_Basic (Item : in String) return String;


--Classifications of and conversions between Character and ISO 646

  subtype ISO_646 is
    Character range Character'Val(0) .. Character'Val(127);

  function Is_ISO_646 (Item : in Character) return Boolean;
  function Is_ISO_646 (Item : in String)    return Boolean;

  function To_ISO_646 (Item       : in Character;
                       Substitute : in ISO_646 := ' ')
    return ISO_646;

  function To_ISO_646 (Item       : in String;
                       Substitute : in ISO_646 := ' ')
    return String;


--Classifications of and conversions between Wide_Character and Character.

  function Is_Character (Item : in Wide_Character) return Boolean;
  function Is_String    (Item : in Wide_String)    return Boolean;


  function To_Character (Item       : in Wide_Character;
                         Substitute : in Character := ' ')
    return Character;

  function To_String    (Item       : in Wide_String;
                         Substitute : in Character := ' ')
    return String;


  function To_Wide_Character (Item : in Character) return Wide_Character;

  function To_Wide_String    (Item : in String)    return Wide_String;

end Ada.Characters.Handling;

-- From MonoCM file ada.characters.handling.spc,v.
