------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2012-2020, AdaCore                     --
--                         [Not really -- this shouldn't be copyrighted]    --
--                                                                          --
--                          G E N _ Q S O R T                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

generic
   type Index_Type is range <>;
   with function Before (Left, Right : Index_Type) return Boolean;
   with procedure Swap (Left, Right : in Index_Type);

procedure Gen_Qsort
  (First, Last : Index_Type'Base);

--  TBD: pragma Pure (Gen_Qsort);
