-- $Source: /home/projects/ada/cvs-repository/rts/portable/ada.strings.spc,v $
-- $Revision: 245876 $ $Date: 2009-07-23 15:34:40 -0400 (Thu, 23 Jul 2009) $ $Author: stt $
--        Copyright (C) 1995 by Intermetrics, Inc.
------------------------------------------------------------------------------
-- From: Ada9x RM5.0(A.4.1)
------------------------------------------------------------------------------

pragma ada_child;
package Ada.Strings is
   pragma Pure(Strings);


   Space      : constant Character      := ' ';
   Wide_Space : constant Wide_Character := ' ';
   Length_Error, Pattern_Error, Index_Error, Translation_Error : exception;

   type Alignment  is (Left, Right, Center);
   type Truncation is (Left, Right, Error);
   type Membership is (Inside, Outside);
   type Direction  is (Forward, Backward);

   type Trim_End   is (Left, Right, Both);

end Ada.Strings;

-- From MonoCM file ada.strings.spc,v.
