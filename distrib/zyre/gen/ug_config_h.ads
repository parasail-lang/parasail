pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with x86_64_linux_gnu_bits_types_h;
with wchar_h;

package uG_config_h is

  -- This file is needed by libio to define various configuration parameters.
  --   These are always the same in the GNU C library.   

  -- Define types for libio in terms of the standard internal type names.   
   --  skipped anonymous struct anon_4

   type u_G_fpos_t is record
      uu_pos : aliased x86_64_linux_gnu_bits_types_h.uu_off_t;  -- /usr/include/_G_config.h:23
      uu_state : aliased wchar_h.uu_mbstate_t;  -- /usr/include/_G_config.h:24
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/_G_config.h:25

   --  skipped anonymous struct anon_5

   type u_G_fpos64_t is record
      uu_pos : aliased x86_64_linux_gnu_bits_types_h.uu_off64_t;  -- /usr/include/_G_config.h:28
      uu_state : aliased wchar_h.uu_mbstate_t;  -- /usr/include/_G_config.h:29
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/_G_config.h:30

  -- These library features are always available in the GNU C library.   
  -- This is defined by <bits/stat.h> if `st_blksize' exists.   
end uG_config_h;
