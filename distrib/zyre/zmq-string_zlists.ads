with ZMQ.Generic_Zlists;
with Interfaces.C.Strings;
package ZMQ.String_Zlists is
  new Generic_Zlists (Interfaces.C.Strings.chars_ptr, null);
