pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;

package czmq_h is

  --  =========================================================================
  --    CZMQ - a high-level binding in C for ZeroMQ
  --    Copyright (c) the Contributors as noted in the AUTHORS file.
  --    This file is part of CZMQ, the high-level C binding for 0MQ:
  --    http://czmq.zeromq.org.
  --    This Source Code Form is subject to the terms of the Mozilla Public
  --    License, v. 2.0. If a copy of the MPL was not distributed with this
  --    file, You can obtain one at http://mozilla.org/MPL/2.0/.
  --    =========================================================================
  --  "Tell them I was a writer.
  --   A maker of software.
  --   A humanist. A father.
  --   And many things.
  --   But above all, a writer.
  --   Thank You. :)
  --   - Pieter Hintjens
  -- 

  --  These are signatures for handler functions that customize the
  --  behavior of CZMQ containers. These are shared between all CZMQ
  --  container types.
  --  -- destroy an item
   --  skipped function type czmq_destructor

  --  -- duplicate an item
   --  skipped function type czmq_duplicator

  --  - compare two items, for sorting
  --  =========================================================================
  --    CZMQ - a high-level binding in C for ZeroMQ
  --    Copyright (c) the Contributors as noted in the AUTHORS file.
  --    This file is part of CZMQ, the high-level C binding for 0MQ:
  --    http://czmq.zeromq.org.
  --    This Source Code Form is subject to the terms of the Mozilla Public
  --    License, v. 2.0. If a copy of the MPL was not distributed with this
  --    file, You can obtain one at http://mozilla.org/MPL/2.0/.
  --    =========================================================================
  --  "Tell them I was a writer.
  --   A maker of software.
  --   A humanist. A father.
  --   And many things.
  --   But above all, a writer.
  --   Thank You. :)
  --   - Pieter Hintjens
  -- 

   --  skipped function type czmq_comparator

end czmq_h;
