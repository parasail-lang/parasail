with zlist_h;
with Ada.Unchecked_Conversion;
with Interfaces.C;
package body ZMQ.Generic_Zlists is

   --  This is a stable class, and may not change except for emergencies. It
   --  is provided in stable builds.

   NYI : exception;

   function To_Item_Ptr is
     new Ada.Unchecked_Conversion (System.Address, Item_Ptr);

   --  Create a new list container
   --  CZMQ_EXPORT zlist_t *
   --  zlist_new (void);
   function Zlist_New return Zlist is
   begin
      return raise NYI;
   end;

   --  Destroy a list container
   --  CZMQ_EXPORT void
   --  zlist_destroy (zlist_t **self_p);
   procedure Zlist_Destroy (Self : in out Zlist) is
   begin
      zlist_h.zlist_destroy (Self.Raw'Address);
   end;

   --  Return the item at the head of list. If the list is empty, returns NULL.
   --  Leaves cursor pointing at the head item, or NULL if the list is empty.
   --  CZMQ_EXPORT void *
   --  zlist_first (zlist_t *self);
   function Zlist_First (Self : in out Zlist) return Item_Ptr is
   begin
      if Self.Raw = null then
         return Null_Ptr;
      else
         return To_Item_Ptr (zlist_h.zlist_first (Self.Raw));
      end if;
   end;

   --  Return the next item. If the list is empty, returns NULL. To move to
   --  the start of the list call zlist_first (). Advances the cursor.
   --  CZMQ_EXPORT void *
   --  zlist_next (zlist_t *self);
   function Zlist_Next (Self : in out Zlist) return Item_Ptr is
   begin
      if Self.Raw = null then
         return Null_Ptr;
      else
         return To_Item_Ptr (zlist_h.zlist_next (Self.Raw));
      end if;
   end;

   --  Return the item at the tail of list. If the list is empty, returns NULL.
   --  Leaves cursor pointing at the tail item, or NULL if the list is empty.
   --  CZMQ_EXPORT void *
   --  zlist_last (zlist_t *self);
   function Zlist_Last (Self : in out Zlist) return Item_Ptr is
   begin
      return To_Item_Ptr (zlist_h.zlist_last (Self.Raw));
   end;

   --  Return first item in the list, or null, leaves the cursor
   --  CZMQ_EXPORT void *
   --  zlist_head (zlist_t *self);
   function Zlist_Head (Self : in out Zlist) return Item_Ptr is
   begin
      if Self.Raw = null then
         return Null_Ptr;
      else
         return To_Item_Ptr (zlist_h.zlist_head (Self.Raw));
      end if;
   end;

   --  Return last item in the list, or null, leaves the cursor
   --  CZMQ_EXPORT void *
   --  zlist_tail (zlist_t *self);
   function Zlist_Tail (Self : in out Zlist) return Item_Ptr is
   begin
      if Self.Raw = null then
         return Null_Ptr;
      else
         return To_Item_Ptr (zlist_h.zlist_tail (Self.Raw));
      end if;
   end;

   --  Return the current item of list. If the list is empty, returns NULL.
   --  Leaves cursor pointing at the current item, or NULL if list is empty.
   --  CZMQ_EXPORT void *
   --  zlist_item (zlist_t *self);
   function Zlist_Item (Self : in out Zlist) return Item_Ptr is
   begin
      if Self.Raw = null then
         return Null_Ptr;
      else
         return To_Item_Ptr (zlist_h.zlist_item (Self.Raw));
      end if;
   end;

   --  Append an item to the end of the list, return 0 if OK or -1 if this
   --  failed for some reason (out of memory). Note that if a duplicator has
   --  been set, this method will also duplicate the item.
   --  CZMQ_EXPORT int
   --  zlist_append (zlist_t *self, void *item);
   function Zlist_Append (Self : in out Zlist; Item : Item_Ptr)
     return Return_Status is
   begin
      return raise NYI;
   end;

   --  Push an item to the start of the list, return 0 if OK or -1 if this
   --  failed for some reason (out of memory). Note that if a duplicator has
   --  been set, this method will also duplicate the item.
   --  CZMQ_EXPORT int
   --  zlist_push (zlist_t *self, void *item);
   function Zlist_Push (Self : in out Zlist; Item : Item_Ptr)
     return Return_Status is
   begin
      return raise NYI;
   end;

   --  Pop the item off the start of the list, if any
   --  CZMQ_EXPORT void *
   --  zlist_pop (zlist_t *self);
   function Zlist_Pop (Self : in out Zlist) return Item_Ptr is
   begin
      if Self.Raw = null then
         return Null_Ptr;
      else
         return To_Item_Ptr (zlist_h.zlist_pop (Self.Raw));
      end if;
   end;

   --  Checks if item already is present. Uses compare method to determine if
   --  items are equal. If the compare method is NULL the
   --  check will only compare
   --  pointers. Returns true if item is present else false.
   --  CZMQ_EXPORT bool
   --  zlist_exists (zlist_t *self, void *item);
   function Zlist_Exists (Self : in out Zlist; Item : Item_Ptr)
     return Boolean is
   begin
      return raise NYI;
   end;

   --  Remove the specified item from the list if present
   --  CZMQ_EXPORT void
   --  zlist_remove (zlist_t *self, void *item);
   procedure Zlist_Remove (Self : in out Zlist; Item : Item_Ptr) is
   begin
      raise NYI;
   end;

   --  Make a copy of list. If list has autofree set, the copied list will
   --  duplicate all items, which must be strings. Otherwise, list will hold
   --  pointers back to items in the original list. If list is null, returns
   --  NULL.
   --  Caller owns return value and must destroy it when done.
   --  CZMQ_EXPORT zlist_t *
   --  zlist_dup (zlist_t *self);
   function Zlist_Dup (Self : Zlist) return Zlist is
   begin
      return raise NYI;
   end;

   --  Purge all items from list
   --  CZMQ_EXPORT void
   --  zlist_purge (zlist_t *self);
   procedure Zlist_Purge (Self : in out Zlist) is
   begin
      raise NYI;
   end;

   --  Return number of items in the list
   --  CZMQ_EXPORT size_t
   --  zlist_size (zlist_t *self);
   function Zlist_Size (Self : Zlist) return Long_Integer is
   begin
      if Self.Raw = null then
         return 0;
      else
         return Long_Integer (zlist_h.zlist_size (Self.Raw));
      end if;
   end;

   --  Sort the list. If the compare function is null, sorts the list by
   --  ascending key value using a straight ASCII comparison. If you specify
   --  a compare function, this decides how items are sorted. The sort is not
   --  stable, so may reorder items with the same keys. The algorithm used is
   --  combsort, a compromise between performance and simplicity.
   --  CZMQ_EXPORT void
   --  zlist_sort (zlist_t *self, zlist_compare_fn compare);
   procedure Zlist_Sort (Self : in out Zlist; Compare : Zlist_Compare_Fn) is

      use Interfaces.C;

      function Local_Compare (arg1 : System.Address; arg2 : System.Address)
        return int
        with Convention => C;

      function Local_Compare (arg1 : System.Address; arg2 : System.Address)
        return int is
      begin
         return int (Compare (To_Item_Ptr (arg1), To_Item_Ptr (arg2)));
      end Local_Compare;

   begin
      zlist_h.zlist_sort (Self.Raw, Local_Compare'Access);
   end;

   --  Set list for automatic item destruction; item values MUST be strings.
   --  By default a list item refers to a value held elsewhere. When you set
   --  this, each time you append or push a list item, zlist will take a copy
   --  of the string value. Then, when you destroy the list, it will free all
   --  item values automatically. If you use any other technique to allocate
   --  list values, you must free them explicitly before destroying the list.
   --  The usual technique is to pop list items and destroy them, until the
   --  list is empty.
   --  CZMQ_EXPORT void
   --  zlist_autofree (zlist_t *self);
   procedure Zlist_Autofree (Self : in out Zlist) is
   begin
      raise NYI;
   end;

   --  Sets a compare function for this list. The function compares two items.
   --  It returns an integer less than, equal to, or greater than zero if the
   --  first item is found, respectively, to be less than, to match, or be
   --  greater than the second item.
   --  This function is used for sorting, removal and exists checking.
   --  CZMQ_EXPORT void
   --  zlist_comparefn (zlist_t *self, zlist_compare_fn fn);
   procedure Zlist_Comparefn (Self : in out Zlist; Fn : Zlist_Compare_Fn) is
   begin
      raise NYI;
   end;

   --  Set a free function for the specified list item. When the item is
   --  destroyed, the free function, if any, is called on that item.
   --  Use this when list items are dynamically allocated, to ensure that
   --  you don't have memory leaks. You can pass 'free' or NULL as a free_fn.
   --  Returns the item, or NULL if there is no such item.
   --  CZMQ_EXPORT void *
   --  zlist_freefn
   --    (zlist_t *self, void *item, zlist_free_fn fn, bool at_tail);
   function Zlist_Freefn
     (Self : in out Zlist;
      Item : Item_Ptr;
      Fn : Zlist_Free_Fn;
      At_Tail : Boolean)
     return Item_Ptr is
   begin
      return raise NYI;
   end;

   --  Self test of this class.
   --  CZMQ_EXPORT void
   --  zlist_test (bool verbose);
   procedure Zlist_Test (Verbose : Boolean) is
   begin
      raise NYI;
   end;

   function GetImpl (Self : in out Zlist)
     return Zmq_Zlist_T_Access is
   begin
      return Self.Raw;
   end GetImpl;

   function GetImplAcc (Self : in out Zlist)
     return access Zmq_Zlist_T_Access is
   begin
      return Self.Raw'Unchecked_Access;
   end GetImplAcc;

   function GetImplAddr (Self : in out Zlist)
     return System.Address is
   begin
      return Self.Raw'Address;
   end GetImplAddr;

end ZMQ.Generic_Zlists;
