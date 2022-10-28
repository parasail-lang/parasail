with System;
with czmq_library_h;
generic
   type Item_Ptr is private;
   -- Should be same size as System.Address or a "thin" access value.
   Null_Ptr : Item_Ptr;
package ZMQ.Generic_Zlists is
   pragma Assert (Item_Ptr'Size = System.Address'Size);

   --  This is a stable class, and may not change except for emergencies. It
   --  is provided in stable builds.
   --  Comparison function e.g. for sorting and removing.
   --  typedef int (zlist_compare_fn) (
   --     void *item1, void *item2);
   type Zlist_Compare_Fn is
     access function (Item1, Item2 : Item_Ptr) return Integer;

   --  Callback function for zlist_freefn method
   --  typedef void (zlist_free_fn) (
   --     void *data);
   type Zlist_Free_Fn is access procedure (Data : Item_Ptr);

   type Zlist is tagged limited private;
   --  Create a new list container
   --  CZMQ_EXPORT zlist_t *
   --  zlist_new (void);
   function Zlist_New return Zlist;

   --  Destroy a list container
   --  CZMQ_EXPORT void
   --  zlist_destroy (zlist_t **self_p);
   procedure Zlist_Destroy (Self : in out Zlist);

   --  Return the item at the head of list. If the list is empty, returns NULL.
   --  Leaves cursor pointing at the head item, or NULL if the list is empty.
   --  CZMQ_EXPORT void *
   --  zlist_first (zlist_t *self);
   function Zlist_First (Self : in out Zlist) return Item_Ptr;

   --  Return the next item. If the list is empty, returns NULL. To move to
   --  the start of the list call zlist_first (). Advances the cursor.
   --  CZMQ_EXPORT void *
   --  zlist_next (zlist_t *self);
   function Zlist_Next (Self : in out Zlist) return Item_Ptr;

   --  Return the item at the tail of list. If the list is empty, returns NULL.
   --  Leaves cursor pointing at the tail item, or NULL if the list is empty.
   --  CZMQ_EXPORT void *
   --  zlist_last (zlist_t *self);
   function Zlist_Last (Self : in out Zlist) return Item_Ptr;

   --  Return first item in the list, or null, leaves the cursor
   --  CZMQ_EXPORT void *
   --  zlist_head (zlist_t *self);
   function Zlist_Head (Self : in out Zlist) return Item_Ptr;

   --  Return last item in the list, or null, leaves the cursor
   --  CZMQ_EXPORT void *
   --  zlist_tail (zlist_t *self);
   function Zlist_Tail (Self : in out Zlist) return Item_Ptr;

   --  Return the current item of list. If the list is empty, returns NULL.
   --  Leaves cursor pointing at the current item, or NULL if list is empty.
   --  CZMQ_EXPORT void *
   --  zlist_item (zlist_t *self);
   function Zlist_Item (Self : in out Zlist) return Item_Ptr;

   subtype Return_Status is Integer range -1 .. 0;

   --  Append an item to the end of the list, return 0 if OK or -1 if this
   --  failed for some reason (out of memory). Note that if a duplicator has
   --  been set, this method will also duplicate the item.
   --  CZMQ_EXPORT int
   --  zlist_append (zlist_t *self, void *item);
   function Zlist_Append (Self : in out Zlist; Item : Item_Ptr)
     return Return_Status;

   --  Push an item to the start of the list, return 0 if OK or -1 if this
   --  failed for some reason (out of memory). Note that if a duplicator has
   --  been set, this method will also duplicate the item.
   --  CZMQ_EXPORT int
   --  zlist_push (zlist_t *self, void *item);
   function Zlist_Push (Self : in out Zlist; Item : Item_Ptr)
     return Return_Status;

   --  Pop the item off the start of the list, if any
   --  CZMQ_EXPORT void *
   --  zlist_pop (zlist_t *self);
   function Zlist_Pop (Self : in out Zlist) return Item_Ptr;

   --  Checks if item already is present. Uses compare method to determine if
   --  items are equal. If the compare method is NULL the
   --  check will only compare
   --  pointers. Returns true if item is present else false.
   --  CZMQ_EXPORT bool
   --  zlist_exists (zlist_t *self, void *item);
   function Zlist_Exists (Self : in out Zlist; Item : Item_Ptr)
     return Boolean;

   --  Remove the specified item from the list if present
   --  CZMQ_EXPORT void
   --  zlist_remove (zlist_t *self, void *item);
   procedure Zlist_Remove (Self : in out Zlist; Item : Item_Ptr);

   --  Make a copy of list. If list has autofree set, the copied list will
   --  duplicate all items, which must be strings. Otherwise, list will hold
   --  pointers back to items in the original list. If list is null, returns
   --  NULL.
   --  Caller owns return value and must destroy it when done.
   --  CZMQ_EXPORT zlist_t *
   --  zlist_dup (zlist_t *self);
   function Zlist_Dup (Self : Zlist) return Zlist;

   --  Purge all items from list
   --  CZMQ_EXPORT void
   --  zlist_purge (zlist_t *self);
   procedure Zlist_Purge (Self : in out Zlist);

   --  Return number of items in the list
   --  CZMQ_EXPORT size_t
   --  zlist_size (zlist_t *self);
   function Zlist_Size (Self : Zlist) return Long_Integer;

   --  Sort the list. If the compare function is null, sorts the list by
   --  ascending key value using a straight ASCII comparison. If you specify
   --  a compare function, this decides how items are sorted. The sort is not
   --  stable, so may reorder items with the same keys. The algorithm used is
   --  combsort, a compromise between performance and simplicity.
   --  CZMQ_EXPORT void
   --  zlist_sort (zlist_t *self, zlist_compare_fn compare);
   procedure Zlist_Sort (Self : in out Zlist; Compare : Zlist_Compare_Fn);

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
   procedure Zlist_Autofree (Self : in out Zlist);

   --  Sets a compare function for this list. The function compares two items.
   --  It returns an integer less than, equal to, or greater than zero if the
   --  first item is found, respectively, to be less than, to match, or be
   --  greater than the second item.
   --  This function is used for sorting, removal and exists checking.
   --  CZMQ_EXPORT void
   --  zlist_comparefn (zlist_t *self, zlist_compare_fn fn);
   procedure Zlist_Comparefn (Self : in out Zlist; Fn : Zlist_Compare_Fn);

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
     return Item_Ptr;

   --  Self test of this class.
   --  CZMQ_EXPORT void
   --  zlist_test (bool verbose);
   procedure Zlist_Test (Verbose : Boolean);

   type Zmq_Zlist_T_Access is access all czmq_library_h.zlist_t;

   function GetImpl (Self : in out Zlist)
     return Zmq_Zlist_T_Access;

   function GetImplAcc (Self : in out Zlist)
     return access Zmq_Zlist_T_Access;

   function GetImplAddr (Self : in out Zlist)
     return System.Address;

private

   type Zlist is tagged limited record
      Raw : aliased Zmq_Zlist_T_Access;
   end record;

end ZMQ.Generic_Zlists;
