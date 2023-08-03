/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */

#include <termbox.h>
#include <iostream>

/**
   UNDER CONSTRUCTION...

   C++ wrapper for termbox.
*/
namespace tb {
    typedef struct tb_cell cell;

    /**
       A wrapper around tb_event, primarily so that we can guaranty
       clean initialization of all tb_event properties.
    */
    struct Event : public tb_event{
      public:
      Event();
      Event(Event const &);
      Event(tb_event const &);
      Event(tb_event const *);
      Event & operator=(Event const &);
      Event & operator=(tb_event const &);
      ~Event();
      private:
      /** Zeroes all property values. */
      void clear();
    };

    std::ostream & operator<<(std::ostream & out, Event const & ev);

    /**
       A sentry type which calls tb_init() when the first instance is
       created and tb_shutdown() when the last instance is destructed.
     */
    class TbModeSentry{
      public:
      /**
         When the first instance of this class is constructed, if
         tb_width()<0 (i.e. tb_init() has not been called, or
         tb_shutdown() has) then tb_init() is called, else this has
         no visible side-effects.
      */
      TbModeSentry();
      /**
         When the last instance of this class is destructed, if this
         class called tb_init() then tb_shutdown() is called else this
         has no visible side effects.
      */
      ~TbModeSentry();

      private:
      /** Incremented by the ctor, decremented by the dtor. */
      static int count;
      /** True if this type's constructor calls tb_init(). */
      static bool thisClassStartedTb;
      /** Not implemented. */
      TbModeSentry( TbModeSentry const & );
      /** Not implemented. */
      TbModeSentry & operator=( TbModeSentry const & );
    };
    
}
