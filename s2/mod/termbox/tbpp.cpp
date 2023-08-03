/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */

#include "tbpp.hpp"

namespace tb{

  // tb_event tb_event_empty = {0,0,0,0,0,0,0,0};
  //const event event_empty;


  Event::Event(){
    this->clear();
  }

  Event::Event(Event const &other){
    this->clear();
    *static_cast<tb_event*>(this) = static_cast<const tb_event &>(other);
  }

  Event::Event(tb_event const &other){
    this->clear();
    if(this != &other){
      *static_cast<tb_event*>(this) = other;
    }
  }

  Event::Event(tb_event const *other){
    this->clear();
    *this = *other;
  }

  Event & Event::operator=(Event const &other){
    if(this != &other){
      *static_cast<tb_event*>(this) = static_cast<const tb_event&>(other);
    }
    return *this;
  }
  
  Event & Event::operator=(tb_event const &other){
    if(this != &other){
      *static_cast<tb_event*>(this) = other;
    }
    return *this;
  }
  
  void Event::clear(){
    this->type = this->mod = 0;
    this->key = 0;
    this->ch = 0;
    this->w = this->h = this->x = this->y = -1;
  }

  Event::~Event(){
  }

  std::ostream & operator<<(std::ostream & out, Event const & ev){
    out << "tb::Event type="<<(int)ev.type<<", mod="<<(int)ev.mod
        <<", key="<<ev.key<<", ch="<<ev.ch
        <<", w="<<ev.w<<", h="<<ev.h;
    return out;
  }
  
  int TbModeSentry::count = 0;
  bool TbModeSentry::thisClassStartedTb = false;
  TbModeSentry::TbModeSentry(){
    if(1 == ++TbModeSentry::count){
      if( tb_width() < 0 ){
        tb_init();
        TbModeSentry::thisClassStartedTb = true;
      }
    }
  }

  TbModeSentry::~TbModeSentry(){
    if(0 == --TbModeSentry::count &&
       TbModeSentry::thisClassStartedTb){
      TbModeSentry::thisClassStartedTb = false;
      if(tb_width()>0){
        /* ^^^ just to be sure (avoid a tb-side assertion) */
        tb_shutdown();
      }
    }
  }


}
