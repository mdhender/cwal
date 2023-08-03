/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
#include <iostream>
#include <cassert>

#include "tbpp.hpp"

#define CERR std::cerr << __FILE__ << ':' << std::dec << __LINE__ << ':' << __FUNCTION__ << "(): "
#define COUT std::cout << __FILE__ << ':' << std::dec << __LINE__ << ':' << __FUNCTION__ << "(): "

int main(){
  tb::Event tv;
  
  assert(tb_width()<0);
  {
    tb::TbModeSentry const sentry;
    assert(tb_width()>0);
    tb_poll_event(&tv);
  }
  assert(tb_width()<0);
  COUT << tv << std::endl;
  return 0;
}
