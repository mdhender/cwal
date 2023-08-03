/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
#if !defined(NET_WANDERINGHORSE_CWAL_S2_CGIMOD_H_INCLUDED)
#define NET_WANDERINGHORSE_CWAL_S2_CGIMOD_H_INCLUDED
/**
   This file houses the loadable module part of the s2_cgi API. The intention of
   this public interface is to experiment with loadable modules which require bits
   of the CGI interface.
*/
#include "s2_cgi.h"

#if defined(__cplusplus)
extern "C" {
#endif


/**
   Custom client-side native type to bind to s2.

   TODO?: we don't strictly *need* this layer of indirection. We can
   move vSelf to s2_cgi. However, the s2_cgi API was developed/derived
   from code at a level below loadable modules, intended to be used in
   standalone apps, and the cgimod bits are the glue between that
   level and the loadable module interface.
*/
struct cgi_native {
  cwal_value * vSelf;
  s2_cgi cgi;
};
typedef struct cgi_native cgi_native;

/**
   Given an s2_engine and a cwal_value (a cwal_native instance), this
   tries to extract a cgi_native from it. On success, the native is
   returned, else 0 is returned.
*/
cgi_native * cgi_native_from_value( s2_engine * se, cwal_value * v );

#if defined(__cplusplus)
} /*extern "C"*/
#endif

#endif
/* NET_WANDERINGHORSE_CWAL_S2_CGIMOD_H_INCLUDED */
