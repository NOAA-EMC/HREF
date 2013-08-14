#if 0
//
// Make this header file available as ESMFVersionDefine.h in order to build
// NEMS against an ESMF installation that is compatible with the ESMF 5.2.0r API.
//
#endif

#undef ESMF_3

#ifndef ESMF_MAJOR_VERSION
#define ESMF_MAJOR_VERSION 5
#define ESMF_MINOR_VERSION 2
#endif

#include "./ESMFVersionLogic.h"
