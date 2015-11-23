#if 0
//
// Make this header file available as ESMFVersionDefine.h in order to build
// NEMS against an ESMF installation that is compatible with the ESMF 6.3.0r API.
//
#endif

#undef ESMF_3

#ifndef ESMF_MAJOR_VERSION
#define ESMF_MAJOR_VERSION 6
#define ESMF_MINOR_VERSION 3
#endif

#include "./ESMFVersionLogic.h"
