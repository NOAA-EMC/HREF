#if 0
// ----------- ERROR handling macros ------------------------------------------
#endif

#define ESMF_ERR_ABORT(rc) if (ESMF_LogFoundError(rc, msg="Aborting NEMS", line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

#define ESMF_ERR_RETURN(rc,rcOut) if (ESMF_LogFoundError(rc, msg="Breaking out of subroutine", line=__LINE__, file=__FILE__, rcToReturn=rcOut)) return


#if 0
// ----------- Begin of version dependent token swapping ----------------------
#endif

#if ((ESMF_MAJOR_VERSION == 5 && ESMF_MINOR_VERSION >= 2) || ESMF_MAJOR_VERSION > 5)

#if 0
// ESMF >= 5.2.0r
#endif

#define ESMF_MAJOR_VERSION 5
#define ESMF_520r

#define esmf_mod ESMF
#define ESMF_MOD ESMF
#define ESMF_Mod ESMF

#define ESMF_TypeKind ESMF_TypeKind_Flag
#define indexCountPDimPDe indexCountPDe

#define ESMF_StateItemType ESMF_StateItem_Flag

#define terminationflag endflag
#define ESMF_ABORT ESMF_END_ABORT
#define esmf_abort ESMF_END_ABORT

#define ESMF_GridCreateShapeTile ESMF_GridCreate

#define STATENAME name
#define FIELDNAME fieldName
#define FIELDNAMELIST fieldNameList
#define FIELDCOUNT fieldCount
#define DSTPET dstPet
#define SRCPET srcPet
#define FARRAYPTR farrayPtr
#define minIndexPDimPDe minIndexPDe
#define maxIndexPDimPDe maxIndexPDe

#define LISTWRAPPER(x) (/x/)

#define ESMF_StateAdd ESMF_StateAddReplace
#define ESMF_STATEADD ESMF_StateAddReplace

#define ESMF_SINGLEPHASE 1
#define esmf_singlephase 1

#else

#if 0
// ESMF < 5.2.0r
#endif

#undef ESMF_520r
#define LISTWRAPPER(x) x

#define ESMF_GRIDCREATE ESMF_GridCreateShapeTile

#define ESMF_METHOD_INITIALIZE ESMF_SETINIT
#define ESMF_METHOD_RUN ESMF_SETRUN
#define ESMF_METHOD_FINALIZE ESMF_SETFINAL

#define stateintent statetype
#define ESMF_STATEINTENT_UNSPECIFIED esmf_state_unspecified
#define ESMF_STATEINTENT_IMPORT ESMF_STATE_IMPORT
#define ESMF_STATEINTENT_EXPORT ESMF_STATE_EXPORT

#define datacopyflag copyflag
#define ESMF_DataCopy_Flag ESMF_CopyFlag
#define ESMF_DATACOPY_REFERENCE ESMF_DATA_REF
#define ESMF_DATACOPY_VALUE ESMF_DATA_VALUE

#define ESMF_LOGMSG_INFO ESMF_LOG_INFO
#define ESMF_LOGERR_PASSTHRU ESMF_LOG_ERRMSG

#define ESMF_LOGKIND_MULTI ESMF_LOG_MULTI 
#define ESMF_LOGKIND_NONE ESMF_LOG_NONE
#define logkindflag defaultlogtype

#define ESMF_CALKIND_GREGORIAN ESMF_CAL_GREGORIAN 
#define defaultCalKind defaultCalendar

#define ESMFBEFORE520rb10

#define esmf_logfounderror esmf_logmsgfounderror
#define ESMF_LogFoundError ESMF_LogMsgFoundError
#define ESMF_LogFoundAllocError ESMF_LogMsgFoundAllocError

#define endflag terminationflag
#define ESMF_END_ABORT ESMF_ABORT

#define STATENAME stateName
#define FIELDNAME name
#define FIELDNAMELIST nameList
#define FIELDCOUNT nameCount
#define DSTPET dst
#define SRCPET src
#define FARRAYPTR fptr
#define FIELDBUNDLE bundle

#define totalUWidth maxHaloUWidth
#define totalLWidth maxHaloLWidth


#if (ESMF_MAJOR_VERSION < 4)

#define farrayPtr farray
#define ITEMCOUNT count
#define itemCount count

#endif


#endif
