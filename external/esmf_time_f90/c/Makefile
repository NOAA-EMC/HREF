
.SUFFIXES: .F90 .o

AR = ar

OBJS = ESMF_Alarm.o ESMF_BaseTime.o ESMF_Clock.o ESMF_Time.o \
        Meat.o ESMF_Base.o ESMF_Calendar.o ESMF_Fraction.o    \
        ESMF_TimeInterval.o ESMF_Mod.o

libesmf_time.a : $(OBJS)
	$(AR) ru libesmf_time.a $(OBJS)

Test1 : Test1.F90 libesmf_time.a
	f90 -c -g Test1.F90
	f90 -o Test1 Test1.o libesmf_time.a

.F90.o :
	f90 -c -w -g -DF90_STANDALONE $*.F90

clean:
	/bin/rm -f *.o 

superclean: clean
	/bin/rm -f libesmf_time.a *.mod Test1

###

ESMF_Alarm.o : ESMF_BaseTime.o ESMF_Time.o ESMF_TimeInterval.o
ESMF_BaseTime.o : ESMF_Base.o
ESMF_Clock.o : ESMF_BaseTime.o
ESMF_Time.o : ESMF_BaseTime.o ESMF_TimeInterval.o
ESMF_Base.o :
ESMF_Calendar.o : ESMF_BaseTime.o
ESMF_Fraction.o : ESMF_BaseTime.o
ESMF_TimeInterval.o : ESMF_BaseTime.o ESMF_Calendar.o ESMF_Fraction.o
ESMF_Mod.o : ESMF_Alarm.o ESMF_BaseTime.o ESMF_Clock.o ESMF_Time.o \
        ESMF_Base.o ESMF_Calendar.o ESMF_Fraction.o    \
        ESMF_TimeInterval.o
Meat.o : ESMF_Alarm.o ESMF_BaseTime.o ESMF_Clock.o ESMF_Time.o \
        ESMF_Base.o ESMF_Calendar.o ESMF_Fraction.o    \
        ESMF_TimeInterval.o
