	  a,  T   k820309              15.0        ÈPæY                                                                                                           
       radlw_main.f MODULE_RADLW_MAIN              LWRAD RLWINIT                                                    
                                                           
  	     ILWRATE ILWRGAS ILWCLIQ ILWCICE ISUBCLW ICLDFLG IOVRLW IVFLIP KIND_PHYS                                                     
       CON_G CON_CP CON_AVGD CON_AMD CON_AMW CON_AMO3                      @                              
       RANDOM_SETSEED RANDOM_NUMBER RANDOM_STAT                      @                              
       TOTPLNK                                                     
       PREFLOG TREF CHI_MLS                                                      u #RANDOM_SETSEED_S    #RANDOM_SETSEED_T 	   #         @     @                                                #INSEED              
                                             #         @     @                           	                   #RANDOM_SETSEED_T%IAND 
   #INSEED    #STAT                  @                           
     IAND           
                                                                                             Ð	              #RANDOM_STAT                                                         u #RANDOM_NUMBER_I    #RANDOM_NUMBER_S    #RANDOM_NUMBER_T    #         @     @                                                #HARVEST    #INSEED                                                                  
               &                                                     
                                             #         @     @                                                #HARVEST                                                                  
               &                                           #         @     @                                              #RANDOM_NUMBER_T%REAL    #RANDOM_NUMBER_T%SIZE    #RANDOM_NUMBER_T%IAND    #RANDOM_NUMBER_T%ISHFT    #RANDOM_NUMBER_T%IOR    #RANDOM_NUMBER_T%IEOR    #HARVEST    #STAT                  @                                REAL               @                                SIZE               @                                IAND               @                                ISHFT               @                                IOR               @                                IEOR                                                              
               &                                                     
                                      Ð	              #RANDOM_STAT                      @                               'Ð	                   #MTI    #MT    #ISET    #GSET                 D                                                                        «                                         q                                D                                   p                         p           & p         p o          p p                                      D                                   Ä	                          D                                   È	         
                 @  @                                 '                    #UPFXC !   #UPFX0 "                                              !                
                                              "               
                 @  @                           #     '                     #UPFXC $   #UPFX0 %   #DNFXC &   #DNFX0 '                                              $                
                                              %               
                                              &               
                                              '               
                 @  @                           (     '                     #UPFXC )   #DNFXC *   #UPFX0 +   #DNFX0 ,                                              )                
                                              *               
                                              +               
                                              ,               
                                              -     P             
      p ¶         p µ         p            p µ         p                          #         @                                   .                   #LWRAD%EXP /   #LWRAD%MIN 0   #LWRAD%MAX 1   #LWRAD%PRESENT 2   #PLYR 3   #PLVL 6   #TLYR 8   #TLVL 9   #QLYR :   #OLYR ;   #GASVMR <   #CLOUDS =   #ICSEED >   #AEROSOLS ?   #SFEMIS @   #SFGTMP A   #NPTS 4   #NLAY 5   #NLP1 7   #LPRNT B   #HLWC C   #TOPFLX D   #SFCFLX E   #HLW0 F   #HLWB G   #FLXPRF H                 @                           /     EXP               @                           0     MIN               @                           1     MAX               @                           2     PRESENT          
                                 3                    
      p        5  p        r 4   p          5  p        r 4     5  p        r 5       5  p        r 4     5  p        r 5                              
                                 6                    
      p        5  p        r 4   p          5  p        r 4     5  p        r 7       5  p        r 4     5  p        r 7                              
                                 8                    
      p        5  p        r 4   p          5  p        r 4     5  p        r 5       5  p        r 4     5  p        r 5                              
                                 9                    
      p        5  p        r 4   p          5  p        r 4     5  p        r 7       5  p        r 4     5  p        r 7                              
                                 :                    
      p        5  p        r 4   p          5  p        r 4     5  p        r 5       5  p        r 4     5  p        r 5                              
                                 ;                    
      p        5  p        r 4   p          5  p        r 4     5  p        r 5       5  p        r 4     5  p        r 5                              
                                 <                    
        p        5  p        r 5   p        5  p        r 4   p          5  p        r 4     5  p        r 5     p 	           5  p        r 4     5  p        r 5     p 	                                  
                                 =                    
        p        5  p        r 5   p        5  p        r 4   p          5  p        r 4     5  p        r 5     p 	           5  p        r 4     5  p        r 5     p 	                                  
                                  >                     
   p          5  p        r 4       5  p        r 4                              
                                 ?                    
          p        p        p        5  p        r 5   p        5  p        r 4   p          5  p        r 4     5  p        r 5     p          p            5  p        r 4     5  p        r 5     p          p                                   
                                 @                    
    p          5  p        r 4       5  p        r 4                              
                                 A                    
    p          5  p        r 4       5  p        r 4                               
                                  4                     
  @                               5                     
  @                               7                     
                                  B                    D                                C                    
       p        5  p        r 4   p          5  p        r 4     5  p        r 5       5  p        r 4     5  p        r 5                              D                                 D                          p          5  p        r 4       5  p        r 4                     #TOPFLW_TYPE              D                                 E                           p          5  p        r 4       5  p        r 4                     #SFCFLW_TYPE #            F @                              F                    
       p        5  p        r 4   p          5  p        r 4     5  p        r 5       5  p        r 4     5  p        r 5                              F @                              G                    
         p        5  p        r 5   p        5  p        r 4   p          5  p        r 4     5  p        r 5     p            5  p        r 4     5  p        r 5     p                                   F @                               H                             p        5  p        r 4   p          5  p        r 4     5  p        r 7       5  p        r 4     5  p        r 7                     #PROFLW_TYPE (   #         @                                   I                   #RLWINIT%TINY J   #RLWINIT%LOG K   #RLWINIT%AINT L   #RLWINIT%ASIN M   #RLWINIT%EXP N   #RLWINIT%REAL O   #ME P                 @                           J     TINY               @                           K     LOG               @                           L     AINT               @                           M     ASIN               @                           N     EXP               @            @              O     REAL           
                                  P                  '      fn#fn '   Ç      b   uapp(MODULE_RADLW_MAIN (   å   @   J  MODULE_RADLW_PARAMETERS    %     J  PHYSPARAM    ­  o   J  PHYSCONS !     i   J  MERSENNE_TWISTER %     H   J  MODULE_RADLW_AVPLANK !   Í  U   J  MODULE_RADLW_REF 4   "  l       gen@RANDOM_SETSEED+MERSENNE_TWISTER 2     T      RANDOM_SETSEED_S+MERSENNE_TWISTER 9   â  @   a   RANDOM_SETSEED_S%INSEED+MERSENNE_TWISTER 2   "  y      RANDOM_SETSEED_T+MERSENNE_TWISTER <     =      RANDOM_SETSEED_T%IAND+MERSENNE_TWISTER=IAND 9   Ø  @   a   RANDOM_SETSEED_T%INSEED+MERSENNE_TWISTER 7     Y   a   RANDOM_SETSEED_T%STAT+MERSENNE_TWISTER 3   q         gen@RANDOM_NUMBER+MERSENNE_TWISTER 1   ð  a      RANDOM_NUMBER_I+MERSENNE_TWISTER 9   Q     a   RANDOM_NUMBER_I%HARVEST+MERSENNE_TWISTER 8   Ý  @   a   RANDOM_NUMBER_I%INSEED+MERSENNE_TWISTER 1     U      RANDOM_NUMBER_S+MERSENNE_TWISTER 9   r     a   RANDOM_NUMBER_S%HARVEST+MERSENNE_TWISTER 1   þ  û      RANDOM_NUMBER_T+MERSENNE_TWISTER ;   ù  =      RANDOM_NUMBER_T%REAL+MERSENNE_TWISTER=REAL ;   6	  =      RANDOM_NUMBER_T%SIZE+MERSENNE_TWISTER=SIZE ;   s	  =      RANDOM_NUMBER_T%IAND+MERSENNE_TWISTER=IAND =   °	  >      RANDOM_NUMBER_T%ISHFT+MERSENNE_TWISTER=ISHFT 9   î	  <      RANDOM_NUMBER_T%IOR+MERSENNE_TWISTER=IOR ;   *
  =      RANDOM_NUMBER_T%IEOR+MERSENNE_TWISTER=IEOR 9   g
     a   RANDOM_NUMBER_T%HARVEST+MERSENNE_TWISTER 6   ó
  Y   a   RANDOM_NUMBER_T%STAT+MERSENNE_TWISTER -   L  u       RANDOM_STAT+MERSENNE_TWISTER 5   Á  €   %   RANDOM_STAT%MTI+MERSENNE_TWISTER=MTI 3   e  ¬   %   RANDOM_STAT%MT+MERSENNE_TWISTER=MT 7     H   %   RANDOM_STAT%ISET+MERSENNE_TWISTER=ISET 7   Y  H   %   RANDOM_STAT%GSET+MERSENNE_TWISTER=GSET 4   ¡  f      TOPFLW_TYPE+MODULE_RADLW_PARAMETERS :     H   a   TOPFLW_TYPE%UPFXC+MODULE_RADLW_PARAMETERS :   O  H   a   TOPFLW_TYPE%UPFX0+MODULE_RADLW_PARAMETERS 4     |      SFCFLW_TYPE+MODULE_RADLW_PARAMETERS :     H   a   SFCFLW_TYPE%UPFXC+MODULE_RADLW_PARAMETERS :   [  H   a   SFCFLW_TYPE%UPFX0+MODULE_RADLW_PARAMETERS :   £  H   a   SFCFLW_TYPE%DNFXC+MODULE_RADLW_PARAMETERS :   ë  H   a   SFCFLW_TYPE%DNFX0+MODULE_RADLW_PARAMETERS 4   3  |      PROFLW_TYPE+MODULE_RADLW_PARAMETERS :   ¯  H   a   PROFLW_TYPE%UPFXC+MODULE_RADLW_PARAMETERS :   ÷  H   a   PROFLW_TYPE%DNFXC+MODULE_RADLW_PARAMETERS :   ?  H   a   PROFLW_TYPE%UPFX0+MODULE_RADLW_PARAMETERS :     H   a   PROFLW_TYPE%DNFX0+MODULE_RADLW_PARAMETERS -   Ï  Ž       TOTPLNK+MODULE_RADLW_AVPLANK      y      LWRAD    ü  <      LWRAD%EXP    8  <      LWRAD%MIN    t  <      LWRAD%MAX    °  @      LWRAD%PRESENT    ð  $  a   LWRAD%PLYR      $  a   LWRAD%PLVL    8  $  a   LWRAD%TLYR    \  $  a   LWRAD%TLVL      $  a   LWRAD%QLYR    €  $  a   LWRAD%OLYR    È  t  a   LWRAD%GASVMR    <  t  a   LWRAD%CLOUDS    °  Ž   a   LWRAD%ICSEED    d  Ž  a   LWRAD%AEROSOLS    !  Ž   a   LWRAD%SFEMIS    Ì!  Ž   a   LWRAD%SFGTMP    "  @   a   LWRAD%NPTS    À"  @   a   LWRAD%NLAY     #  @   a   LWRAD%NLP1    @#  @   a   LWRAD%LPRNT    #  $  a   LWRAD%HLWC    €$  Å   a   LWRAD%TOPFLX    i%  Å   a   LWRAD%SFCFLX    .&  $  a   LWRAD%HLW0    R'  t  a   LWRAD%HLWB    Æ(  5  a   LWRAD%FLXPRF    û)  º       RLWINIT    µ*  =      RLWINIT%TINY    ò*  <      RLWINIT%LOG    .+  =      RLWINIT%AINT    k+  =      RLWINIT%ASIN    š+  <      RLWINIT%EXP    ä+  =      RLWINIT%REAL    !,  @   a   RLWINIT%ME 