





SUBROUTINE not_a_real_sub_b
END SUBROUTINE not_a_real_sub_b







SUBROUTINE nl_get_inputout_end_h ( id_id , inputout_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: inputout_end_h
  INTEGER id_id
  CHARACTER*80 emess
  inputout_end_h = model_config_rec%inputout_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_end_h
SUBROUTINE nl_get_inputout_end_m ( id_id , inputout_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: inputout_end_m
  INTEGER id_id
  CHARACTER*80 emess
  inputout_end_m = model_config_rec%inputout_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_end_m
SUBROUTINE nl_get_inputout_end_s ( id_id , inputout_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: inputout_end_s
  INTEGER id_id
  CHARACTER*80 emess
  inputout_end_s = model_config_rec%inputout_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_end_s
SUBROUTINE nl_get_auxhist1_end_y ( id_id , auxhist1_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist1_end_y
  INTEGER id_id
  CHARACTER*80 emess
  auxhist1_end_y = model_config_rec%auxhist1_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_end_y
SUBROUTINE nl_get_auxhist1_end_mo ( id_id , auxhist1_end_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist1_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxhist1_end_mo = model_config_rec%auxhist1_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_end_mo
SUBROUTINE nl_get_auxhist1_end_d ( id_id , auxhist1_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist1_end_d
  INTEGER id_id
  CHARACTER*80 emess
  auxhist1_end_d = model_config_rec%auxhist1_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_end_d
SUBROUTINE nl_get_auxhist1_end_h ( id_id , auxhist1_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist1_end_h
  INTEGER id_id
  CHARACTER*80 emess
  auxhist1_end_h = model_config_rec%auxhist1_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_end_h
SUBROUTINE nl_get_auxhist1_end_m ( id_id , auxhist1_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist1_end_m
  INTEGER id_id
  CHARACTER*80 emess
  auxhist1_end_m = model_config_rec%auxhist1_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_end_m
SUBROUTINE nl_get_auxhist1_end_s ( id_id , auxhist1_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist1_end_s
  INTEGER id_id
  CHARACTER*80 emess
  auxhist1_end_s = model_config_rec%auxhist1_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_end_s
SUBROUTINE nl_get_auxhist2_end_y ( id_id , auxhist2_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist2_end_y
  INTEGER id_id
  CHARACTER*80 emess
  auxhist2_end_y = model_config_rec%auxhist2_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_end_y
SUBROUTINE nl_get_auxhist2_end_mo ( id_id , auxhist2_end_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist2_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxhist2_end_mo = model_config_rec%auxhist2_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_end_mo
SUBROUTINE nl_get_auxhist2_end_d ( id_id , auxhist2_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist2_end_d
  INTEGER id_id
  CHARACTER*80 emess
  auxhist2_end_d = model_config_rec%auxhist2_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_end_d
SUBROUTINE nl_get_auxhist2_end_h ( id_id , auxhist2_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist2_end_h
  INTEGER id_id
  CHARACTER*80 emess
  auxhist2_end_h = model_config_rec%auxhist2_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_end_h
SUBROUTINE nl_get_auxhist2_end_m ( id_id , auxhist2_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist2_end_m
  INTEGER id_id
  CHARACTER*80 emess
  auxhist2_end_m = model_config_rec%auxhist2_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_end_m
SUBROUTINE nl_get_auxhist2_end_s ( id_id , auxhist2_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist2_end_s
  INTEGER id_id
  CHARACTER*80 emess
  auxhist2_end_s = model_config_rec%auxhist2_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_end_s
SUBROUTINE nl_get_auxhist3_end_y ( id_id , auxhist3_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist3_end_y
  INTEGER id_id
  CHARACTER*80 emess
  auxhist3_end_y = model_config_rec%auxhist3_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_end_y
SUBROUTINE nl_get_auxhist3_end_mo ( id_id , auxhist3_end_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist3_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxhist3_end_mo = model_config_rec%auxhist3_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_end_mo
SUBROUTINE nl_get_auxhist3_end_d ( id_id , auxhist3_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist3_end_d
  INTEGER id_id
  CHARACTER*80 emess
  auxhist3_end_d = model_config_rec%auxhist3_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_end_d
SUBROUTINE nl_get_auxhist3_end_h ( id_id , auxhist3_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist3_end_h
  INTEGER id_id
  CHARACTER*80 emess
  auxhist3_end_h = model_config_rec%auxhist3_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_end_h
SUBROUTINE nl_get_auxhist3_end_m ( id_id , auxhist3_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist3_end_m
  INTEGER id_id
  CHARACTER*80 emess
  auxhist3_end_m = model_config_rec%auxhist3_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_end_m
SUBROUTINE nl_get_auxhist3_end_s ( id_id , auxhist3_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist3_end_s
  INTEGER id_id
  CHARACTER*80 emess
  auxhist3_end_s = model_config_rec%auxhist3_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_end_s
SUBROUTINE nl_get_auxhist4_end_y ( id_id , auxhist4_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist4_end_y
  INTEGER id_id
  CHARACTER*80 emess
  auxhist4_end_y = model_config_rec%auxhist4_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_end_y
SUBROUTINE nl_get_auxhist4_end_mo ( id_id , auxhist4_end_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist4_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxhist4_end_mo = model_config_rec%auxhist4_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_end_mo
SUBROUTINE nl_get_auxhist4_end_d ( id_id , auxhist4_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist4_end_d
  INTEGER id_id
  CHARACTER*80 emess
  auxhist4_end_d = model_config_rec%auxhist4_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_end_d
SUBROUTINE nl_get_auxhist4_end_h ( id_id , auxhist4_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist4_end_h
  INTEGER id_id
  CHARACTER*80 emess
  auxhist4_end_h = model_config_rec%auxhist4_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_end_h
SUBROUTINE nl_get_auxhist4_end_m ( id_id , auxhist4_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist4_end_m
  INTEGER id_id
  CHARACTER*80 emess
  auxhist4_end_m = model_config_rec%auxhist4_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_end_m
SUBROUTINE nl_get_auxhist4_end_s ( id_id , auxhist4_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist4_end_s
  INTEGER id_id
  CHARACTER*80 emess
  auxhist4_end_s = model_config_rec%auxhist4_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_end_s
SUBROUTINE nl_get_auxhist5_end_y ( id_id , auxhist5_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist5_end_y
  INTEGER id_id
  CHARACTER*80 emess
  auxhist5_end_y = model_config_rec%auxhist5_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_end_y
SUBROUTINE nl_get_auxhist5_end_mo ( id_id , auxhist5_end_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist5_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxhist5_end_mo = model_config_rec%auxhist5_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_end_mo
SUBROUTINE nl_get_auxhist5_end_d ( id_id , auxhist5_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist5_end_d
  INTEGER id_id
  CHARACTER*80 emess
  auxhist5_end_d = model_config_rec%auxhist5_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_end_d
SUBROUTINE nl_get_auxhist5_end_h ( id_id , auxhist5_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist5_end_h
  INTEGER id_id
  CHARACTER*80 emess
  auxhist5_end_h = model_config_rec%auxhist5_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_end_h
SUBROUTINE nl_get_auxhist5_end_m ( id_id , auxhist5_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist5_end_m
  INTEGER id_id
  CHARACTER*80 emess
  auxhist5_end_m = model_config_rec%auxhist5_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_end_m
SUBROUTINE nl_get_auxhist5_end_s ( id_id , auxhist5_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist5_end_s
  INTEGER id_id
  CHARACTER*80 emess
  auxhist5_end_s = model_config_rec%auxhist5_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_end_s
SUBROUTINE nl_get_auxhist6_end_y ( id_id , auxhist6_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist6_end_y
  INTEGER id_id
  CHARACTER*80 emess
  auxhist6_end_y = model_config_rec%auxhist6_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_end_y
SUBROUTINE nl_get_auxhist6_end_mo ( id_id , auxhist6_end_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist6_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxhist6_end_mo = model_config_rec%auxhist6_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_end_mo
SUBROUTINE nl_get_auxhist6_end_d ( id_id , auxhist6_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist6_end_d
  INTEGER id_id
  CHARACTER*80 emess
  auxhist6_end_d = model_config_rec%auxhist6_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_end_d
SUBROUTINE nl_get_auxhist6_end_h ( id_id , auxhist6_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist6_end_h
  INTEGER id_id
  CHARACTER*80 emess
  auxhist6_end_h = model_config_rec%auxhist6_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_end_h
SUBROUTINE nl_get_auxhist6_end_m ( id_id , auxhist6_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist6_end_m
  INTEGER id_id
  CHARACTER*80 emess
  auxhist6_end_m = model_config_rec%auxhist6_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_end_m
SUBROUTINE nl_get_auxhist6_end_s ( id_id , auxhist6_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist6_end_s
  INTEGER id_id
  CHARACTER*80 emess
  auxhist6_end_s = model_config_rec%auxhist6_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_end_s
SUBROUTINE nl_get_auxhist7_end_y ( id_id , auxhist7_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist7_end_y
  INTEGER id_id
  CHARACTER*80 emess
  auxhist7_end_y = model_config_rec%auxhist7_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_end_y
SUBROUTINE nl_get_auxhist7_end_mo ( id_id , auxhist7_end_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist7_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxhist7_end_mo = model_config_rec%auxhist7_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_end_mo
SUBROUTINE nl_get_auxhist7_end_d ( id_id , auxhist7_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist7_end_d
  INTEGER id_id
  CHARACTER*80 emess
  auxhist7_end_d = model_config_rec%auxhist7_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_end_d
SUBROUTINE nl_get_auxhist7_end_h ( id_id , auxhist7_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist7_end_h
  INTEGER id_id
  CHARACTER*80 emess
  auxhist7_end_h = model_config_rec%auxhist7_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_end_h
SUBROUTINE nl_get_auxhist7_end_m ( id_id , auxhist7_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist7_end_m
  INTEGER id_id
  CHARACTER*80 emess
  auxhist7_end_m = model_config_rec%auxhist7_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_end_m
SUBROUTINE nl_get_auxhist7_end_s ( id_id , auxhist7_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist7_end_s
  INTEGER id_id
  CHARACTER*80 emess
  auxhist7_end_s = model_config_rec%auxhist7_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_end_s
SUBROUTINE nl_get_auxhist8_end_y ( id_id , auxhist8_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist8_end_y
  INTEGER id_id
  CHARACTER*80 emess
  auxhist8_end_y = model_config_rec%auxhist8_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_end_y
SUBROUTINE nl_get_auxhist8_end_mo ( id_id , auxhist8_end_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist8_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxhist8_end_mo = model_config_rec%auxhist8_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_end_mo
SUBROUTINE nl_get_auxhist8_end_d ( id_id , auxhist8_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist8_end_d
  INTEGER id_id
  CHARACTER*80 emess
  auxhist8_end_d = model_config_rec%auxhist8_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_end_d
SUBROUTINE nl_get_auxhist8_end_h ( id_id , auxhist8_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist8_end_h
  INTEGER id_id
  CHARACTER*80 emess
  auxhist8_end_h = model_config_rec%auxhist8_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_end_h
SUBROUTINE nl_get_auxhist8_end_m ( id_id , auxhist8_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist8_end_m
  INTEGER id_id
  CHARACTER*80 emess
  auxhist8_end_m = model_config_rec%auxhist8_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_end_m
SUBROUTINE nl_get_auxhist8_end_s ( id_id , auxhist8_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist8_end_s
  INTEGER id_id
  CHARACTER*80 emess
  auxhist8_end_s = model_config_rec%auxhist8_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_end_s
SUBROUTINE nl_get_auxhist9_end_y ( id_id , auxhist9_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist9_end_y
  INTEGER id_id
  CHARACTER*80 emess
  auxhist9_end_y = model_config_rec%auxhist9_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_end_y
SUBROUTINE nl_get_auxhist9_end_mo ( id_id , auxhist9_end_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist9_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxhist9_end_mo = model_config_rec%auxhist9_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_end_mo
SUBROUTINE nl_get_auxhist9_end_d ( id_id , auxhist9_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist9_end_d
  INTEGER id_id
  CHARACTER*80 emess
  auxhist9_end_d = model_config_rec%auxhist9_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_end_d
SUBROUTINE nl_get_auxhist9_end_h ( id_id , auxhist9_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist9_end_h
  INTEGER id_id
  CHARACTER*80 emess
  auxhist9_end_h = model_config_rec%auxhist9_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_end_h
SUBROUTINE nl_get_auxhist9_end_m ( id_id , auxhist9_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist9_end_m
  INTEGER id_id
  CHARACTER*80 emess
  auxhist9_end_m = model_config_rec%auxhist9_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_end_m
SUBROUTINE nl_get_auxhist9_end_s ( id_id , auxhist9_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist9_end_s
  INTEGER id_id
  CHARACTER*80 emess
  auxhist9_end_s = model_config_rec%auxhist9_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_end_s
SUBROUTINE nl_get_auxhist10_end_y ( id_id , auxhist10_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist10_end_y
  INTEGER id_id
  CHARACTER*80 emess
  auxhist10_end_y = model_config_rec%auxhist10_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_end_y
SUBROUTINE nl_get_auxhist10_end_mo ( id_id , auxhist10_end_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist10_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxhist10_end_mo = model_config_rec%auxhist10_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_end_mo
SUBROUTINE nl_get_auxhist10_end_d ( id_id , auxhist10_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist10_end_d
  INTEGER id_id
  CHARACTER*80 emess
  auxhist10_end_d = model_config_rec%auxhist10_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_end_d
SUBROUTINE nl_get_auxhist10_end_h ( id_id , auxhist10_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist10_end_h
  INTEGER id_id
  CHARACTER*80 emess
  auxhist10_end_h = model_config_rec%auxhist10_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_end_h
SUBROUTINE nl_get_auxhist10_end_m ( id_id , auxhist10_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist10_end_m
  INTEGER id_id
  CHARACTER*80 emess
  auxhist10_end_m = model_config_rec%auxhist10_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_end_m
SUBROUTINE nl_get_auxhist10_end_s ( id_id , auxhist10_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist10_end_s
  INTEGER id_id
  CHARACTER*80 emess
  auxhist10_end_s = model_config_rec%auxhist10_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_end_s
SUBROUTINE nl_get_auxhist11_end_y ( id_id , auxhist11_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist11_end_y
  INTEGER id_id
  CHARACTER*80 emess
  auxhist11_end_y = model_config_rec%auxhist11_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_end_y
SUBROUTINE nl_get_auxhist11_end_mo ( id_id , auxhist11_end_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist11_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxhist11_end_mo = model_config_rec%auxhist11_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_end_mo
SUBROUTINE nl_get_auxhist11_end_d ( id_id , auxhist11_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist11_end_d
  INTEGER id_id
  CHARACTER*80 emess
  auxhist11_end_d = model_config_rec%auxhist11_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_end_d
SUBROUTINE nl_get_auxhist11_end_h ( id_id , auxhist11_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist11_end_h
  INTEGER id_id
  CHARACTER*80 emess
  auxhist11_end_h = model_config_rec%auxhist11_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_end_h
SUBROUTINE nl_get_auxhist11_end_m ( id_id , auxhist11_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist11_end_m
  INTEGER id_id
  CHARACTER*80 emess
  auxhist11_end_m = model_config_rec%auxhist11_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_end_m
SUBROUTINE nl_get_auxhist11_end_s ( id_id , auxhist11_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist11_end_s
  INTEGER id_id
  CHARACTER*80 emess
  auxhist11_end_s = model_config_rec%auxhist11_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_end_s
SUBROUTINE nl_get_auxinput1_end_y ( id_id , auxinput1_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput1_end_y
  INTEGER id_id
  CHARACTER*80 emess
  auxinput1_end_y = model_config_rec%auxinput1_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_end_y
SUBROUTINE nl_get_auxinput1_end_mo ( id_id , auxinput1_end_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput1_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxinput1_end_mo = model_config_rec%auxinput1_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_end_mo
SUBROUTINE nl_get_auxinput1_end_d ( id_id , auxinput1_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput1_end_d
  INTEGER id_id
  CHARACTER*80 emess
  auxinput1_end_d = model_config_rec%auxinput1_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_end_d
SUBROUTINE nl_get_auxinput1_end_h ( id_id , auxinput1_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput1_end_h
  INTEGER id_id
  CHARACTER*80 emess
  auxinput1_end_h = model_config_rec%auxinput1_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_end_h
SUBROUTINE nl_get_auxinput1_end_m ( id_id , auxinput1_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput1_end_m
  INTEGER id_id
  CHARACTER*80 emess
  auxinput1_end_m = model_config_rec%auxinput1_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_end_m
SUBROUTINE nl_get_auxinput1_end_s ( id_id , auxinput1_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput1_end_s
  INTEGER id_id
  CHARACTER*80 emess
  auxinput1_end_s = model_config_rec%auxinput1_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_end_s
SUBROUTINE nl_get_auxinput2_end_y ( id_id , auxinput2_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput2_end_y
  INTEGER id_id
  CHARACTER*80 emess
  auxinput2_end_y = model_config_rec%auxinput2_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_end_y
SUBROUTINE nl_get_auxinput2_end_mo ( id_id , auxinput2_end_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput2_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxinput2_end_mo = model_config_rec%auxinput2_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_end_mo
SUBROUTINE nl_get_auxinput2_end_d ( id_id , auxinput2_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput2_end_d
  INTEGER id_id
  CHARACTER*80 emess
  auxinput2_end_d = model_config_rec%auxinput2_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_end_d
SUBROUTINE nl_get_auxinput2_end_h ( id_id , auxinput2_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput2_end_h
  INTEGER id_id
  CHARACTER*80 emess
  auxinput2_end_h = model_config_rec%auxinput2_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_end_h
SUBROUTINE nl_get_auxinput2_end_m ( id_id , auxinput2_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput2_end_m
  INTEGER id_id
  CHARACTER*80 emess
  auxinput2_end_m = model_config_rec%auxinput2_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_end_m
SUBROUTINE nl_get_auxinput2_end_s ( id_id , auxinput2_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput2_end_s
  INTEGER id_id
  CHARACTER*80 emess
  auxinput2_end_s = model_config_rec%auxinput2_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_end_s
SUBROUTINE nl_get_auxinput3_end_y ( id_id , auxinput3_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput3_end_y
  INTEGER id_id
  CHARACTER*80 emess
  auxinput3_end_y = model_config_rec%auxinput3_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_end_y
SUBROUTINE nl_get_auxinput3_end_mo ( id_id , auxinput3_end_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput3_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxinput3_end_mo = model_config_rec%auxinput3_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_end_mo
SUBROUTINE nl_get_auxinput3_end_d ( id_id , auxinput3_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput3_end_d
  INTEGER id_id
  CHARACTER*80 emess
  auxinput3_end_d = model_config_rec%auxinput3_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_end_d
SUBROUTINE nl_get_auxinput3_end_h ( id_id , auxinput3_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput3_end_h
  INTEGER id_id
  CHARACTER*80 emess
  auxinput3_end_h = model_config_rec%auxinput3_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_end_h
SUBROUTINE nl_get_auxinput3_end_m ( id_id , auxinput3_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput3_end_m
  INTEGER id_id
  CHARACTER*80 emess
  auxinput3_end_m = model_config_rec%auxinput3_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_end_m
SUBROUTINE nl_get_auxinput3_end_s ( id_id , auxinput3_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput3_end_s
  INTEGER id_id
  CHARACTER*80 emess
  auxinput3_end_s = model_config_rec%auxinput3_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_end_s
SUBROUTINE nl_get_auxinput4_end_y ( id_id , auxinput4_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput4_end_y
  INTEGER id_id
  CHARACTER*80 emess
  auxinput4_end_y = model_config_rec%auxinput4_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_end_y
SUBROUTINE nl_get_auxinput4_end_mo ( id_id , auxinput4_end_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput4_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxinput4_end_mo = model_config_rec%auxinput4_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_end_mo
SUBROUTINE nl_get_auxinput4_end_d ( id_id , auxinput4_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput4_end_d
  INTEGER id_id
  CHARACTER*80 emess
  auxinput4_end_d = model_config_rec%auxinput4_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_end_d
SUBROUTINE nl_get_auxinput4_end_h ( id_id , auxinput4_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput4_end_h
  INTEGER id_id
  CHARACTER*80 emess
  auxinput4_end_h = model_config_rec%auxinput4_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_end_h
SUBROUTINE nl_get_auxinput4_end_m ( id_id , auxinput4_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput4_end_m
  INTEGER id_id
  CHARACTER*80 emess
  auxinput4_end_m = model_config_rec%auxinput4_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_end_m
SUBROUTINE nl_get_auxinput4_end_s ( id_id , auxinput4_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput4_end_s
  INTEGER id_id
  CHARACTER*80 emess
  auxinput4_end_s = model_config_rec%auxinput4_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_end_s
SUBROUTINE nl_get_auxinput5_end_y ( id_id , auxinput5_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput5_end_y
  INTEGER id_id
  CHARACTER*80 emess
  auxinput5_end_y = model_config_rec%auxinput5_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_end_y
SUBROUTINE nl_get_auxinput5_end_mo ( id_id , auxinput5_end_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput5_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxinput5_end_mo = model_config_rec%auxinput5_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_end_mo
SUBROUTINE nl_get_auxinput5_end_d ( id_id , auxinput5_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput5_end_d
  INTEGER id_id
  CHARACTER*80 emess
  auxinput5_end_d = model_config_rec%auxinput5_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_end_d
SUBROUTINE nl_get_auxinput5_end_h ( id_id , auxinput5_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput5_end_h
  INTEGER id_id
  CHARACTER*80 emess
  auxinput5_end_h = model_config_rec%auxinput5_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_end_h
SUBROUTINE nl_get_auxinput5_end_m ( id_id , auxinput5_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput5_end_m
  INTEGER id_id
  CHARACTER*80 emess
  auxinput5_end_m = model_config_rec%auxinput5_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_end_m
SUBROUTINE nl_get_auxinput5_end_s ( id_id , auxinput5_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput5_end_s
  INTEGER id_id
  CHARACTER*80 emess
  auxinput5_end_s = model_config_rec%auxinput5_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_end_s
SUBROUTINE nl_get_auxinput6_end_y ( id_id , auxinput6_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput6_end_y
  INTEGER id_id
  CHARACTER*80 emess
  auxinput6_end_y = model_config_rec%auxinput6_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_end_y
SUBROUTINE nl_get_auxinput6_end_mo ( id_id , auxinput6_end_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput6_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxinput6_end_mo = model_config_rec%auxinput6_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_end_mo
SUBROUTINE nl_get_auxinput6_end_d ( id_id , auxinput6_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput6_end_d
  INTEGER id_id
  CHARACTER*80 emess
  auxinput6_end_d = model_config_rec%auxinput6_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_end_d
SUBROUTINE nl_get_auxinput6_end_h ( id_id , auxinput6_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput6_end_h
  INTEGER id_id
  CHARACTER*80 emess
  auxinput6_end_h = model_config_rec%auxinput6_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_end_h
SUBROUTINE nl_get_auxinput6_end_m ( id_id , auxinput6_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput6_end_m
  INTEGER id_id
  CHARACTER*80 emess
  auxinput6_end_m = model_config_rec%auxinput6_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_end_m
SUBROUTINE nl_get_auxinput6_end_s ( id_id , auxinput6_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput6_end_s
  INTEGER id_id
  CHARACTER*80 emess
  auxinput6_end_s = model_config_rec%auxinput6_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_end_s
SUBROUTINE nl_get_auxinput7_end_y ( id_id , auxinput7_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput7_end_y
  INTEGER id_id
  CHARACTER*80 emess
  auxinput7_end_y = model_config_rec%auxinput7_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_end_y
SUBROUTINE nl_get_auxinput7_end_mo ( id_id , auxinput7_end_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput7_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxinput7_end_mo = model_config_rec%auxinput7_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_end_mo
SUBROUTINE nl_get_auxinput7_end_d ( id_id , auxinput7_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput7_end_d
  INTEGER id_id
  CHARACTER*80 emess
  auxinput7_end_d = model_config_rec%auxinput7_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_end_d
SUBROUTINE nl_get_auxinput7_end_h ( id_id , auxinput7_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput7_end_h
  INTEGER id_id
  CHARACTER*80 emess
  auxinput7_end_h = model_config_rec%auxinput7_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_end_h
SUBROUTINE nl_get_auxinput7_end_m ( id_id , auxinput7_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput7_end_m
  INTEGER id_id
  CHARACTER*80 emess
  auxinput7_end_m = model_config_rec%auxinput7_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_end_m
SUBROUTINE nl_get_auxinput7_end_s ( id_id , auxinput7_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput7_end_s
  INTEGER id_id
  CHARACTER*80 emess
  auxinput7_end_s = model_config_rec%auxinput7_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_end_s
SUBROUTINE nl_get_auxinput8_end_y ( id_id , auxinput8_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput8_end_y
  INTEGER id_id
  CHARACTER*80 emess
  auxinput8_end_y = model_config_rec%auxinput8_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_end_y
SUBROUTINE nl_get_auxinput8_end_mo ( id_id , auxinput8_end_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput8_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxinput8_end_mo = model_config_rec%auxinput8_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_end_mo
SUBROUTINE nl_get_auxinput8_end_d ( id_id , auxinput8_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput8_end_d
  INTEGER id_id
  CHARACTER*80 emess
  auxinput8_end_d = model_config_rec%auxinput8_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_end_d
SUBROUTINE nl_get_auxinput8_end_h ( id_id , auxinput8_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput8_end_h
  INTEGER id_id
  CHARACTER*80 emess
  auxinput8_end_h = model_config_rec%auxinput8_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_end_h
SUBROUTINE nl_get_auxinput8_end_m ( id_id , auxinput8_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput8_end_m
  INTEGER id_id
  CHARACTER*80 emess
  auxinput8_end_m = model_config_rec%auxinput8_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_end_m
SUBROUTINE nl_get_auxinput8_end_s ( id_id , auxinput8_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput8_end_s
  INTEGER id_id
  CHARACTER*80 emess
  auxinput8_end_s = model_config_rec%auxinput8_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_end_s
SUBROUTINE nl_get_sgfdda_end_y ( id_id , sgfdda_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: sgfdda_end_y
  INTEGER id_id
  CHARACTER*80 emess
  sgfdda_end_y = model_config_rec%sgfdda_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_sgfdda_end_y
SUBROUTINE nl_get_sgfdda_end_mo ( id_id , sgfdda_end_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: sgfdda_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  sgfdda_end_mo = model_config_rec%sgfdda_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_sgfdda_end_mo
SUBROUTINE nl_get_sgfdda_end_d ( id_id , sgfdda_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: sgfdda_end_d
  INTEGER id_id
  CHARACTER*80 emess
  sgfdda_end_d = model_config_rec%sgfdda_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_sgfdda_end_d
SUBROUTINE nl_get_sgfdda_end_h ( id_id , sgfdda_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: sgfdda_end_h
  INTEGER id_id
  CHARACTER*80 emess
  sgfdda_end_h = model_config_rec%sgfdda_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_sgfdda_end_h
SUBROUTINE nl_get_sgfdda_end_m ( id_id , sgfdda_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: sgfdda_end_m
  INTEGER id_id
  CHARACTER*80 emess
  sgfdda_end_m = model_config_rec%sgfdda_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_sgfdda_end_m
SUBROUTINE nl_get_sgfdda_end_s ( id_id , sgfdda_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: sgfdda_end_s
  INTEGER id_id
  CHARACTER*80 emess
  sgfdda_end_s = model_config_rec%sgfdda_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_sgfdda_end_s
SUBROUTINE nl_get_gfdda_end_y ( id_id , gfdda_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: gfdda_end_y
  INTEGER id_id
  CHARACTER*80 emess
  gfdda_end_y = model_config_rec%gfdda_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_gfdda_end_y
SUBROUTINE nl_get_gfdda_end_mo ( id_id , gfdda_end_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: gfdda_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  gfdda_end_mo = model_config_rec%gfdda_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_gfdda_end_mo
SUBROUTINE nl_get_gfdda_end_d ( id_id , gfdda_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: gfdda_end_d
  INTEGER id_id
  CHARACTER*80 emess
  gfdda_end_d = model_config_rec%gfdda_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_gfdda_end_d
SUBROUTINE nl_get_gfdda_end_h ( id_id , gfdda_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: gfdda_end_h
  INTEGER id_id
  CHARACTER*80 emess
  gfdda_end_h = model_config_rec%gfdda_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_gfdda_end_h
SUBROUTINE nl_get_gfdda_end_m ( id_id , gfdda_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: gfdda_end_m
  INTEGER id_id
  CHARACTER*80 emess
  gfdda_end_m = model_config_rec%gfdda_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_gfdda_end_m
SUBROUTINE nl_get_gfdda_end_s ( id_id , gfdda_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: gfdda_end_s
  INTEGER id_id
  CHARACTER*80 emess
  gfdda_end_s = model_config_rec%gfdda_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_gfdda_end_s
SUBROUTINE nl_get_auxinput11_end_y ( id_id , auxinput11_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput11_end_y
  INTEGER id_id
  CHARACTER*80 emess
  auxinput11_end_y = model_config_rec%auxinput11_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_end_y
SUBROUTINE nl_get_auxinput11_end_mo ( id_id , auxinput11_end_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput11_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxinput11_end_mo = model_config_rec%auxinput11_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_end_mo
SUBROUTINE nl_get_auxinput11_end_d ( id_id , auxinput11_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput11_end_d
  INTEGER id_id
  CHARACTER*80 emess
  auxinput11_end_d = model_config_rec%auxinput11_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_end_d
SUBROUTINE nl_get_auxinput11_end_h ( id_id , auxinput11_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput11_end_h
  INTEGER id_id
  CHARACTER*80 emess
  auxinput11_end_h = model_config_rec%auxinput11_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_end_h
SUBROUTINE nl_get_auxinput11_end_m ( id_id , auxinput11_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput11_end_m
  INTEGER id_id
  CHARACTER*80 emess
  auxinput11_end_m = model_config_rec%auxinput11_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_end_m
SUBROUTINE nl_get_auxinput11_end_s ( id_id , auxinput11_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput11_end_s
  INTEGER id_id
  CHARACTER*80 emess
  auxinput11_end_s = model_config_rec%auxinput11_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_end_s
SUBROUTINE nl_get_io_form_auxinput1 ( id_id , io_form_auxinput1 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxinput1
  INTEGER id_id
  CHARACTER*80 emess
  io_form_auxinput1 = model_config_rec%io_form_auxinput1
  RETURN
END SUBROUTINE nl_get_io_form_auxinput1
SUBROUTINE nl_get_io_form_auxinput2 ( id_id , io_form_auxinput2 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxinput2
  INTEGER id_id
  CHARACTER*80 emess
  io_form_auxinput2 = model_config_rec%io_form_auxinput2
  RETURN
END SUBROUTINE nl_get_io_form_auxinput2
SUBROUTINE nl_get_io_form_auxinput3 ( id_id , io_form_auxinput3 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxinput3
  INTEGER id_id
  CHARACTER*80 emess
  io_form_auxinput3 = model_config_rec%io_form_auxinput3
  RETURN
END SUBROUTINE nl_get_io_form_auxinput3
SUBROUTINE nl_get_io_form_auxinput4 ( id_id , io_form_auxinput4 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxinput4
  INTEGER id_id
  CHARACTER*80 emess
  io_form_auxinput4 = model_config_rec%io_form_auxinput4
  RETURN
END SUBROUTINE nl_get_io_form_auxinput4
SUBROUTINE nl_get_io_form_auxinput5 ( id_id , io_form_auxinput5 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxinput5
  INTEGER id_id
  CHARACTER*80 emess
  io_form_auxinput5 = model_config_rec%io_form_auxinput5
  RETURN
END SUBROUTINE nl_get_io_form_auxinput5
SUBROUTINE nl_get_io_form_auxinput6 ( id_id , io_form_auxinput6 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxinput6
  INTEGER id_id
  CHARACTER*80 emess
  io_form_auxinput6 = model_config_rec%io_form_auxinput6
  RETURN
END SUBROUTINE nl_get_io_form_auxinput6
SUBROUTINE nl_get_io_form_auxinput7 ( id_id , io_form_auxinput7 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxinput7
  INTEGER id_id
  CHARACTER*80 emess
  io_form_auxinput7 = model_config_rec%io_form_auxinput7
  RETURN
END SUBROUTINE nl_get_io_form_auxinput7
SUBROUTINE nl_get_io_form_auxinput8 ( id_id , io_form_auxinput8 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxinput8
  INTEGER id_id
  CHARACTER*80 emess
  io_form_auxinput8 = model_config_rec%io_form_auxinput8
  RETURN
END SUBROUTINE nl_get_io_form_auxinput8
SUBROUTINE nl_get_io_form_sgfdda ( id_id , io_form_sgfdda )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_sgfdda
  INTEGER id_id
  CHARACTER*80 emess
  io_form_sgfdda = model_config_rec%io_form_sgfdda
  RETURN
END SUBROUTINE nl_get_io_form_sgfdda
SUBROUTINE nl_get_io_form_gfdda ( id_id , io_form_gfdda )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_gfdda
  INTEGER id_id
  CHARACTER*80 emess
  io_form_gfdda = model_config_rec%io_form_gfdda
  RETURN
END SUBROUTINE nl_get_io_form_gfdda
SUBROUTINE nl_get_io_form_auxinput11 ( id_id , io_form_auxinput11 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxinput11
  INTEGER id_id
  CHARACTER*80 emess
  io_form_auxinput11 = model_config_rec%io_form_auxinput11
  RETURN
END SUBROUTINE nl_get_io_form_auxinput11
SUBROUTINE nl_get_io_form_auxhist1 ( id_id , io_form_auxhist1 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxhist1
  INTEGER id_id
  CHARACTER*80 emess
  io_form_auxhist1 = model_config_rec%io_form_auxhist1
  RETURN
END SUBROUTINE nl_get_io_form_auxhist1
SUBROUTINE nl_get_io_form_auxhist2 ( id_id , io_form_auxhist2 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxhist2
  INTEGER id_id
  CHARACTER*80 emess
  io_form_auxhist2 = model_config_rec%io_form_auxhist2
  RETURN
END SUBROUTINE nl_get_io_form_auxhist2
SUBROUTINE nl_get_io_form_auxhist3 ( id_id , io_form_auxhist3 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxhist3
  INTEGER id_id
  CHARACTER*80 emess
  io_form_auxhist3 = model_config_rec%io_form_auxhist3
  RETURN
END SUBROUTINE nl_get_io_form_auxhist3
SUBROUTINE nl_get_io_form_auxhist4 ( id_id , io_form_auxhist4 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxhist4
  INTEGER id_id
  CHARACTER*80 emess
  io_form_auxhist4 = model_config_rec%io_form_auxhist4
  RETURN
END SUBROUTINE nl_get_io_form_auxhist4
SUBROUTINE nl_get_io_form_auxhist5 ( id_id , io_form_auxhist5 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxhist5
  INTEGER id_id
  CHARACTER*80 emess
  io_form_auxhist5 = model_config_rec%io_form_auxhist5
  RETURN
END SUBROUTINE nl_get_io_form_auxhist5
SUBROUTINE nl_get_io_form_auxhist6 ( id_id , io_form_auxhist6 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxhist6
  INTEGER id_id
  CHARACTER*80 emess
  io_form_auxhist6 = model_config_rec%io_form_auxhist6
  RETURN
END SUBROUTINE nl_get_io_form_auxhist6
SUBROUTINE nl_get_io_form_auxhist7 ( id_id , io_form_auxhist7 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxhist7
  INTEGER id_id
  CHARACTER*80 emess
  io_form_auxhist7 = model_config_rec%io_form_auxhist7
  RETURN
END SUBROUTINE nl_get_io_form_auxhist7
SUBROUTINE nl_get_io_form_auxhist8 ( id_id , io_form_auxhist8 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxhist8
  INTEGER id_id
  CHARACTER*80 emess
  io_form_auxhist8 = model_config_rec%io_form_auxhist8
  RETURN
END SUBROUTINE nl_get_io_form_auxhist8
SUBROUTINE nl_get_io_form_auxhist9 ( id_id , io_form_auxhist9 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxhist9
  INTEGER id_id
  CHARACTER*80 emess
  io_form_auxhist9 = model_config_rec%io_form_auxhist9
  RETURN
END SUBROUTINE nl_get_io_form_auxhist9
SUBROUTINE nl_get_io_form_auxhist10 ( id_id , io_form_auxhist10 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxhist10
  INTEGER id_id
  CHARACTER*80 emess
  io_form_auxhist10 = model_config_rec%io_form_auxhist10
  RETURN
END SUBROUTINE nl_get_io_form_auxhist10
SUBROUTINE nl_get_io_form_auxhist11 ( id_id , io_form_auxhist11 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxhist11
  INTEGER id_id
  CHARACTER*80 emess
  io_form_auxhist11 = model_config_rec%io_form_auxhist11
  RETURN
END SUBROUTINE nl_get_io_form_auxhist11
SUBROUTINE nl_get_simulation_start_year ( id_id , simulation_start_year )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: simulation_start_year
  INTEGER id_id
  CHARACTER*80 emess
  simulation_start_year = model_config_rec%simulation_start_year
  RETURN
END SUBROUTINE nl_get_simulation_start_year
SUBROUTINE nl_get_simulation_start_month ( id_id , simulation_start_month )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: simulation_start_month
  INTEGER id_id
  CHARACTER*80 emess
  simulation_start_month = model_config_rec%simulation_start_month
  RETURN
END SUBROUTINE nl_get_simulation_start_month
SUBROUTINE nl_get_simulation_start_day ( id_id , simulation_start_day )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: simulation_start_day
  INTEGER id_id
  CHARACTER*80 emess
  simulation_start_day = model_config_rec%simulation_start_day
  RETURN
END SUBROUTINE nl_get_simulation_start_day
SUBROUTINE nl_get_simulation_start_hour ( id_id , simulation_start_hour )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: simulation_start_hour
  INTEGER id_id
  CHARACTER*80 emess
  simulation_start_hour = model_config_rec%simulation_start_hour
  RETURN
END SUBROUTINE nl_get_simulation_start_hour
SUBROUTINE nl_get_simulation_start_minute ( id_id , simulation_start_minute )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: simulation_start_minute
  INTEGER id_id
  CHARACTER*80 emess
  simulation_start_minute = model_config_rec%simulation_start_minute
  RETURN
END SUBROUTINE nl_get_simulation_start_minute
SUBROUTINE nl_get_simulation_start_second ( id_id , simulation_start_second )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: simulation_start_second
  INTEGER id_id
  CHARACTER*80 emess
  simulation_start_second = model_config_rec%simulation_start_second
  RETURN
END SUBROUTINE nl_get_simulation_start_second
SUBROUTINE nl_get_reset_simulation_start ( id_id , reset_simulation_start )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: reset_simulation_start
  INTEGER id_id
  CHARACTER*80 emess
  reset_simulation_start = model_config_rec%reset_simulation_start
  RETURN
END SUBROUTINE nl_get_reset_simulation_start
SUBROUTINE nl_get_sr_x ( id_id , sr_x )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: sr_x
  INTEGER id_id
  CHARACTER*80 emess
  sr_x = model_config_rec%sr_x(id_id)
  RETURN
END SUBROUTINE nl_get_sr_x
SUBROUTINE nl_get_sr_y ( id_id , sr_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: sr_y
  INTEGER id_id
  CHARACTER*80 emess
  sr_y = model_config_rec%sr_y(id_id)
  RETURN
END SUBROUTINE nl_get_sr_y
SUBROUTINE nl_get_julyr ( id_id , julyr )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: julyr
  INTEGER id_id
  CHARACTER*80 emess
  julyr = model_config_rec%julyr(id_id)
  RETURN
END SUBROUTINE nl_get_julyr
SUBROUTINE nl_get_julday ( id_id , julday )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: julday
  INTEGER id_id
  CHARACTER*80 emess
  julday = model_config_rec%julday(id_id)
  RETURN
END SUBROUTINE nl_get_julday
SUBROUTINE nl_get_gmt ( id_id , gmt )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: gmt
  INTEGER id_id
  CHARACTER*80 emess
  gmt = model_config_rec%gmt(id_id)
  RETURN
END SUBROUTINE nl_get_gmt
SUBROUTINE nl_get_input_inname ( id_id , input_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: input_inname
  INTEGER id_id
  CHARACTER*80 emess
  input_inname = trim(model_config_rec%input_inname)
  RETURN
END SUBROUTINE nl_get_input_inname
SUBROUTINE nl_get_input_outname ( id_id , input_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: input_outname
  INTEGER id_id
  CHARACTER*80 emess
  input_outname = trim(model_config_rec%input_outname)
  RETURN
END SUBROUTINE nl_get_input_outname
SUBROUTINE nl_get_bdy_inname ( id_id , bdy_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: bdy_inname
  INTEGER id_id
  CHARACTER*80 emess
  bdy_inname = trim(model_config_rec%bdy_inname)
  RETURN
END SUBROUTINE nl_get_bdy_inname
SUBROUTINE nl_get_bdy_outname ( id_id , bdy_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: bdy_outname
  INTEGER id_id
  CHARACTER*80 emess
  bdy_outname = trim(model_config_rec%bdy_outname)
  RETURN
END SUBROUTINE nl_get_bdy_outname
SUBROUTINE nl_get_rst_inname ( id_id , rst_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: rst_inname
  INTEGER id_id
  CHARACTER*80 emess
  rst_inname = trim(model_config_rec%rst_inname)
  RETURN
END SUBROUTINE nl_get_rst_inname
SUBROUTINE nl_get_rst_outname ( id_id , rst_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: rst_outname
  INTEGER id_id
  CHARACTER*80 emess
  rst_outname = trim(model_config_rec%rst_outname)
  RETURN
END SUBROUTINE nl_get_rst_outname
SUBROUTINE nl_get_write_input ( id_id , write_input )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: write_input
  INTEGER id_id
  CHARACTER*80 emess
  write_input = model_config_rec%write_input
  RETURN
END SUBROUTINE nl_get_write_input
SUBROUTINE nl_get_write_restart_at_0h ( id_id , write_restart_at_0h )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: write_restart_at_0h
  INTEGER id_id
  CHARACTER*80 emess
  write_restart_at_0h = model_config_rec%write_restart_at_0h
  RETURN
END SUBROUTINE nl_get_write_restart_at_0h
SUBROUTINE nl_get_adjust_output_times ( id_id , adjust_output_times )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: adjust_output_times
  INTEGER id_id
  CHARACTER*80 emess
  adjust_output_times = model_config_rec%adjust_output_times
  RETURN
END SUBROUTINE nl_get_adjust_output_times
SUBROUTINE nl_get_adjust_input_times ( id_id , adjust_input_times )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: adjust_input_times
  INTEGER id_id
  CHARACTER*80 emess
  adjust_input_times = model_config_rec%adjust_input_times
  RETURN
END SUBROUTINE nl_get_adjust_input_times
SUBROUTINE nl_get_tstart ( id_id , tstart )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: tstart
  INTEGER id_id
  CHARACTER*80 emess
  tstart = model_config_rec%tstart(id_id)
  RETURN
END SUBROUTINE nl_get_tstart
SUBROUTINE nl_get_nocolons ( id_id , nocolons )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: nocolons
  INTEGER id_id
  CHARACTER*80 emess
  nocolons = model_config_rec%nocolons
  RETURN
END SUBROUTINE nl_get_nocolons
SUBROUTINE nl_get_cycling ( id_id , cycling )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: cycling
  INTEGER id_id
  CHARACTER*80 emess
  cycling = model_config_rec%cycling
  RETURN
END SUBROUTINE nl_get_cycling
SUBROUTINE nl_get_dfi_opt ( id_id , dfi_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_opt
  INTEGER id_id
  CHARACTER*80 emess
  dfi_opt = model_config_rec%dfi_opt
  RETURN
END SUBROUTINE nl_get_dfi_opt
SUBROUTINE nl_get_dfi_nfilter ( id_id , dfi_nfilter )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_nfilter
  INTEGER id_id
  CHARACTER*80 emess
  dfi_nfilter = model_config_rec%dfi_nfilter
  RETURN
END SUBROUTINE nl_get_dfi_nfilter
SUBROUTINE nl_get_dfi_write_filtered_input ( id_id , dfi_write_filtered_input )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: dfi_write_filtered_input
  INTEGER id_id
  CHARACTER*80 emess
  dfi_write_filtered_input = model_config_rec%dfi_write_filtered_input
  RETURN
END SUBROUTINE nl_get_dfi_write_filtered_input
SUBROUTINE nl_get_dfi_write_dfi_history ( id_id , dfi_write_dfi_history )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: dfi_write_dfi_history
  INTEGER id_id
  CHARACTER*80 emess
  dfi_write_dfi_history = model_config_rec%dfi_write_dfi_history
  RETURN
END SUBROUTINE nl_get_dfi_write_dfi_history
SUBROUTINE nl_get_dfi_cutoff_seconds ( id_id , dfi_cutoff_seconds )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_cutoff_seconds
  INTEGER id_id
  CHARACTER*80 emess
  dfi_cutoff_seconds = model_config_rec%dfi_cutoff_seconds
  RETURN
END SUBROUTINE nl_get_dfi_cutoff_seconds
SUBROUTINE nl_get_dfi_time_dim ( id_id , dfi_time_dim )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_time_dim
  INTEGER id_id
  CHARACTER*80 emess
  dfi_time_dim = model_config_rec%dfi_time_dim
  RETURN
END SUBROUTINE nl_get_dfi_time_dim
SUBROUTINE nl_get_dfi_fwdstop_year ( id_id , dfi_fwdstop_year )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_fwdstop_year
  INTEGER id_id
  CHARACTER*80 emess
  dfi_fwdstop_year = model_config_rec%dfi_fwdstop_year
  RETURN
END SUBROUTINE nl_get_dfi_fwdstop_year
SUBROUTINE nl_get_dfi_fwdstop_month ( id_id , dfi_fwdstop_month )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_fwdstop_month
  INTEGER id_id
  CHARACTER*80 emess
  dfi_fwdstop_month = model_config_rec%dfi_fwdstop_month
  RETURN
END SUBROUTINE nl_get_dfi_fwdstop_month
SUBROUTINE nl_get_dfi_fwdstop_day ( id_id , dfi_fwdstop_day )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_fwdstop_day
  INTEGER id_id
  CHARACTER*80 emess
  dfi_fwdstop_day = model_config_rec%dfi_fwdstop_day
  RETURN
END SUBROUTINE nl_get_dfi_fwdstop_day
SUBROUTINE nl_get_dfi_fwdstop_hour ( id_id , dfi_fwdstop_hour )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_fwdstop_hour
  INTEGER id_id
  CHARACTER*80 emess
  dfi_fwdstop_hour = model_config_rec%dfi_fwdstop_hour
  RETURN
END SUBROUTINE nl_get_dfi_fwdstop_hour
SUBROUTINE nl_get_dfi_fwdstop_minute ( id_id , dfi_fwdstop_minute )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_fwdstop_minute
  INTEGER id_id
  CHARACTER*80 emess
  dfi_fwdstop_minute = model_config_rec%dfi_fwdstop_minute
  RETURN
END SUBROUTINE nl_get_dfi_fwdstop_minute
SUBROUTINE nl_get_dfi_fwdstop_second ( id_id , dfi_fwdstop_second )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_fwdstop_second
  INTEGER id_id
  CHARACTER*80 emess
  dfi_fwdstop_second = model_config_rec%dfi_fwdstop_second
  RETURN
END SUBROUTINE nl_get_dfi_fwdstop_second
SUBROUTINE nl_get_dfi_bckstop_year ( id_id , dfi_bckstop_year )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_bckstop_year
  INTEGER id_id
  CHARACTER*80 emess
  dfi_bckstop_year = model_config_rec%dfi_bckstop_year
  RETURN
END SUBROUTINE nl_get_dfi_bckstop_year
SUBROUTINE nl_get_dfi_bckstop_month ( id_id , dfi_bckstop_month )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_bckstop_month
  INTEGER id_id
  CHARACTER*80 emess
  dfi_bckstop_month = model_config_rec%dfi_bckstop_month
  RETURN
END SUBROUTINE nl_get_dfi_bckstop_month
SUBROUTINE nl_get_dfi_bckstop_day ( id_id , dfi_bckstop_day )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_bckstop_day
  INTEGER id_id
  CHARACTER*80 emess
  dfi_bckstop_day = model_config_rec%dfi_bckstop_day
  RETURN
END SUBROUTINE nl_get_dfi_bckstop_day
SUBROUTINE nl_get_dfi_bckstop_hour ( id_id , dfi_bckstop_hour )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_bckstop_hour
  INTEGER id_id
  CHARACTER*80 emess
  dfi_bckstop_hour = model_config_rec%dfi_bckstop_hour
  RETURN
END SUBROUTINE nl_get_dfi_bckstop_hour
SUBROUTINE nl_get_dfi_bckstop_minute ( id_id , dfi_bckstop_minute )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_bckstop_minute
  INTEGER id_id
  CHARACTER*80 emess
  dfi_bckstop_minute = model_config_rec%dfi_bckstop_minute
  RETURN
END SUBROUTINE nl_get_dfi_bckstop_minute
SUBROUTINE nl_get_dfi_bckstop_second ( id_id , dfi_bckstop_second )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_bckstop_second
  INTEGER id_id
  CHARACTER*80 emess
  dfi_bckstop_second = model_config_rec%dfi_bckstop_second
  RETURN
END SUBROUTINE nl_get_dfi_bckstop_second
SUBROUTINE nl_get_time_step ( id_id , time_step )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: time_step
  INTEGER id_id
  CHARACTER*80 emess
  time_step = model_config_rec%time_step
  RETURN
END SUBROUTINE nl_get_time_step
SUBROUTINE nl_get_time_step_fract_num ( id_id , time_step_fract_num )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: time_step_fract_num
  INTEGER id_id
  CHARACTER*80 emess
  time_step_fract_num = model_config_rec%time_step_fract_num
  RETURN
END SUBROUTINE nl_get_time_step_fract_num
SUBROUTINE nl_get_time_step_fract_den ( id_id , time_step_fract_den )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: time_step_fract_den
  INTEGER id_id
  CHARACTER*80 emess
  time_step_fract_den = model_config_rec%time_step_fract_den
  RETURN
END SUBROUTINE nl_get_time_step_fract_den
SUBROUTINE nl_get_max_dom ( id_id , max_dom )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: max_dom
  INTEGER id_id
  CHARACTER*80 emess
  max_dom = model_config_rec%max_dom
  RETURN
END SUBROUTINE nl_get_max_dom
SUBROUTINE nl_get_s_we ( id_id , s_we )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: s_we
  INTEGER id_id
  CHARACTER*80 emess
  s_we = model_config_rec%s_we(id_id)
  RETURN
END SUBROUTINE nl_get_s_we
SUBROUTINE nl_get_e_we ( id_id , e_we )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: e_we
  INTEGER id_id
  CHARACTER*80 emess
  e_we = model_config_rec%e_we(id_id)
  RETURN
END SUBROUTINE nl_get_e_we
SUBROUTINE nl_get_s_sn ( id_id , s_sn )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: s_sn
  INTEGER id_id
  CHARACTER*80 emess
  s_sn = model_config_rec%s_sn(id_id)
  RETURN
END SUBROUTINE nl_get_s_sn
SUBROUTINE nl_get_e_sn ( id_id , e_sn )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: e_sn
  INTEGER id_id
  CHARACTER*80 emess
  e_sn = model_config_rec%e_sn(id_id)
  RETURN
END SUBROUTINE nl_get_e_sn
SUBROUTINE nl_get_s_vert ( id_id , s_vert )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: s_vert
  INTEGER id_id
  CHARACTER*80 emess
  s_vert = model_config_rec%s_vert(id_id)
  RETURN
END SUBROUTINE nl_get_s_vert
SUBROUTINE nl_get_e_vert ( id_id , e_vert )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: e_vert
  INTEGER id_id
  CHARACTER*80 emess
  e_vert = model_config_rec%e_vert(id_id)
  RETURN
END SUBROUTINE nl_get_e_vert
SUBROUTINE nl_get_dx ( id_id , dx )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: dx
  INTEGER id_id
  CHARACTER*80 emess
  dx = model_config_rec%dx(id_id)
  RETURN
END SUBROUTINE nl_get_dx
SUBROUTINE nl_get_dy ( id_id , dy )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: dy
  INTEGER id_id
  CHARACTER*80 emess
  dy = model_config_rec%dy(id_id)
  RETURN
END SUBROUTINE nl_get_dy
SUBROUTINE nl_get_grid_id ( id_id , grid_id )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: grid_id
  INTEGER id_id
  CHARACTER*80 emess
  grid_id = model_config_rec%grid_id(id_id)
  RETURN
END SUBROUTINE nl_get_grid_id
SUBROUTINE nl_get_grid_allowed ( id_id , grid_allowed )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: grid_allowed
  INTEGER id_id
  CHARACTER*80 emess
  grid_allowed = model_config_rec%grid_allowed(id_id)
  RETURN
END SUBROUTINE nl_get_grid_allowed
SUBROUTINE nl_get_parent_id ( id_id , parent_id )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: parent_id
  INTEGER id_id
  CHARACTER*80 emess
  parent_id = model_config_rec%parent_id(id_id)
  RETURN
END SUBROUTINE nl_get_parent_id
SUBROUTINE nl_get_i_parent_start ( id_id , i_parent_start )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: i_parent_start
  INTEGER id_id
  CHARACTER*80 emess
  i_parent_start = model_config_rec%i_parent_start(id_id)
  RETURN
END SUBROUTINE nl_get_i_parent_start
SUBROUTINE nl_get_j_parent_start ( id_id , j_parent_start )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: j_parent_start
  INTEGER id_id
  CHARACTER*80 emess
  j_parent_start = model_config_rec%j_parent_start(id_id)
  RETURN
END SUBROUTINE nl_get_j_parent_start
SUBROUTINE nl_get_parent_grid_ratio ( id_id , parent_grid_ratio )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: parent_grid_ratio
  INTEGER id_id
  CHARACTER*80 emess
  parent_grid_ratio = model_config_rec%parent_grid_ratio(id_id)
  RETURN
END SUBROUTINE nl_get_parent_grid_ratio
SUBROUTINE nl_get_parent_time_step_ratio ( id_id , parent_time_step_ratio )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: parent_time_step_ratio
  INTEGER id_id
  CHARACTER*80 emess
  parent_time_step_ratio = model_config_rec%parent_time_step_ratio(id_id)
  RETURN
END SUBROUTINE nl_get_parent_time_step_ratio
SUBROUTINE nl_get_feedback ( id_id , feedback )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: feedback
  INTEGER id_id
  CHARACTER*80 emess
  feedback = model_config_rec%feedback
  RETURN
END SUBROUTINE nl_get_feedback
SUBROUTINE nl_get_smooth_option ( id_id , smooth_option )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: smooth_option
  INTEGER id_id
  CHARACTER*80 emess
  smooth_option = model_config_rec%smooth_option
  RETURN
END SUBROUTINE nl_get_smooth_option
SUBROUTINE nl_get_ztop ( id_id , ztop )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: ztop
  INTEGER id_id
  CHARACTER*80 emess
  ztop = model_config_rec%ztop(id_id)
  RETURN
END SUBROUTINE nl_get_ztop
SUBROUTINE nl_get_moad_grid_ratio ( id_id , moad_grid_ratio )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: moad_grid_ratio
  INTEGER id_id
  CHARACTER*80 emess
  moad_grid_ratio = model_config_rec%moad_grid_ratio(id_id)
  RETURN
END SUBROUTINE nl_get_moad_grid_ratio
SUBROUTINE nl_get_moad_time_step_ratio ( id_id , moad_time_step_ratio )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: moad_time_step_ratio
  INTEGER id_id
  CHARACTER*80 emess
  moad_time_step_ratio = model_config_rec%moad_time_step_ratio(id_id)
  RETURN
END SUBROUTINE nl_get_moad_time_step_ratio
SUBROUTINE nl_get_shw ( id_id , shw )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: shw
  INTEGER id_id
  CHARACTER*80 emess
  shw = model_config_rec%shw(id_id)
  RETURN
END SUBROUTINE nl_get_shw
SUBROUTINE nl_get_tile_sz_x ( id_id , tile_sz_x )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: tile_sz_x
  INTEGER id_id
  CHARACTER*80 emess
  tile_sz_x = model_config_rec%tile_sz_x
  RETURN
END SUBROUTINE nl_get_tile_sz_x
SUBROUTINE nl_get_tile_sz_y ( id_id , tile_sz_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: tile_sz_y
  INTEGER id_id
  CHARACTER*80 emess
  tile_sz_y = model_config_rec%tile_sz_y
  RETURN
END SUBROUTINE nl_get_tile_sz_y
SUBROUTINE nl_get_numtiles ( id_id , numtiles )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: numtiles
  INTEGER id_id
  CHARACTER*80 emess
  numtiles = model_config_rec%numtiles
  RETURN
END SUBROUTINE nl_get_numtiles
SUBROUTINE nl_get_nproc_x ( id_id , nproc_x )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: nproc_x
  INTEGER id_id
  CHARACTER*80 emess
  nproc_x = model_config_rec%nproc_x
  RETURN
END SUBROUTINE nl_get_nproc_x
SUBROUTINE nl_get_nproc_y ( id_id , nproc_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: nproc_y
  INTEGER id_id
  CHARACTER*80 emess
  nproc_y = model_config_rec%nproc_y
  RETURN
END SUBROUTINE nl_get_nproc_y
SUBROUTINE nl_get_irand ( id_id , irand )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: irand
  INTEGER id_id
  CHARACTER*80 emess
  irand = model_config_rec%irand
  RETURN
END SUBROUTINE nl_get_irand
SUBROUTINE nl_get_dt ( id_id , dt )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: dt
  INTEGER id_id
  CHARACTER*80 emess
  dt = model_config_rec%dt(id_id)
  RETURN
END SUBROUTINE nl_get_dt
SUBROUTINE nl_get_ts_buf_size ( id_id , ts_buf_size )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: ts_buf_size
  INTEGER id_id
  CHARACTER*80 emess
  ts_buf_size = model_config_rec%ts_buf_size
  RETURN
END SUBROUTINE nl_get_ts_buf_size
SUBROUTINE nl_get_max_ts_locs ( id_id , max_ts_locs )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: max_ts_locs
  INTEGER id_id
  CHARACTER*80 emess
  max_ts_locs = model_config_rec%max_ts_locs
  RETURN
END SUBROUTINE nl_get_max_ts_locs
SUBROUTINE nl_get_num_moves ( id_id , num_moves )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: num_moves
  INTEGER id_id
  CHARACTER*80 emess
  num_moves = model_config_rec%num_moves
  RETURN
END SUBROUTINE nl_get_num_moves
SUBROUTINE nl_get_move_id ( id_id , move_id )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: move_id
  INTEGER id_id
  CHARACTER*80 emess
  move_id = model_config_rec%move_id(id_id)
  RETURN
END SUBROUTINE nl_get_move_id
SUBROUTINE nl_get_move_interval ( id_id , move_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: move_interval
  INTEGER id_id
  CHARACTER*80 emess
  move_interval = model_config_rec%move_interval(id_id)
  RETURN
END SUBROUTINE nl_get_move_interval
SUBROUTINE nl_get_move_cd_x ( id_id , move_cd_x )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: move_cd_x
  INTEGER id_id
  CHARACTER*80 emess
  move_cd_x = model_config_rec%move_cd_x(id_id)
  RETURN
END SUBROUTINE nl_get_move_cd_x
SUBROUTINE nl_get_move_cd_y ( id_id , move_cd_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: move_cd_y
  INTEGER id_id
  CHARACTER*80 emess
  move_cd_y = model_config_rec%move_cd_y(id_id)
  RETURN
END SUBROUTINE nl_get_move_cd_y
SUBROUTINE nl_get_swap_x ( id_id , swap_x )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: swap_x
  INTEGER id_id
  CHARACTER*80 emess
  swap_x = model_config_rec%swap_x(id_id)
  RETURN
END SUBROUTINE nl_get_swap_x
SUBROUTINE nl_get_swap_y ( id_id , swap_y )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: swap_y
  INTEGER id_id
  CHARACTER*80 emess
  swap_y = model_config_rec%swap_y(id_id)
  RETURN
END SUBROUTINE nl_get_swap_y
SUBROUTINE nl_get_cycle_x ( id_id , cycle_x )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: cycle_x
  INTEGER id_id
  CHARACTER*80 emess
  cycle_x = model_config_rec%cycle_x(id_id)
  RETURN
END SUBROUTINE nl_get_cycle_x
SUBROUTINE nl_get_cycle_y ( id_id , cycle_y )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: cycle_y
  INTEGER id_id
  CHARACTER*80 emess
  cycle_y = model_config_rec%cycle_y(id_id)
  RETURN
END SUBROUTINE nl_get_cycle_y
SUBROUTINE nl_get_reorder_mesh ( id_id , reorder_mesh )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: reorder_mesh
  INTEGER id_id
  CHARACTER*80 emess
  reorder_mesh = model_config_rec%reorder_mesh
  RETURN
END SUBROUTINE nl_get_reorder_mesh
SUBROUTINE nl_get_perturb_input ( id_id , perturb_input )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: perturb_input
  INTEGER id_id
  CHARACTER*80 emess
  perturb_input = model_config_rec%perturb_input
  RETURN
END SUBROUTINE nl_get_perturb_input
SUBROUTINE nl_get_eta_levels ( id_id , eta_levels )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: eta_levels
  INTEGER id_id
  CHARACTER*80 emess
  eta_levels = model_config_rec%eta_levels(id_id)
  RETURN
END SUBROUTINE nl_get_eta_levels
SUBROUTINE nl_get_ptsgm ( id_id , ptsgm )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: ptsgm
  INTEGER id_id
  CHARACTER*80 emess
  ptsgm = model_config_rec%ptsgm
  RETURN
END SUBROUTINE nl_get_ptsgm
SUBROUTINE nl_get_num_metgrid_levels ( id_id , num_metgrid_levels )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: num_metgrid_levels
  INTEGER id_id
  CHARACTER*80 emess
  num_metgrid_levels = model_config_rec%num_metgrid_levels
  RETURN
END SUBROUTINE nl_get_num_metgrid_levels
SUBROUTINE nl_get_p_top_requested ( id_id , p_top_requested )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: p_top_requested
  INTEGER id_id
  CHARACTER*80 emess
  p_top_requested = model_config_rec%p_top_requested
  RETURN
END SUBROUTINE nl_get_p_top_requested
SUBROUTINE nl_get_mp_physics ( id_id , mp_physics )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: mp_physics
  INTEGER id_id
  CHARACTER*80 emess
  mp_physics = model_config_rec%mp_physics(id_id)
  RETURN
END SUBROUTINE nl_get_mp_physics
SUBROUTINE nl_get_ra_lw_physics ( id_id , ra_lw_physics )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: ra_lw_physics
  INTEGER id_id
  CHARACTER*80 emess
  ra_lw_physics = model_config_rec%ra_lw_physics(id_id)
  RETURN
END SUBROUTINE nl_get_ra_lw_physics
SUBROUTINE nl_get_ra_sw_physics ( id_id , ra_sw_physics )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: ra_sw_physics
  INTEGER id_id
  CHARACTER*80 emess
  ra_sw_physics = model_config_rec%ra_sw_physics(id_id)
  RETURN
END SUBROUTINE nl_get_ra_sw_physics
SUBROUTINE nl_get_radt ( id_id , radt )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: radt
  INTEGER id_id
  CHARACTER*80 emess
  radt = model_config_rec%radt(id_id)
  RETURN
END SUBROUTINE nl_get_radt
SUBROUTINE nl_get_sf_sfclay_physics ( id_id , sf_sfclay_physics )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: sf_sfclay_physics
  INTEGER id_id
  CHARACTER*80 emess
  sf_sfclay_physics = model_config_rec%sf_sfclay_physics(id_id)
  RETURN
END SUBROUTINE nl_get_sf_sfclay_physics
SUBROUTINE nl_get_sf_surface_physics ( id_id , sf_surface_physics )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: sf_surface_physics
  INTEGER id_id
  CHARACTER*80 emess
  sf_surface_physics = model_config_rec%sf_surface_physics(id_id)
  RETURN
END SUBROUTINE nl_get_sf_surface_physics
SUBROUTINE nl_get_bl_pbl_physics ( id_id , bl_pbl_physics )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: bl_pbl_physics
  INTEGER id_id
  CHARACTER*80 emess
  bl_pbl_physics = model_config_rec%bl_pbl_physics(id_id)
  RETURN
END SUBROUTINE nl_get_bl_pbl_physics
SUBROUTINE nl_get_sf_urban_physics ( id_id , sf_urban_physics )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: sf_urban_physics
  INTEGER id_id
  CHARACTER*80 emess
  sf_urban_physics = model_config_rec%sf_urban_physics(id_id)
  RETURN
END SUBROUTINE nl_get_sf_urban_physics
SUBROUTINE nl_get_bldt ( id_id , bldt )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: bldt
  INTEGER id_id
  CHARACTER*80 emess
  bldt = model_config_rec%bldt(id_id)
  RETURN
END SUBROUTINE nl_get_bldt
SUBROUTINE nl_get_cu_physics ( id_id , cu_physics )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: cu_physics
  INTEGER id_id
  CHARACTER*80 emess
  cu_physics = model_config_rec%cu_physics(id_id)
  RETURN
END SUBROUTINE nl_get_cu_physics
SUBROUTINE nl_get_cudt ( id_id , cudt )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: cudt
  INTEGER id_id
  CHARACTER*80 emess
  cudt = model_config_rec%cudt(id_id)
  RETURN
END SUBROUTINE nl_get_cudt
SUBROUTINE nl_get_gsmdt ( id_id , gsmdt )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: gsmdt
  INTEGER id_id
  CHARACTER*80 emess
  gsmdt = model_config_rec%gsmdt(id_id)
  RETURN
END SUBROUTINE nl_get_gsmdt
SUBROUTINE nl_get_isfflx ( id_id , isfflx )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: isfflx
  INTEGER id_id
  CHARACTER*80 emess
  isfflx = model_config_rec%isfflx
  RETURN
END SUBROUTINE nl_get_isfflx
SUBROUTINE nl_get_ifsnow ( id_id , ifsnow )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: ifsnow
  INTEGER id_id
  CHARACTER*80 emess
  ifsnow = model_config_rec%ifsnow
  RETURN
END SUBROUTINE nl_get_ifsnow
SUBROUTINE nl_get_icloud ( id_id , icloud )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: icloud
  INTEGER id_id
  CHARACTER*80 emess
  icloud = model_config_rec%icloud
  RETURN
END SUBROUTINE nl_get_icloud
SUBROUTINE nl_get_swrad_scat ( id_id , swrad_scat )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: swrad_scat
  INTEGER id_id
  CHARACTER*80 emess
  swrad_scat = model_config_rec%swrad_scat
  RETURN
END SUBROUTINE nl_get_swrad_scat
SUBROUTINE nl_get_surface_input_source ( id_id , surface_input_source )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: surface_input_source
  INTEGER id_id
  CHARACTER*80 emess
  surface_input_source = model_config_rec%surface_input_source
  RETURN
END SUBROUTINE nl_get_surface_input_source
SUBROUTINE nl_get_num_soil_layers ( id_id , num_soil_layers )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: num_soil_layers
  INTEGER id_id
  CHARACTER*80 emess
  num_soil_layers = model_config_rec%num_soil_layers
  RETURN
END SUBROUTINE nl_get_num_soil_layers
SUBROUTINE nl_get_num_urban_layers ( id_id , num_urban_layers )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: num_urban_layers
  INTEGER id_id
  CHARACTER*80 emess
  num_urban_layers = model_config_rec%num_urban_layers
  RETURN
END SUBROUTINE nl_get_num_urban_layers
SUBROUTINE nl_get_maxiens ( id_id , maxiens )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: maxiens
  INTEGER id_id
  CHARACTER*80 emess
  maxiens = model_config_rec%maxiens
  RETURN
END SUBROUTINE nl_get_maxiens
SUBROUTINE nl_get_maxens ( id_id , maxens )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: maxens
  INTEGER id_id
  CHARACTER*80 emess
  maxens = model_config_rec%maxens
  RETURN
END SUBROUTINE nl_get_maxens
SUBROUTINE nl_get_maxens2 ( id_id , maxens2 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: maxens2
  INTEGER id_id
  CHARACTER*80 emess
  maxens2 = model_config_rec%maxens2
  RETURN
END SUBROUTINE nl_get_maxens2
SUBROUTINE nl_get_maxens3 ( id_id , maxens3 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: maxens3
  INTEGER id_id
  CHARACTER*80 emess
  maxens3 = model_config_rec%maxens3
  RETURN
END SUBROUTINE nl_get_maxens3
SUBROUTINE nl_get_ensdim ( id_id , ensdim )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: ensdim
  INTEGER id_id
  CHARACTER*80 emess
  ensdim = model_config_rec%ensdim
  RETURN
END SUBROUTINE nl_get_ensdim
SUBROUTINE nl_get_chem_opt ( id_id , chem_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: chem_opt
  INTEGER id_id
  CHARACTER*80 emess
  chem_opt = model_config_rec%chem_opt(id_id)
  RETURN
END SUBROUTINE nl_get_chem_opt
SUBROUTINE nl_get_num_land_cat ( id_id , num_land_cat )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: num_land_cat
  INTEGER id_id
  CHARACTER*80 emess
  num_land_cat = model_config_rec%num_land_cat
  RETURN
END SUBROUTINE nl_get_num_land_cat
SUBROUTINE nl_get_num_soil_cat ( id_id , num_soil_cat )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: num_soil_cat
  INTEGER id_id
  CHARACTER*80 emess
  num_soil_cat = model_config_rec%num_soil_cat
  RETURN
END SUBROUTINE nl_get_num_soil_cat
SUBROUTINE nl_get_mp_zero_out ( id_id , mp_zero_out )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: mp_zero_out
  INTEGER id_id
  CHARACTER*80 emess
  mp_zero_out = model_config_rec%mp_zero_out
  RETURN
END SUBROUTINE nl_get_mp_zero_out
SUBROUTINE nl_get_mp_zero_out_thresh ( id_id , mp_zero_out_thresh )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: mp_zero_out_thresh
  INTEGER id_id
  CHARACTER*80 emess
  mp_zero_out_thresh = model_config_rec%mp_zero_out_thresh
  RETURN
END SUBROUTINE nl_get_mp_zero_out_thresh
SUBROUTINE nl_get_seaice_threshold ( id_id , seaice_threshold )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: seaice_threshold
  INTEGER id_id
  CHARACTER*80 emess
  seaice_threshold = model_config_rec%seaice_threshold
  RETURN
END SUBROUTINE nl_get_seaice_threshold
SUBROUTINE nl_get_fractional_seaice ( id_id , fractional_seaice )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: fractional_seaice
  INTEGER id_id
  CHARACTER*80 emess
  fractional_seaice = model_config_rec%fractional_seaice
  RETURN
END SUBROUTINE nl_get_fractional_seaice
SUBROUTINE nl_get_sst_update ( id_id , sst_update )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: sst_update
  INTEGER id_id
  CHARACTER*80 emess
  sst_update = model_config_rec%sst_update
  RETURN
END SUBROUTINE nl_get_sst_update
SUBROUTINE nl_get_usemonalb ( id_id , usemonalb )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: usemonalb
  INTEGER id_id
  CHARACTER*80 emess
  usemonalb = model_config_rec%usemonalb
  RETURN
END SUBROUTINE nl_get_usemonalb
SUBROUTINE nl_get_rdmaxalb ( id_id , rdmaxalb )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: rdmaxalb
  INTEGER id_id
  CHARACTER*80 emess
  rdmaxalb = model_config_rec%rdmaxalb
  RETURN
END SUBROUTINE nl_get_rdmaxalb
SUBROUTINE nl_get_rdlai2d ( id_id , rdlai2d )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: rdlai2d
  INTEGER id_id
  CHARACTER*80 emess
  rdlai2d = model_config_rec%rdlai2d
  RETURN
END SUBROUTINE nl_get_rdlai2d
SUBROUTINE nl_get_gwd_opt ( id_id , gwd_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: gwd_opt
  INTEGER id_id
  CHARACTER*80 emess
  gwd_opt = model_config_rec%gwd_opt(id_id)
  RETURN
END SUBROUTINE nl_get_gwd_opt
SUBROUTINE nl_get_idtad ( id_id , idtad )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: idtad
  INTEGER id_id
  CHARACTER*80 emess
  idtad = model_config_rec%idtad(id_id)
  RETURN
END SUBROUTINE nl_get_idtad
SUBROUTINE nl_get_nsoil ( id_id , nsoil )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: nsoil
  INTEGER id_id
  CHARACTER*80 emess
  nsoil = model_config_rec%nsoil(id_id)
  RETURN
END SUBROUTINE nl_get_nsoil
SUBROUTINE nl_get_nphs ( id_id , nphs )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: nphs
  INTEGER id_id
  CHARACTER*80 emess
  nphs = model_config_rec%nphs(id_id)
  RETURN
END SUBROUTINE nl_get_nphs
SUBROUTINE nl_get_ncnvc ( id_id , ncnvc )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: ncnvc
  INTEGER id_id
  CHARACTER*80 emess
  ncnvc = model_config_rec%ncnvc(id_id)
  RETURN
END SUBROUTINE nl_get_ncnvc
SUBROUTINE nl_get_nrads ( id_id , nrads )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: nrads
  INTEGER id_id
  CHARACTER*80 emess
  nrads = model_config_rec%nrads(id_id)
  RETURN
END SUBROUTINE nl_get_nrads
SUBROUTINE nl_get_nradl ( id_id , nradl )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: nradl
  INTEGER id_id
  CHARACTER*80 emess
  nradl = model_config_rec%nradl(id_id)
  RETURN
END SUBROUTINE nl_get_nradl
SUBROUTINE nl_get_tprec ( id_id , tprec )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: tprec
  INTEGER id_id
  CHARACTER*80 emess
  tprec = model_config_rec%tprec(id_id)
  RETURN
END SUBROUTINE nl_get_tprec
SUBROUTINE nl_get_theat ( id_id , theat )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: theat
  INTEGER id_id
  CHARACTER*80 emess
  theat = model_config_rec%theat(id_id)
  RETURN
END SUBROUTINE nl_get_theat
SUBROUTINE nl_get_tclod ( id_id , tclod )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: tclod
  INTEGER id_id
  CHARACTER*80 emess
  tclod = model_config_rec%tclod(id_id)
  RETURN
END SUBROUTINE nl_get_tclod
SUBROUTINE nl_get_trdsw ( id_id , trdsw )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: trdsw
  INTEGER id_id
  CHARACTER*80 emess
  trdsw = model_config_rec%trdsw(id_id)
  RETURN
END SUBROUTINE nl_get_trdsw
SUBROUTINE nl_get_trdlw ( id_id , trdlw )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: trdlw
  INTEGER id_id
  CHARACTER*80 emess
  trdlw = model_config_rec%trdlw(id_id)
  RETURN
END SUBROUTINE nl_get_trdlw
SUBROUTINE nl_get_tsrfc ( id_id , tsrfc )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: tsrfc
  INTEGER id_id
  CHARACTER*80 emess
  tsrfc = model_config_rec%tsrfc(id_id)
  RETURN
END SUBROUTINE nl_get_tsrfc
SUBROUTINE nl_get_pcpflg ( id_id , pcpflg )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: pcpflg
  INTEGER id_id
  CHARACTER*80 emess
  pcpflg = model_config_rec%pcpflg(id_id)
  RETURN
END SUBROUTINE nl_get_pcpflg
SUBROUTINE nl_get_sigma ( id_id , sigma )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: sigma
  INTEGER id_id
  CHARACTER*80 emess
  sigma = model_config_rec%sigma(id_id)
  RETURN
END SUBROUTINE nl_get_sigma
SUBROUTINE nl_get_sfenth ( id_id , sfenth )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: sfenth
  INTEGER id_id
  CHARACTER*80 emess
  sfenth = model_config_rec%sfenth(id_id)
  RETURN
END SUBROUTINE nl_get_sfenth
SUBROUTINE nl_get_co2tf ( id_id , co2tf )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: co2tf
  INTEGER id_id
  CHARACTER*80 emess
  co2tf = model_config_rec%co2tf
  RETURN
END SUBROUTINE nl_get_co2tf
SUBROUTINE nl_get_ra_call_offset ( id_id , ra_call_offset )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: ra_call_offset
  INTEGER id_id
  CHARACTER*80 emess
  ra_call_offset = model_config_rec%ra_call_offset
  RETURN
END SUBROUTINE nl_get_ra_call_offset
SUBROUTINE nl_get_cam_abs_freq_s ( id_id , cam_abs_freq_s )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: cam_abs_freq_s
  INTEGER id_id
  CHARACTER*80 emess
  cam_abs_freq_s = model_config_rec%cam_abs_freq_s
  RETURN
END SUBROUTINE nl_get_cam_abs_freq_s
SUBROUTINE nl_get_levsiz ( id_id , levsiz )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: levsiz
  INTEGER id_id
  CHARACTER*80 emess
  levsiz = model_config_rec%levsiz
  RETURN
END SUBROUTINE nl_get_levsiz
SUBROUTINE nl_get_paerlev ( id_id , paerlev )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: paerlev
  INTEGER id_id
  CHARACTER*80 emess
  paerlev = model_config_rec%paerlev
  RETURN
END SUBROUTINE nl_get_paerlev
SUBROUTINE nl_get_cam_abs_dim1 ( id_id , cam_abs_dim1 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: cam_abs_dim1
  INTEGER id_id
  CHARACTER*80 emess
  cam_abs_dim1 = model_config_rec%cam_abs_dim1
  RETURN
END SUBROUTINE nl_get_cam_abs_dim1
SUBROUTINE nl_get_cam_abs_dim2 ( id_id , cam_abs_dim2 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: cam_abs_dim2
  INTEGER id_id
  CHARACTER*80 emess
  cam_abs_dim2 = model_config_rec%cam_abs_dim2
  RETURN
END SUBROUTINE nl_get_cam_abs_dim2
SUBROUTINE nl_get_cu_rad_feedback ( id_id , cu_rad_feedback )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: cu_rad_feedback
  INTEGER id_id
  CHARACTER*80 emess
  cu_rad_feedback = model_config_rec%cu_rad_feedback(id_id)
  RETURN
END SUBROUTINE nl_get_cu_rad_feedback
SUBROUTINE nl_get_dyn_opt ( id_id , dyn_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dyn_opt
  INTEGER id_id
  CHARACTER*80 emess
  dyn_opt = model_config_rec%dyn_opt
  RETURN
END SUBROUTINE nl_get_dyn_opt
SUBROUTINE nl_get_rk_ord ( id_id , rk_ord )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: rk_ord
  INTEGER id_id
  CHARACTER*80 emess
  rk_ord = model_config_rec%rk_ord
  RETURN
END SUBROUTINE nl_get_rk_ord
SUBROUTINE nl_get_w_damping ( id_id , w_damping )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: w_damping
  INTEGER id_id
  CHARACTER*80 emess
  w_damping = model_config_rec%w_damping
  RETURN
END SUBROUTINE nl_get_w_damping
SUBROUTINE nl_get_diff_opt ( id_id , diff_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: diff_opt
  INTEGER id_id
  CHARACTER*80 emess
  diff_opt = model_config_rec%diff_opt
  RETURN
END SUBROUTINE nl_get_diff_opt
SUBROUTINE nl_get_km_opt ( id_id , km_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: km_opt
  INTEGER id_id
  CHARACTER*80 emess
  km_opt = model_config_rec%km_opt
  RETURN
END SUBROUTINE nl_get_km_opt
SUBROUTINE nl_get_damp_opt ( id_id , damp_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: damp_opt
  INTEGER id_id
  CHARACTER*80 emess
  damp_opt = model_config_rec%damp_opt
  RETURN
END SUBROUTINE nl_get_damp_opt
SUBROUTINE nl_get_zdamp ( id_id , zdamp )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: zdamp
  INTEGER id_id
  CHARACTER*80 emess
  zdamp = model_config_rec%zdamp(id_id)
  RETURN
END SUBROUTINE nl_get_zdamp
SUBROUTINE nl_get_base_pres ( id_id , base_pres )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: base_pres
  INTEGER id_id
  CHARACTER*80 emess
  base_pres = model_config_rec%base_pres
  RETURN
END SUBROUTINE nl_get_base_pres
SUBROUTINE nl_get_base_temp ( id_id , base_temp )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: base_temp
  INTEGER id_id
  CHARACTER*80 emess
  base_temp = model_config_rec%base_temp
  RETURN
END SUBROUTINE nl_get_base_temp
SUBROUTINE nl_get_base_lapse ( id_id , base_lapse )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: base_lapse
  INTEGER id_id
  CHARACTER*80 emess
  base_lapse = model_config_rec%base_lapse
  RETURN
END SUBROUTINE nl_get_base_lapse
SUBROUTINE nl_get_iso_temp ( id_id , iso_temp )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: iso_temp
  INTEGER id_id
  CHARACTER*80 emess
  iso_temp = model_config_rec%iso_temp
  RETURN
END SUBROUTINE nl_get_iso_temp
SUBROUTINE nl_get_dampcoef ( id_id , dampcoef )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: dampcoef
  INTEGER id_id
  CHARACTER*80 emess
  dampcoef = model_config_rec%dampcoef(id_id)
  RETURN
END SUBROUTINE nl_get_dampcoef
SUBROUTINE nl_get_khdif ( id_id , khdif )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: khdif
  INTEGER id_id
  CHARACTER*80 emess
  khdif = model_config_rec%khdif(id_id)
  RETURN
END SUBROUTINE nl_get_khdif
SUBROUTINE nl_get_kvdif ( id_id , kvdif )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: kvdif
  INTEGER id_id
  CHARACTER*80 emess
  kvdif = model_config_rec%kvdif(id_id)
  RETURN
END SUBROUTINE nl_get_kvdif
SUBROUTINE nl_get_c_s ( id_id , c_s )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: c_s
  INTEGER id_id
  CHARACTER*80 emess
  c_s = model_config_rec%c_s(id_id)
  RETURN
END SUBROUTINE nl_get_c_s
SUBROUTINE nl_get_c_k ( id_id , c_k )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: c_k
  INTEGER id_id
  CHARACTER*80 emess
  c_k = model_config_rec%c_k(id_id)
  RETURN
END SUBROUTINE nl_get_c_k
SUBROUTINE nl_get_smdiv ( id_id , smdiv )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: smdiv
  INTEGER id_id
  CHARACTER*80 emess
  smdiv = model_config_rec%smdiv(id_id)
  RETURN
END SUBROUTINE nl_get_smdiv
SUBROUTINE nl_get_emdiv ( id_id , emdiv )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: emdiv
  INTEGER id_id
  CHARACTER*80 emess
  emdiv = model_config_rec%emdiv(id_id)
  RETURN
END SUBROUTINE nl_get_emdiv
SUBROUTINE nl_get_epssm ( id_id , epssm )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: epssm
  INTEGER id_id
  CHARACTER*80 emess
  epssm = model_config_rec%epssm(id_id)
  RETURN
END SUBROUTINE nl_get_epssm
SUBROUTINE nl_get_non_hydrostatic ( id_id , non_hydrostatic )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: non_hydrostatic
  INTEGER id_id
  CHARACTER*80 emess
  non_hydrostatic = model_config_rec%non_hydrostatic(id_id)
  RETURN
END SUBROUTINE nl_get_non_hydrostatic
SUBROUTINE nl_get_time_step_sound ( id_id , time_step_sound )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: time_step_sound
  INTEGER id_id
  CHARACTER*80 emess
  time_step_sound = model_config_rec%time_step_sound(id_id)
  RETURN
END SUBROUTINE nl_get_time_step_sound
SUBROUTINE nl_get_h_mom_adv_order ( id_id , h_mom_adv_order )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: h_mom_adv_order
  INTEGER id_id
  CHARACTER*80 emess
  h_mom_adv_order = model_config_rec%h_mom_adv_order(id_id)
  RETURN
END SUBROUTINE nl_get_h_mom_adv_order
SUBROUTINE nl_get_v_mom_adv_order ( id_id , v_mom_adv_order )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: v_mom_adv_order
  INTEGER id_id
  CHARACTER*80 emess
  v_mom_adv_order = model_config_rec%v_mom_adv_order(id_id)
  RETURN
END SUBROUTINE nl_get_v_mom_adv_order
SUBROUTINE nl_get_h_sca_adv_order ( id_id , h_sca_adv_order )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: h_sca_adv_order
  INTEGER id_id
  CHARACTER*80 emess
  h_sca_adv_order = model_config_rec%h_sca_adv_order(id_id)
  RETURN
END SUBROUTINE nl_get_h_sca_adv_order
SUBROUTINE nl_get_v_sca_adv_order ( id_id , v_sca_adv_order )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: v_sca_adv_order
  INTEGER id_id
  CHARACTER*80 emess
  v_sca_adv_order = model_config_rec%v_sca_adv_order(id_id)
  RETURN
END SUBROUTINE nl_get_v_sca_adv_order
SUBROUTINE nl_get_top_radiation ( id_id , top_radiation )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: top_radiation
  INTEGER id_id
  CHARACTER*80 emess
  top_radiation = model_config_rec%top_radiation(id_id)
  RETURN
END SUBROUTINE nl_get_top_radiation
SUBROUTINE nl_get_tke_upper_bound ( id_id , tke_upper_bound )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: tke_upper_bound
  INTEGER id_id
  CHARACTER*80 emess
  tke_upper_bound = model_config_rec%tke_upper_bound(id_id)
  RETURN
END SUBROUTINE nl_get_tke_upper_bound
SUBROUTINE nl_get_tke_drag_coefficient ( id_id , tke_drag_coefficient )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: tke_drag_coefficient
  INTEGER id_id
  CHARACTER*80 emess
  tke_drag_coefficient = model_config_rec%tke_drag_coefficient(id_id)
  RETURN
END SUBROUTINE nl_get_tke_drag_coefficient
SUBROUTINE nl_get_tke_heat_flux ( id_id , tke_heat_flux )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: tke_heat_flux
  INTEGER id_id
  CHARACTER*80 emess
  tke_heat_flux = model_config_rec%tke_heat_flux(id_id)
  RETURN
END SUBROUTINE nl_get_tke_heat_flux
SUBROUTINE nl_get_pert_coriolis ( id_id , pert_coriolis )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: pert_coriolis
  INTEGER id_id
  CHARACTER*80 emess
  pert_coriolis = model_config_rec%pert_coriolis(id_id)
  RETURN
END SUBROUTINE nl_get_pert_coriolis
SUBROUTINE nl_get_euler_adv ( id_id , euler_adv )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: euler_adv
  INTEGER id_id
  CHARACTER*80 emess
  euler_adv = model_config_rec%euler_adv
  RETURN
END SUBROUTINE nl_get_euler_adv
SUBROUTINE nl_get_idtadt ( id_id , idtadt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: idtadt
  INTEGER id_id
  CHARACTER*80 emess
  idtadt = model_config_rec%idtadt
  RETURN
END SUBROUTINE nl_get_idtadt
SUBROUTINE nl_get_idtadc ( id_id , idtadc )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: idtadc
  INTEGER id_id
  CHARACTER*80 emess
  idtadc = model_config_rec%idtadc
  RETURN
END SUBROUTINE nl_get_idtadc
SUBROUTINE nl_get_boundary_flux ( id_id , boundary_flux )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: boundary_flux
  INTEGER id_id
  CHARACTER*80 emess
  boundary_flux = model_config_rec%boundary_flux
  RETURN
END SUBROUTINE nl_get_boundary_flux
SUBROUTINE nl_get_spec_bdy_width ( id_id , spec_bdy_width )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: spec_bdy_width
  INTEGER id_id
  CHARACTER*80 emess
  spec_bdy_width = model_config_rec%spec_bdy_width
  RETURN
END SUBROUTINE nl_get_spec_bdy_width
SUBROUTINE nl_get_spec_zone ( id_id , spec_zone )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: spec_zone
  INTEGER id_id
  CHARACTER*80 emess
  spec_zone = model_config_rec%spec_zone
  RETURN
END SUBROUTINE nl_get_spec_zone
SUBROUTINE nl_get_relax_zone ( id_id , relax_zone )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: relax_zone
  INTEGER id_id
  CHARACTER*80 emess
  relax_zone = model_config_rec%relax_zone
  RETURN
END SUBROUTINE nl_get_relax_zone
SUBROUTINE nl_get_specified ( id_id , specified )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: specified
  INTEGER id_id
  CHARACTER*80 emess
  specified = model_config_rec%specified(id_id)
  RETURN
END SUBROUTINE nl_get_specified
SUBROUTINE nl_get_periodic_x ( id_id , periodic_x )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: periodic_x
  INTEGER id_id
  CHARACTER*80 emess
  periodic_x = model_config_rec%periodic_x(id_id)
  RETURN
END SUBROUTINE nl_get_periodic_x
SUBROUTINE nl_get_symmetric_xs ( id_id , symmetric_xs )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: symmetric_xs
  INTEGER id_id
  CHARACTER*80 emess
  symmetric_xs = model_config_rec%symmetric_xs(id_id)
  RETURN
END SUBROUTINE nl_get_symmetric_xs
SUBROUTINE nl_get_symmetric_xe ( id_id , symmetric_xe )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: symmetric_xe
  INTEGER id_id
  CHARACTER*80 emess
  symmetric_xe = model_config_rec%symmetric_xe(id_id)
  RETURN
END SUBROUTINE nl_get_symmetric_xe
SUBROUTINE nl_get_open_xs ( id_id , open_xs )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: open_xs
  INTEGER id_id
  CHARACTER*80 emess
  open_xs = model_config_rec%open_xs(id_id)
  RETURN
END SUBROUTINE nl_get_open_xs
SUBROUTINE nl_get_open_xe ( id_id , open_xe )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: open_xe
  INTEGER id_id
  CHARACTER*80 emess
  open_xe = model_config_rec%open_xe(id_id)
  RETURN
END SUBROUTINE nl_get_open_xe
SUBROUTINE nl_get_periodic_y ( id_id , periodic_y )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: periodic_y
  INTEGER id_id
  CHARACTER*80 emess
  periodic_y = model_config_rec%periodic_y(id_id)
  RETURN
END SUBROUTINE nl_get_periodic_y
SUBROUTINE nl_get_symmetric_ys ( id_id , symmetric_ys )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: symmetric_ys
  INTEGER id_id
  CHARACTER*80 emess
  symmetric_ys = model_config_rec%symmetric_ys(id_id)
  RETURN
END SUBROUTINE nl_get_symmetric_ys
SUBROUTINE nl_get_symmetric_ye ( id_id , symmetric_ye )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: symmetric_ye
  INTEGER id_id
  CHARACTER*80 emess
  symmetric_ye = model_config_rec%symmetric_ye(id_id)
  RETURN
END SUBROUTINE nl_get_symmetric_ye
SUBROUTINE nl_get_open_ys ( id_id , open_ys )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: open_ys
  INTEGER id_id
  CHARACTER*80 emess
  open_ys = model_config_rec%open_ys(id_id)
  RETURN
END SUBROUTINE nl_get_open_ys
SUBROUTINE nl_get_open_ye ( id_id , open_ye )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: open_ye
  INTEGER id_id
  CHARACTER*80 emess
  open_ye = model_config_rec%open_ye(id_id)
  RETURN
END SUBROUTINE nl_get_open_ye
SUBROUTINE nl_get_polar ( id_id , polar )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: polar
  INTEGER id_id
  CHARACTER*80 emess
  polar = model_config_rec%polar(id_id)
  RETURN
END SUBROUTINE nl_get_polar
SUBROUTINE nl_get_nested ( id_id , nested )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: nested
  INTEGER id_id
  CHARACTER*80 emess
  nested = model_config_rec%nested(id_id)
  RETURN
END SUBROUTINE nl_get_nested
SUBROUTINE nl_get_real_data_init_type ( id_id , real_data_init_type )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: real_data_init_type
  INTEGER id_id
  CHARACTER*80 emess
  real_data_init_type = model_config_rec%real_data_init_type
  RETURN
END SUBROUTINE nl_get_real_data_init_type
SUBROUTINE nl_get_background_proc_id ( id_id , background_proc_id )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: background_proc_id
  INTEGER id_id
  CHARACTER*80 emess
  background_proc_id = model_config_rec%background_proc_id
  RETURN
END SUBROUTINE nl_get_background_proc_id
SUBROUTINE nl_get_forecast_proc_id ( id_id , forecast_proc_id )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: forecast_proc_id
  INTEGER id_id
  CHARACTER*80 emess
  forecast_proc_id = model_config_rec%forecast_proc_id
  RETURN
END SUBROUTINE nl_get_forecast_proc_id
SUBROUTINE nl_get_production_status ( id_id , production_status )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: production_status
  INTEGER id_id
  CHARACTER*80 emess
  production_status = model_config_rec%production_status
  RETURN
END SUBROUTINE nl_get_production_status
SUBROUTINE nl_get_compression ( id_id , compression )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: compression
  INTEGER id_id
  CHARACTER*80 emess
  compression = model_config_rec%compression
  RETURN
END SUBROUTINE nl_get_compression
SUBROUTINE nl_get_cen_lat ( id_id , cen_lat )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: cen_lat
  INTEGER id_id
  CHARACTER*80 emess
  cen_lat = model_config_rec%cen_lat(id_id)
  RETURN
END SUBROUTINE nl_get_cen_lat
SUBROUTINE nl_get_cen_lon ( id_id , cen_lon )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: cen_lon
  INTEGER id_id
  CHARACTER*80 emess
  cen_lon = model_config_rec%cen_lon(id_id)
  RETURN
END SUBROUTINE nl_get_cen_lon
SUBROUTINE nl_get_truelat1 ( id_id , truelat1 )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: truelat1
  INTEGER id_id
  CHARACTER*80 emess
  truelat1 = model_config_rec%truelat1(id_id)
  RETURN
END SUBROUTINE nl_get_truelat1
SUBROUTINE nl_get_truelat2 ( id_id , truelat2 )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: truelat2
  INTEGER id_id
  CHARACTER*80 emess
  truelat2 = model_config_rec%truelat2(id_id)
  RETURN
END SUBROUTINE nl_get_truelat2
SUBROUTINE nl_get_moad_cen_lat ( id_id , moad_cen_lat )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: moad_cen_lat
  INTEGER id_id
  CHARACTER*80 emess
  moad_cen_lat = model_config_rec%moad_cen_lat(id_id)
  RETURN
END SUBROUTINE nl_get_moad_cen_lat
SUBROUTINE nl_get_stand_lon ( id_id , stand_lon )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: stand_lon
  INTEGER id_id
  CHARACTER*80 emess
  stand_lon = model_config_rec%stand_lon(id_id)
  RETURN
END SUBROUTINE nl_get_stand_lon
SUBROUTINE nl_get_flag_metgrid ( id_id , flag_metgrid )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: flag_metgrid
  INTEGER id_id
  CHARACTER*80 emess
  flag_metgrid = model_config_rec%flag_metgrid
  RETURN
END SUBROUTINE nl_get_flag_metgrid
SUBROUTINE nl_get_flag_snow ( id_id , flag_snow )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: flag_snow
  INTEGER id_id
  CHARACTER*80 emess
  flag_snow = model_config_rec%flag_snow
  RETURN
END SUBROUTINE nl_get_flag_snow
SUBROUTINE nl_get_flag_psfc ( id_id , flag_psfc )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: flag_psfc
  INTEGER id_id
  CHARACTER*80 emess
  flag_psfc = model_config_rec%flag_psfc
  RETURN
END SUBROUTINE nl_get_flag_psfc
SUBROUTINE nl_get_flag_sm000010 ( id_id , flag_sm000010 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: flag_sm000010
  INTEGER id_id
  CHARACTER*80 emess
  flag_sm000010 = model_config_rec%flag_sm000010
  RETURN
END SUBROUTINE nl_get_flag_sm000010
SUBROUTINE nl_get_flag_sm010040 ( id_id , flag_sm010040 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: flag_sm010040
  INTEGER id_id
  CHARACTER*80 emess
  flag_sm010040 = model_config_rec%flag_sm010040
  RETURN
END SUBROUTINE nl_get_flag_sm010040
SUBROUTINE nl_get_flag_sm040100 ( id_id , flag_sm040100 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: flag_sm040100
  INTEGER id_id
  CHARACTER*80 emess
  flag_sm040100 = model_config_rec%flag_sm040100
  RETURN
END SUBROUTINE nl_get_flag_sm040100
SUBROUTINE nl_get_flag_sm100200 ( id_id , flag_sm100200 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: flag_sm100200
  INTEGER id_id
  CHARACTER*80 emess
  flag_sm100200 = model_config_rec%flag_sm100200
  RETURN
END SUBROUTINE nl_get_flag_sm100200
SUBROUTINE nl_get_flag_st000010 ( id_id , flag_st000010 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: flag_st000010
  INTEGER id_id
  CHARACTER*80 emess
  flag_st000010 = model_config_rec%flag_st000010
  RETURN
END SUBROUTINE nl_get_flag_st000010
SUBROUTINE nl_get_flag_st010040 ( id_id , flag_st010040 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: flag_st010040
  INTEGER id_id
  CHARACTER*80 emess
  flag_st010040 = model_config_rec%flag_st010040
  RETURN
END SUBROUTINE nl_get_flag_st010040
SUBROUTINE nl_get_flag_st040100 ( id_id , flag_st040100 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: flag_st040100
  INTEGER id_id
  CHARACTER*80 emess
  flag_st040100 = model_config_rec%flag_st040100
  RETURN
END SUBROUTINE nl_get_flag_st040100
SUBROUTINE nl_get_flag_st100200 ( id_id , flag_st100200 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: flag_st100200
  INTEGER id_id
  CHARACTER*80 emess
  flag_st100200 = model_config_rec%flag_st100200
  RETURN
END SUBROUTINE nl_get_flag_st100200
SUBROUTINE nl_get_flag_slp ( id_id , flag_slp )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: flag_slp
  INTEGER id_id
  CHARACTER*80 emess
  flag_slp = model_config_rec%flag_slp
  RETURN
END SUBROUTINE nl_get_flag_slp
SUBROUTINE nl_get_flag_soilhgt ( id_id , flag_soilhgt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: flag_soilhgt
  INTEGER id_id
  CHARACTER*80 emess
  flag_soilhgt = model_config_rec%flag_soilhgt
  RETURN
END SUBROUTINE nl_get_flag_soilhgt
SUBROUTINE nl_get_flag_mf_xy ( id_id , flag_mf_xy )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: flag_mf_xy
  INTEGER id_id
  CHARACTER*80 emess
  flag_mf_xy = model_config_rec%flag_mf_xy
  RETURN
END SUBROUTINE nl_get_flag_mf_xy
SUBROUTINE nl_get_bdyfrq ( id_id , bdyfrq )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: bdyfrq
  INTEGER id_id
  CHARACTER*80 emess
  bdyfrq = model_config_rec%bdyfrq(id_id)
  RETURN
END SUBROUTINE nl_get_bdyfrq
SUBROUTINE nl_get_mminlu ( id_id , mminlu )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: mminlu
  INTEGER id_id
  CHARACTER*80 emess
  mminlu = model_config_rec%mminlu(id_id)
  RETURN
END SUBROUTINE nl_get_mminlu
SUBROUTINE nl_get_iswater ( id_id , iswater )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: iswater
  INTEGER id_id
  CHARACTER*80 emess
  iswater = model_config_rec%iswater(id_id)
  RETURN
END SUBROUTINE nl_get_iswater
SUBROUTINE nl_get_islake ( id_id , islake )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: islake
  INTEGER id_id
  CHARACTER*80 emess
  islake = model_config_rec%islake(id_id)
  RETURN
END SUBROUTINE nl_get_islake
SUBROUTINE nl_get_isice ( id_id , isice )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: isice
  INTEGER id_id
  CHARACTER*80 emess
  isice = model_config_rec%isice(id_id)
  RETURN
END SUBROUTINE nl_get_isice
SUBROUTINE nl_get_isurban ( id_id , isurban )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: isurban
  INTEGER id_id
  CHARACTER*80 emess
  isurban = model_config_rec%isurban(id_id)
  RETURN
END SUBROUTINE nl_get_isurban
SUBROUTINE nl_get_isoilwater ( id_id , isoilwater )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: isoilwater
  INTEGER id_id
  CHARACTER*80 emess
  isoilwater = model_config_rec%isoilwater(id_id)
  RETURN
END SUBROUTINE nl_get_isoilwater
SUBROUTINE nl_get_map_proj ( id_id , map_proj )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: map_proj
  INTEGER id_id
  CHARACTER*80 emess
  map_proj = model_config_rec%map_proj(id_id)
  RETURN
END SUBROUTINE nl_get_map_proj
SUBROUTINE nl_get_dfi_stage ( id_id , dfi_stage )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_stage
  INTEGER id_id
  CHARACTER*80 emess
  dfi_stage = model_config_rec%dfi_stage
  RETURN
END SUBROUTINE nl_get_dfi_stage
SUBROUTINE nl_get_mp_physics_dfi ( id_id , mp_physics_dfi )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: mp_physics_dfi
  INTEGER id_id
  CHARACTER*80 emess
  mp_physics_dfi = model_config_rec%mp_physics_dfi(id_id)
  RETURN
END SUBROUTINE nl_get_mp_physics_dfi




