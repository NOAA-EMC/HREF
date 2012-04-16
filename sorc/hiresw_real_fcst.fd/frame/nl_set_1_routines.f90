





SUBROUTINE not_a_real_sub_d
END SUBROUTINE not_a_real_sub_d







SUBROUTINE nl_set_inputout_end_h ( id_id , inputout_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: inputout_end_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%inputout_end_h(id_id) = inputout_end_h
  RETURN
END SUBROUTINE nl_set_inputout_end_h
SUBROUTINE nl_set_inputout_end_m ( id_id , inputout_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: inputout_end_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%inputout_end_m(id_id) = inputout_end_m
  RETURN
END SUBROUTINE nl_set_inputout_end_m
SUBROUTINE nl_set_inputout_end_s ( id_id , inputout_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: inputout_end_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%inputout_end_s(id_id) = inputout_end_s
  RETURN
END SUBROUTINE nl_set_inputout_end_s
SUBROUTINE nl_set_auxhist1_end_y ( id_id , auxhist1_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_end_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist1_end_y(id_id) = auxhist1_end_y
  RETURN
END SUBROUTINE nl_set_auxhist1_end_y
SUBROUTINE nl_set_auxhist1_end_mo ( id_id , auxhist1_end_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist1_end_mo(id_id) = auxhist1_end_mo
  RETURN
END SUBROUTINE nl_set_auxhist1_end_mo
SUBROUTINE nl_set_auxhist1_end_d ( id_id , auxhist1_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_end_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist1_end_d(id_id) = auxhist1_end_d
  RETURN
END SUBROUTINE nl_set_auxhist1_end_d
SUBROUTINE nl_set_auxhist1_end_h ( id_id , auxhist1_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_end_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist1_end_h(id_id) = auxhist1_end_h
  RETURN
END SUBROUTINE nl_set_auxhist1_end_h
SUBROUTINE nl_set_auxhist1_end_m ( id_id , auxhist1_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_end_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist1_end_m(id_id) = auxhist1_end_m
  RETURN
END SUBROUTINE nl_set_auxhist1_end_m
SUBROUTINE nl_set_auxhist1_end_s ( id_id , auxhist1_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_end_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist1_end_s(id_id) = auxhist1_end_s
  RETURN
END SUBROUTINE nl_set_auxhist1_end_s
SUBROUTINE nl_set_auxhist2_end_y ( id_id , auxhist2_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_end_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist2_end_y(id_id) = auxhist2_end_y
  RETURN
END SUBROUTINE nl_set_auxhist2_end_y
SUBROUTINE nl_set_auxhist2_end_mo ( id_id , auxhist2_end_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist2_end_mo(id_id) = auxhist2_end_mo
  RETURN
END SUBROUTINE nl_set_auxhist2_end_mo
SUBROUTINE nl_set_auxhist2_end_d ( id_id , auxhist2_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_end_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist2_end_d(id_id) = auxhist2_end_d
  RETURN
END SUBROUTINE nl_set_auxhist2_end_d
SUBROUTINE nl_set_auxhist2_end_h ( id_id , auxhist2_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_end_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist2_end_h(id_id) = auxhist2_end_h
  RETURN
END SUBROUTINE nl_set_auxhist2_end_h
SUBROUTINE nl_set_auxhist2_end_m ( id_id , auxhist2_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_end_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist2_end_m(id_id) = auxhist2_end_m
  RETURN
END SUBROUTINE nl_set_auxhist2_end_m
SUBROUTINE nl_set_auxhist2_end_s ( id_id , auxhist2_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_end_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist2_end_s(id_id) = auxhist2_end_s
  RETURN
END SUBROUTINE nl_set_auxhist2_end_s
SUBROUTINE nl_set_auxhist3_end_y ( id_id , auxhist3_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_end_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist3_end_y(id_id) = auxhist3_end_y
  RETURN
END SUBROUTINE nl_set_auxhist3_end_y
SUBROUTINE nl_set_auxhist3_end_mo ( id_id , auxhist3_end_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist3_end_mo(id_id) = auxhist3_end_mo
  RETURN
END SUBROUTINE nl_set_auxhist3_end_mo
SUBROUTINE nl_set_auxhist3_end_d ( id_id , auxhist3_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_end_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist3_end_d(id_id) = auxhist3_end_d
  RETURN
END SUBROUTINE nl_set_auxhist3_end_d
SUBROUTINE nl_set_auxhist3_end_h ( id_id , auxhist3_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_end_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist3_end_h(id_id) = auxhist3_end_h
  RETURN
END SUBROUTINE nl_set_auxhist3_end_h
SUBROUTINE nl_set_auxhist3_end_m ( id_id , auxhist3_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_end_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist3_end_m(id_id) = auxhist3_end_m
  RETURN
END SUBROUTINE nl_set_auxhist3_end_m
SUBROUTINE nl_set_auxhist3_end_s ( id_id , auxhist3_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_end_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist3_end_s(id_id) = auxhist3_end_s
  RETURN
END SUBROUTINE nl_set_auxhist3_end_s
SUBROUTINE nl_set_auxhist4_end_y ( id_id , auxhist4_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_end_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist4_end_y(id_id) = auxhist4_end_y
  RETURN
END SUBROUTINE nl_set_auxhist4_end_y
SUBROUTINE nl_set_auxhist4_end_mo ( id_id , auxhist4_end_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist4_end_mo(id_id) = auxhist4_end_mo
  RETURN
END SUBROUTINE nl_set_auxhist4_end_mo
SUBROUTINE nl_set_auxhist4_end_d ( id_id , auxhist4_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_end_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist4_end_d(id_id) = auxhist4_end_d
  RETURN
END SUBROUTINE nl_set_auxhist4_end_d
SUBROUTINE nl_set_auxhist4_end_h ( id_id , auxhist4_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_end_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist4_end_h(id_id) = auxhist4_end_h
  RETURN
END SUBROUTINE nl_set_auxhist4_end_h
SUBROUTINE nl_set_auxhist4_end_m ( id_id , auxhist4_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_end_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist4_end_m(id_id) = auxhist4_end_m
  RETURN
END SUBROUTINE nl_set_auxhist4_end_m
SUBROUTINE nl_set_auxhist4_end_s ( id_id , auxhist4_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_end_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist4_end_s(id_id) = auxhist4_end_s
  RETURN
END SUBROUTINE nl_set_auxhist4_end_s
SUBROUTINE nl_set_auxhist5_end_y ( id_id , auxhist5_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist5_end_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist5_end_y(id_id) = auxhist5_end_y
  RETURN
END SUBROUTINE nl_set_auxhist5_end_y
SUBROUTINE nl_set_auxhist5_end_mo ( id_id , auxhist5_end_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist5_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist5_end_mo(id_id) = auxhist5_end_mo
  RETURN
END SUBROUTINE nl_set_auxhist5_end_mo
SUBROUTINE nl_set_auxhist5_end_d ( id_id , auxhist5_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist5_end_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist5_end_d(id_id) = auxhist5_end_d
  RETURN
END SUBROUTINE nl_set_auxhist5_end_d
SUBROUTINE nl_set_auxhist5_end_h ( id_id , auxhist5_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist5_end_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist5_end_h(id_id) = auxhist5_end_h
  RETURN
END SUBROUTINE nl_set_auxhist5_end_h
SUBROUTINE nl_set_auxhist5_end_m ( id_id , auxhist5_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist5_end_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist5_end_m(id_id) = auxhist5_end_m
  RETURN
END SUBROUTINE nl_set_auxhist5_end_m
SUBROUTINE nl_set_auxhist5_end_s ( id_id , auxhist5_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist5_end_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist5_end_s(id_id) = auxhist5_end_s
  RETURN
END SUBROUTINE nl_set_auxhist5_end_s
SUBROUTINE nl_set_auxhist6_end_y ( id_id , auxhist6_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist6_end_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist6_end_y(id_id) = auxhist6_end_y
  RETURN
END SUBROUTINE nl_set_auxhist6_end_y
SUBROUTINE nl_set_auxhist6_end_mo ( id_id , auxhist6_end_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist6_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist6_end_mo(id_id) = auxhist6_end_mo
  RETURN
END SUBROUTINE nl_set_auxhist6_end_mo
SUBROUTINE nl_set_auxhist6_end_d ( id_id , auxhist6_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist6_end_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist6_end_d(id_id) = auxhist6_end_d
  RETURN
END SUBROUTINE nl_set_auxhist6_end_d
SUBROUTINE nl_set_auxhist6_end_h ( id_id , auxhist6_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist6_end_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist6_end_h(id_id) = auxhist6_end_h
  RETURN
END SUBROUTINE nl_set_auxhist6_end_h
SUBROUTINE nl_set_auxhist6_end_m ( id_id , auxhist6_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist6_end_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist6_end_m(id_id) = auxhist6_end_m
  RETURN
END SUBROUTINE nl_set_auxhist6_end_m
SUBROUTINE nl_set_auxhist6_end_s ( id_id , auxhist6_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist6_end_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist6_end_s(id_id) = auxhist6_end_s
  RETURN
END SUBROUTINE nl_set_auxhist6_end_s
SUBROUTINE nl_set_auxhist7_end_y ( id_id , auxhist7_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist7_end_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist7_end_y(id_id) = auxhist7_end_y
  RETURN
END SUBROUTINE nl_set_auxhist7_end_y
SUBROUTINE nl_set_auxhist7_end_mo ( id_id , auxhist7_end_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist7_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist7_end_mo(id_id) = auxhist7_end_mo
  RETURN
END SUBROUTINE nl_set_auxhist7_end_mo
SUBROUTINE nl_set_auxhist7_end_d ( id_id , auxhist7_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist7_end_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist7_end_d(id_id) = auxhist7_end_d
  RETURN
END SUBROUTINE nl_set_auxhist7_end_d
SUBROUTINE nl_set_auxhist7_end_h ( id_id , auxhist7_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist7_end_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist7_end_h(id_id) = auxhist7_end_h
  RETURN
END SUBROUTINE nl_set_auxhist7_end_h
SUBROUTINE nl_set_auxhist7_end_m ( id_id , auxhist7_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist7_end_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist7_end_m(id_id) = auxhist7_end_m
  RETURN
END SUBROUTINE nl_set_auxhist7_end_m
SUBROUTINE nl_set_auxhist7_end_s ( id_id , auxhist7_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist7_end_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist7_end_s(id_id) = auxhist7_end_s
  RETURN
END SUBROUTINE nl_set_auxhist7_end_s
SUBROUTINE nl_set_auxhist8_end_y ( id_id , auxhist8_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist8_end_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist8_end_y(id_id) = auxhist8_end_y
  RETURN
END SUBROUTINE nl_set_auxhist8_end_y
SUBROUTINE nl_set_auxhist8_end_mo ( id_id , auxhist8_end_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist8_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist8_end_mo(id_id) = auxhist8_end_mo
  RETURN
END SUBROUTINE nl_set_auxhist8_end_mo
SUBROUTINE nl_set_auxhist8_end_d ( id_id , auxhist8_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist8_end_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist8_end_d(id_id) = auxhist8_end_d
  RETURN
END SUBROUTINE nl_set_auxhist8_end_d
SUBROUTINE nl_set_auxhist8_end_h ( id_id , auxhist8_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist8_end_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist8_end_h(id_id) = auxhist8_end_h
  RETURN
END SUBROUTINE nl_set_auxhist8_end_h
SUBROUTINE nl_set_auxhist8_end_m ( id_id , auxhist8_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist8_end_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist8_end_m(id_id) = auxhist8_end_m
  RETURN
END SUBROUTINE nl_set_auxhist8_end_m
SUBROUTINE nl_set_auxhist8_end_s ( id_id , auxhist8_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist8_end_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist8_end_s(id_id) = auxhist8_end_s
  RETURN
END SUBROUTINE nl_set_auxhist8_end_s
SUBROUTINE nl_set_auxhist9_end_y ( id_id , auxhist9_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist9_end_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist9_end_y(id_id) = auxhist9_end_y
  RETURN
END SUBROUTINE nl_set_auxhist9_end_y
SUBROUTINE nl_set_auxhist9_end_mo ( id_id , auxhist9_end_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist9_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist9_end_mo(id_id) = auxhist9_end_mo
  RETURN
END SUBROUTINE nl_set_auxhist9_end_mo
SUBROUTINE nl_set_auxhist9_end_d ( id_id , auxhist9_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist9_end_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist9_end_d(id_id) = auxhist9_end_d
  RETURN
END SUBROUTINE nl_set_auxhist9_end_d
SUBROUTINE nl_set_auxhist9_end_h ( id_id , auxhist9_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist9_end_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist9_end_h(id_id) = auxhist9_end_h
  RETURN
END SUBROUTINE nl_set_auxhist9_end_h
SUBROUTINE nl_set_auxhist9_end_m ( id_id , auxhist9_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist9_end_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist9_end_m(id_id) = auxhist9_end_m
  RETURN
END SUBROUTINE nl_set_auxhist9_end_m
SUBROUTINE nl_set_auxhist9_end_s ( id_id , auxhist9_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist9_end_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist9_end_s(id_id) = auxhist9_end_s
  RETURN
END SUBROUTINE nl_set_auxhist9_end_s
SUBROUTINE nl_set_auxhist10_end_y ( id_id , auxhist10_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist10_end_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist10_end_y(id_id) = auxhist10_end_y
  RETURN
END SUBROUTINE nl_set_auxhist10_end_y
SUBROUTINE nl_set_auxhist10_end_mo ( id_id , auxhist10_end_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist10_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist10_end_mo(id_id) = auxhist10_end_mo
  RETURN
END SUBROUTINE nl_set_auxhist10_end_mo
SUBROUTINE nl_set_auxhist10_end_d ( id_id , auxhist10_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist10_end_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist10_end_d(id_id) = auxhist10_end_d
  RETURN
END SUBROUTINE nl_set_auxhist10_end_d
SUBROUTINE nl_set_auxhist10_end_h ( id_id , auxhist10_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist10_end_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist10_end_h(id_id) = auxhist10_end_h
  RETURN
END SUBROUTINE nl_set_auxhist10_end_h
SUBROUTINE nl_set_auxhist10_end_m ( id_id , auxhist10_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist10_end_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist10_end_m(id_id) = auxhist10_end_m
  RETURN
END SUBROUTINE nl_set_auxhist10_end_m
SUBROUTINE nl_set_auxhist10_end_s ( id_id , auxhist10_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist10_end_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist10_end_s(id_id) = auxhist10_end_s
  RETURN
END SUBROUTINE nl_set_auxhist10_end_s
SUBROUTINE nl_set_auxhist11_end_y ( id_id , auxhist11_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist11_end_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist11_end_y(id_id) = auxhist11_end_y
  RETURN
END SUBROUTINE nl_set_auxhist11_end_y
SUBROUTINE nl_set_auxhist11_end_mo ( id_id , auxhist11_end_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist11_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist11_end_mo(id_id) = auxhist11_end_mo
  RETURN
END SUBROUTINE nl_set_auxhist11_end_mo
SUBROUTINE nl_set_auxhist11_end_d ( id_id , auxhist11_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist11_end_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist11_end_d(id_id) = auxhist11_end_d
  RETURN
END SUBROUTINE nl_set_auxhist11_end_d
SUBROUTINE nl_set_auxhist11_end_h ( id_id , auxhist11_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist11_end_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist11_end_h(id_id) = auxhist11_end_h
  RETURN
END SUBROUTINE nl_set_auxhist11_end_h
SUBROUTINE nl_set_auxhist11_end_m ( id_id , auxhist11_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist11_end_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist11_end_m(id_id) = auxhist11_end_m
  RETURN
END SUBROUTINE nl_set_auxhist11_end_m
SUBROUTINE nl_set_auxhist11_end_s ( id_id , auxhist11_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist11_end_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist11_end_s(id_id) = auxhist11_end_s
  RETURN
END SUBROUTINE nl_set_auxhist11_end_s
SUBROUTINE nl_set_auxinput1_end_y ( id_id , auxinput1_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput1_end_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput1_end_y(id_id) = auxinput1_end_y
  RETURN
END SUBROUTINE nl_set_auxinput1_end_y
SUBROUTINE nl_set_auxinput1_end_mo ( id_id , auxinput1_end_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput1_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput1_end_mo(id_id) = auxinput1_end_mo
  RETURN
END SUBROUTINE nl_set_auxinput1_end_mo
SUBROUTINE nl_set_auxinput1_end_d ( id_id , auxinput1_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput1_end_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput1_end_d(id_id) = auxinput1_end_d
  RETURN
END SUBROUTINE nl_set_auxinput1_end_d
SUBROUTINE nl_set_auxinput1_end_h ( id_id , auxinput1_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput1_end_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput1_end_h(id_id) = auxinput1_end_h
  RETURN
END SUBROUTINE nl_set_auxinput1_end_h
SUBROUTINE nl_set_auxinput1_end_m ( id_id , auxinput1_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput1_end_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput1_end_m(id_id) = auxinput1_end_m
  RETURN
END SUBROUTINE nl_set_auxinput1_end_m
SUBROUTINE nl_set_auxinput1_end_s ( id_id , auxinput1_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput1_end_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput1_end_s(id_id) = auxinput1_end_s
  RETURN
END SUBROUTINE nl_set_auxinput1_end_s
SUBROUTINE nl_set_auxinput2_end_y ( id_id , auxinput2_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput2_end_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput2_end_y(id_id) = auxinput2_end_y
  RETURN
END SUBROUTINE nl_set_auxinput2_end_y
SUBROUTINE nl_set_auxinput2_end_mo ( id_id , auxinput2_end_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput2_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput2_end_mo(id_id) = auxinput2_end_mo
  RETURN
END SUBROUTINE nl_set_auxinput2_end_mo
SUBROUTINE nl_set_auxinput2_end_d ( id_id , auxinput2_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput2_end_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput2_end_d(id_id) = auxinput2_end_d
  RETURN
END SUBROUTINE nl_set_auxinput2_end_d
SUBROUTINE nl_set_auxinput2_end_h ( id_id , auxinput2_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput2_end_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput2_end_h(id_id) = auxinput2_end_h
  RETURN
END SUBROUTINE nl_set_auxinput2_end_h
SUBROUTINE nl_set_auxinput2_end_m ( id_id , auxinput2_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput2_end_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput2_end_m(id_id) = auxinput2_end_m
  RETURN
END SUBROUTINE nl_set_auxinput2_end_m
SUBROUTINE nl_set_auxinput2_end_s ( id_id , auxinput2_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput2_end_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput2_end_s(id_id) = auxinput2_end_s
  RETURN
END SUBROUTINE nl_set_auxinput2_end_s
SUBROUTINE nl_set_auxinput3_end_y ( id_id , auxinput3_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput3_end_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput3_end_y(id_id) = auxinput3_end_y
  RETURN
END SUBROUTINE nl_set_auxinput3_end_y
SUBROUTINE nl_set_auxinput3_end_mo ( id_id , auxinput3_end_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput3_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput3_end_mo(id_id) = auxinput3_end_mo
  RETURN
END SUBROUTINE nl_set_auxinput3_end_mo
SUBROUTINE nl_set_auxinput3_end_d ( id_id , auxinput3_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput3_end_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput3_end_d(id_id) = auxinput3_end_d
  RETURN
END SUBROUTINE nl_set_auxinput3_end_d
SUBROUTINE nl_set_auxinput3_end_h ( id_id , auxinput3_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput3_end_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput3_end_h(id_id) = auxinput3_end_h
  RETURN
END SUBROUTINE nl_set_auxinput3_end_h
SUBROUTINE nl_set_auxinput3_end_m ( id_id , auxinput3_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput3_end_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput3_end_m(id_id) = auxinput3_end_m
  RETURN
END SUBROUTINE nl_set_auxinput3_end_m
SUBROUTINE nl_set_auxinput3_end_s ( id_id , auxinput3_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput3_end_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput3_end_s(id_id) = auxinput3_end_s
  RETURN
END SUBROUTINE nl_set_auxinput3_end_s
SUBROUTINE nl_set_auxinput4_end_y ( id_id , auxinput4_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput4_end_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput4_end_y(id_id) = auxinput4_end_y
  RETURN
END SUBROUTINE nl_set_auxinput4_end_y
SUBROUTINE nl_set_auxinput4_end_mo ( id_id , auxinput4_end_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput4_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput4_end_mo(id_id) = auxinput4_end_mo
  RETURN
END SUBROUTINE nl_set_auxinput4_end_mo
SUBROUTINE nl_set_auxinput4_end_d ( id_id , auxinput4_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput4_end_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput4_end_d(id_id) = auxinput4_end_d
  RETURN
END SUBROUTINE nl_set_auxinput4_end_d
SUBROUTINE nl_set_auxinput4_end_h ( id_id , auxinput4_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput4_end_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput4_end_h(id_id) = auxinput4_end_h
  RETURN
END SUBROUTINE nl_set_auxinput4_end_h
SUBROUTINE nl_set_auxinput4_end_m ( id_id , auxinput4_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput4_end_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput4_end_m(id_id) = auxinput4_end_m
  RETURN
END SUBROUTINE nl_set_auxinput4_end_m
SUBROUTINE nl_set_auxinput4_end_s ( id_id , auxinput4_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput4_end_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput4_end_s(id_id) = auxinput4_end_s
  RETURN
END SUBROUTINE nl_set_auxinput4_end_s
SUBROUTINE nl_set_auxinput5_end_y ( id_id , auxinput5_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput5_end_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput5_end_y(id_id) = auxinput5_end_y
  RETURN
END SUBROUTINE nl_set_auxinput5_end_y
SUBROUTINE nl_set_auxinput5_end_mo ( id_id , auxinput5_end_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput5_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput5_end_mo(id_id) = auxinput5_end_mo
  RETURN
END SUBROUTINE nl_set_auxinput5_end_mo
SUBROUTINE nl_set_auxinput5_end_d ( id_id , auxinput5_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput5_end_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput5_end_d(id_id) = auxinput5_end_d
  RETURN
END SUBROUTINE nl_set_auxinput5_end_d
SUBROUTINE nl_set_auxinput5_end_h ( id_id , auxinput5_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput5_end_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput5_end_h(id_id) = auxinput5_end_h
  RETURN
END SUBROUTINE nl_set_auxinput5_end_h
SUBROUTINE nl_set_auxinput5_end_m ( id_id , auxinput5_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput5_end_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput5_end_m(id_id) = auxinput5_end_m
  RETURN
END SUBROUTINE nl_set_auxinput5_end_m
SUBROUTINE nl_set_auxinput5_end_s ( id_id , auxinput5_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput5_end_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput5_end_s(id_id) = auxinput5_end_s
  RETURN
END SUBROUTINE nl_set_auxinput5_end_s
SUBROUTINE nl_set_auxinput6_end_y ( id_id , auxinput6_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput6_end_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput6_end_y(id_id) = auxinput6_end_y
  RETURN
END SUBROUTINE nl_set_auxinput6_end_y
SUBROUTINE nl_set_auxinput6_end_mo ( id_id , auxinput6_end_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput6_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput6_end_mo(id_id) = auxinput6_end_mo
  RETURN
END SUBROUTINE nl_set_auxinput6_end_mo
SUBROUTINE nl_set_auxinput6_end_d ( id_id , auxinput6_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput6_end_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput6_end_d(id_id) = auxinput6_end_d
  RETURN
END SUBROUTINE nl_set_auxinput6_end_d
SUBROUTINE nl_set_auxinput6_end_h ( id_id , auxinput6_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput6_end_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput6_end_h(id_id) = auxinput6_end_h
  RETURN
END SUBROUTINE nl_set_auxinput6_end_h
SUBROUTINE nl_set_auxinput6_end_m ( id_id , auxinput6_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput6_end_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput6_end_m(id_id) = auxinput6_end_m
  RETURN
END SUBROUTINE nl_set_auxinput6_end_m
SUBROUTINE nl_set_auxinput6_end_s ( id_id , auxinput6_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput6_end_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput6_end_s(id_id) = auxinput6_end_s
  RETURN
END SUBROUTINE nl_set_auxinput6_end_s
SUBROUTINE nl_set_auxinput7_end_y ( id_id , auxinput7_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput7_end_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput7_end_y(id_id) = auxinput7_end_y
  RETURN
END SUBROUTINE nl_set_auxinput7_end_y
SUBROUTINE nl_set_auxinput7_end_mo ( id_id , auxinput7_end_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput7_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput7_end_mo(id_id) = auxinput7_end_mo
  RETURN
END SUBROUTINE nl_set_auxinput7_end_mo
SUBROUTINE nl_set_auxinput7_end_d ( id_id , auxinput7_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput7_end_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput7_end_d(id_id) = auxinput7_end_d
  RETURN
END SUBROUTINE nl_set_auxinput7_end_d
SUBROUTINE nl_set_auxinput7_end_h ( id_id , auxinput7_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput7_end_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput7_end_h(id_id) = auxinput7_end_h
  RETURN
END SUBROUTINE nl_set_auxinput7_end_h
SUBROUTINE nl_set_auxinput7_end_m ( id_id , auxinput7_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput7_end_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput7_end_m(id_id) = auxinput7_end_m
  RETURN
END SUBROUTINE nl_set_auxinput7_end_m
SUBROUTINE nl_set_auxinput7_end_s ( id_id , auxinput7_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput7_end_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput7_end_s(id_id) = auxinput7_end_s
  RETURN
END SUBROUTINE nl_set_auxinput7_end_s
SUBROUTINE nl_set_auxinput8_end_y ( id_id , auxinput8_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput8_end_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput8_end_y(id_id) = auxinput8_end_y
  RETURN
END SUBROUTINE nl_set_auxinput8_end_y
SUBROUTINE nl_set_auxinput8_end_mo ( id_id , auxinput8_end_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput8_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput8_end_mo(id_id) = auxinput8_end_mo
  RETURN
END SUBROUTINE nl_set_auxinput8_end_mo
SUBROUTINE nl_set_auxinput8_end_d ( id_id , auxinput8_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput8_end_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput8_end_d(id_id) = auxinput8_end_d
  RETURN
END SUBROUTINE nl_set_auxinput8_end_d
SUBROUTINE nl_set_auxinput8_end_h ( id_id , auxinput8_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput8_end_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput8_end_h(id_id) = auxinput8_end_h
  RETURN
END SUBROUTINE nl_set_auxinput8_end_h
SUBROUTINE nl_set_auxinput8_end_m ( id_id , auxinput8_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput8_end_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput8_end_m(id_id) = auxinput8_end_m
  RETURN
END SUBROUTINE nl_set_auxinput8_end_m
SUBROUTINE nl_set_auxinput8_end_s ( id_id , auxinput8_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput8_end_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput8_end_s(id_id) = auxinput8_end_s
  RETURN
END SUBROUTINE nl_set_auxinput8_end_s
SUBROUTINE nl_set_sgfdda_end_y ( id_id , sgfdda_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sgfdda_end_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%sgfdda_end_y(id_id) = sgfdda_end_y
  RETURN
END SUBROUTINE nl_set_sgfdda_end_y
SUBROUTINE nl_set_sgfdda_end_mo ( id_id , sgfdda_end_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sgfdda_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%sgfdda_end_mo(id_id) = sgfdda_end_mo
  RETURN
END SUBROUTINE nl_set_sgfdda_end_mo
SUBROUTINE nl_set_sgfdda_end_d ( id_id , sgfdda_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sgfdda_end_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%sgfdda_end_d(id_id) = sgfdda_end_d
  RETURN
END SUBROUTINE nl_set_sgfdda_end_d
SUBROUTINE nl_set_sgfdda_end_h ( id_id , sgfdda_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sgfdda_end_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%sgfdda_end_h(id_id) = sgfdda_end_h
  RETURN
END SUBROUTINE nl_set_sgfdda_end_h
SUBROUTINE nl_set_sgfdda_end_m ( id_id , sgfdda_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sgfdda_end_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%sgfdda_end_m(id_id) = sgfdda_end_m
  RETURN
END SUBROUTINE nl_set_sgfdda_end_m
SUBROUTINE nl_set_sgfdda_end_s ( id_id , sgfdda_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sgfdda_end_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%sgfdda_end_s(id_id) = sgfdda_end_s
  RETURN
END SUBROUTINE nl_set_sgfdda_end_s
SUBROUTINE nl_set_gfdda_end_y ( id_id , gfdda_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: gfdda_end_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%gfdda_end_y(id_id) = gfdda_end_y
  RETURN
END SUBROUTINE nl_set_gfdda_end_y
SUBROUTINE nl_set_gfdda_end_mo ( id_id , gfdda_end_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: gfdda_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%gfdda_end_mo(id_id) = gfdda_end_mo
  RETURN
END SUBROUTINE nl_set_gfdda_end_mo
SUBROUTINE nl_set_gfdda_end_d ( id_id , gfdda_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: gfdda_end_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%gfdda_end_d(id_id) = gfdda_end_d
  RETURN
END SUBROUTINE nl_set_gfdda_end_d
SUBROUTINE nl_set_gfdda_end_h ( id_id , gfdda_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: gfdda_end_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%gfdda_end_h(id_id) = gfdda_end_h
  RETURN
END SUBROUTINE nl_set_gfdda_end_h
SUBROUTINE nl_set_gfdda_end_m ( id_id , gfdda_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: gfdda_end_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%gfdda_end_m(id_id) = gfdda_end_m
  RETURN
END SUBROUTINE nl_set_gfdda_end_m
SUBROUTINE nl_set_gfdda_end_s ( id_id , gfdda_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: gfdda_end_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%gfdda_end_s(id_id) = gfdda_end_s
  RETURN
END SUBROUTINE nl_set_gfdda_end_s
SUBROUTINE nl_set_auxinput11_end_y ( id_id , auxinput11_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput11_end_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput11_end_y(id_id) = auxinput11_end_y
  RETURN
END SUBROUTINE nl_set_auxinput11_end_y
SUBROUTINE nl_set_auxinput11_end_mo ( id_id , auxinput11_end_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput11_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput11_end_mo(id_id) = auxinput11_end_mo
  RETURN
END SUBROUTINE nl_set_auxinput11_end_mo
SUBROUTINE nl_set_auxinput11_end_d ( id_id , auxinput11_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput11_end_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput11_end_d(id_id) = auxinput11_end_d
  RETURN
END SUBROUTINE nl_set_auxinput11_end_d
SUBROUTINE nl_set_auxinput11_end_h ( id_id , auxinput11_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput11_end_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput11_end_h(id_id) = auxinput11_end_h
  RETURN
END SUBROUTINE nl_set_auxinput11_end_h
SUBROUTINE nl_set_auxinput11_end_m ( id_id , auxinput11_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput11_end_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput11_end_m(id_id) = auxinput11_end_m
  RETURN
END SUBROUTINE nl_set_auxinput11_end_m
SUBROUTINE nl_set_auxinput11_end_s ( id_id , auxinput11_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput11_end_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput11_end_s(id_id) = auxinput11_end_s
  RETURN
END SUBROUTINE nl_set_auxinput11_end_s
SUBROUTINE nl_set_io_form_auxinput1 ( id_id , io_form_auxinput1 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxinput1
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%io_form_auxinput1 = io_form_auxinput1 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput1
SUBROUTINE nl_set_io_form_auxinput2 ( id_id , io_form_auxinput2 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxinput2
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%io_form_auxinput2 = io_form_auxinput2 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput2
SUBROUTINE nl_set_io_form_auxinput3 ( id_id , io_form_auxinput3 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxinput3
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%io_form_auxinput3 = io_form_auxinput3 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput3
SUBROUTINE nl_set_io_form_auxinput4 ( id_id , io_form_auxinput4 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxinput4
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%io_form_auxinput4 = io_form_auxinput4 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput4
SUBROUTINE nl_set_io_form_auxinput5 ( id_id , io_form_auxinput5 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxinput5
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%io_form_auxinput5 = io_form_auxinput5 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput5
SUBROUTINE nl_set_io_form_auxinput6 ( id_id , io_form_auxinput6 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxinput6
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%io_form_auxinput6 = io_form_auxinput6 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput6
SUBROUTINE nl_set_io_form_auxinput7 ( id_id , io_form_auxinput7 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxinput7
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%io_form_auxinput7 = io_form_auxinput7 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput7
SUBROUTINE nl_set_io_form_auxinput8 ( id_id , io_form_auxinput8 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxinput8
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%io_form_auxinput8 = io_form_auxinput8 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput8
SUBROUTINE nl_set_io_form_sgfdda ( id_id , io_form_sgfdda )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_sgfdda
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%io_form_sgfdda = io_form_sgfdda 
  RETURN
END SUBROUTINE nl_set_io_form_sgfdda
SUBROUTINE nl_set_io_form_gfdda ( id_id , io_form_gfdda )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_gfdda
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%io_form_gfdda = io_form_gfdda 
  RETURN
END SUBROUTINE nl_set_io_form_gfdda
SUBROUTINE nl_set_io_form_auxinput11 ( id_id , io_form_auxinput11 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxinput11
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%io_form_auxinput11 = io_form_auxinput11 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput11
SUBROUTINE nl_set_io_form_auxhist1 ( id_id , io_form_auxhist1 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxhist1
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%io_form_auxhist1 = io_form_auxhist1 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist1
SUBROUTINE nl_set_io_form_auxhist2 ( id_id , io_form_auxhist2 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxhist2
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%io_form_auxhist2 = io_form_auxhist2 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist2
SUBROUTINE nl_set_io_form_auxhist3 ( id_id , io_form_auxhist3 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxhist3
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%io_form_auxhist3 = io_form_auxhist3 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist3
SUBROUTINE nl_set_io_form_auxhist4 ( id_id , io_form_auxhist4 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxhist4
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%io_form_auxhist4 = io_form_auxhist4 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist4
SUBROUTINE nl_set_io_form_auxhist5 ( id_id , io_form_auxhist5 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxhist5
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%io_form_auxhist5 = io_form_auxhist5 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist5
SUBROUTINE nl_set_io_form_auxhist6 ( id_id , io_form_auxhist6 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxhist6
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%io_form_auxhist6 = io_form_auxhist6 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist6
SUBROUTINE nl_set_io_form_auxhist7 ( id_id , io_form_auxhist7 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxhist7
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%io_form_auxhist7 = io_form_auxhist7 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist7
SUBROUTINE nl_set_io_form_auxhist8 ( id_id , io_form_auxhist8 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxhist8
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%io_form_auxhist8 = io_form_auxhist8 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist8
SUBROUTINE nl_set_io_form_auxhist9 ( id_id , io_form_auxhist9 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxhist9
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%io_form_auxhist9 = io_form_auxhist9 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist9
SUBROUTINE nl_set_io_form_auxhist10 ( id_id , io_form_auxhist10 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxhist10
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%io_form_auxhist10 = io_form_auxhist10 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist10
SUBROUTINE nl_set_io_form_auxhist11 ( id_id , io_form_auxhist11 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxhist11
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%io_form_auxhist11 = io_form_auxhist11 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist11
SUBROUTINE nl_set_simulation_start_year ( id_id , simulation_start_year )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: simulation_start_year
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%simulation_start_year = simulation_start_year 
  RETURN
END SUBROUTINE nl_set_simulation_start_year
SUBROUTINE nl_set_simulation_start_month ( id_id , simulation_start_month )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: simulation_start_month
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%simulation_start_month = simulation_start_month 
  RETURN
END SUBROUTINE nl_set_simulation_start_month
SUBROUTINE nl_set_simulation_start_day ( id_id , simulation_start_day )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: simulation_start_day
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%simulation_start_day = simulation_start_day 
  RETURN
END SUBROUTINE nl_set_simulation_start_day
SUBROUTINE nl_set_simulation_start_hour ( id_id , simulation_start_hour )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: simulation_start_hour
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%simulation_start_hour = simulation_start_hour 
  RETURN
END SUBROUTINE nl_set_simulation_start_hour
SUBROUTINE nl_set_simulation_start_minute ( id_id , simulation_start_minute )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: simulation_start_minute
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%simulation_start_minute = simulation_start_minute 
  RETURN
END SUBROUTINE nl_set_simulation_start_minute
SUBROUTINE nl_set_simulation_start_second ( id_id , simulation_start_second )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: simulation_start_second
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%simulation_start_second = simulation_start_second 
  RETURN
END SUBROUTINE nl_set_simulation_start_second
SUBROUTINE nl_set_reset_simulation_start ( id_id , reset_simulation_start )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: reset_simulation_start
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%reset_simulation_start = reset_simulation_start 
  RETURN
END SUBROUTINE nl_set_reset_simulation_start
SUBROUTINE nl_set_sr_x ( id_id , sr_x )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sr_x
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%sr_x(id_id) = sr_x
  RETURN
END SUBROUTINE nl_set_sr_x
SUBROUTINE nl_set_sr_y ( id_id , sr_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sr_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%sr_y(id_id) = sr_y
  RETURN
END SUBROUTINE nl_set_sr_y
SUBROUTINE nl_set_julyr ( id_id , julyr )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: julyr
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%julyr(id_id) = julyr
  RETURN
END SUBROUTINE nl_set_julyr
SUBROUTINE nl_set_julday ( id_id , julday )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: julday
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%julday(id_id) = julday
  RETURN
END SUBROUTINE nl_set_julday
SUBROUTINE nl_set_gmt ( id_id , gmt )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: gmt
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%gmt(id_id) = gmt
  RETURN
END SUBROUTINE nl_set_gmt
SUBROUTINE nl_set_input_inname ( id_id , input_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: input_inname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%input_inname = trim(input_inname) 
  RETURN
END SUBROUTINE nl_set_input_inname
SUBROUTINE nl_set_input_outname ( id_id , input_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: input_outname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%input_outname = trim(input_outname) 
  RETURN
END SUBROUTINE nl_set_input_outname
SUBROUTINE nl_set_bdy_inname ( id_id , bdy_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: bdy_inname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%bdy_inname = trim(bdy_inname) 
  RETURN
END SUBROUTINE nl_set_bdy_inname
SUBROUTINE nl_set_bdy_outname ( id_id , bdy_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: bdy_outname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%bdy_outname = trim(bdy_outname) 
  RETURN
END SUBROUTINE nl_set_bdy_outname
SUBROUTINE nl_set_rst_inname ( id_id , rst_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: rst_inname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%rst_inname = trim(rst_inname) 
  RETURN
END SUBROUTINE nl_set_rst_inname
SUBROUTINE nl_set_rst_outname ( id_id , rst_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: rst_outname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%rst_outname = trim(rst_outname) 
  RETURN
END SUBROUTINE nl_set_rst_outname
SUBROUTINE nl_set_write_input ( id_id , write_input )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: write_input
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%write_input = write_input 
  RETURN
END SUBROUTINE nl_set_write_input
SUBROUTINE nl_set_write_restart_at_0h ( id_id , write_restart_at_0h )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: write_restart_at_0h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%write_restart_at_0h = write_restart_at_0h 
  RETURN
END SUBROUTINE nl_set_write_restart_at_0h
SUBROUTINE nl_set_adjust_output_times ( id_id , adjust_output_times )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: adjust_output_times
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%adjust_output_times = adjust_output_times 
  RETURN
END SUBROUTINE nl_set_adjust_output_times
SUBROUTINE nl_set_adjust_input_times ( id_id , adjust_input_times )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: adjust_input_times
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%adjust_input_times = adjust_input_times 
  RETURN
END SUBROUTINE nl_set_adjust_input_times
SUBROUTINE nl_set_tstart ( id_id , tstart )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: tstart
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%tstart(id_id) = tstart
  RETURN
END SUBROUTINE nl_set_tstart
SUBROUTINE nl_set_nocolons ( id_id , nocolons )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: nocolons
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%nocolons = nocolons 
  RETURN
END SUBROUTINE nl_set_nocolons
SUBROUTINE nl_set_cycling ( id_id , cycling )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: cycling
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%cycling = cycling 
  RETURN
END SUBROUTINE nl_set_cycling
SUBROUTINE nl_set_dfi_opt ( id_id , dfi_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: dfi_opt
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%dfi_opt = dfi_opt 
  RETURN
END SUBROUTINE nl_set_dfi_opt
SUBROUTINE nl_set_dfi_nfilter ( id_id , dfi_nfilter )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: dfi_nfilter
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%dfi_nfilter = dfi_nfilter 
  RETURN
END SUBROUTINE nl_set_dfi_nfilter
SUBROUTINE nl_set_dfi_write_filtered_input ( id_id , dfi_write_filtered_input )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: dfi_write_filtered_input
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%dfi_write_filtered_input = dfi_write_filtered_input 
  RETURN
END SUBROUTINE nl_set_dfi_write_filtered_input
SUBROUTINE nl_set_dfi_write_dfi_history ( id_id , dfi_write_dfi_history )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: dfi_write_dfi_history
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%dfi_write_dfi_history = dfi_write_dfi_history 
  RETURN
END SUBROUTINE nl_set_dfi_write_dfi_history
SUBROUTINE nl_set_dfi_cutoff_seconds ( id_id , dfi_cutoff_seconds )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: dfi_cutoff_seconds
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%dfi_cutoff_seconds = dfi_cutoff_seconds 
  RETURN
END SUBROUTINE nl_set_dfi_cutoff_seconds
SUBROUTINE nl_set_dfi_time_dim ( id_id , dfi_time_dim )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: dfi_time_dim
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%dfi_time_dim = dfi_time_dim 
  RETURN
END SUBROUTINE nl_set_dfi_time_dim
SUBROUTINE nl_set_dfi_fwdstop_year ( id_id , dfi_fwdstop_year )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: dfi_fwdstop_year
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%dfi_fwdstop_year = dfi_fwdstop_year 
  RETURN
END SUBROUTINE nl_set_dfi_fwdstop_year
SUBROUTINE nl_set_dfi_fwdstop_month ( id_id , dfi_fwdstop_month )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: dfi_fwdstop_month
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%dfi_fwdstop_month = dfi_fwdstop_month 
  RETURN
END SUBROUTINE nl_set_dfi_fwdstop_month
SUBROUTINE nl_set_dfi_fwdstop_day ( id_id , dfi_fwdstop_day )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: dfi_fwdstop_day
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%dfi_fwdstop_day = dfi_fwdstop_day 
  RETURN
END SUBROUTINE nl_set_dfi_fwdstop_day
SUBROUTINE nl_set_dfi_fwdstop_hour ( id_id , dfi_fwdstop_hour )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: dfi_fwdstop_hour
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%dfi_fwdstop_hour = dfi_fwdstop_hour 
  RETURN
END SUBROUTINE nl_set_dfi_fwdstop_hour
SUBROUTINE nl_set_dfi_fwdstop_minute ( id_id , dfi_fwdstop_minute )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: dfi_fwdstop_minute
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%dfi_fwdstop_minute = dfi_fwdstop_minute 
  RETURN
END SUBROUTINE nl_set_dfi_fwdstop_minute
SUBROUTINE nl_set_dfi_fwdstop_second ( id_id , dfi_fwdstop_second )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: dfi_fwdstop_second
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%dfi_fwdstop_second = dfi_fwdstop_second 
  RETURN
END SUBROUTINE nl_set_dfi_fwdstop_second
SUBROUTINE nl_set_dfi_bckstop_year ( id_id , dfi_bckstop_year )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: dfi_bckstop_year
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%dfi_bckstop_year = dfi_bckstop_year 
  RETURN
END SUBROUTINE nl_set_dfi_bckstop_year
SUBROUTINE nl_set_dfi_bckstop_month ( id_id , dfi_bckstop_month )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: dfi_bckstop_month
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%dfi_bckstop_month = dfi_bckstop_month 
  RETURN
END SUBROUTINE nl_set_dfi_bckstop_month
SUBROUTINE nl_set_dfi_bckstop_day ( id_id , dfi_bckstop_day )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: dfi_bckstop_day
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%dfi_bckstop_day = dfi_bckstop_day 
  RETURN
END SUBROUTINE nl_set_dfi_bckstop_day
SUBROUTINE nl_set_dfi_bckstop_hour ( id_id , dfi_bckstop_hour )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: dfi_bckstop_hour
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%dfi_bckstop_hour = dfi_bckstop_hour 
  RETURN
END SUBROUTINE nl_set_dfi_bckstop_hour
SUBROUTINE nl_set_dfi_bckstop_minute ( id_id , dfi_bckstop_minute )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: dfi_bckstop_minute
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%dfi_bckstop_minute = dfi_bckstop_minute 
  RETURN
END SUBROUTINE nl_set_dfi_bckstop_minute
SUBROUTINE nl_set_dfi_bckstop_second ( id_id , dfi_bckstop_second )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: dfi_bckstop_second
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%dfi_bckstop_second = dfi_bckstop_second 
  RETURN
END SUBROUTINE nl_set_dfi_bckstop_second
SUBROUTINE nl_set_time_step ( id_id , time_step )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: time_step
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%time_step = time_step 
  RETURN
END SUBROUTINE nl_set_time_step
SUBROUTINE nl_set_time_step_fract_num ( id_id , time_step_fract_num )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: time_step_fract_num
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%time_step_fract_num = time_step_fract_num 
  RETURN
END SUBROUTINE nl_set_time_step_fract_num
SUBROUTINE nl_set_time_step_fract_den ( id_id , time_step_fract_den )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: time_step_fract_den
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%time_step_fract_den = time_step_fract_den 
  RETURN
END SUBROUTINE nl_set_time_step_fract_den
SUBROUTINE nl_set_max_dom ( id_id , max_dom )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: max_dom
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%max_dom = max_dom 
  RETURN
END SUBROUTINE nl_set_max_dom
SUBROUTINE nl_set_s_we ( id_id , s_we )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: s_we
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%s_we(id_id) = s_we
  RETURN
END SUBROUTINE nl_set_s_we
SUBROUTINE nl_set_e_we ( id_id , e_we )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: e_we
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%e_we(id_id) = e_we
  RETURN
END SUBROUTINE nl_set_e_we
SUBROUTINE nl_set_s_sn ( id_id , s_sn )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: s_sn
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%s_sn(id_id) = s_sn
  RETURN
END SUBROUTINE nl_set_s_sn
SUBROUTINE nl_set_e_sn ( id_id , e_sn )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: e_sn
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%e_sn(id_id) = e_sn
  RETURN
END SUBROUTINE nl_set_e_sn
SUBROUTINE nl_set_s_vert ( id_id , s_vert )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: s_vert
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%s_vert(id_id) = s_vert
  RETURN
END SUBROUTINE nl_set_s_vert
SUBROUTINE nl_set_e_vert ( id_id , e_vert )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: e_vert
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%e_vert(id_id) = e_vert
  RETURN
END SUBROUTINE nl_set_e_vert
SUBROUTINE nl_set_dx ( id_id , dx )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: dx
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%dx(id_id) = dx
  RETURN
END SUBROUTINE nl_set_dx
SUBROUTINE nl_set_dy ( id_id , dy )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: dy
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%dy(id_id) = dy
  RETURN
END SUBROUTINE nl_set_dy
SUBROUTINE nl_set_grid_id ( id_id , grid_id )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: grid_id
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%grid_id(id_id) = grid_id
  RETURN
END SUBROUTINE nl_set_grid_id
SUBROUTINE nl_set_grid_allowed ( id_id , grid_allowed )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: grid_allowed
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%grid_allowed(id_id) = grid_allowed
  RETURN
END SUBROUTINE nl_set_grid_allowed
SUBROUTINE nl_set_parent_id ( id_id , parent_id )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: parent_id
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%parent_id(id_id) = parent_id
  RETURN
END SUBROUTINE nl_set_parent_id
SUBROUTINE nl_set_i_parent_start ( id_id , i_parent_start )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: i_parent_start
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%i_parent_start(id_id) = i_parent_start
  RETURN
END SUBROUTINE nl_set_i_parent_start
SUBROUTINE nl_set_j_parent_start ( id_id , j_parent_start )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: j_parent_start
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%j_parent_start(id_id) = j_parent_start
  RETURN
END SUBROUTINE nl_set_j_parent_start
SUBROUTINE nl_set_parent_grid_ratio ( id_id , parent_grid_ratio )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: parent_grid_ratio
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%parent_grid_ratio(id_id) = parent_grid_ratio
  RETURN
END SUBROUTINE nl_set_parent_grid_ratio
SUBROUTINE nl_set_parent_time_step_ratio ( id_id , parent_time_step_ratio )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: parent_time_step_ratio
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%parent_time_step_ratio(id_id) = parent_time_step_ratio
  RETURN
END SUBROUTINE nl_set_parent_time_step_ratio
SUBROUTINE nl_set_feedback ( id_id , feedback )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: feedback
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%feedback = feedback 
  RETURN
END SUBROUTINE nl_set_feedback
SUBROUTINE nl_set_smooth_option ( id_id , smooth_option )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: smooth_option
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%smooth_option = smooth_option 
  RETURN
END SUBROUTINE nl_set_smooth_option
SUBROUTINE nl_set_ztop ( id_id , ztop )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: ztop
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%ztop(id_id) = ztop
  RETURN
END SUBROUTINE nl_set_ztop
SUBROUTINE nl_set_moad_grid_ratio ( id_id , moad_grid_ratio )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: moad_grid_ratio
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%moad_grid_ratio(id_id) = moad_grid_ratio
  RETURN
END SUBROUTINE nl_set_moad_grid_ratio
SUBROUTINE nl_set_moad_time_step_ratio ( id_id , moad_time_step_ratio )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: moad_time_step_ratio
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%moad_time_step_ratio(id_id) = moad_time_step_ratio
  RETURN
END SUBROUTINE nl_set_moad_time_step_ratio
SUBROUTINE nl_set_shw ( id_id , shw )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: shw
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%shw(id_id) = shw
  RETURN
END SUBROUTINE nl_set_shw
SUBROUTINE nl_set_tile_sz_x ( id_id , tile_sz_x )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: tile_sz_x
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%tile_sz_x = tile_sz_x 
  RETURN
END SUBROUTINE nl_set_tile_sz_x
SUBROUTINE nl_set_tile_sz_y ( id_id , tile_sz_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: tile_sz_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%tile_sz_y = tile_sz_y 
  RETURN
END SUBROUTINE nl_set_tile_sz_y
SUBROUTINE nl_set_numtiles ( id_id , numtiles )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: numtiles
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%numtiles = numtiles 
  RETURN
END SUBROUTINE nl_set_numtiles
SUBROUTINE nl_set_nproc_x ( id_id , nproc_x )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: nproc_x
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%nproc_x = nproc_x 
  RETURN
END SUBROUTINE nl_set_nproc_x
SUBROUTINE nl_set_nproc_y ( id_id , nproc_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: nproc_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%nproc_y = nproc_y 
  RETURN
END SUBROUTINE nl_set_nproc_y
SUBROUTINE nl_set_irand ( id_id , irand )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: irand
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%irand = irand 
  RETURN
END SUBROUTINE nl_set_irand
SUBROUTINE nl_set_dt ( id_id , dt )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: dt
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%dt(id_id) = dt
  RETURN
END SUBROUTINE nl_set_dt
SUBROUTINE nl_set_ts_buf_size ( id_id , ts_buf_size )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: ts_buf_size
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%ts_buf_size = ts_buf_size 
  RETURN
END SUBROUTINE nl_set_ts_buf_size
SUBROUTINE nl_set_max_ts_locs ( id_id , max_ts_locs )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: max_ts_locs
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%max_ts_locs = max_ts_locs 
  RETURN
END SUBROUTINE nl_set_max_ts_locs
SUBROUTINE nl_set_num_moves ( id_id , num_moves )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: num_moves
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%num_moves = num_moves 
  RETURN
END SUBROUTINE nl_set_num_moves
SUBROUTINE nl_set_move_id ( id_id , move_id )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: move_id
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%move_id(id_id) = move_id
  RETURN
END SUBROUTINE nl_set_move_id
SUBROUTINE nl_set_move_interval ( id_id , move_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: move_interval
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%move_interval(id_id) = move_interval
  RETURN
END SUBROUTINE nl_set_move_interval
SUBROUTINE nl_set_move_cd_x ( id_id , move_cd_x )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: move_cd_x
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%move_cd_x(id_id) = move_cd_x
  RETURN
END SUBROUTINE nl_set_move_cd_x
SUBROUTINE nl_set_move_cd_y ( id_id , move_cd_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: move_cd_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%move_cd_y(id_id) = move_cd_y
  RETURN
END SUBROUTINE nl_set_move_cd_y
SUBROUTINE nl_set_swap_x ( id_id , swap_x )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: swap_x
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%swap_x(id_id) = swap_x
  RETURN
END SUBROUTINE nl_set_swap_x
SUBROUTINE nl_set_swap_y ( id_id , swap_y )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: swap_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%swap_y(id_id) = swap_y
  RETURN
END SUBROUTINE nl_set_swap_y
SUBROUTINE nl_set_cycle_x ( id_id , cycle_x )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: cycle_x
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%cycle_x(id_id) = cycle_x
  RETURN
END SUBROUTINE nl_set_cycle_x
SUBROUTINE nl_set_cycle_y ( id_id , cycle_y )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: cycle_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%cycle_y(id_id) = cycle_y
  RETURN
END SUBROUTINE nl_set_cycle_y
SUBROUTINE nl_set_reorder_mesh ( id_id , reorder_mesh )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: reorder_mesh
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%reorder_mesh = reorder_mesh 
  RETURN
END SUBROUTINE nl_set_reorder_mesh
SUBROUTINE nl_set_perturb_input ( id_id , perturb_input )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: perturb_input
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%perturb_input = perturb_input 
  RETURN
END SUBROUTINE nl_set_perturb_input
SUBROUTINE nl_set_eta_levels ( id_id , eta_levels )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: eta_levels
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%eta_levels(id_id) = eta_levels
  RETURN
END SUBROUTINE nl_set_eta_levels
SUBROUTINE nl_set_ptsgm ( id_id , ptsgm )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: ptsgm
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%ptsgm = ptsgm 
  RETURN
END SUBROUTINE nl_set_ptsgm
SUBROUTINE nl_set_num_metgrid_levels ( id_id , num_metgrid_levels )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: num_metgrid_levels
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%num_metgrid_levels = num_metgrid_levels 
  RETURN
END SUBROUTINE nl_set_num_metgrid_levels
SUBROUTINE nl_set_p_top_requested ( id_id , p_top_requested )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: p_top_requested
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%p_top_requested = p_top_requested 
  RETURN
END SUBROUTINE nl_set_p_top_requested
SUBROUTINE nl_set_mp_physics ( id_id , mp_physics )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: mp_physics
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%mp_physics(id_id) = mp_physics
  RETURN
END SUBROUTINE nl_set_mp_physics
SUBROUTINE nl_set_ra_lw_physics ( id_id , ra_lw_physics )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: ra_lw_physics
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%ra_lw_physics(id_id) = ra_lw_physics
  RETURN
END SUBROUTINE nl_set_ra_lw_physics
SUBROUTINE nl_set_ra_sw_physics ( id_id , ra_sw_physics )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: ra_sw_physics
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%ra_sw_physics(id_id) = ra_sw_physics
  RETURN
END SUBROUTINE nl_set_ra_sw_physics
SUBROUTINE nl_set_radt ( id_id , radt )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: radt
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%radt(id_id) = radt
  RETURN
END SUBROUTINE nl_set_radt
SUBROUTINE nl_set_sf_sfclay_physics ( id_id , sf_sfclay_physics )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sf_sfclay_physics
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%sf_sfclay_physics(id_id) = sf_sfclay_physics
  RETURN
END SUBROUTINE nl_set_sf_sfclay_physics
SUBROUTINE nl_set_sf_surface_physics ( id_id , sf_surface_physics )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sf_surface_physics
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%sf_surface_physics(id_id) = sf_surface_physics
  RETURN
END SUBROUTINE nl_set_sf_surface_physics
SUBROUTINE nl_set_bl_pbl_physics ( id_id , bl_pbl_physics )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: bl_pbl_physics
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%bl_pbl_physics(id_id) = bl_pbl_physics
  RETURN
END SUBROUTINE nl_set_bl_pbl_physics
SUBROUTINE nl_set_sf_urban_physics ( id_id , sf_urban_physics )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sf_urban_physics
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%sf_urban_physics(id_id) = sf_urban_physics
  RETURN
END SUBROUTINE nl_set_sf_urban_physics
SUBROUTINE nl_set_bldt ( id_id , bldt )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: bldt
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%bldt(id_id) = bldt
  RETURN
END SUBROUTINE nl_set_bldt
SUBROUTINE nl_set_cu_physics ( id_id , cu_physics )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: cu_physics
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%cu_physics(id_id) = cu_physics
  RETURN
END SUBROUTINE nl_set_cu_physics
SUBROUTINE nl_set_cudt ( id_id , cudt )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: cudt
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%cudt(id_id) = cudt
  RETURN
END SUBROUTINE nl_set_cudt
SUBROUTINE nl_set_gsmdt ( id_id , gsmdt )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: gsmdt
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%gsmdt(id_id) = gsmdt
  RETURN
END SUBROUTINE nl_set_gsmdt
SUBROUTINE nl_set_isfflx ( id_id , isfflx )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: isfflx
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%isfflx = isfflx 
  RETURN
END SUBROUTINE nl_set_isfflx
SUBROUTINE nl_set_ifsnow ( id_id , ifsnow )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: ifsnow
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%ifsnow = ifsnow 
  RETURN
END SUBROUTINE nl_set_ifsnow
SUBROUTINE nl_set_icloud ( id_id , icloud )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: icloud
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%icloud = icloud 
  RETURN
END SUBROUTINE nl_set_icloud
SUBROUTINE nl_set_swrad_scat ( id_id , swrad_scat )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: swrad_scat
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%swrad_scat = swrad_scat 
  RETURN
END SUBROUTINE nl_set_swrad_scat
SUBROUTINE nl_set_surface_input_source ( id_id , surface_input_source )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: surface_input_source
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%surface_input_source = surface_input_source 
  RETURN
END SUBROUTINE nl_set_surface_input_source
SUBROUTINE nl_set_num_soil_layers ( id_id , num_soil_layers )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: num_soil_layers
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%num_soil_layers = num_soil_layers 
  RETURN
END SUBROUTINE nl_set_num_soil_layers
SUBROUTINE nl_set_num_urban_layers ( id_id , num_urban_layers )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: num_urban_layers
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%num_urban_layers = num_urban_layers 
  RETURN
END SUBROUTINE nl_set_num_urban_layers
SUBROUTINE nl_set_maxiens ( id_id , maxiens )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: maxiens
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%maxiens = maxiens 
  RETURN
END SUBROUTINE nl_set_maxiens
SUBROUTINE nl_set_maxens ( id_id , maxens )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: maxens
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%maxens = maxens 
  RETURN
END SUBROUTINE nl_set_maxens
SUBROUTINE nl_set_maxens2 ( id_id , maxens2 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: maxens2
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%maxens2 = maxens2 
  RETURN
END SUBROUTINE nl_set_maxens2
SUBROUTINE nl_set_maxens3 ( id_id , maxens3 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: maxens3
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%maxens3 = maxens3 
  RETURN
END SUBROUTINE nl_set_maxens3
SUBROUTINE nl_set_ensdim ( id_id , ensdim )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: ensdim
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%ensdim = ensdim 
  RETURN
END SUBROUTINE nl_set_ensdim
SUBROUTINE nl_set_chem_opt ( id_id , chem_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: chem_opt
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%chem_opt(id_id) = chem_opt
  RETURN
END SUBROUTINE nl_set_chem_opt
SUBROUTINE nl_set_num_land_cat ( id_id , num_land_cat )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: num_land_cat
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%num_land_cat = num_land_cat 
  RETURN
END SUBROUTINE nl_set_num_land_cat
SUBROUTINE nl_set_num_soil_cat ( id_id , num_soil_cat )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: num_soil_cat
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%num_soil_cat = num_soil_cat 
  RETURN
END SUBROUTINE nl_set_num_soil_cat
SUBROUTINE nl_set_mp_zero_out ( id_id , mp_zero_out )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: mp_zero_out
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%mp_zero_out = mp_zero_out 
  RETURN
END SUBROUTINE nl_set_mp_zero_out
SUBROUTINE nl_set_mp_zero_out_thresh ( id_id , mp_zero_out_thresh )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: mp_zero_out_thresh
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%mp_zero_out_thresh = mp_zero_out_thresh 
  RETURN
END SUBROUTINE nl_set_mp_zero_out_thresh
SUBROUTINE nl_set_seaice_threshold ( id_id , seaice_threshold )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: seaice_threshold
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%seaice_threshold = seaice_threshold 
  RETURN
END SUBROUTINE nl_set_seaice_threshold
SUBROUTINE nl_set_fractional_seaice ( id_id , fractional_seaice )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: fractional_seaice
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%fractional_seaice = fractional_seaice 
  RETURN
END SUBROUTINE nl_set_fractional_seaice
SUBROUTINE nl_set_sst_update ( id_id , sst_update )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sst_update
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%sst_update = sst_update 
  RETURN
END SUBROUTINE nl_set_sst_update
SUBROUTINE nl_set_usemonalb ( id_id , usemonalb )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: usemonalb
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%usemonalb = usemonalb 
  RETURN
END SUBROUTINE nl_set_usemonalb
SUBROUTINE nl_set_rdmaxalb ( id_id , rdmaxalb )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: rdmaxalb
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%rdmaxalb = rdmaxalb 
  RETURN
END SUBROUTINE nl_set_rdmaxalb
SUBROUTINE nl_set_rdlai2d ( id_id , rdlai2d )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: rdlai2d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%rdlai2d = rdlai2d 
  RETURN
END SUBROUTINE nl_set_rdlai2d
SUBROUTINE nl_set_gwd_opt ( id_id , gwd_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: gwd_opt
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%gwd_opt(id_id) = gwd_opt
  RETURN
END SUBROUTINE nl_set_gwd_opt
SUBROUTINE nl_set_idtad ( id_id , idtad )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: idtad
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%idtad(id_id) = idtad
  RETURN
END SUBROUTINE nl_set_idtad
SUBROUTINE nl_set_nsoil ( id_id , nsoil )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: nsoil
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%nsoil(id_id) = nsoil
  RETURN
END SUBROUTINE nl_set_nsoil
SUBROUTINE nl_set_nphs ( id_id , nphs )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: nphs
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%nphs(id_id) = nphs
  RETURN
END SUBROUTINE nl_set_nphs
SUBROUTINE nl_set_ncnvc ( id_id , ncnvc )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: ncnvc
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%ncnvc(id_id) = ncnvc
  RETURN
END SUBROUTINE nl_set_ncnvc
SUBROUTINE nl_set_nrads ( id_id , nrads )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: nrads
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%nrads(id_id) = nrads
  RETURN
END SUBROUTINE nl_set_nrads
SUBROUTINE nl_set_nradl ( id_id , nradl )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: nradl
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%nradl(id_id) = nradl
  RETURN
END SUBROUTINE nl_set_nradl
SUBROUTINE nl_set_tprec ( id_id , tprec )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: tprec
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%tprec(id_id) = tprec
  RETURN
END SUBROUTINE nl_set_tprec
SUBROUTINE nl_set_theat ( id_id , theat )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: theat
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%theat(id_id) = theat
  RETURN
END SUBROUTINE nl_set_theat
SUBROUTINE nl_set_tclod ( id_id , tclod )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: tclod
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%tclod(id_id) = tclod
  RETURN
END SUBROUTINE nl_set_tclod
SUBROUTINE nl_set_trdsw ( id_id , trdsw )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: trdsw
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%trdsw(id_id) = trdsw
  RETURN
END SUBROUTINE nl_set_trdsw
SUBROUTINE nl_set_trdlw ( id_id , trdlw )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: trdlw
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%trdlw(id_id) = trdlw
  RETURN
END SUBROUTINE nl_set_trdlw
SUBROUTINE nl_set_tsrfc ( id_id , tsrfc )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: tsrfc
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%tsrfc(id_id) = tsrfc
  RETURN
END SUBROUTINE nl_set_tsrfc
SUBROUTINE nl_set_pcpflg ( id_id , pcpflg )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: pcpflg
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%pcpflg(id_id) = pcpflg
  RETURN
END SUBROUTINE nl_set_pcpflg
SUBROUTINE nl_set_sigma ( id_id , sigma )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sigma
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%sigma(id_id) = sigma
  RETURN
END SUBROUTINE nl_set_sigma
SUBROUTINE nl_set_sfenth ( id_id , sfenth )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: sfenth
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%sfenth(id_id) = sfenth
  RETURN
END SUBROUTINE nl_set_sfenth
SUBROUTINE nl_set_co2tf ( id_id , co2tf )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: co2tf
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%co2tf = co2tf 
  RETURN
END SUBROUTINE nl_set_co2tf
SUBROUTINE nl_set_ra_call_offset ( id_id , ra_call_offset )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: ra_call_offset
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%ra_call_offset = ra_call_offset 
  RETURN
END SUBROUTINE nl_set_ra_call_offset
SUBROUTINE nl_set_cam_abs_freq_s ( id_id , cam_abs_freq_s )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: cam_abs_freq_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%cam_abs_freq_s = cam_abs_freq_s 
  RETURN
END SUBROUTINE nl_set_cam_abs_freq_s
SUBROUTINE nl_set_levsiz ( id_id , levsiz )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: levsiz
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%levsiz = levsiz 
  RETURN
END SUBROUTINE nl_set_levsiz
SUBROUTINE nl_set_paerlev ( id_id , paerlev )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: paerlev
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%paerlev = paerlev 
  RETURN
END SUBROUTINE nl_set_paerlev
SUBROUTINE nl_set_cam_abs_dim1 ( id_id , cam_abs_dim1 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: cam_abs_dim1
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%cam_abs_dim1 = cam_abs_dim1 
  RETURN
END SUBROUTINE nl_set_cam_abs_dim1
SUBROUTINE nl_set_cam_abs_dim2 ( id_id , cam_abs_dim2 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: cam_abs_dim2
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%cam_abs_dim2 = cam_abs_dim2 
  RETURN
END SUBROUTINE nl_set_cam_abs_dim2
SUBROUTINE nl_set_cu_rad_feedback ( id_id , cu_rad_feedback )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: cu_rad_feedback
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%cu_rad_feedback(id_id) = cu_rad_feedback
  RETURN
END SUBROUTINE nl_set_cu_rad_feedback
SUBROUTINE nl_set_dyn_opt ( id_id , dyn_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: dyn_opt
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%dyn_opt = dyn_opt 
  RETURN
END SUBROUTINE nl_set_dyn_opt
SUBROUTINE nl_set_rk_ord ( id_id , rk_ord )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: rk_ord
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%rk_ord = rk_ord 
  RETURN
END SUBROUTINE nl_set_rk_ord
SUBROUTINE nl_set_w_damping ( id_id , w_damping )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: w_damping
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%w_damping = w_damping 
  RETURN
END SUBROUTINE nl_set_w_damping
SUBROUTINE nl_set_diff_opt ( id_id , diff_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: diff_opt
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%diff_opt = diff_opt 
  RETURN
END SUBROUTINE nl_set_diff_opt
SUBROUTINE nl_set_km_opt ( id_id , km_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: km_opt
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%km_opt = km_opt 
  RETURN
END SUBROUTINE nl_set_km_opt
SUBROUTINE nl_set_damp_opt ( id_id , damp_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: damp_opt
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%damp_opt = damp_opt 
  RETURN
END SUBROUTINE nl_set_damp_opt
SUBROUTINE nl_set_zdamp ( id_id , zdamp )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: zdamp
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%zdamp(id_id) = zdamp
  RETURN
END SUBROUTINE nl_set_zdamp
SUBROUTINE nl_set_base_pres ( id_id , base_pres )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: base_pres
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%base_pres = base_pres 
  RETURN
END SUBROUTINE nl_set_base_pres
SUBROUTINE nl_set_base_temp ( id_id , base_temp )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: base_temp
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%base_temp = base_temp 
  RETURN
END SUBROUTINE nl_set_base_temp
SUBROUTINE nl_set_base_lapse ( id_id , base_lapse )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: base_lapse
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%base_lapse = base_lapse 
  RETURN
END SUBROUTINE nl_set_base_lapse
SUBROUTINE nl_set_iso_temp ( id_id , iso_temp )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: iso_temp
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%iso_temp = iso_temp 
  RETURN
END SUBROUTINE nl_set_iso_temp
SUBROUTINE nl_set_dampcoef ( id_id , dampcoef )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: dampcoef
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%dampcoef(id_id) = dampcoef
  RETURN
END SUBROUTINE nl_set_dampcoef
SUBROUTINE nl_set_khdif ( id_id , khdif )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: khdif
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%khdif(id_id) = khdif
  RETURN
END SUBROUTINE nl_set_khdif
SUBROUTINE nl_set_kvdif ( id_id , kvdif )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: kvdif
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%kvdif(id_id) = kvdif
  RETURN
END SUBROUTINE nl_set_kvdif
SUBROUTINE nl_set_c_s ( id_id , c_s )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: c_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%c_s(id_id) = c_s
  RETURN
END SUBROUTINE nl_set_c_s
SUBROUTINE nl_set_c_k ( id_id , c_k )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: c_k
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%c_k(id_id) = c_k
  RETURN
END SUBROUTINE nl_set_c_k
SUBROUTINE nl_set_smdiv ( id_id , smdiv )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: smdiv
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%smdiv(id_id) = smdiv
  RETURN
END SUBROUTINE nl_set_smdiv
SUBROUTINE nl_set_emdiv ( id_id , emdiv )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: emdiv
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%emdiv(id_id) = emdiv
  RETURN
END SUBROUTINE nl_set_emdiv
SUBROUTINE nl_set_epssm ( id_id , epssm )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: epssm
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%epssm(id_id) = epssm
  RETURN
END SUBROUTINE nl_set_epssm
SUBROUTINE nl_set_non_hydrostatic ( id_id , non_hydrostatic )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: non_hydrostatic
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%non_hydrostatic(id_id) = non_hydrostatic
  RETURN
END SUBROUTINE nl_set_non_hydrostatic
SUBROUTINE nl_set_time_step_sound ( id_id , time_step_sound )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: time_step_sound
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%time_step_sound(id_id) = time_step_sound
  RETURN
END SUBROUTINE nl_set_time_step_sound
SUBROUTINE nl_set_h_mom_adv_order ( id_id , h_mom_adv_order )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: h_mom_adv_order
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%h_mom_adv_order(id_id) = h_mom_adv_order
  RETURN
END SUBROUTINE nl_set_h_mom_adv_order
SUBROUTINE nl_set_v_mom_adv_order ( id_id , v_mom_adv_order )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: v_mom_adv_order
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%v_mom_adv_order(id_id) = v_mom_adv_order
  RETURN
END SUBROUTINE nl_set_v_mom_adv_order
SUBROUTINE nl_set_h_sca_adv_order ( id_id , h_sca_adv_order )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: h_sca_adv_order
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%h_sca_adv_order(id_id) = h_sca_adv_order
  RETURN
END SUBROUTINE nl_set_h_sca_adv_order
SUBROUTINE nl_set_v_sca_adv_order ( id_id , v_sca_adv_order )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: v_sca_adv_order
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%v_sca_adv_order(id_id) = v_sca_adv_order
  RETURN
END SUBROUTINE nl_set_v_sca_adv_order
SUBROUTINE nl_set_top_radiation ( id_id , top_radiation )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: top_radiation
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%top_radiation(id_id) = top_radiation
  RETURN
END SUBROUTINE nl_set_top_radiation
SUBROUTINE nl_set_tke_upper_bound ( id_id , tke_upper_bound )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: tke_upper_bound
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%tke_upper_bound(id_id) = tke_upper_bound
  RETURN
END SUBROUTINE nl_set_tke_upper_bound
SUBROUTINE nl_set_tke_drag_coefficient ( id_id , tke_drag_coefficient )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: tke_drag_coefficient
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%tke_drag_coefficient(id_id) = tke_drag_coefficient
  RETURN
END SUBROUTINE nl_set_tke_drag_coefficient
SUBROUTINE nl_set_tke_heat_flux ( id_id , tke_heat_flux )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: tke_heat_flux
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%tke_heat_flux(id_id) = tke_heat_flux
  RETURN
END SUBROUTINE nl_set_tke_heat_flux
SUBROUTINE nl_set_pert_coriolis ( id_id , pert_coriolis )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: pert_coriolis
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%pert_coriolis(id_id) = pert_coriolis
  RETURN
END SUBROUTINE nl_set_pert_coriolis
SUBROUTINE nl_set_euler_adv ( id_id , euler_adv )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: euler_adv
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%euler_adv = euler_adv 
  RETURN
END SUBROUTINE nl_set_euler_adv
SUBROUTINE nl_set_idtadt ( id_id , idtadt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: idtadt
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%idtadt = idtadt 
  RETURN
END SUBROUTINE nl_set_idtadt
SUBROUTINE nl_set_idtadc ( id_id , idtadc )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: idtadc
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%idtadc = idtadc 
  RETURN
END SUBROUTINE nl_set_idtadc
SUBROUTINE nl_set_boundary_flux ( id_id , boundary_flux )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: boundary_flux
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%boundary_flux = boundary_flux 
  RETURN
END SUBROUTINE nl_set_boundary_flux
SUBROUTINE nl_set_spec_bdy_width ( id_id , spec_bdy_width )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: spec_bdy_width
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%spec_bdy_width = spec_bdy_width 
  RETURN
END SUBROUTINE nl_set_spec_bdy_width
SUBROUTINE nl_set_spec_zone ( id_id , spec_zone )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: spec_zone
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%spec_zone = spec_zone 
  RETURN
END SUBROUTINE nl_set_spec_zone
SUBROUTINE nl_set_relax_zone ( id_id , relax_zone )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: relax_zone
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%relax_zone = relax_zone 
  RETURN
END SUBROUTINE nl_set_relax_zone
SUBROUTINE nl_set_specified ( id_id , specified )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: specified
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%specified(id_id) = specified
  RETURN
END SUBROUTINE nl_set_specified
SUBROUTINE nl_set_periodic_x ( id_id , periodic_x )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: periodic_x
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%periodic_x(id_id) = periodic_x
  RETURN
END SUBROUTINE nl_set_periodic_x
SUBROUTINE nl_set_symmetric_xs ( id_id , symmetric_xs )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: symmetric_xs
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%symmetric_xs(id_id) = symmetric_xs
  RETURN
END SUBROUTINE nl_set_symmetric_xs
SUBROUTINE nl_set_symmetric_xe ( id_id , symmetric_xe )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: symmetric_xe
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%symmetric_xe(id_id) = symmetric_xe
  RETURN
END SUBROUTINE nl_set_symmetric_xe
SUBROUTINE nl_set_open_xs ( id_id , open_xs )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: open_xs
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%open_xs(id_id) = open_xs
  RETURN
END SUBROUTINE nl_set_open_xs
SUBROUTINE nl_set_open_xe ( id_id , open_xe )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: open_xe
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%open_xe(id_id) = open_xe
  RETURN
END SUBROUTINE nl_set_open_xe
SUBROUTINE nl_set_periodic_y ( id_id , periodic_y )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: periodic_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%periodic_y(id_id) = periodic_y
  RETURN
END SUBROUTINE nl_set_periodic_y
SUBROUTINE nl_set_symmetric_ys ( id_id , symmetric_ys )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: symmetric_ys
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%symmetric_ys(id_id) = symmetric_ys
  RETURN
END SUBROUTINE nl_set_symmetric_ys
SUBROUTINE nl_set_symmetric_ye ( id_id , symmetric_ye )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: symmetric_ye
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%symmetric_ye(id_id) = symmetric_ye
  RETURN
END SUBROUTINE nl_set_symmetric_ye
SUBROUTINE nl_set_open_ys ( id_id , open_ys )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: open_ys
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%open_ys(id_id) = open_ys
  RETURN
END SUBROUTINE nl_set_open_ys
SUBROUTINE nl_set_open_ye ( id_id , open_ye )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: open_ye
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%open_ye(id_id) = open_ye
  RETURN
END SUBROUTINE nl_set_open_ye
SUBROUTINE nl_set_polar ( id_id , polar )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: polar
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%polar(id_id) = polar
  RETURN
END SUBROUTINE nl_set_polar
SUBROUTINE nl_set_nested ( id_id , nested )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: nested
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%nested(id_id) = nested
  RETURN
END SUBROUTINE nl_set_nested
SUBROUTINE nl_set_real_data_init_type ( id_id , real_data_init_type )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: real_data_init_type
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%real_data_init_type = real_data_init_type 
  RETURN
END SUBROUTINE nl_set_real_data_init_type
SUBROUTINE nl_set_background_proc_id ( id_id , background_proc_id )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: background_proc_id
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%background_proc_id = background_proc_id 
  RETURN
END SUBROUTINE nl_set_background_proc_id
SUBROUTINE nl_set_forecast_proc_id ( id_id , forecast_proc_id )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: forecast_proc_id
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%forecast_proc_id = forecast_proc_id 
  RETURN
END SUBROUTINE nl_set_forecast_proc_id
SUBROUTINE nl_set_production_status ( id_id , production_status )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: production_status
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%production_status = production_status 
  RETURN
END SUBROUTINE nl_set_production_status
SUBROUTINE nl_set_compression ( id_id , compression )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: compression
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%compression = compression 
  RETURN
END SUBROUTINE nl_set_compression
SUBROUTINE nl_set_cen_lat ( id_id , cen_lat )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: cen_lat
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%cen_lat(id_id) = cen_lat
  RETURN
END SUBROUTINE nl_set_cen_lat
SUBROUTINE nl_set_cen_lon ( id_id , cen_lon )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: cen_lon
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%cen_lon(id_id) = cen_lon
  RETURN
END SUBROUTINE nl_set_cen_lon
SUBROUTINE nl_set_truelat1 ( id_id , truelat1 )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: truelat1
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%truelat1(id_id) = truelat1
  RETURN
END SUBROUTINE nl_set_truelat1
SUBROUTINE nl_set_truelat2 ( id_id , truelat2 )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: truelat2
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%truelat2(id_id) = truelat2
  RETURN
END SUBROUTINE nl_set_truelat2
SUBROUTINE nl_set_moad_cen_lat ( id_id , moad_cen_lat )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: moad_cen_lat
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%moad_cen_lat(id_id) = moad_cen_lat
  RETURN
END SUBROUTINE nl_set_moad_cen_lat
SUBROUTINE nl_set_stand_lon ( id_id , stand_lon )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: stand_lon
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%stand_lon(id_id) = stand_lon
  RETURN
END SUBROUTINE nl_set_stand_lon
SUBROUTINE nl_set_flag_metgrid ( id_id , flag_metgrid )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: flag_metgrid
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%flag_metgrid = flag_metgrid 
  RETURN
END SUBROUTINE nl_set_flag_metgrid
SUBROUTINE nl_set_flag_snow ( id_id , flag_snow )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: flag_snow
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%flag_snow = flag_snow 
  RETURN
END SUBROUTINE nl_set_flag_snow
SUBROUTINE nl_set_flag_psfc ( id_id , flag_psfc )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: flag_psfc
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%flag_psfc = flag_psfc 
  RETURN
END SUBROUTINE nl_set_flag_psfc
SUBROUTINE nl_set_flag_sm000010 ( id_id , flag_sm000010 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: flag_sm000010
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%flag_sm000010 = flag_sm000010 
  RETURN
END SUBROUTINE nl_set_flag_sm000010
SUBROUTINE nl_set_flag_sm010040 ( id_id , flag_sm010040 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: flag_sm010040
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%flag_sm010040 = flag_sm010040 
  RETURN
END SUBROUTINE nl_set_flag_sm010040
SUBROUTINE nl_set_flag_sm040100 ( id_id , flag_sm040100 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: flag_sm040100
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%flag_sm040100 = flag_sm040100 
  RETURN
END SUBROUTINE nl_set_flag_sm040100
SUBROUTINE nl_set_flag_sm100200 ( id_id , flag_sm100200 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: flag_sm100200
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%flag_sm100200 = flag_sm100200 
  RETURN
END SUBROUTINE nl_set_flag_sm100200
SUBROUTINE nl_set_flag_st000010 ( id_id , flag_st000010 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: flag_st000010
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%flag_st000010 = flag_st000010 
  RETURN
END SUBROUTINE nl_set_flag_st000010
SUBROUTINE nl_set_flag_st010040 ( id_id , flag_st010040 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: flag_st010040
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%flag_st010040 = flag_st010040 
  RETURN
END SUBROUTINE nl_set_flag_st010040
SUBROUTINE nl_set_flag_st040100 ( id_id , flag_st040100 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: flag_st040100
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%flag_st040100 = flag_st040100 
  RETURN
END SUBROUTINE nl_set_flag_st040100
SUBROUTINE nl_set_flag_st100200 ( id_id , flag_st100200 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: flag_st100200
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%flag_st100200 = flag_st100200 
  RETURN
END SUBROUTINE nl_set_flag_st100200
SUBROUTINE nl_set_flag_slp ( id_id , flag_slp )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: flag_slp
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%flag_slp = flag_slp 
  RETURN
END SUBROUTINE nl_set_flag_slp
SUBROUTINE nl_set_flag_soilhgt ( id_id , flag_soilhgt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: flag_soilhgt
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%flag_soilhgt = flag_soilhgt 
  RETURN
END SUBROUTINE nl_set_flag_soilhgt
SUBROUTINE nl_set_flag_mf_xy ( id_id , flag_mf_xy )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: flag_mf_xy
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%flag_mf_xy = flag_mf_xy 
  RETURN
END SUBROUTINE nl_set_flag_mf_xy
SUBROUTINE nl_set_bdyfrq ( id_id , bdyfrq )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: bdyfrq
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%bdyfrq(id_id) = bdyfrq
  RETURN
END SUBROUTINE nl_set_bdyfrq
SUBROUTINE nl_set_mminlu ( id_id , mminlu )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: mminlu
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%mminlu(id_id) = mminlu
  RETURN
END SUBROUTINE nl_set_mminlu
SUBROUTINE nl_set_iswater ( id_id , iswater )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: iswater
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%iswater(id_id) = iswater
  RETURN
END SUBROUTINE nl_set_iswater
SUBROUTINE nl_set_islake ( id_id , islake )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: islake
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%islake(id_id) = islake
  RETURN
END SUBROUTINE nl_set_islake
SUBROUTINE nl_set_isice ( id_id , isice )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: isice
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%isice(id_id) = isice
  RETURN
END SUBROUTINE nl_set_isice
SUBROUTINE nl_set_isurban ( id_id , isurban )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: isurban
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%isurban(id_id) = isurban
  RETURN
END SUBROUTINE nl_set_isurban
SUBROUTINE nl_set_isoilwater ( id_id , isoilwater )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: isoilwater
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%isoilwater(id_id) = isoilwater
  RETURN
END SUBROUTINE nl_set_isoilwater
SUBROUTINE nl_set_map_proj ( id_id , map_proj )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: map_proj
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%map_proj(id_id) = map_proj
  RETURN
END SUBROUTINE nl_set_map_proj
SUBROUTINE nl_set_dfi_stage ( id_id , dfi_stage )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: dfi_stage
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%dfi_stage = dfi_stage 
  RETURN
END SUBROUTINE nl_set_dfi_stage
SUBROUTINE nl_set_mp_physics_dfi ( id_id , mp_physics_dfi )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: mp_physics_dfi
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%mp_physics_dfi(id_id) = mp_physics_dfi
  RETURN
END SUBROUTINE nl_set_mp_physics_dfi




