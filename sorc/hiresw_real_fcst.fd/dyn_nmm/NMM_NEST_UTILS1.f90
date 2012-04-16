SUBROUTINE stub_nmm_nest_stub
END SUBROUTINE stub_nmm_nest_stub

RECURSIVE SUBROUTINE find_ijstart_level ( grid, i_start, j_start, level )



   USE module_domain

   IMPLICIT NONE

   

   TYPE(domain) :: grid
   INTEGER, INTENT (OUT) :: i_start, j_start, level
   INTEGER :: iadd

      if (grid%parent_id == 0 ) then
         i_start = 1
         j_start = 1
         level = 0
      else
         call find_ijstart_level ( grid%parents(1)%ptr, i_start, j_start, level )
         if (level > 0) then
             iadd = (i_start-1)*3
             if ( mod(j_start,2).ne.0 .and. mod(grid%j_parent_start,2).ne.0 ) iadd = iadd - 1
             if ( mod(j_start,2).eq.0 .and. mod(grid%j_parent_start,2).eq.0 ) iadd = iadd + 2
         else
             iadd = -mod(grid%j_parent_start,2)
         end if
         i_start = iadd + grid%i_parent_start*3 - 1
         j_start = ( (j_start-1) + (grid%j_parent_start-1) ) * 3 + 1
         level = level + 1
      end if

END SUBROUTINE find_ijstart_level
