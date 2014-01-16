      function num_parthds()
      use omp_lib
      num_parthds = 1
!d !$OMP PARALLEL
!d       num_parthds=omp_get_num_threads()
!d !$OMP END PARALLEL
      return
      end 
