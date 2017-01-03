





MODULE  module_sf_pxlsm_data































      REAL, DIMENSION(20), TARGET  :: RSMIN_MODIS, Z00_MODIS,     &
                                      VEG0_MODIS, VEGMN0_MODIS,   &
                                      LAI0_MODIS, LAIMN0_MODIS,   &
                                      SNUP0_MODIS, ALBF_MODIS

      DATA RSMIN_MODIS                                      &
             / 175.0,   120.0,   175.0,   200.0,    200.0,   &
               200.0,   200.0,   150.0,   120.0,    100.0,   &
               160.0,    70.0,   150.0,   100.0,   9999.0,   &
               100.0,  9999.0,   175.0,   120.0,    100.0  /

      DATA Z00_MODIS                                        &
             / 100.0,    90.0,   100.0,   100.0,   100.0,    &
                15.0,    15.0,    25.0,    15.0,     7.0,    &
                20.0,    10.0,    80.0,    30.0,     1.2,    &
                 5.0,     0.1,    30.0,    15.0,    10.0  /
                    
      DATA VEG0_MODIS                                       &
             /  90.0,    95.0,    95.0,    95.0,    95.0,    &
                90.0,    75.0,    80.0,    70.0,    85.0,    &
                75.0,    95.0,    40.0,    95.0,     5.0,    &
                20.0,     0.0,    70.0,    40.0,    20.0  /
                   
      DATA VEGMN0_MODIS                                     &
             /  80.0,    85.0,    50.0,    50.0,    60.0,    &
                50.0,    50.0,    60.0,    50.0,    60.0,    &
                45.0,    10.0,    20.0,    40.0,     2.0,    &
                 5.0,     0.0,    50.0,    20.0,     5.0 /
                     
      DATA LAI0_MODIS                                       &
             /   4.0,     5.0,     5.0,     5.0,     5.0,    &
                 3.0,     2.5,     2.5,     2.0,     2.5,    &
                 3.0,     3.0,     3.0,     3.0,     0.1,    &
                 1.0,     0.0,     3.4,     2.4,     1.4  /
                    
      DATA LAIMN0_MODIS                                     &
             /   3.0,     4.0,     1.0,     1.0,     2.0,    &
                 1.0,     1.0,     1.0,     1.0,     1.0,    &
                 1.0,     0.5,     1.0,     1.0,     0.1,    &
                 0.5,     0.0,     2.0,     1.0,     0.1  /

      DATA SNUP0_MODIS                                      &
             /   0.08,    0.08,    0.08,    0.08,    0.08,   &
                 0.03,    0.035,   0.03,    0.04,    0.04,   &
                0.015,    0.04,    0.04,    0.04,    0.02,   &
                 0.02,    0.01,    0.80,    0.40,    0.015 /

      DATA ALBF_MODIS                                        &
             /  12.0,    12.0,    14.0,    16.0,    13.0,    &
                22.0,    20.0,    22.0,    20.0,    19.0,    &
                14.0,    18.0,    11.0,    18.0,    60.0,    &
                25.0,     8.0,    15.0,    15.0,    25.0 /






























































      REAL, DIMENSION(50), TARGET  :: RSMIN_NLCD50, Z00_NLCD50,      &
                                      VEG0_NLCD50, VEGMN0_NLCD50,    &
                                      LAI0_NLCD50, LAIMN0_NLCD50,    &
                                      SNUP0_NLCD50, ALBF_NLCD50

      DATA RSMIN_NLCD50                                      &
             / 9999.0,  9999.0,   120.0,   120.0,   140.0,   &
                160.0,   100.0,   100.0,   200.0,   175.0,   &
                200.0,   200.0,   200.0,   100.0,   100.0,   &
                100.0,   100.0,   100.0,    80.0,    70.0,   &
                200.0,   200.0,   164.0,   200.0,   164.0,   &
                120.0,   120.0,   120.0,   100.0,   100.0,   &
               9999.0,   175.0,   120.0,   175.0,   200.0,   &
                200.0,   200.0,   200.0,   150.0,   120.0,   &
                100.0,   160.0,    70.0,   150.0,   100.0,   &
               9999.0,   100.0,  9999.0,  9999.0,  9999.0  /

      DATA Z00_NLCD50                                        &
             /   0.10,    1.20,   30.0,    40.0,    60.0,    &
               100.0,     5.0,     5.0,   100.0,   100.0,    &
               100.0,    10.0,    15.0,     7.0,     7.0,    &
                 5.0,     5.0,     5.0,     7.0,    10.0,    &
                55.0,    80.0,    30.0,    60.0,    30.0,    &
                11.0,    11.0,    11.0,     5.0,     5.0,    &
                 0.1,   100.0,    90.0,   100.0,   100.0,    &
               100.0,    15.0,    15.0,    25.0,    15.0,    &
                 7.0,    20.0,    10.0,    80.0,    30.0,    &
                 1.2,     5.0,     0.1,     0.1,     0.1   /
                    
      DATA VEG0_NLCD50                                       &
             /  00.0,     5.0,    90.0,    70.0,    40.0,    &
                15.0,    20.0,    15.0,    95.0,    90.0,    &
                95.0,    50.0,    75.0,    85.0,    80.0,    &
                80.0,    80.0,    50.0,    95.0,    95.0,    &
                90.0,    90.0,    90.0,    90.0,    90.0,    &
                60.0,    80.0,    80.0,    60.0,    60.0,    &
                 0.0,    90.0,    95.0,    95.0,    95.0,    &
                95.0,    90.0,    75.0,    80.0,    70.0,    &
                85.0,    75.0,    95.0,    40.0,    95.0,    &
                 5.0,    20.0,     0.0,     0.0,     0.0   /
                   
      DATA VEGMN0_NLCD50                                     &
             /  00.0,     2.0,    80.0,    60.0,    30.0,    &
                05.0,    05.0,     5.0,    50.0,    80.0,    &
                60.0,    20.0,    50.0,    60.0,    20.0,    &
                20.0,    20.0,    20.0,    80.0,    10.0,    &
                80.0,    80.0,    80.0,    80.0,    80.0,    &
                40.0,    40.0,    40.0,    20.0,    20.0,    &
                 0.0,    80.0,    85.0,    50.0,    50.0,    &
                60.0,    50.0,    50.0,    60.0,    50.0,    &
                60.0,    45.0,    10.0,    20.0,    40.0,    &
                 2.0,     5.0,     0.0,     0.0,     0.0   /
                     
      DATA LAI0_NLCD50                                       &
             /   0.0,     0.1,     3.0,     3.0,     3.0,    &
                 3.0,     1.0,     0.5,     5.0,     4.0,    &
                 5.0,     2.0,     2.5,     2.5,     2.0,    &
                 1.0,     1.0,     1.0,     3.0,     3.0,    &
                 5.0,     5.0,     3.0,     5.0,     3.0,    &
                 2.0,     2.0,     2.0,     1.0,     1.0,    &
                 0.0,     4.0,     5.0,     5.0,     5.0,    &
                 5.0,     3.0,     2.5,     2.5,     2.0,    &
                 2.5,     3.0,     3.0,     3.0,     3.0,    &
                 0.1,     1.0,     0.0,     0.0,     0.0   /
                    
      DATA LAIMN0_NLCD50                                     &
             /   0.0,     0.1,     1.0,     1.0,     1.0,    &
                 1.0,     0.5,     0.2,     1.0,     3.0,    &
                 2.0,     1.0,     1.0,     1.0,     1.0,    &
                 1.0,     1.0,     1.0,     1.0,     0.5,    &
                 2.0,     2.0,     1.0,     2.0,     1.0,    &
                 1.0,     1.0,     1.0,     0.5,     0.5,    &
                 0.0,     3.0,     4.0,     1.0,     1.0,    &
                 2.0,     1.0,     1.0,     1.0,     1.0,    &
                 1.0,     1.0,     0.5,     1.0,     1.0,    &
                 0.1,     0.5,     0.0,     0.0,     0.0   /

      DATA SNUP0_NLCD50                                      &
             /   0.01,    0.02,    0.04,    0.04,    0.04,   &
                 0.04,    0.02,    0.02,    0.08,    0.08,   &
                 0.08,    0.04,    0.04,    0.04,    0.01,   &
                 0.01,    0.01,    0.02,    0.04,    0.04,   &
                 0.08,    0.08,    0.04,    0.08,    0.04,   &
                 0.04,    0.06,    0.06,    0.02,    0.02,   &
                 0.08,    0.08,    0.08,    0.08,    0.08,   &
                 0.08,    0.03,    0.035,   0.03,    0.04,   &
                 0.04,   0.015,    0.04,    0.04,    0.04,   &
                 0.02,    0.02,    0.01,    0.01,    0.01  /

      DATA ALBF_NLCD50                                       &
             /   8.0,    60.0,    12.0,    11.0,    10.0,    &
                10.0,    20.0,    35.0,    15.0,    10.0,    &
                13.0,    20.0,    20.0,    19.0,    23.0,    &
                20.0,    20.0,    15.0,    18.0,    18.0,    &
                15.0,    15.0,    15.0,    15.0,    15.0,    &
                18.0,    18.0,    18.0,    10.0,    10.0,    &
                 8.0,    12.0,    12.0,    14.0,    16.0,    &
                13.0,    22.0,    20.0,    22.0,    20.0,    &
                19.0,    14.0,    18.0,    11.0,    18.0,    &
                60.0,    25.0,     8.0,     8.0,     8.0   /






















































      REAL, DIMENSION(40), TARGET  :: RSMIN_NLCD40, Z00_NLCD40,      &
                                      VEG0_NLCD40, VEGMN0_NLCD40,    &
                                      LAI0_NLCD40, LAIMN0_NLCD40,    &
                                      SNUP0_NLCD40, ALBF_NLCD40

      DATA RSMIN_NLCD40                                      &
             / 175.0,   120.0,   175.0,   200.0,    200.0,   &
               200.0,   200.0,   150.0,   120.0,    100.0,   &
               160.0,    70.0,   150.0,   100.0,   9999.0,   &
               100.0,  9999.0,  9999.0,  9999.0,   9999.0,   &
              9999.0,  9999.0,   120.0,   120.0,    140.0,   &
               160.0,   100.0,   200.0,   175.0,    200.0,   &
               200.0,   200.0,   100.0,   100.0,    100.0,   &
               100.0,    80.0,    70.0,   200.0,    120.0  /

      DATA Z00_NLCD40                                        &
             / 100.0,    90.0,   100.0,   100.0,   100.0,    &
                15.0,    15.0,    25.0,    15.0,     7.0,    &
                20.0,    10.0,    80.0,    30.0,     1.2,    &
                 5.0,     0.1,     0.1,     0.1,     0.1,    &
                0.10,    1.20,    30.0,    40.0,    60.0,    &
               100.0,     5.0,   100.0,   100.0,   100.0,    &
                10.0,    15.0,     7.0,     7.0,     5.0,    &
                 5.0,     7.0,    10.0,    55.0,    11.0   /
                    
      DATA VEG0_NLCD40                                       &
             /  90.0,    95.0,    95.0,    95.0,    95.0,    &
                90.0,    75.0,    80.0,    70.0,    85.0,    &
                75.0,    95.0,    40.0,    95.0,     5.0,    &
                20.0,     0.0,     0.0,     0.0,     0.0,    &
                 0.0,     5.0,    90.0,    70.0,    40.0,    &
                15.0,    20.0,    95.0,    90.0,    95.0,    &
                50.0,    75.0,    85.0,    80.0,    80.0,    &
                80.0,    95.0,    95.0,    90.0,    60.0   /
                   
      DATA VEGMN0_NLCD40                                     &
             /  80.0,    85.0,    50.0,    50.0,    60.0,    &
                50.0,    50.0,    60.0,    50.0,    60.0,    &
                45.0,    10.0,    20.0,    40.0,     2.0,    &
                 5.0,     0.0,     0.0,     0.0,     0.0,    &
                 0.0,     2.0,    80.0,    60.0,    30.0,    &
                05.0,    05.0,    50.0,    80.0,    60.0,    &
                20.0,    50.0,    60.0,    20.0,    20.0,    &
                20.0,    80.0,    10.0,    80.0,    40.0   /
                     
      DATA LAI0_NLCD40                                       &
             /   4.0,     5.0,     5.0,     5.0,     5.0,    &
                 3.0,     2.5,     2.5,     2.0,     2.5,    &
                 3.0,     3.0,     3.0,     3.0,     0.1,    &
                 1.0,     0.0,     0.0,     0.0,     0.0,    &
                 0.0,     0.1,     3.0,     3.0,     3.0,    &
                 3.0,     1.0,     5.0,     4.0,     5.0,    &
                 2.0,     2.5,     2.5,     2.0,     1.0,    &
                 1.0,     3.0,     3.0,     5.0,     2.0   /
                    
      DATA LAIMN0_NLCD40                                     &
             /   3.0,     4.0,     1.0,     1.0,     2.0,    &
                 1.0,     1.0,     1.0,     1.0,     1.0,    &
                 1.0,     0.5,     1.0,     1.0,     0.1,    &
                 0.5,     0.0,     0.0,     0.0,     0.0,    &
                 0.0,     0.1,     1.0,     1.0,     1.0,    &
                 1.0,     0.5,     1.0,     3.0,     2.0,    &
                 1.0,     1.0,     1.0,     1.0,     1.0,    &
                 1.0,     1.0,     0.5,     2.0,     1.0   /

      DATA SNUP0_NLCD40                                      &
             /   0.08,    0.08,    0.08,    0.08,    0.08,   &
                 0.03,    0.035,   0.03,    0.04,    0.04,   &
                0.015,    0.04,    0.04,    0.04,    0.02,   &
                 0.02,    0.01,    0.01,    0.01,    0.01,   &
                 0.01,    0.02,    0.04,    0.04,    0.04,   &
                 0.04,    0.02,    0.08,    0.08,    0.08,   &
                 0.04,    0.04,    0.04,    0.01,    0.01,   &
                 0.01,    0.04,    0.04,    0.08,    0.04  /

      DATA ALBF_NLCD40                                       &
             /  12.0,    12.0,    14.0,    16.0,    13.0,    &
                22.0,    20.0,    22.0,    20.0,    19.0,    &
                14.0,    18.0,    11.0,    18.0,    60.0,    &
                25.0,     8.0,     8.0,     8.0,     8.0,    &
                 8.0,    60.0,    12.0,    11.0,    10.0,    &
                10.0,    20.0,    15.0,    12.0,    13.0,    &
                20.0,    20.0,    19.0,    23.0,    20.0,    &
                20.0,    18.0,    18.0,    15.0,    18.0   /
































      REAL, DIMENSION(24), TARGET  :: RSMIN_USGS, Z00_USGS,   &
                                      VEG0_USGS, VEGMN0_USGS, &
                                      LAI0_USGS, LAIMN0_USGS, &
                                      SNUP0_USGS, ALBF_USGS

      DATA RSMIN_USGS                                        & 
             /  150.0,    70.0,    60.0,    70.0,    80.0,   &
                180.0,   100.0,   200.0,   150.0,   120.0,   &
                200.0,   175.0,   120.0,   175.0,   200.0,   &
               9999.0,   164.0,   200.0,   100.0,   150.0,   &
                200.0,   150.0,   100.0,   300.0           /
      DATA Z00_USGS                                          & 
             /   50.0,    10.0,    10.0,    10.0,    10.0,   &
                 40.0,     7.0,    20.0,    20.0,    20.0,   &
                 50.0,    50.0,    40.0,    50.0,    50.0,   &
                  0.1,    15.0,    45.0,     5.0,    10.0,   &
                 10.0,     5.0,     5.0,     5.0           /
      DATA VEG0_USGS                                         & 
             /   40.0,    95.0,    95.0,    95.0,    95.0,   &
                 95.0,    95.0,    70.0,    85.0,    80.0,   &
                 95.0,    95.0,    95.0,    90.0,    95.0,   &
                  0.00,   60.0,    90.0,    10.0,    20.0,   &
                 30.0,    20.0,     5.0,     5.0           /

      DATA VEGMN0_USGS                                       & 
             /   20.0,    15.0,    10.0,    15.0,    35.0,   &
                 40.0,    70.0,    50.0,    60.0,    60.0,   &
                 50.0,    50.0,    85.0,    80.0,    60.0,   &
                  0.0,    40.0,    80.0,     5.0,    10.0,   &
                 10.0,     5.0,     2.0,     2.0           /

      DATA LAI0_USGS                                         & 
             /    2.0,     3.0,     3.0,     3.0,     2.5,   &
                  4.0,     2.5,     3.0,     3.0,     2.0,   &
                  5.0,     5.0,     5.0,     4.0,     5.0,   &
                  0.0,     2.0,     5.0,     0.50,    1.0,   &
                  1.0,     1.0,     0.1,     0.1           /

      DATA LAIMN0_USGS                                       & 
             /    0.50,    0.50,    0.50,    0.50,    1.0,   &
                  1.5,     1.0,     1.0,     1.0,     1.0,   &
                  1.0,     1.0,     4.0,     3.0,     2.0,   &
                  0.0,     1.0,     3.0,     0.20,    0.50,  &
                  0.50,    0.50,    0.10,    0.10          /

      DATA SNUP0_USGS                                        & 
             /    0.04,    0.04,    0.04,    0.04,    0.04,  &
                  0.04,    0.04,    0.03,    0.035,   0.04,  &
                  0.08,    0.08,    0.08,    0.08,    0.08,  &
                  0.01,    0.01,    0.01,    0.02,    0.02,  &
                  0.025,   0.025,   0.025,   0.02          /

      DATA ALBF_USGS                                         & 
             /    15.0,   17.0,    18.0,    18.0,    18.0,   &
                  16.0,   19.0,    22.0,    20.0,    20.0,   &
                  16.0,   14.0,    12.0,    12.0,    13.0,   &
                   8.0,   14.0,    14.0,    25.0,    15.0,   &
                  15.0,   15.0,    25.0,    55.0           /



END MODULE  module_sf_pxlsm_data
