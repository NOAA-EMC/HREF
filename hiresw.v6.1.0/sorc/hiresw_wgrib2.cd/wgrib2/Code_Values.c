#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"

/*
 * this file contains nice to know values 
 */

/*
 * HEADER:-1:pds_fcst_time:inv:0:fcst_time(1) in units given by pds
 */
int f_pds_fcst_time(ARG0) {
    unsigned int p;
    if (mode >= 0) {
        p = forecast_time_in_units(sec);
	if (p != 0xffffffff) sprintf(inv_out,"pds_fcst_time1=%u", p);
    }
    return 0;
}


int number_of_forecasts_in_the_ensemble(unsigned char **sec) {
    unsigned char *p;
    p = number_of_forecasts_in_the_ensemble_location(sec);
    if (p) return (int) *p;
    return -1;
}

unsigned char *number_of_forecasts_in_the_ensemble_location(unsigned char **sec) {
    int pdt;
    unsigned char *p;

    pdt = code_table_4_0(sec);
    switch(pdt) {
	case 1:
	case 11:
        case 60:
        case 61:
		p = sec[4]+36; break;
	case 2:
	case 3:
	case 4:
	case 12:
	case 13:
		p = sec[4]+35; break;
	case 41:
	case 43:
		p = sec[4]+38; break;
	default: p=NULL; break;
    }
    return p;
}

int perturbation_number(unsigned char **sec) {
    unsigned char *p;
    p = perturbation_number_location(sec);
    if (p) return (int) *p;
    return -1;
}

unsigned char *perturbation_number_location(unsigned char **sec) {
    int pdt;
    unsigned char *p;

    pdt = code_table_4_0(sec);
    switch(pdt) {
	case 1:
	case 11:
	case 60:
	case 61:
		p = sec[4]+35; break;
	case 41:
	case 43:
		p = sec[4]+37; break;
	default: p = NULL; break;
    }
    return p;
}

unsigned int forecast_time_in_units(unsigned char **sec) {

    unsigned char *code_4_4;
    int pdt;

    code_4_4 = code_table_4_4_location(sec);
    if (code_4_4) {
	// silly WMO codes group
        pdt = code_table_4_0(sec);
	if (pdt != 44) return uint4(code_4_4 + 1);
	else return uint2(code_4_4 + 1);
    }
    return 0xffffffff;
}


void fixed_surfaces(unsigned char **sec, int *type1, float *surface1, 
	int *undef_val1, int *type2, float *surface2, int *undef_val2) {

    unsigned char *p1, *p2;
    *undef_val1 = *undef_val2 = 1;
    *surface1 = *surface2 = UNDEFINED;
    *type1 = *type2 = 255;

    p1 = code_table_4_5a_location(sec);
    p2 = code_table_4_5b_location(sec);

    if (p1 != NULL && *p1 != 255) {
	*type1 = *p1;
        if (p1[1] != 255) {
	    if (p1[2] != 255 || p1[3] != 255 || p1[4] != 255 || p1[5] != 255) {
		*undef_val1 = 0;
                *surface1 = scaled2flt(INT1(p1[1]), int4(p1+2));
	    }
	}
    }
    if (p2 != NULL && *p2 != 255) {
	*type2 = *p2;
        if (p2[1] != 255) {
	    if (p2[2] != 255 || p2[3] != 255 || p2[4] != 255 || p2[5] != 255) {
		*undef_val2 = 0;
                *surface2 = scaled2flt(INT1(p2[1]), int4(p2+2));
	    }
	}
    }
    return ;
}

int background_generating_process_identifier(unsigned char **sec) {
    unsigned char *p;
    p = background_generating_process_identifier_location(sec);
    if (p) return (int) *p;
    return -1;
}
unsigned char *background_generating_process_identifier_location(unsigned char **sec) {
    int p;
    p = GB2_ProdDefTemplateNo(sec);
    if (p <= 15 || p == 60 || p == 61 || p == 1000 || p == 1001 || p == 1002 || p == 1100 || p == 1101)
        return sec[4]+12;
    if ( (p >= 40 && p <= 43) )
        return sec[4]+14;
    if ( p == 44 ) return sec[4]+25;
    if ( p == 48 ) return sec[4]+36;
    if ( p == 52 ) return sec[4]+15;
    return NULL;
}


int analysis_or_forecast_generating_process_identifier(unsigned char **sec) {
    unsigned char *p;
    p = analysis_or_forecast_generating_process_identifier_location(sec);
    if (p) return (int) *p;
    return -1;
}
unsigned char *analysis_or_forecast_generating_process_identifier_location(unsigned char **sec) {
    int p;
    p = GB2_ProdDefTemplateNo(sec);
    if (p <= 15 || p == 60 || p == 61 || p == 1000 || p == 1001 || p == 1002 || p == 1100 || p == 1101)
        return sec[4]+13;
    if ( (p >= 40 && p <= 43) )
        return sec[4]+15;
    if ( p == 48 ) return sec[4]+37;
    if ( p == 52 ) return sec[4]+16;
    return NULL;
}

int hours_of_observational_data_cutoff_after_reference_time(unsigned char **sec) {
    unsigned char *p;
    p = hours_of_observational_data_cutoff_after_reference_time_location(sec);
    if (p) return int2(p);
    return -1;
}

unsigned char *hours_of_observational_data_cutoff_after_reference_time_location(unsigned char **sec) {
    int p;
    p = GB2_ProdDefTemplateNo(sec);
    if (p <= 15 || p == 60 || p == 61 || p == 1000 || p == 1001 || p == 1002 || p == 1100 || p == 1101)
        return sec[4]+14;
    if ( p == 44 ) return sec[4]+27;
    if ( p == 48 ) return sec[4]+38;
    return NULL;
}

int minutes_of_observational_data_cutoff_after_reference_time(unsigned char **sec) {
    unsigned char *p;
    p = minutes_of_observational_data_cutoff_after_reference_time_location(sec);
    if (p) return int1(p);
    return -1;
}

unsigned char *minutes_of_observational_data_cutoff_after_reference_time_location(unsigned char **sec) {
    int p;
    p = GB2_ProdDefTemplateNo(sec);
    if (p <= 15 || p == 60 || p == 61 || p == 1000 || p == 1001 || p == 1002 || p == 1100 || p == 1101)
        return sec[4]+16;
    if ( p == 44 ) return sec[4]+29;
    if ( p == 48 ) return sec[4]+40;
    return NULL;
}


int observation_generating_process_identifier(unsigned char **sec) {
    unsigned char *p;
    p = observation_generating_process_identifier_location(sec);
    if (p) return (int) *p;
    return -1;
}

unsigned char *observation_generating_process_identifier_location(unsigned char **sec) {
    int p;
    p = GB2_ProdDefTemplateNo(sec);
    if (p == 30 || p == 31) return sec[4]+12;
    return NULL;
}


/*
 * get substitute umissing value
 *
 * returns number of missing values
 */

int sub_missing_values(unsigned char **sec, float *missing1, float *missing2) {
    int i, j;
    unsigned char *p;

    i = code_table_5_5(sec);
    if (i < 1 || i > 2) return 0;
    j = code_table_5_1(sec);
    p = sec[5];
    if (j == 0) {		// ieee
	if (p[23] == 255 && p[24] == 255 && p[25] == 255 && p[26] == 255) *missing1 = UNDEFINED;
	else *missing1 = ieee2flt(p+23);
	if (i == 2) {
	    if (p[27] == 255 && p[28] == 255 && p[29] == 255 && p[30] == 255) *missing1 = UNDEFINED;
	    else *missing2 = ieee2flt(p+27);
	}
    }
    else if (j == 1) {		// integer
	if (p[23] == 255 && p[24] == 255 && p[25] == 255 && p[26] == 255) *missing1 = UNDEFINED;
	else *missing1 = (float) int4(p+23);
	if (i == 2) {
	    if (p[27] == 255 && p[28] == 255 && p[29] == 255 && p[30] == 255) *missing1 = UNDEFINED;
	    else *missing2 = (float) int4(p+27);
	}
    }
    return i;
}

/*
 * returns location of statistical time processing section
 *  ie location of overall time
 *
 */

unsigned char *stat_proc_verf_time_location(unsigned char **sec) {
    int i, j;
    i = code_table_4_0(sec);
    j = 0;
    if (i == 8) j = 34;
    else if (i == 9) j = 47;
    else if (i == 10) j = 35;
    else if (i == 11) j = 37;
    else if (i == 12) j = 36;
    else if (i == 13) j = 68;
    else if (i == 14) j = 64;
    else if (i == 42) j = 36;
    else if (i == 43) j = 39;
    else if (i == 46) j = 47;
    else if (i == 47) j = 50;
    else if (i == 61) j = 44;

    if (j == 0) return NULL;
    return sec[4]+j;
}

/*
 * index of n ― number of time range specifications
 *
 * if none, then return -1
 */

int stat_proc_n_time_ranges_index(unsigned char **sec) {
   unsigned char *ptr;

   ptr = stat_proc_verf_time_location(sec);
   if (ptr == NULL) return -1;
   return ptr - sec[4] + 7;
}


int stat_proc_verf_time(unsigned char **sec, int *year, int *month, int *day, int *hour, int *minute, int *second)
{
    unsigned char *stat_proc_time;

    stat_proc_time = stat_proc_verf_time_location(sec);

    if (stat_proc_time) {
	get_time(stat_proc_time, year, month, day, hour, minute, second);
	return 0;
    }
    else {
	*year = *month = *day = *hour = *minute = *second = 0;
    }
    return 1;
}

/*
 * returns the location of the year of the model version date
 */
unsigned char *year_of_model_version_date_location(unsigned char **sec) {
    int pdt;
    unsigned char *p;

    pdt = code_table_4_0(sec);

    switch (pdt) {
      case 60: 
      case 61: p = sec[4] + 37; break;
      default: p = NULL; break;
    }
    return p;
}

/*
 * HEADER:-1:percent:inv:0:percentage probability
 */
int f_percent(ARG0) {
    int percent;
    if (mode >= 0) {
        percent = percentile_value(sec);
        if (percent >= 0) sprintf(inv_out,"%d%%",percent);
    }
    return 0;
}


/*
 * returns the percentile value
 */
int percentile_value(unsigned char **sec) {
	unsigned char *p;
	p = percentile_value_location(sec);
	if (p == NULL) return -1;
	return (int) *p;
}
/*
 * returns locatin of the percentile value
 */
unsigned char *percentile_value_location(unsigned char **sec) {

    int pdt;
    unsigned char *p;
    pdt = code_table_4_0(sec);
    switch (pdt) {
	case 6:
	case 10: p = sec[4] + 34; break;
	default: p = NULL; break;
    }
    return p;
}

/*
 *  * returns reference value, binary and decimal scaling and number of bits
 */

int scaling(unsigned char **sec, double *ref_value, int *decimal_scaling, int *binary_scaling, int *nbits) {
    int pack;
    unsigned char *p;

    pack = (int) code_table_5_0(sec);
    p = sec[5];
    if (pack == 0 || pack == 1 || pack == 2 || pack == 3 || pack == 40 || pack == 41 ||
                pack == 50 || pack == 51 || pack == 61 || pack == 40000 || pack == 40010) {
       *ref_value = ieee2flt(p+11);
       *binary_scaling = int2(p+15);
       *decimal_scaling = -int2(p+17);
       *nbits = p[19];
    }
    else {
      return 1;
    }
    return 0;
}

