#include <stdio.h>


#include "protos.h"
#include "protos_kpp.h"
#include "kpp_data.h"


int gen_kpp_warning( FILE * ofile, char * gen_by_name, char*cchar )
{
    fprintf(ofile, "%s \n", cchar);
    fprintf(ofile, "%s THIS FILE WAS AUTOMATICALLY GENERATED BY \n%s\n",cchar,cchar );
    fprintf(ofile, "%s     %s   \n%s\n", cchar, gen_by_name, cchar);
    fprintf(ofile, "%s MANUAL CHANGES TO THIS FILE WILL BE LOST !!! \n", cchar);    fprintf(ofile, "%s \n", cchar);
    fprintf(ofile, "%s \n", cchar);
}



int 
gen_kpp_pass_down ( FILE * ofile )
{

    fprintf(ofile,"!\n");
    fprintf(ofile,"#include <fixed_args_kpp_interf.inc>\n"); 
    fprintf(ofile,"!\n");

    /* pass down all radicals */ 
    gen_kpp_argl( ofile,  WRFC_radicals );


    /* pass down jvals */ 
    gen_kpp_argl( ofile,  WRFC_jvals );


    /* pass down dimensions */
    gen_kpp_argd ( ofile );

}


int 
gen_kpp_decl ( FILE * ofile )
{
     /* declare dimensions */
     gen_kpp_decld ( ofile );


     fprintf(ofile,"#include <fixed_decl_kpp_interf.inc>\n\n\n"); 



     /* declare radicals */
    fprintf(ofile, "\n\n! \n");
    fprintf(ofile, "! radicals \n");
    fprintf(ofile, "! \n");

    gen_kpp_decl3d( ofile, WRFC_radicals);


     /* declare photolysis rates */
    fprintf(ofile, "\n\n! \n");
    fprintf(ofile, "! photolysis rates \n");
    fprintf(ofile, "! \n");

    gen_kpp_decl3d( ofile, WRFC_jvals);


    fprintf(ofile, " \n\n\n");


}

int gen_kpp_argl( FILE * ofile, knode_t * nl  )
{
 knode_t * pml;
 int countit;
 int max_per_line=5;

             fprintf(ofile,"            ");

	         countit=0;   
              for ( pml = nl -> members;  pml != NULL ; pml = pml->next ) {
                 fprintf(ofile," %s,", pml->name);
		 countit = countit+1;
                  if ( countit % max_per_line ==  0) {
		   fprintf(ofile," & \n            ");
		   } 
		 }   


		 fprintf(ofile,"  & \n"); 


}



int gen_kpp_argl_new( FILE * ofile, knode_t * nl  )
{
 knode_t * pml;
 int countit;
 int max_per_line=4;

             fprintf(ofile,"            ");

	         countit=0;   
              for ( pml = nl -> members;  pml != NULL ; pml = pml->next ) {
                 fprintf(ofile," grid%%%s,", pml->name);
		 countit = countit+1;
                  if ( countit % max_per_line ==  0) {
		   fprintf(ofile," & \n            ");
		   } 
		 }   


		 fprintf(ofile,"  & \n"); 


}





int  gen_kpp_argd ( FILE * ofile )
{
    fprintf(ofile, "              ids,ide, jds,jde, kds,kde,         &\n");
    fprintf(ofile, "              ims,ime, jms,jme, kms,kme,         &\n");
    fprintf(ofile, "              its,ite, jts,jte, kts,kte         )\n\n\n");
}


int gen_kpp_decld ( FILE * ofile )
{
     fprintf(ofile, "\n\n\n    INTEGER,      INTENT(IN   ) ::    &\n");
     fprintf(ofile, "                      ids,ide, jds,jde, kds,kde,      & \n");
     fprintf(ofile, "                      ims,ime, jms,jme, kms,kme,      & \n");
     fprintf(ofile, "                      its,ite, jts,jte, kts,kte \n\n\n\n");
}

int gen_kpp_decl3d( FILE * ofile, knode_t * nl  )
{
 knode_t * pml;
 int countit;
 int max_per_line=5;

    fprintf(ofile, "      REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),   & \n");
    fprintf(ofile, "         INTENT(INOUT ) ::  & \n              ");    

          
	         countit=0;   
              for ( pml = nl -> members;  pml != NULL ; pml = pml->next ) {

                 

		 if ( pml->next != NULL ){
                    fprintf(ofile," %s,", pml->name);
                  }
                  else{
                    fprintf(ofile," %s", pml->name);
		  } 
  
		    countit = countit+1;
                  if ( countit % max_per_line ==  0) {
                   if ( pml->next != NULL ){
		   fprintf(ofile," & \n              ");
                   }
		   } 
		 }   
}











