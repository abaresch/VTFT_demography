/****************************************************************************/
/**                                                                        **/
/**                        o  u  t  p  u  t  .  h                          **/
/**                                                                        **/
/**     pseudo code                                                        **/
/**     header file for output structure to store ageclass state variables **/
/**     note: not all outputs shown in pseudo code                         **/
/**                                                                        **/
/**     Leonardo Calle                                                     **/
/**     Montana State University                                           **/
/**     Bozeman, Montana, USA                                              **/
/**                                                                        **/
/**     Last change: 31 Dec 2019                                           **/
/**                                                                        **/
/****************************************************************************/

#ifndef OUTPUT_H
#define OUTPUT_H

/* Definition of datatypes */

/*-----------------------------*/
/*  O U T P U T   S T R U C T  */
/*-----------------------------*/
typedef struct
{
  /* standard outputs are aggregated at grid-cell level      */
  /* ..area-weighted means or sums, depending on output type */
  Real mnpp;                             // monthly net primary productivity NPP, (g C/m2/month), total for all tiles
  Real mrh;                              // monthly heterotrophic respiration, (g C/m2/month), total for all tiles
  Real mgpp;                             // monthly gross primary productivity GPP, (g C/m2/month)
  Real mra;                              // monthly autotrophic respiration (g C/m2/month)

 //if wanting ageclass outputs, use compiler flag below
 #ifdef OUTPUT_BYAGECLASS
  /*----------------------------------------------------------------------*/
  /* note: number of ageclasses fixed at 12                               */
  /*       ..but correspond to different ages, depending on compiler flag */
  /*       ..see pseduo_stand.h for corresponding ages                    */
  /*----------------------------------------------------------------------*/
  //primary vegitation outputs
  Real ageclass_pri_firec[12];           // veg loss from fire, (g C/m2/yr)
  Real ageclass_pri_mnpp[12];            // monthly NPP, (g C/m2/month)
  Real ageclass_pri_mrh[12];             // monthly hetotrophic respiration, (g C/m2/month)
  Real ageclass_pri_fluxestab[12];       // establishment flux, (g C/m2/yr)
  //written in output.c: 
  //..ageclass_pri_pftfpc[(NPFT+1)*12];  // foliar projective cover FPC per pft by ageclass (fraction [0,1])
  //..ageclass_pri_frac[12] = {0};       // stand area as fraction of grid-cell area by ageclass (fraction [0,1])

  //secondary vegitation outputs  
  Real ageclass_sec_firec[12];           // veg loss from fire, (g C/m2/yr)
  Real ageclass_sec_mnpp[12];            // monthly NPP, (g C/m2/month)
  Real ageclass_sec_mrh[12];             // monthly heterotrophic respiration, (g C/m2/month)
  Real ageclass_sec_fluxestab[12];       // establishment flux, (g C/m2/yr)
  //written in output.c: 
  //..ageclass_sec_pftfpc[(NPFT+1)*12];  // foliar projective cover FPC per pft by ageclass, (fraction [0,1])
  //..ageclass_sec_frac[12] = {0};       // stand area as fraction of grid-cell area by ageclass (fraction [0,1])
 #endif
 
} Output;

/*--------------------------*/
/* Declaration of functions */
/* ..pseudo code not shown  */
/*--------------------------*/
extern FILE **fopenoutput(Config);
extern void fcloseoutput(FILE **,int);
extern void initoutput_annual(Output *,int,int);
extern void initoutput_monthly(Output *, int); 
extern void freeoutput(Output *);

#endif

