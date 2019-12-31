/***************************************************************************/
/**                                                                       **/
/**                    s  t  a  n  d  .  h                                **/
/**                                                                       **/
/**     pseudo code                                                       **/
/**     header file for stand structure and types                         **/
/**     note: stands are stored in a list via pointer to stand structure  **/
/**                                                                       **/
/**     Leonardo Calle                                                    **/
/**     Montana State University                                          **/
/**     Bozeman, Montana, USA                                             **/
/**                                                                       **/
/**     Last change: 31 Dec 2019                                          **/
/**                                                                       **/
/***************************************************************************/

#ifndef STAND_H  /* Already included? */
#define STAND_H

/*------------------------------------------*/
/* stands represented in grid-cell          */
/* ..integer value useful for sorting       */
/* ..negative values purposeful for sorting */
/*------------------------------------------*/
#define GRASSLAND 0
#define GRASSLAND_TEMP 1
#define SETASIDE 2
#define AGRICULTURE 3
#define MANAGEDFOREST 4
#define KILL 5
#define PRIMARY 6          /* primary forest stand   */
#define SECFOREST 7        /* secondary forest stand */
#define SECFOREST_TEMP -1  /* temporary stand        */
#define PRIMARY_TEMP -2    /* temporary stand        */
#define STAND_DUMMY -3     /* temporary stand        */

/*----------------------------------------*/
/* tag for type of sorting the stand list */
/*----------------------------------------*/
#define AGESTAND 0
#define DBHSTAND 1
#define MGLAND 0
#define FOREST 1
#define PASTURE 1

/*-----------------------------------------------------*/
/* LPJ compiler flags to use ageclasses in the model   */
/* ..AGECLASS_PRIMARY, AGECLASS_SECFOREST              */
/*     agelcasses on either stand type                 */
/* ..EQUAL_AGEWIDTH, UNEQUAL_AGEWIDTH                  */
/*     defines the agewidths of each of the 12 stands  */
/*     AGECLASS_FYR is the first year in ageclass      */
/*     AGECLASS_WIDTH is the width of ageclass in yrs  */
/*     ..if no agewidth compiler flag is defined,      */
/*     ..then the default is the EQUAL_AGEWIDTH        */
/*-----------------------------------------------------*/
#if defined(AGECLASS_PRIMARY) || defined(AGECLASS_SECFOREST)
  #ifdef UNEQUAL_AGEWIDTH //not default
	//maximum within-stand transitions)
	#define MAX_WITHINSTAND_TRANS 25

	#define AGECLASS_FYR_1 1
	#define AGECLASS_FYR_2 3
	#define AGECLASS_FYR_3 5
	#define AGECLASS_FYR_4 7
	#define AGECLASS_FYR_5 9
	#define AGECLASS_FYR_6 11
	#define AGECLASS_FYR_7 16
	#define AGECLASS_FYR_8 21
	#define AGECLASS_FYR_9 26
	#define AGECLASS_FYR_10 51
	#define AGECLASS_FYR_11 76
	#define AGECLASS_FYR_12 101

    #define AGECLASS_WIDTH_1 1
    #define AGECLASS_WIDTH_2 2
    #define AGECLASS_WIDTH_3 2
    #define AGECLASS_WIDTH_4 2
    #define AGECLASS_WIDTH_5 2
    #define AGECLASS_WIDTH_6 5
    #define AGECLASS_WIDTH_7 5
    #define AGECLASS_WIDTH_8 5
    #define AGECLASS_WIDTH_9 25
    #define AGECLASS_WIDTH_10 25
    #define AGECLASS_WIDTH_11 25
    #define AGECLASS_WIDTH_12 1
  #else //EQUAL_AGEWIDTH is default
	//maximum within-stand transitions
	#define MAX_WITHINSTAND_TRANS 50

	#define AGECLASS_FYR_1 1
    #define AGECLASS_FYR_2 11
    #define AGECLASS_FYR_3 21
    #define AGECLASS_FYR_4 31
    #define AGECLASS_FYR_5 41
    #define AGECLASS_FYR_6 51
    #define AGECLASS_FYR_7 61
    #define AGECLASS_FYR_8 71
    #define AGECLASS_FYR_9 81
    #define AGECLASS_FYR_10 91
    #define AGECLASS_FYR_11 101
    #define AGECLASS_FYR_12 151

    #define AGECLASS_WIDTH_1 10
    #define AGECLASS_WIDTH_2 10
    #define AGECLASS_WIDTH_3 10
    #define AGECLASS_WIDTH_4 10
    #define AGECLASS_WIDTH_5 10
    #define AGECLASS_WIDTH_6 10
    #define AGECLASS_WIDTH_7 10
    #define AGECLASS_WIDTH_8 10
    #define AGECLASS_WIDTH_9 10
    #define AGECLASS_WIDTH_10 10
    #define AGECLASS_WIDTH_11 50
    #define AGECLASS_WIDTH_12 1
  #endif

#else //NO ageclases
  //reqd, but not used
  #define MAX_WITHINSTAND_TRANS 1
#endif

/* Definition of datatypes */

/*---------------------------------------------*/
/*        S T A N D   S T R U C T              */
/* ..used for every ageclass, land type        */
/* ..ageclass of stand as integer [1,12]       */
/*   corresponding to FYR_ and WIDTH_ as above */
/*---------------------------------------------*/
typedef struct
{
  /* note: list of pft structs in the stand struct */
  /*       ..pft struct not shown                  */
  Pftlist pftlist;
  Soil soil;       //soil struct for each stand
  Real frac;       //stand fractional area of grid-cell
  int landusetype;
  int count;
  
  //for LUC
  int TimeSinceDist;//for keeping track of stand age
  int CropYrs;      //for keeping track of crop-use land history
  
  //for AGECLASS
  int  ageclass_standid;    //for tracking age-classes
  int  len_frac_transition; //length of the vector for fractional transitions
  /*  note: length of fractional transition array is flexible */
  /*        ,memory allocated based on complier flag          */
  /*        ..definition of MAX_WITHINSTAND_TRANS             */
  Real frac_transition[MAX_WITHINSTAND_TRANS];
}Stand;

/* stands stored in a list struct    */
/* ..easy addition,removal of stands */
typedef List *Standlist;

/*--------------------------------------------*/
/*        F U N C T I O N S   declared        */
/* note: not all fns described in pseudo code */
/*--------------------------------------------*/

/* read,write of stand list and stand structs to,from file */
extern Bool fwritestand(FILE *,const Stand *,Bool);
extern void fprintstand(FILE *,const Stand *);
extern int fwritestandlist(FILE *,const Standlist,Bool);
extern void fprintstandlist(FILE *,const Standlist);
extern Stand *freadstand(FILE *,const Pftpar[],const Soilpar *,Bool);
extern Standlist freadstandlist(FILE *,const Pftpar [],const Soilpar *,Bool);

/* stand struct initilization and memory handling */
extern int addstand(List *);
extern void initstand (Stand *);
extern void delstand(List *,int);
extern int freestand(List *,int);

/* simple copy of soil struct from one stand to another */
extern void copysoil(Stand *, Stand *);
/* area-weighted copy of soil struct from one stand to another */
extern void mixsoil(Stand *,Stand *);

/* simple copy of pfts to a new stand with zero pfts */
extern int copy_pft(Pft *, Stand *, const Pftpar[], int);

/* determine if pft exists in stand, for mixing ageclasses */
extern Bool exist_pft(int , Stand *,int *);

/* deep pft copy -- complex mixing of pfts when two stands have pfts */
/* ..maintain carbon mass balance                                    */
/* ..area- and density-weighted mean of values                       */
/* ..updated stand fractions                                         */
/* ..updated number of individuals per pft                           */
extern void mixingRule_pft(Pft *, Real , Pft *, Real , const Pftpar [], int );

/* for new (youngest) stand, determines if a youngest ageclass exists */
/* ..if no youngest stand exists, then creates one and adds to list   */
extern Bool ageclass_createORmix(List *, int *, Bool);

/* finds if an agelcass of ageX exists already              */
/* ..determines if fractional transition and merging occurs */
extern Bool find_exist_ageclass (List *, int , int*, int);


/* for AGECLASS sorting by age or dbh, and by mgLand or forest */
extern void sort_ForestTile(const Standlist, int, int, int);

/* Definition of macros */
#define getstand(list,index) ((Stand *)getlistitem(list,index))
#define foreachstand(stand,i,list) for(i=0;i<getlistlen(list) && (stand=getstand(list,i));i++)
#define printstandlist(standlist) fprintstandlist(stdout,standlist)

#endif
