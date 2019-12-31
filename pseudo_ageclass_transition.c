/********************************************************************************/
/**                                                                            **/
/**         s e c f o r e s t _ t r a n s i t i o n s  .  c    		           **/
/**                                                                            **/
/**   pseudo code                                                              **/
/**   functions below:                                                         **/
/**     ageclass_createORmix -- create of mix young ageclass                   **/
/**     find_exist_ageclass  -- determine if an ageclass exists in list        **/
/**     getstand_standid -- returns position of an ageclass in list            **/
/**     mixStand_ageclass_transition -- frac transition, mix existing ageclass **/
/**     addStand_ageclass_transition -- frac transition, new ageclass          **/
/**     main_transStand -- determines if between-stand frac transition occurs  **/
/**                        ..updates vector of fractional transitions          **/
/**     ageclass_transition -- MAIN FN, calls main_transStand, passes          **/
/**                            ..two ageclasses to main_transStand at a time   **/
/**                                                                            **/
/**                                                                            **/
/**     Leonardo Calle                                                         **/
/**     Montana State University                                               **/
/**     Bozeman, Montana, USA                                                  **/
/**                                                                            **/
/**     Last change: 31 Dec 2019                                               **/
/**                                                                            **/
/********************************************************************************/

#include "lpj.h" /* main header, pseudo code not shown  */

/*-----------------------------------------------------------------*/
/* determines if a youngest ageclass exists                        */
/* ..if no,  then a youngest ageclass is created                   */
/*           ..returns FALSE to indicate no youngest stand in list */
/* ..if yes, then a pointer to the position of the youngest stand  */
/*           ..is updated, returns TRUE to indicate stand exists   */
/*-----------------------------------------------------------------*/
Bool ageclass_createORmix(Standlist standlist, int *s, Bool landusetype){
	int t;
	Stand *stand;

	foreachstand(stand,t,standlist){
	  if(stand->landusetype==landusetype && stand->ageclass_standid==1){
		//standid '1' is the youngest ageclass (1-2yr or 1-10yr, depending on flag {EQUAL_AGEWIDTH,UNEQUAL_AGEWIDTH)
		*s=t;
		return TRUE;
	  }
	}
	return FALSE;
}

/*----------------------------------------------------------*/
/*  searches for an ageclass (search_standid) in stand list */
/*  ..if found                                              */
/*      updates pointer to position of stand in list        */
/*      ..returns TRUE                                      */
/*  ..otherwise, returns FALSE                              */
/*----------------------------------------------------------*/
Bool find_exist_ageclass(Standlist standlist, int search_standid, int *pos_stand, int PRIMARY_or_SECFOREST){
	int s;
	Stand *stand;

	//find a stand by standid (ageClass identifier)
	for(s=0; s < standlist->n && (stand=getstand(standlist,s)); s++){
		if(stand->landusetype==PRIMARY_or_SECFOREST && stand->ageclass_standid==search_standid){
				*pos_stand=s;
				return TRUE;
		}
	}
	return FALSE;
}

//========================================================//
// secforest tiling: main ageclass transition functions   //
//========================================================//

/*-----------------------------------------*/
/* searches for an ageclass in stand list  */
/* ..returns the position in list          */
/*-----------------------------------------*/
int getstand_standid(Cell *cell, int stand_standid, int PRIMARY_or_SECFOREST){
	Stand *stand;
	int s, k=-1;

	//return the ageclass stand if it exists
	for(s=0; s < cell->standlist->n && (stand=getstand(cell->standlist,s)); s++){
		if(stand->landusetype==PRIMARY_or_SECFOREST && stand->ageclass_standid==stand_standid){
			return(s);
		}else if(stand->landusetype==STAND_DUMMY){
			k=s;
		}
	}
    //if ageclass not found, then send indicator that stand is the stand_dummy (-3)
	return(k);
}

/*----------------------------------------------------------*/
/* (called by main_transStand)                              */
/* between-ageclass fractional transition                   */
/* ..older ageclass exists, requires mixing state variables */
/* ..updates vector of fractional transitions               */
/*----------------------------------------------------------*/
void mixStand_ageclass_transition(Cell *cell, Stand *stand1, int pos_stand1, Stand *stand2, Real frac_transition,
		const Pftpar *pftpar, int npft, int PRIMARY_or_SECFOREST){

	int p, pos_transStand, old_standid;
	Stand *transStand;
	Pft *pft;

	////////////////
	// create tempStand with characteristics of younger stand
    // ..but smaller fractional area
	pos_transStand=addstand(cell->standlist);
	pos_transStand--;
	transStand=getstand(cell->standlist,pos_transStand);

	//copy fire, soil, veg and harvest wood
	transStand->fire_sum=stand1->fire_sum;

	//store incoming fraction
	//(simple bc tempStand will be deleted after mixing with older stand)
	transStand->frac=frac_transition;

	//temp designation, this stand gets deleted
	transStand->landusetype=PRIMARY_or_SECFOREST;
	transStand->ageclass_standid=9999;
	copysoil(stand1, transStand);
	foreachpft(pft, p, stand1->pftlist){
		copy_pft(pft, transStand, pftpar, npft);
	}

	//----------------------------------------------------------------------------------
	//update total stand fraction and fractional transitions for younger (donor) stand
    //----------------------------------------------------------------------------------
    //update fraction transitions; start at end, end at start+1
    //update to total stand fraction occurs at end of secforest_transition() fn
	//overwrite fraction transitioning between stands (frac_transition[n])
    for(p=(stand1->len_frac_transition - 1); p > 0; p--){
		stand1->frac_transition[p]=stand1->frac_transition[p-1];
    }

    //update value at incoming fraction: set to zero
    //updated if there is an incoming fraction from a younger stand
    stand1->frac_transition[0]=0.0;

    //update total standfrac of younger stand
    stand1->frac-=frac_transition;

	//if empty, delete stand
	if(stand1->frac < 0.0000004){
		//store standid before deleting stand1 and standlist is updated
        //..reqd for searching
		old_standid=stand2->ageclass_standid;

        //remove empty stand, update list
		delstand_preserveOrder(cell, stand1, 1, pos_stand1);

		//update position of transStand
		pos_transStand=getstand_standid(cell, 9999, PRIMARY_or_SECFOREST);
		transStand=getstand(cell->standlist, pos_transStand);

		//update stand2
		stand2=getstand(cell->standlist, getstand_standid(cell, old_standid, PRIMARY_or_SECFOREST));
	}
	//-------------
	//NOTE: stand deletions occur at end of ageclass_transtion()
    //      ..and catch full transitions (i.e., when stand->frac==0)
	//------------

	//mix and delete tempStand
	mix_standComplex(cell, stand2, transStand, pos_transStand, pftpar, npft, TRUE);

	return;
}

/*-----------------------------------------------*/
/* (called by main_transStand)                   */
/* between-ageclass fractional transition        */
/* ..new ageclass stand created                  */
/* ..updates vector of fractional transitions    */
/*-----------------------------------------------*/
void addStand_ageclass_transition(Cell *cell, Stand *stand1, int pos_stand1, Real frac_transition, int num_trans_older, int new_TimeSinceDist,
		int new_standid, const Pftpar *pftpar, int npft, int PRIMARY_or_SECFOREST){

	int p, pos_transStand;
	Stand *transStand;
	Pft *pft;

	////////////////
	// create tempStand with characteristics of younger stand
    // ..but smaller fractional area
	pos_transStand=addstand(cell->standlist);
	pos_transStand--;
	transStand=getstand(cell->standlist,pos_transStand);

	//copy fire, soil, veg and harvest wood
	transStand->fire_sum=stand1->fire_sum;
	copysoil(stand1, transStand);
	foreachpft(pft, p, stand1->pftlist){
		copy_pft(pft, transStand, pftpar, npft);
	}

	//store the length of the vector for fractional transitions
	transStand->len_frac_transition=num_trans_older;

	//store incoming fraction
	transStand->frac_transition[0]=frac_transition;

	//store total stand fraction (simple bc this is new stand)
	transStand->frac=frac_transition;

	//------------------------------------------------
	// update total stand fraction
    //..and fractional transitions for younger stand
    //------------------------------------------------
    //update fraction transitions; start at end, end at start+1
    //update to total stand fraction occurs at end of secforest_transition() fn
	//overwrite fraction transitioning between stands (frac_transition[n])
   	for(p=(stand1->len_frac_transition - 1); p > 0; p--){
		stand1->frac_transition[p]=stand1->frac_transition[p-1];
    }

    //update value at incoming fraction: set to zero
    //updated if there is an incoming fraction from a younger stand
    stand1->frac_transition[0]=0.0;

   	//update total standfrac of younger stand
    stand1->frac-=frac_transition;

    //-----------------
	// NOTE: stand deletions occur at end of ageclass_transtion()
    //       ..and catch full transitions (i.e., when stand->frac==0)
    //--------------

    //update stand attributes
    transStand->landusetype=PRIMARY_or_SECFOREST;
    transStand->TimeSinceDist=new_TimeSinceDist;
    transStand->ageclass_standid=new_standid;

	//if empty, delete stand
	if(stand1->frac < 0.0000004){
		delstand_preserveOrder(cell, stand1, 1, pos_stand1);
	}

	return;
}

/*-------------------------------------------------------------*/
/* (called by ageclass_transition)                             */
/* updates within-stand fractional transition                  */
/* determines if between-ageclass fractional transitions occur */
/* ..calls below if new stand reqd or if mixing reqd           */
/*     addStand_ageclass_transition                            */
/*     mixStand_ageclass_transition                            */
/*-------------------------------------------------------------*/
void main_transStand(Cell *cell, Stand *stand1, Stand *stand2, int num_trans_older, int new_TimeSinceDist, int new_standid,
		const Pftpar *pftpar, int npft, int PRIMARY_or_SECFOREST){

	//define
	int p, k, pos_stand1=-1;
	Real out_frac_transition;
	Stand *stand_temp;

	//work
	if(stand1->landusetype!=STAND_DUMMY){

		//outgoing fractional transition is the outgoing (last) fraction in vector [frac_transition]
		out_frac_transition=stand1->frac_transition[(stand1->len_frac_transition - 1)];

		if(out_frac_transition < 0.0000004){
			//--------------------------------------------
			// update total stand fraction
            //..fractional transitions for current stand
			//--------------------------------------------
		    //update fraction transitions; start at end, end at start+1
		    //update to total stand fraction occurs at end of secforest_transition() fn
			//overwrite fraction transitioning between stands (frac_transition[n])
		    for(p=(stand1->len_frac_transition - 1); p > 0; p--){
			  stand1->frac_transition[p]=stand1->frac_transition[p-1];
		    }

		    //update value at incoming fraction: set to zero
		    //updated if there is an incoming fraction from a younger stand
		    stand1->frac_transition[0]=0.0000000;

		    //stand->frac stays the same; no change

		}else{
			//only do something if there is actual between-stand fractional transition
			//else frac_transitions get updated at end of fn:ageclass_transition

			//return position of stand1
			foreachstand(stand_temp, k, cell->standlist){
				if(stand_temp->landusetype==PRIMARY_or_SECFOREST && stand_temp->ageclass_standid==stand1->ageclass_standid){
					pos_stand1=k;
					break;
				}
			}

			//determine if older stand exists, else create new stand
			if(stand2->landusetype==STAND_DUMMY){
				//create new stand
				addStand_ageclass_transition(cell, stand1, pos_stand1, out_frac_transition, num_trans_older, new_TimeSinceDist, new_standid, pftpar, npft, PRIMARY_or_SECFOREST);
			}else if(stand2->landusetype==PRIMARY_or_SECFOREST){
				//if standXXXX(older) exists, then transition fraction of standXXX(younger) and mix
				mixStand_ageclass_transition(cell, stand1, pos_stand1, stand2, out_frac_transition, pftpar, npft, PRIMARY_or_SECFOREST);
			}else{
				//error trap
				fail("\n NO MATCHING FOREST...ERROR");
			}
		}//end fractional transitions
	}//end if(STAND_DUMMY)

	return;
}

/*--------------------------------------------------------------------------*/
/*           M A I N   F N  (externally called)                             */
/* before call, stand list is sorted                                        */
/* determines if between-ageclass fractional transitions occur              */
/* ..calls below if new stand reqd or if mixing reqd                        */
/*     addStand_ageclass_transition                                         */
/*     mixStand_ageclass_transition                                         */
/* ageclass transitions proceed from oldest to youngest stands              */
/* ..two ageclass ids are evaluated at one time                             */
/* ..if they don't exist, then nothing is done (stand_dummy_id is passed)   */
/* ..otherwise, within- and between-ageclass fractional transitions updated */
/* ..oldest to youngest procedure                                           */
/*   avoids dilution of biomass in transitional fraction                    */
/*--------------------------------------------------------------------------*/
void ageclass_transition(Cell *cell, const Pftpar *pftpar, int npft, int PRIMARY_or_SECFOREST){
	#if defined(AGECLASS_PRIMARY) || defined(AGECLASS_SECFOREST)
	int s,pos_standTemp;
	int num_trans_older,new_TimeSinceDist;
	Stand *stand1, *stand2;
	Stand *standTemp;
	Pft *pft;

	//after sort_ForestTile, if stand exists,
	//then PRIMARY_or_SECFOREST will be the first stand in standlist
	//..(stands sorted in decreasing order of landusetype [integer], PRIMARY_or_SECFOREST)
	standTemp=getstand(cell->standlist,0);
	if(standTemp->landusetype!=PRIMARY_or_SECFOREST){
		return;
	}

	//create dummy stand to initialize pointers
	pos_standTemp=addstand(cell->standlist);
	pos_standTemp--;
	standTemp=getstand(cell->standlist,pos_standTemp);
	standTemp->landusetype=STAND_DUMMY;
	standTemp->frac=0.0;

	//---------------------------------------------------------------------------------------------------
	// T R A N S I T I O N S
	//	-- transition from oldest to youngest to avoid dilution of biomass in the transitional fraction
	//  -- max number of ageclasses are hard coded (12)
    //     ..but the years and width of each ageclass are flexible, dependent on compiler flags
    //---------------------------------------------------------------------------------------------------

	//----------------------------------
    num_trans_older=AGECLASS_WIDTH_12;
    new_TimeSinceDist=AGECLASS_FYR_12;

    stand1=getstand(cell->standlist, getstand_standid(cell, 11, PRIMARY_or_SECFOREST));
    stand2=getstand(cell->standlist, getstand_standid(cell, 12, PRIMARY_or_SECFOREST));
    main_transStand(cell, stand1, stand2, num_trans_older, new_TimeSinceDist, 12, pftpar, npft, PRIMARY_or_SECFOREST);

    //----------------------------------
    num_trans_older=AGECLASS_WIDTH_11;
    new_TimeSinceDist=AGECLASS_FYR_11;

    stand1=getstand(cell->standlist, getstand_standid(cell, 10, PRIMARY_or_SECFOREST));
    stand2=getstand(cell->standlist, getstand_standid(cell, 11, PRIMARY_or_SECFOREST));
    main_transStand(cell, stand1, stand2, num_trans_older, new_TimeSinceDist, 11, pftpar, npft, PRIMARY_or_SECFOREST);

    //----------------------------------
    num_trans_older=AGECLASS_WIDTH_10;
    new_TimeSinceDist=AGECLASS_FYR_10;

	stand1=getstand(cell->standlist, getstand_standid(cell, 9, PRIMARY_or_SECFOREST));
	stand2=getstand(cell->standlist, getstand_standid(cell, 10, PRIMARY_or_SECFOREST));
	main_transStand(cell, stand1, stand2, num_trans_older, new_TimeSinceDist, 10, pftpar, npft, PRIMARY_or_SECFOREST);

    //----------------------------------
    num_trans_older=AGECLASS_WIDTH_9;
    new_TimeSinceDist=AGECLASS_FYR_9;

	stand1=getstand(cell->standlist, getstand_standid(cell, 8, PRIMARY_or_SECFOREST));
	stand2=getstand(cell->standlist, getstand_standid(cell, 9, PRIMARY_or_SECFOREST));
	main_transStand(cell, stand1, stand2, num_trans_older, new_TimeSinceDist, 9, pftpar, npft, PRIMARY_or_SECFOREST);

    //----------------------------------
    num_trans_older=AGECLASS_WIDTH_8;
    new_TimeSinceDist=AGECLASS_FYR_8;

	stand1=getstand(cell->standlist, getstand_standid(cell, 7, PRIMARY_or_SECFOREST));
	stand2=getstand(cell->standlist, getstand_standid(cell, 8, PRIMARY_or_SECFOREST));
	main_transStand(cell, stand1, stand2, num_trans_older, new_TimeSinceDist, 8, pftpar, npft, PRIMARY_or_SECFOREST);

    //----------------------------------
    num_trans_older=AGECLASS_WIDTH_7;
    new_TimeSinceDist=AGECLASS_FYR_7;

	stand1=getstand(cell->standlist, getstand_standid(cell, 6, PRIMARY_or_SECFOREST));
	stand2=getstand(cell->standlist, getstand_standid(cell, 7, PRIMARY_or_SECFOREST));
	main_transStand(cell, stand1, stand2, num_trans_older, new_TimeSinceDist, 7, pftpar, npft, PRIMARY_or_SECFOREST);

    //----------------------------------
    num_trans_older=AGECLASS_WIDTH_6;
    new_TimeSinceDist=AGECLASS_FYR_6;

	stand1=getstand(cell->standlist, getstand_standid(cell, 5, PRIMARY_or_SECFOREST));
	stand2=getstand(cell->standlist, getstand_standid(cell, 6, PRIMARY_or_SECFOREST));
	main_transStand(cell, stand1,  stand2, num_trans_older, new_TimeSinceDist, 6, pftpar, npft, PRIMARY_or_SECFOREST);

    //----------------------------------
    num_trans_older=AGECLASS_WIDTH_5;
    new_TimeSinceDist=AGECLASS_FYR_5;

	stand1=getstand(cell->standlist, getstand_standid(cell, 4, PRIMARY_or_SECFOREST));
	stand2=getstand(cell->standlist, getstand_standid(cell, 5, PRIMARY_or_SECFOREST));
	main_transStand(cell, stand1, stand2, num_trans_older, new_TimeSinceDist, 5, pftpar, npft, PRIMARY_or_SECFOREST);

    //----------------------------------
    num_trans_older=AGECLASS_WIDTH_4;
    new_TimeSinceDist=AGECLASS_FYR_4;

	stand1=getstand(cell->standlist, getstand_standid(cell, 3, PRIMARY_or_SECFOREST));
	stand2=getstand(cell->standlist, getstand_standid(cell, 4, PRIMARY_or_SECFOREST));
	main_transStand(cell, stand1, stand2, num_trans_older, new_TimeSinceDist, 4, pftpar, npft, PRIMARY_or_SECFOREST);

    //----------------------------------
    num_trans_older=AGECLASS_WIDTH_3;
    new_TimeSinceDist=AGECLASS_FYR_3;

	stand1=getstand(cell->standlist, getstand_standid(cell, 2, PRIMARY_or_SECFOREST));
	stand2=getstand(cell->standlist, getstand_standid(cell, 3, PRIMARY_or_SECFOREST));
	main_transStand(cell, stand1, stand2, num_trans_older, new_TimeSinceDist, 3, pftpar, npft, PRIMARY_or_SECFOREST);

    //----------------------------------
    num_trans_older=AGECLASS_WIDTH_2;
    new_TimeSinceDist=AGECLASS_FYR_2;

	stand1=getstand(cell->standlist, getstand_standid(cell, 1, PRIMARY_or_SECFOREST));
	stand2=getstand(cell->standlist, getstand_standid(cell, 2, PRIMARY_or_SECFOREST));
	main_transStand(cell, stand1, stand2, num_trans_older, new_TimeSinceDist, 2, pftpar, npft, PRIMARY_or_SECFOREST);

	//delete STAND_DUMMY stand
	for(s=0; s<getlistlen(cell->standlist) && (standTemp=getstand(cell->standlist,s)); s++){
		if(standTemp->landusetype==STAND_DUMMY){
			delstand(cell->standlist, s);
			break;
		}
	}
  	#endif //AGECLASS fn, to avoid compiler err
	return;
}//end secforest_transition

