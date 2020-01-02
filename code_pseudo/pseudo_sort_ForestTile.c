/***************************************************************************/
/**                                                                       **/
/**         s o r t  F o r e s t  T i l e  .  c        		          **/
/**                                                                       **/
/**     pseudo code                                                       **/
/**     sorting stands in stand list (young -> old)                       **/
/**     ..based on bubble sort                                            **/
/**     note: stands stored in a list via pointer to stand structure      **/
/**           ..sort by swapping pointer address in list                  **/
/**           ..based on integer id of stand type and ageclass_standid    **/
/**                                                                       **/
/**     Leonardo Calle                                                    **/
/**     Montana State University                                          **/
/**     Bozeman, Montana, USA                                             **/
/**                                                                       **/
/**     Last change: 31 Dec 2019                                          **/
/**                                                                       **/
/***************************************************************************/

#include "lpj.h" /* main header, pseudo code not shown */
void sort_ForestTile(Standlist standlist, int AGE_or_DBH, int MGLAND_or_FOREST, int PRIMARY_or_SECFOREST)
{
  int i,j;
  int TMP_VAL=100;
  Stand *stand_swap,*stand_current,*stand_next;

  #ifdef DEBUGLC
  	//print stand order and state variables before sorting
	foreachstand(stand_current,i,standlist){
		printf("BEFORE SORT: standID: %d, landuse#: %d, ageclass_standid: %d, standfrac: %f, soil_Cpool_slow: %f\n",
			i, stand_current->landusetype,stand_current->ageclass_standid, stand_current->frac,stand_current->soil.cpool.slow);
	}
  #endif

  //===========================================================================
  // sorting method is based on 'bubble' sort, common sorting algorithm
  //	..less efficient than linked-lists, but more suitable for accessing
  //    ..and in a small list (n=12), difference in time is minimal	
  //	  list members by index than linked-list structure
  //===========================================================================

  //-----------------------------------------------------------------
  // sorting preference: primary or secforest
  // ..(1) set landusetype to high number (eg, TMP_VAL=100)
  // ..(2) sort all stands w/ landusetype==TMP_VAL at front of list
  // ..(3) sort ageclass within same landusetype
  // ..(4) finally: set landusetype to original value
  //------------------------------------------------------------------
  for(i=0; i< standlist->n; i++){
  	stand_current=(Stand *)standlist->data[i];
  	if(stand_current->landusetype==PRIMARY_or_SECFOREST){
		//set landusetype to tmp_val
		stand_current->landusetype=TMP_VAL;
	}
  }

  //------------------------------------------------------------
  // FIRST: sort by landusetype (all forest to front of list)
  //------------------------------------------------------------
  if(MGLAND_or_FOREST == FOREST){
	  for(i=0;i< (standlist->n - 1);i++){
	  	  for(j=0;j< (standlist->n - i - 1);j++){
	  		  stand_current=(Stand *)standlist->data[j];
	  		  stand_next=(Stand *)standlist->data[j+1];
	  		  if(stand_current->landusetype < stand_next->landusetype){
	  		  	//use ">" comparison operator for increasing sort
                  		//use "<" for decreasing sort (largest landusetype id first)
                  		//..which means TMP_VAL stands are first
	  			stand_swap=standlist->data[j];
	  			standlist->data[j]= standlist->data[j+1];
	  			standlist->data[j+1]= stand_swap;
              		  }
	  	  }
	  }
  }else{
      fprint("ERROR in sort_ForestTile, wrong MGLAND_or_FOREST: %s",MGLAND_or_FOREST);
  }

  //-------------------------------------------------
  // SECOND: sort by age (stand->ageclass_standid)
  //-------------------------------------------------
  if(AGE_or_DBH == AGESTAND){
	  for(i=0;i< (standlist->n - 1);i++){
	  	  for(j=0;j< (standlist->n - i - 1);j++){
	  		  stand_current=(Stand *)standlist->data[j];
	  		  stand_next=(Stand *)standlist->data[j+1];
	  		  if(stand_current->landusetype == stand_next->landusetype &&
                	  	  stand_current->ageclass_standid < stand_next->ageclass_standid){
	  			  //non-secforests used ageclass_standids of zero
	  			  //use ">" comparison operator for increasing sort (youngest first)
	  			  //use "<" for decreasing sort (oldest first)
	  			  stand_swap=standlist->data[j];
	  			  standlist->data[j]= standlist->data[j+1];
	  			  standlist->data[j+1]= stand_swap;
	  		  }
	  	  }
	  }
  }else{
      fprint("ERROR in sort_ForestTile, wrong AGE_or_DBH: %s",AGE_or_DBH);
  }

  //---------------------------------------------------
  // reset stand landusetype to original value
  //---------------------------------------------------
  for(i=0; i< standlist->n; i++){
        stand_current=(Stand *)standlist->data[i];
        if(stand_current->landusetype==TMP_VAL){
          //reset landusetype to original value
          stand_current->landusetype=PRIMARY_or_SECFOREST;
        }
  }

  #ifdef DEBUGLC
    //print stand order and state variables before sorting
  	printf("Cell->standlist->: %d\n",standlist->n);
	foreachstand(stand_current,i,standlist){
		printf("AFTER SORT: standID: %d, landuse#: %d, ageclass_standid: %d, standfrac: %f, soil_Cpool_slow: %f\n",
		  i, stand_current->landusetype,stand_current->ageclass_standid,stand_current->frac,stand_current->soil.cpool.slow);
  	}
  #endif

  //stands sorted in stand list upon completion
  //..null return
  return;
}
