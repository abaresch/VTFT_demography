/***************************************************************************/
/**                                                                       **/
/**                  f  i  n  d  s  t  a  n  d  .  c                      **/
/**                                                                       **/
/**     pseudo code                                                       **/
/**     finds a stand in list based on landusetype and irrigation status  **/
/**     ..returns position of stand in the list                           **/
/**     note:                                                             **/
/**       returns position of first stand it finds that meets criteria    **/
/**                                                                       **/
/**     Leonardo Calle                                                    **/
/**     Montana State University                                          **/
/**     Bozeman, Montana, USA                                             **/
/**                                                                       **/
/**     Last change: 31 Dec 2019                                          **/
/**                                                                       **/
/***************************************************************************/

#include "lpj.h" /* main header, pseudo code not shown */

Bool findstand(Standlist standlist, int *t, Bool landusetype, Bool irrigation)
{
  int s;
  Stand *stand;
  
  foreachstand(stand,s,standlist)
    if(stand->landusetype==landusetype && stand->irrigation==irrigation){
      *t=s;
      return TRUE;
    }
  return FALSE;
}/* of findstand*/
