

/***************************************************************************************\
*												 										* 
*  Macro: G_1_effort_pvk_YYYY															*
*																						*
*	Tekij‰: Antti Sykkˆ																	*
*	Luotu: 23.5.2022																	*
* 												 										*	
*	Parametrit: 								 										*
*		- y = aineistovuosi 										 					*
*  																						*
*	Aineistot:																			*
* 		- metier 'G:\Luke2\Stat_kala_merikalastus\Metier\data';							*
*		- pvkarvoYYYY_metier															*
*																						*
*	Kuvaus: 									 										*
*												 										*
\***************************************************************************************/ 





%macro G_1_effort_pvk_YYYY(y) / minoperator mindelimiter=','; 


DATA effort&y._copy;

/* DROP all the columns with missing values only in here! */ 

%if not(&y.=2020) %then %do; 
	SET metier.pvkarvo&y._metier;

	%if &y. in (2021,2022) %then %do; 
	rename kuukausi=kk; 
	%end; 

%end; 

 %if &y.=2020 %then %do; 
    SET metier.pvkarvo&y._metier_paivat;
 %end; 

run; 


DATA effort;
  set effort&y._copy;

/* Metier-tiedostossa kaikki lajit ovat jo kokonaisen kalan painoina                                                   */


/***********************************************************************************************************************/
/* Gear1, jota k‰ytet‰‰n apuna, kun m‰‰ritell‰‰n MESHSIZERANGE                                                         */
/***********************************************************************************************************************/

if turska gt 0 and pyydys in (19,20)                  then gear1='OTTER    ';  /* turskan pohjatroolaus                */
if pyydys in (38,40)                                  then gear1='OTTER_B  ';  /* Bacoma, pohjatroolaus                */
if pyydys in (39,41)                                  then gear1='OTTER_T  ';  /* T90,pohjatroolaus                    */
if turska lt 1 and pyydys in (16,17,18,19,20)         then gear1='PEL_TRAWL';  /* muun kalan troolaus                  */

/************************************************************************************************************************/
/* MESH SIZE                                                                                                            */
/************************************************************************************************************************/

if gear1='OTTER'     and (16 <= silmakoko <= 32) then meshsizerange='16D32  '; /* vuonna 2015 havaintoja 5, vuonna 2016 yksi */

if gear1='OTTER_B'  and (110 > silmakoko >= 105) then meshsizerange='105D110'; /* Bacoma                                    */   
if gear1='OTTER_B'  and silmakoko => 110         then meshsizerange='110DXX ';  /* Bacoma                                   */

if gear1='OTTER_T'  and (110 > silmakoko >= 105) then meshsizerange='105D110'; /* muut turskan pohjatroolaukset t‰h‰n       */
if gear1='OTTER_T'  and silmakoko >= 110         then meshsizerange='110DXX ';  /* muut turskan pohjatroolaukset t‰h‰n       */

if gear1='PEL_TRAWL' and silmakoko < 16          then meshsizerange='00D16  '; /* silakan ja kilohailin troolaus < 16 mm     */
if gear1='PEL_TRAWL' and 16 <= silmakoko <= 31   then meshsizerange='16D32  '; /* silakan ja kilohailin troolaus 16-32 mm    */

if pyydys=5                                      then meshsizerange='16D32  ';  /* silakka- ja kilohailiverkko               */
if pyydys in (8,9,10,44,45,32,33)                then meshsizerange='32D90  ';  /* pes‰verkko, < 36, 36-45 mm verkko         */
if pyydys in (11,12)                             then meshsizerange='90D110 ';  /* 46-60 mm verkko                           */
if pyydys=13                                     then meshsizerange='110D157';  /* > 60 mm verkko                            */
if pyydys=22                                     then meshsizerange='157DXX ';   /* pintaverkko                              */
if pyydys in (23,24) and (47 >  silmakoko => 18) then meshsizerange='32D90  ';   /* pohjaverkko 18-47 mm                     */
if pyydys in (23,24) and (57 >  silmakoko => 47) then meshsizerange='90D110 ';   /* pohjaverkko 47-57 mm                     */
if pyydys in (23,24) and (80 >  silmakoko => 57) then meshsizerange='110D157';   /* pohjaverkko 58-79 mm                     */
if pyydys in (23,24) and silmakoko >= 80         then meshsizerange='157DXX ';   /* pohjaverkko 80 mm -                      */

if gear in ('FPN','SSC')                         then meshsizerange='16D32  ';   /* silakkarys‰ - pienin luokka mit‰ on!     */

if gear in ('LLD','LLS','FPO','SSC')             then meshsizerange='NA     ';

/*     Muutama puuttuva mesh size                                             */

if alus='AAL-124'       and meshsizerange=' ' then do; meshsizerange='16D32'; end;

 /* Quarter coding */
if kk in (1,2,3)    then do; quarter=1; end;
if kk in (4,5,6)    then do; quarter=2; end;
if kk in (7,8,9)    then do; quarter=3; end;
if kk in (10,11,12) then do; quarter=4; end;

/* SPECON_TECH */
if pyydys in (38,40)          then specon_tech='BACOMA';
if pyydys=39                  then specon_tech='T90   ';
if pyydys < 38 or pyydys > 41 then specon_tech='NA    '; 

/*   Samat segmentit kuin economic fleet data call                  */

if ft="TM" and length="VL1012" then do; length="VL1218"; end;

if ft="TM" and length="VL40XX" then do; length="VL2440"; end;

if paluupvm=' ' then do; paluupvm=purkupvm; end;

if kalastusaikamin=. then do; kalastusaikamin=0; end;
h=kalastusaikahh*60;                 /* tunnit minuuteiksi */
t=(h+kalastusaikamin)/60;              /* minuutit tunneiksi */

kk=month(datepart(pvm));
pv=day(datepart(pvm));

pvmday=datepart(pvm);
lahto=datepart(lahtopvm);
paluu=datepart(paluupvm);

if vessel_length='VL>=40' then do; vessel_length='VL40XX'; end;

format pvmday lahto paluu DDMMYYP10.;

saalis_yht=all;

pyyntipv=1;

keep alus quarter length gear ices meshsizerange specon_tech ft metier level5 
     saalis_yht all paluupvm purkupvm lahtopvm teho vetoisuus kalastusaikahh kalastusaikamin t kk pv pyyntipv pvmday lahto paluu;
run;

* Valitaan mukaan se ICES-alue, jolla saalis suurin;
* Lajitellaan matkan alku- ja loppup‰ivien mukaan;
proc sort data=effort out=alus_saalis_sort;
by alus pvmday descending saalis_yht;
run;

data alus_saalis;
set alus_saalis_sort;
by alus pvmday;

if first.pvmday;
run;


PROC SUMMARY missing nway data=alus_saalis;
    class alus quarter length ft gear metier level5 ices specon_tech meshsizerange;
    var pyyntipv;
    output out=paivat2 sum=TOTFISHDAYS;
run;


DATA meripv;
    set alus_saalis;
    meripaiva=(paluupvm-lahtopvm)/86400;
run;



DATA meripv;
    set meripv;
    if meripaiva=0 then do;
       meripaiva=1;
    end;
    keep alus lahtopvm paluupvm quarter length ft gear metier level5 ices specon_tech meshsizerange meripaiva;

PROC SORT nodupkey data=meripv out=meripaivat;
    by alus lahtopvm paluupvm quarter length ft gear metier level5 ices specon_tech meshsizerange meripaiva;
run;

PROC SUMMARY missing nway data=meripaivat;
    class alus quarter length ft gear metier level5 ices specon_tech meshsizerange;
    var meripaiva;
    output out=meripaivat2 sum=TOTSEADAYS;
run;

/* kalastustunnit - hours at sea                    */
PROC SUMMARY missing nway data=alus_saalis;
    class alus quarter length ft gear metier level5 ices specon_tech meshsizerange lahtopvm paluupvm;
    var t;
    *format t 4.1;
    output out=tunnit sum=;

PROC SUMMARY missing nway data=tunnit;
    class alus quarter length ft gear metier level5 ices specon_tech meshsizerange;
    var t;
    output out=tunnit2 sum=HRSEA;


/* Aluksen teho ja vetoisuus                       */

PROC SUMMARY missing nway data=alus_saalis;
    class alus quarter length ft gear metier level5 ices specon_tech meshsizerange;
    var teho vetoisuus;
    output out=taustat min=KW GT;
run;

DATA pvk_effort_&y.;
    merge taustat meripaivat2 tunnit2 paivat2;
    by alus quarter length ft gear metier level5 ices specon_tech meshsizerange;
run;



%mend G_1_effort_pvk_YYYY; 





