

/***************************************************************************************\
*												 										* 
*  Macro: A_4_Catch_pvk_YYYY_1 															*
*																						*
*	Tekij‰: Antti Sykkˆ																	*
*	Luotu: 5.5.2022																		*
* 												 										*	
*	Parametrit: 								 										*
*		- 										 										*
*  																						*
*	Aineistot:																			*
* 		- metier 'G:\Luke2\Stat_kala_merikalastus\Metier\data';							*
*		- pvkarvoYYYY_metier															*
*																						*
*	Kuvaus: 									 										*
*												 										*
\***************************************************************************************/ 



%macro A_4_Catch_pvk_YYYY_1(y) / minoperator mindelimiter=','; /* in operaattori k‰yttˆˆn locaalisti t‰ss‰ makrossa */ 


DATA pvk_saalis&y._copy;

/* DROP all the columns with missing values only in here! */ 

%if not(&y.=2020) %then %do; 
	SET metier.pvkarvo&y._metier;

%end; 

 %if &y.=2020 %then %do; 
    SET metier.pvkarvo&y._metier_paivat;
 %end; 

run; 


/*Tehd‰‰n ensin lista muuttujista, jotka pelkk‰‰ tyhj‰‰ (n‰m‰ j‰‰ pois)*/ 
/*Tehd‰‰n ensin lista muuttujista, jotka pelkk‰‰ tyhj‰‰ (n‰m‰ j‰‰ pois)*/ 
ods select none;
ods output nlevels=want;
proc freq data=pvk_saalis&y._copy nlevels;
table _all_;
run;
ods select all; 

data want; set want;
where NNonMissLevels=0;
run; 

proc sql noprint;
	select count(*) into :nobs_1 
	from want;
quit;


/*Tehd‰‰n datan perusteella saalislajilistat*/ 

%if &nobs_1. = 0 %then %do;

proc sql noprint; 

   select name 
          into :pvk_catch_species /* lista, jossa saalislajimuuttujat */ 
          separated by ' '
          from dictionary.columns
          where libname = 'WORK' and memname = "PVK_SAALIS&y._COPY"
          and lowcase(name) in ('silakka', 'kilohail', 'turska', 'kampela', 'pkamp', 'lohi_kg', 'taimen', 'kirjoloh', 'siika', 'muikku',
     'sarki', 'kuore', 'ahven', 'ankerias' ,'hauki' ,'lahna' ,'kuha' ,'made' ,'sayne', 'valkoturska', 'punakampela', 'muu_kala'); 
		

	    select cats('h',name) 
          into :pvk_catch_species_prices /*lista, jossa saalislajimuuttujien hinnat */ 
          separated by ' '
          from dictionary.columns
          where libname = 'WORK' and memname = "PVK_SAALIS&y._COPY"
          and lowcase(name) in ('silakka', 'kilohail', 'turska', 'kampela', 'pkamp', 'lohi_kg', 'taimen', 'kirjoloh', 'siika', 'muikku',
     'sarki', 'kuore', 'ahven', 'ankerias' ,'hauki' ,'lahna' ,'kuha' ,'made' ,'sayne', 'valkoturska', 'punakampela', 'muu_kala'); 
quit; 

%end; 

%if &nobs_1. > 0 %then %do;
proc sql noprint;
select cats('"',LOWER(TableVar),'"')  into :missing_variables separated by ','
from want;
quit;

proc sql noprint; 

   select name 
          into :pvk_catch_species /* lista, jossa saalislajimuuttujat */ 
          separated by ' '
          from dictionary.columns
          where libname = 'WORK' and memname = "PVK_SAALIS&y._COPY"
          and lowcase(name) in ('silakka', 'kilohail', 'turska', 'kampela', 'pkamp', 'lohi_kg', 'taimen', 'kirjoloh', 'siika', 'muikku',
     'sarki', 'kuore', 'ahven', 'ankerias' ,'hauki' ,'lahna' ,'kuha' ,'made' ,'sayne', 'valkoturska', 'punakampela', 'muu_kala')
	 	and lowcase(name) not in (&missing_variables.); 
		

	    select cats('h',name) 
          into :pvk_catch_species_prices /*lista, jossa saalislajimuuttujien hinnat */ 
          separated by ' '
          from dictionary.columns
          where libname = 'WORK' and memname = "PVK_SAALIS&y._COPY"
          and lowcase(name) in ('silakka', 'kilohail', 'turska', 'kampela', 'pkamp', 'lohi_kg', 'taimen', 'kirjoloh', 'siika', 'muikku',
     'sarki', 'kuore', 'ahven', 'ankerias' ,'hauki' ,'lahna' ,'kuha' ,'made' ,'sayne', 'valkoturska', 'punakampela', 'muu_kala')
	 and lowcase(name) not in (&missing_variables.); 
quit; 
%end; 




/*******************************************************************/ 

	/* Tarkistuksia*/ 

%put ********************************************************; 
%put Vuonna &y. saalislajit ja niit‰ vastaavat hintamuuttujat:; 
%put ; 
%put &pvk_catch_species.; 
%put ; 
%put &pvk_catch_species_prices.; 

%put ; 

%let specie_cnt=%eval(%sysfunc(count(%cmpres(&pvk_catch_species.),%str( )))+1);
%put Saalislajeja yhteens‰: &specie_cnt;

%let specieprice_cnt=%eval(%sysfunc(count(%cmpres(&pvk_catch_species_prices.),%str( )))+1);
%put Saalislajeja vastaavia hintamuuttujia yhteens‰: &specie_cnt;


%if not(&specie_cnt.=&specieprice_cnt.) %then %do; 
%put WARNING: Saalislajien ja niit‰ vastaavien hintamuuttujien m‰‰r‰ ei t‰sm‰‰.; 
%end; 

%put ; 
%put Vuoden &y. aineistossa nollamuuttujia yhteens‰ &nobs_1. kpl.; 

%if &nobs_1 > 0 %then %do; 
%put Nollamuuttujat: &missing_variables.; 
%end; 




%put ********************************************************; 


/*******************************************************************/  

data pvk_saalis&y.;
	set pvk_saalis&y._copy; 


/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*/ 
/*lis‰ykset "gear ja ft tiedostoihin.sas"-koodin pohjalta */ 


/* Laitetaan kaikki troolit samaan luokkaan - Suomessa vain muutama pohjatroolari */
/* ja pohjatroolauksena ilmoitettu kalastus on pelagista troolausta l‰helt‰ pohjaa */

	if pyydys in (16,17,18,19,20,38,39,40,41)                 then ft='TM ';      /* pelaginen trooli, myˆs pohjatroolialus/alukset t‰h‰n luokkaan*/
    if pyydys in (21)                                         then ft='MGO';      /* nuotat = muu aktiivinen pyydys */
    if pyydys in (14,15,30,31,32)                             then ft='HOK';      /* Koukut */
	if pyydys in (5,6,7,8,9,10,11,12,13,22,23,24,25,35,44,45) then ft='DFN';	  /* ajo- ja seisovat verkot */
    if pyydys in (1,2,3,4,26,27,28,29,34,37)                  then ft='FPO'; 	  /* rys‰t, merrat ja muut sulkupyydykset, rysille vain yksi ft  */
    if pyydys in (32,33,36)                                   then ft='PGO';	  /* muut passiiviset pyydykset */
    
/* Alle 12 metriset passiivipyydyst‰ k‰ytt‰v‰t alukset samaan luokkaan*/

    if (pituus < 1200) and pyydys in (1,2,3,4,26,27,28,29,34,37,5,6,7,8,9,10,11,12,13,22,23,24,25,35,44,45,14,15,30,31,32,32,33,36) 
                                                              then do; ft='PG'; end;


    if ft='MGO' then ft='PG';  
	if ft='FPO' then ft='PG';  
	if ft='DFN' then ft='PG';



	/* Acvive gears */

    if turska gt 0 and pyydys in (19)               then gear='OTB';  /* turskan pohjatroolaus  yksin  */
    if turska gt 0 and pyydys in (20)               then gear='OTT';  /* turskan pohjatroolaus  pari   */
    if pyydys in (38,39)                            then gear='OTB';  /* Bacoma ja T90 aina pohjatroolausta, yksin*/
    if pyydys in (40,41)                            then gear='OTT';  /* Bacoma ja T90 aina pohjatroolausta, pari*/

    if turska lt 1 and pyydys in (19)               then gear='OTM';  /* muun kalan pohjatroolaus yksin */
    if turska lt 1 and pyydys in (20)               then gear='PTM';  /* muun kalan pohjatroolaus pari  */

    if pyydys in (16,17)                            then gear='OTM';  /* pelaginen trooli yksin*/
    if pyydys in (18)                               then gear='PTM';  /* pelaginen trooli pari */

    if pyydys in (21)                               then gear='SSC';  /* nuotat = muu aktiivinen pyydys */

/* Passive gears */

    if pyydys in (14)                                  then gear='LLD';     /* ajosiima    */
    if pyydys in (15,31,42)                            then gear='LLS';     /* muut siimat */
    if pyydys in (30)                                  then gear='LHP';     /* vapapyydys ja vetouistin*/
    if pyydys in (5,6,7,8,9,10,11,12,13,22,23,24,25,35,43,44,45) then gear='GNS'; /* ajo- ja seisovat verkot */
    if pyydys in (1,2,3,4,26,27,28,29,37)              then gear='FYK';     /* kaikki rys‰t ja paunetit */
    if pyydys in (34)                                  then gear='FPO';     /* katiska     */        
    if pyydys in (32,33,36)                            then gear='GNS';     /* tuntemattomat pyydykset verkkoihin, ei ole montaa*/

/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*/ 



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

if gear1='OTTER_B'  and (110 > silmakoko >= 105) then meshsizerange='105D110 '; /* Bacoma                                   */   
if gear1='OTTER_B'  and silmakoko => 110         then meshsizerange='110DXX ';  /* Bacoma                                   */
 
if gear1='OTTER_T'  and (110 > silmakoko => 105) then meshsizerange='105D110 '; /*                                           */   
if gear1='OTTER_T'  and silmakoko => 110         then meshsizerange='110DXX ';  /*                                           */

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

if gear in ('FPN')                               then meshsizerange='16D32 ';   /* silakkarys‰ - pienin luokka mit‰ on!      */

if gear in ('LLD','LLS','FPO')                   then meshsizerange='NONE   ';


 /* Quarter coding */

%if not(&y.=2021) %then %do; 
if kk in (1,2,3)    then do; quarter=1; end;
if kk in (4,5,6)    then do; quarter=2; end;
if kk in (7,8,9)    then do; quarter=3; end;
if kk in (10,11,12) then do; quarter=4; end;
%end; 

%if &y.=2021 %then %do; 
if kuukausi in (1,2,3)    then do; quarter=1; end;
if kuukausi in (4,5,6)    then do; quarter=2; end;
if kuukausi in (7,8,9)    then do; quarter=3; end;
if kuukausi in (10,11,12) then do; quarter=4; end;
kk=kuukausi; 
%end; 

%if &y.=2022 %then %do; 
if kuukausi in (1,2,3)    then do; quarter=1; end;
if kuukausi in (4,5,6)    then do; quarter=2; end;
if kuukausi in (7,8,9)    then do; quarter=3; end;
if kuukausi in (10,11,12) then do; quarter=4; end;
kk=kuukausi; 
%end; 


/* SPECON_TECH */
if pyydys in (38,40)          then specon_tech='BACOMA';
if pyydys=39                  then specon_tech='T90   ';
if pyydys < 38 or pyydys > 41 then specon_tech='NONE  '; 

/* Samat segmentit kuin economic fleet data call                                               */

/* Kaikki PG-ryhm‰n alukset laitetaan samaan pituusluokkaan  */
if ft='PG'  then vessel_length = 'VL1012'; 

if ft="TM" and vessel_length="VL1012" then do; vessel_length="VL1218"; end;

if ft="TM" and (vessel_length="VL40XX" or vessel_length="VL>=40")  then do; vessel_length="VL2440"; end;

kaikki=sum(of &pvk_catch_species.);
if kaikki gt 0; /* paritroolauksen pelk‰t effortit pois */
year=&y.;
kalastaja=1;

/*aineistoriippuvaisia muokkauksia*/

/* !!! 2013 Metier-pvk-datasta puuttuu:  
hankerias -> k‰ytet‰‰n 2014 kerrointa 4.0
astunnus on nimell‰ ASIAKASTUNNUS -> "nimet‰‰n" uudelleen */ 
%if &y.=2013 %then %do; 
hankerias = ankerias*4.0; 
astunnus = ASIAKASTUNNUS; 
%end; 


%if &y. in (2019,2020,2021,2022) %then %do; 
astunnus = NIMI_LISENSSINHALTIJA; 
%end; 


%if &y. in (2020,2021,2022) %then %do; 
pyyntipv = kalastuspaivat; 
%end; 



/* Painokorjaus tehd‰‰n vain 2013 ja 2014 */ 
%if &y. in (2013,2014) %then %do; 
keep astunnus kalastuskertatunnus year alus kalastaja pvm year kk quarter pituus vessel_length pyydys &pvk_catch_species. kaikki
	 &pvk_catch_species_prices. ices pyyntipv gear1 ices paino silmakoko meshsizerange specon_tech ft gear 
     level5 metier;
run;
%end; 

/*muille vuosille identtinen, mutta painoa ei mukana*/ 
%if &y. in (2015,2016) %then %do; 
keep astunnus kalastuskertatunnus year alus kalastaja pvm year kk quarter pituus vessel_length pyydys &pvk_catch_species. kaikki
	 &pvk_catch_species_prices. ices pyyntipv gear1 ices silmakoko meshsizerange specon_tech ft gear 
     level5 metier;
run;
%end; 

/*muille vuosille identtinen, mutta painoa ei mukana*/ 
%if &y. in (2017,2018,2019,2020,2021,2022) %then %do; 
keep astunnus kalastuskertatunnus year alus kalastaja pvm year kk quarter pituus vessel_length pyydys &pvk_catch_species. kaikki
	 &pvk_catch_species_prices. ices pyyntipv gear1 ices silmakoko meshsizerange specon_tech ft gear 
     level5 metier;
run;
%end; 





/* Summataan saalis ja arvo ensin kalastajittain, jotta saadaan selville ryhm‰kohtaiset kalastajam‰‰r‰t */
%if &y. in (2013,2014) %then %do; 
PROC SUMMARY missing nway data=pvk_saalis&y.;
    class astunnus year quarter vessel_length ft gear level5 metier ices meshsizerange specon_tech;
    var &pvk_catch_species.
        &pvk_catch_species_prices. kalastaja;
    output out=pvk_saalis2a&y. sum(&pvk_catch_species. &pvk_catch_species_prices.)= min(kalastaja)=;
run;

/* Lopuksi summaus ryhmittelymuuttujien mukaan */
PROC SUMMARY missing nway data=pvk_saalis2a&y.;
    class quarter vessel_length ft gear metier level5 ices meshsizerange specon_tech;
	*weight paino;
    var kalastaja &pvk_catch_species. &pvk_catch_species_prices.;
    output out=pvk_saalis2&y. sum=;
run;

%end; 


/* Summataan saalis ja arvo ensin kalastajittain, jotta saadaan selville ryhm‰kohtaiset kalastajam‰‰r‰t */
%if &y. in (2015,2016) %then %do; 
PROC SUMMARY missing nway data=pvk_saalis&y.;
    class astunnus year quarter vessel_length ft gear level5 metier ices meshsizerange specon_tech;
    var &pvk_catch_species. &pvk_catch_species_prices. kalastaja;
    output out=pvk_saalis2a&y. sum(&pvk_catch_species. &pvk_catch_species_prices.)= min(kalastaja)=;
run;

/* Lopuksi summaus ryhmittelymuuttujien mukaan */
PROC SUMMARY missing nway data=pvk_saalis2a&y.;
    class quarter vessel_length ft gear metier level5 ices meshsizerange specon_tech;
	*weight paino;
    var kalastaja &pvk_catch_species. &pvk_catch_species_prices.;
    output out=pvk_saalis2&y. sum=;
run;

%end; 


/* Summataan saalis ja arvo ensin kalastajittain, jotta saadaan selville ryhm‰kohtaiset kalastajam‰‰r‰t */
%if &y. in (2017,2018,2019,2020,2021,2022) %then %do; 
PROC SUMMARY missing nway data=pvk_saalis&y.;
    class astunnus year quarter vessel_length ft gear level5 metier ices meshsizerange specon_tech;
    var &pvk_catch_species. &pvk_catch_species_prices. kalastaja;
    output out=pvk_saalis2a&y. sum(&pvk_catch_species. &pvk_catch_species_prices.)= min(kalastaja)=;
run;

/* Lopuksi summaus ryhmittelymuuttujien mukaan */
PROC SUMMARY missing nway data=pvk_saalis2a&y.;
    class quarter vessel_length ft gear metier level5 ices meshsizerange specon_tech;
	*weight paino;
    var kalastaja &pvk_catch_species. &pvk_catch_species_prices.;
    output out=pvk_saalis2&y. sum=;
run;

%end; 


%mend A_4_Catch_pvk_YYYY_1; 
/***************************************/ 

