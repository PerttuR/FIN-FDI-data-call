

/***************************************************************************************\
*												 										* 
*  Macro: G_2_effort_rk_YYYY															*
*																						*
*	Tekijä: Antti Sykkö																	*
*	Luotu: 23.5.2022																	*
* 												 										*	
*	Parametrit: 								 										*
*		- y = aineistovuosi 										 					*
*  																						*
*	Aineistot:																			*
* 		- metier 'G:\Luke2\Stat_kala_merikalastus\Metier\data';							*
*		- rkarvoYYYY_metier																*
*																						*
*	Kuvaus: 									 										*
*												 										*
\***************************************************************************************/ 


%macro G_2_effort_rk_YYYY(y) / minoperator mindelimiter=','; 

DATA saalis;
    set metier.rkarvo&y._metier;

/************************************************************************************************************************/
/* MESH SIZE                                                                                                            */
/************************************************************************************************************************/

if pyydys in (16,17,18,19,20,21)                 then meshsizerange='16D32  '; /* kaikki troolaus ja nuotta tähän       */

if pyydys=5                                      then meshsizerange='16D32  ';  /* silakka- ja kilohailiverkko          */
if pyydys in (8,9,10,44,45,32)                   then meshsizerange='32D90  ';  /* pesäverkko, < 36, 36-45 mm verkko    */
if pyydys in (11,12)                             then meshsizerange='90D110 ';  /* 46-60 mm verkko                      */
if pyydys=13                                     then meshsizerange='110D157';  /* > 60 mm verkko                       */
if pyydys=22                                     then meshsizerange='157DXX ';   /* pintaverkko                         */

if gear in ('FPN','FYK','SSC')                   then meshsizerange='16D32  ';   /* silakkarysä - pienin luokka mitä on! */

if gear in ('LLD','LLS','FPO','FYK')             then meshsizerange='NA     ';

if pituus=. then do; vessel_length='NONE'; end;

 /* Quarter coding */

%if not(&y. in (2021)) %then %do; 
if kk in (1,2,3)    then do; quarter=1; end;
if kk in (4,5,6)    then do; quarter=2; end;
if kk in (7,8,9)    then do; quarter=3; end;
if kk in (10,11,12) then do; quarter=4; end;
%end; 

%if &y. in (2021,2022) %then %do; 
if kuukausi in (1,2,3)    then do; quarter=1; end;
if kuukausi in (4,5,6)    then do; quarter=2; end;
if kuukausi in (7,8,9)    then do; quarter=3; end;
if kuukausi in (10,11,12) then do; quarter=4; end;
kk=kuukausi; 
%end; 

%if &y.=2013 %then %do; 
ft = 'PG';
%end; 

year=&y.;

/* Vuosina 2013 - 2019 
on käytetty lonro nimi_lisenssinhaltija sijaan*/ 
%if &y. < 2020 %then %do; 
nimi_lisenssinhaltija=lonro;
%end; 



PROC FREQ;
    tables pyydys*meshsizerange/list missing;
run;

PROC SUMMARY missing nway data=saalis;
    class year nimi_lisenssinhaltija alus vessel_length ft quarter kk metier level5 gear meshsizerange ices ;
    id teho vetoisuus;
    var pyyntipv;
    output out=effort1 max=;
run;

/* Vuosineljänneksittäiset pyyntipäivät lomakkeittain ja aluksittain                                       */
/* Jos kalastajalla on useampi gear, sama pyyntipäivä saattaa tulla eri gear-luokkiin - tämä pitää korjata */

PROC SUMMARY missing nway data=effort1;
    class year nimi_lisenssinhaltija alus vessel_length ft quarter metier level5 gear meshsizerange ices ;
    id teho vetoisuus;
    var pyyntipv;
    output out=rk_effort_&y. sum=TOTSEADAYS;
run;


DATA rk_effort;
    format TOTSEADAYS 8.;
    set rk_effort_&y.;

    rename teho=KW;
    rename vetoisuus=GT;
    TOTFISHDAYS= TOTSEADAYS;
    SPECON_TECH='NA';
run;


/* HUOM: TÄSSÄ VIITATAAN WORKIN PVK-AINEISTOIHIN */ 
DATA pvk_effort_&y.;
    set pvk_effort_&y.;
    year=&y.;
    if totfishdays > totseadays then do; totseadays=totfishdays; end;
run;


DATA effort;
length level5 $20.
    format TOTKWDAYSATSEA TOTGTDAYSATSEA TOTKWFISHDAYS TOTGTFISHDAYS KWHRSEA GTHRSEA HRSEA 8.; 
    set pvk_effort_&y. rk_effort;

    TOTKWDAYSATSEA=KW*TOTSEADAYS;
    TOTGTDAYSATSEA=GT*TOTSEADAYS;
    TOTKWFISHDAYS=KW*TOTFISHDAYS;
    TOTGTFISHDAYS=GT*TOTFISHDAYS;
    KWHRSEA=KW*HRSEA;
    GTHRSEA=GT*HRSEA;
run;

PROC SUMMARY missing nway data=effort;
    class year quarter vessel_length ft gear meshsizerange metier level5 ices specon_tech;
    var TOTSEADAYS TOTKWDAYSATSEA TOTGTDAYSATSEA TOTFISHDAYS TOTKWFISHDAYS TOTGTFISHDAYS HRSEA KWHRSEA GTHRSEA;
    output out=effortit sum= n(totseadays)=aluksia;
run;



DATA effort&y.;
    set effortit;
    drop _type_ _freq_;
    COUNTRY='FIN';
    SUPRA_REGION='AREA27';
    GEO_INDICATOR='NGI';
    EEZ_INDICATOR='NA';
    DEEP='NA';
    FISHING_TECH=ft;
    GEAR_TYPE=gear;
   	EEZ_INDICATOR='NA';
    GEO_INDICATOR='NGI';
	if vessel_length ne 'NONE' then do; 
	   TOTVES=_freq_;
	end;
	if aluksia < 3 then do;
	   CONFIDENTIAL='Y';
	end;

	if aluksia >= 3 then do;
	   CONFIDENTIAL='N';
	end;
    rename meshsizerange=MESH_SIZE_RANGE;

if level5='Anadromous      ' then target_assemblage='ANA';
if level5='Demersal        ' then target_assemblage='DEF';
if level5='Freshwater      ' then target_assemblage='FWS';
if level5='Small pelagic   ' then target_assemblage='SPF';
if level5='Activity missing' then target_assemblage='NK '; 

if ices=22 then SUB_REGION='27.3.D.22  ';
if ices=24 then SUB_REGION='27.3.D.24  ';
if ices=25 then SUB_REGION='27.3.D.25  ';
if ices=26 then SUB_REGION='27.3.D.26  ';
if ices=27 then SUB_REGION='27.3.D.27  ';
if ices=28 then SUB_REGION='27.3.D.28.2';
if ices=29 then SUB_REGION='27.3.D.29  ';
if ices=30 then SUB_REGION='27.3.D.30  ';
if ices=31 then SUB_REGION='27.3.D.31  ';
if ices=32 then SUB_REGION='27.3.D.32  ';


if HRSEA=.          then HRSEA=-1;
if KWHRSEA=.        then KWHRSEA=-1;
if GTHRSEA=.        then GTHRSEA=-1;
if TOTKWDAYSATSEA=. then TOTKWDAYSATSEA=-1;
if TOTGTDAYSATSEA=. then TOTGTDAYSATSEA=-1;
if TOTKWFISHDAYS=.  then TOTKWFISHDAYS=-1;
if TOTGTFISHDAYS=.  then TOTGTFISHDAYS=-1;
if TOTVES=.         then TOTVES=-1;

if SPECON_TECH=' ' then SPECON_TECH='NK';

run;


/* Puuttuvat tiedot paikataan NK:lla eli numeeriset muuttujat muutetaan merkkimuotoisiksi              */
DATA effort_1;
    set effort&y.;
    TOTKWDAYSATSEAu=int(TOTKWDAYSATSEA);
    TOTGTDAYSATSEAu=int(TOTGTDAYSATSEA);
    TOTKWFISHDAYSu=int(TOTKWFISHDAYS);
    TOTGTFISHDAYSu=int(TOTGTFISHDAYS);
    HRSEAu=int(HRSEA);
    kwHRSEAu=int(KWHRSEA);
    gtHRSEAu=int(GTHRSEA);
    TOTVESu=int(TOTVES);

run;

DATA effort_2;
    set effort_1;
format TOTKWDAYSATSEAuu $ 8.;
if TOTKWDAYSATSEAu=-1    then TOTKWDAYSATSEAuu='NK'; 
else do;
if TOTKWDAYSATSEAu ne -1 then TOTKWDAYSATSEAuu=TOTKWDAYSATSEAu;
end;

format TOTGTDAYSATSEAuu $ 8.;
if TOTGTDAYSATSEAu=-1    then TOTGTDAYSATSEAuu='NK'; 
else do;
if TOTGTDAYSATSEAu ne -1 then TOTGTDAYSATSEAuu=TOTGTDAYSATSEAu;
end;

format TOTKWFISHDAYSuu $ 8.;
if TOTKWFISHDAYSu=-1    then TOTKWFISHDAYSuu='NK'; 
else do;
if TOTKWFISHDAYSu ne -1 then TOTKWFISHDAYSuu=TOTKWFISHDAYSu;
end;

format TOTGTFISHDAYSuu $ 8.;
if TOTGTFISHDAYSu=-1    then TOTGTFISHDAYSuu='NK'; 
else do;
if TOTGTFISHDAYSu ne -1 then TOTGTFISHDAYSuu=TOTGTFISHDAYSu;
end;

format HRSEAuu $ 8.;
if HRSEAu=-1    then HRSEAuu='NK'; 
else do;
if HRSEAu ne -1 then HRSEAuu=HRSEAu;
end;

format kwHRSEAuu $ 8.;
if kwHRSEAu=-1    then kwHRSEAuu='NK'; 
else do;
if kwHRSEAu ne -1 then kwHRSEAuu=kwHRSEAu;
end;

format gtHRSEAuu $ 8.;
if gtHRSEAu=-1    then gtHRSEAuu='NK'; 
else do;
if gtHRSEAu ne -1 then gtHRSEAuu=gtHRSEAu;
end;

format TOTVESuu $ 8.;
if TOTVESu=-1    then TOTVESuu='0'; 
else do;
if TOTVESu ne -1 then TOTVESuu=TOTVESu;
end;

if vessel_length='NONE' then do; vessel_length='NK'; end;

run;



DATA effort_3;
    set effort_2;
    drop
TOTKWDAYSATSEA
TOTGTDAYSATSEA
TOTKWFISHDAYS
TOTGTFISHDAYS
HRSEA
KWHRSEA
GTHRSEA
TOTVES

TOTKWDAYSATSEAu
TOTGTDAYSATSEAu
TOTKWFISHDAYSu
TOTGTFISHDAYSu
HRSEAu
kwHRSEAu
gtHRSEAu
TOTVESu; 

data effort_4;
set effort_3;
rename TOTKWDAYSATSEAuu=TOTKWDAYSATSEA;
rename TOTGTDAYSATSEAuu=TOTGTDAYSATSEA;
rename TOTKWFISHDAYSuu=TOTKWFISHDAYS;
rename TOTGTFISHDAYSuu=TOTGTFISHDAYS;
rename HRSEAuu=HRSEA;
rename kwHRSEAuu=KWHRSEA;
rename gtHRSEAuu=GTHRSEA;
rename TOTVESuu=TOTVES; 
run;



data fdi.G_EFFORT_&y.;
drop ices ft gear level5 aluksia;
put COUNTRY $ YEAR QUARTER VESSEL_LENGTH $ FISHING_TECH $ GEAR_TYPE $ TARGET_ASSEMBLAGE $ MESH_SIZE_RANGE $ METIER $ 21. SUPRA_REGION $ 
    SUB_REGION $ 11. EEZ_INDICATOR $ GEO_INDICATOR $ SPECON_TECH $ DEEP $ TOTSEADAYS  TOTKWDAYSATSEA $ TOTGTDAYSATSEA $ TOTFISHDAYS  
    TOTKWFISHDAYS $ TOTGTFISHDAYS $ HRSEA $ KWHRSEA $ GTHRSEA $ TOTVES $ CONFIDENTIAL $ ;
set effort_4;
run;


%mend G_2_effort_rk_YYYY; 
