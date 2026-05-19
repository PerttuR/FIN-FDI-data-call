

/***************************************************************************************\
*												 										* 
*  Macro: A_1_Catch_rk_YYYY_1 															*
*																						*
*	Tekijä: Antti Sykkö																	*
*	Luotu: 27.4.2022																	*
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



%macro A_1_Catch_rk_YYYY_1(y) / minoperator mindelimiter=','; /* in operaattori käyttöön locaalisti tässä makrossa */ 



/*Tehdään datan perusteella saalislajilistat*/ 

/*Tehdään ensin lista muuttujista, jotka pelkkää tyhjää (nämä jää pois)*/ 
/*Tehdään ensin lista muuttujista, jotka pelkkää tyhjää (nämä jää pois)*/ 
ods select none;
ods output nlevels=want;
proc freq data=metier.rkarvo&y._metier nlevels;
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



%if &nobs_1. = 0 %then %do;
proc sql noprint; 

   select name 
          into :rk_catch_species /* lista, jossa saalislajimuuttujat */ 
          separated by ' '
          from dictionary.columns
          where libname = 'METIER' and memname = "RKARVO&y._METIER"
          and lowcase(name) in ('silakka', 'kilohail', 'turska', 'kampela', 'pkamp', 'lohi_kg', 'taimen', 'kirjoloh', 'siika', 'muikku',
     'sarki', 'kuore', 'ahven', 'ankerias' ,'hauki' ,'lahna' ,'kuha' ,'made' ,'sayne', 'valkoturska', 'punakampela', 'muu_kala')
		;

	    select cats('h',name) 
          into :rk_catch_species_prices /*lista, jossa saalislajimuuttujien hinnat */ 
          separated by ' '
          from dictionary.columns
          where libname = 'METIER' and memname = "RKARVO&y._METIER"
          and lowcase(name) in ('silakka', 'kilohail', 'turska', 'kampela', 'pkamp', 'lohi_kg', 'taimen', 'kirjoloh', 'siika', 'muikku',
     'sarki', 'kuore', 'ahven', 'ankerias' ,'hauki' ,'lahna' ,'kuha' ,'made' ,'sayne', 'valkoturska', 'punakampela', 'muu_kala')
		;

quit; 
%end; 

%if &nobs_1. > 0 %then %do;
proc sql noprint;
select cats('"',LOWER(TableVar),'"')  into :missing_variables separated by ','
from want;
quit;




proc sql noprint; 

   select name 
          into :rk_catch_species /* lista, jossa saalislajimuuttujat */ 
          separated by ' '
          from dictionary.columns
          where libname = 'METIER' and memname = "RKARVO&y._METIER"
          and lowcase(name) in ('silakka', 'kilohail', 'turska', 'kampela', 'pkamp', 'lohi_kg', 'taimen', 'kirjoloh', 'siika', 'muikku',
     'sarki', 'kuore', 'ahven', 'ankerias' ,'hauki' ,'lahna' ,'kuha' ,'made' ,'sayne', 'valkoturska', 'punakampela', 'muu_kala')
		and lowcase(name) not in (&missing_variables.);

	    select cats('h',name) 
          into :rk_catch_species_prices /*lista, jossa saalislajimuuttujien hinnat */ 
          separated by ' '
          from dictionary.columns
          where libname = 'METIER' and memname = "RKARVO&y._METIER"
          and lowcase(name) in ('silakka', 'kilohail', 'turska', 'kampela', 'pkamp', 'lohi_kg', 'taimen', 'kirjoloh', 'siika', 'muikku',
     'sarki', 'kuore', 'ahven', 'ankerias' ,'hauki' ,'lahna' ,'kuha' ,'made' ,'sayne', 'valkoturska', 'punakampela', 'muu_kala')
		and lowcase(name) not in (&missing_variables.);

quit; 
%end; 



%if &y. = 2022 %then; %do; 
data rkarvo2022_metier_mod;
SET metier.rkarvo2022_metier;
drop kk;
run;

data rkarvo2022_metier_mod;
set rkarvo2022_metier_mod;
rename kuukausi=kk;
run;
%end; 


DATA saalis&y.;

	%if &y. = 2022 %then %do; 
    set rkarvo2022_metier_mod; 
	%end;

	%else %do; 
	SET metier.rkarvo&y._metier;
	%end;  


/************************************************************************************************************************/
/* MESH SIZE                                                                                                            */
/************************************************************************************************************************/

if pyydys in (16,17,18,19,20,21)                 then meshsizerange='16D32  '; /* kaikki troolaus ja nuotta tähän       */

if pyydys=5                                      then meshsizerange='16D32  ';  /* silakka- ja kilohailiverkko          */
if pyydys in (8,9,10,44,45,32,33)                then meshsizerange='32D90  ';  /* pesäverkko, < 36, 36-45 mm verkko    */
if pyydys in (11,12)                             then meshsizerange='90D110 ';  /* 46-60 mm verkko                      */
if pyydys=13                                     then meshsizerange='110D157';  /* > 60 mm verkko                       */
if pyydys=22                                     then meshsizerange='157DXX';   /* pintaverkko                          */

if gear in ('FPN','FYK')                         then meshsizerange='16D32 ';   /* silakkarysä ja katiska               */

if gear in ('LLD','LLS','FPO','SSC')             then meshsizerange='NONE   ';


kaikki=sum(of &rk_catch_species.);
kalastaja=1;

/*if pituus=. then do; vessel_length='NONE'; end;*/

/* !!! 2013 Metier-rkdatasta puuttuu:  
ft -> lisätään se. Rannikkokalastuksessa kaikilla PG
hankerias -> käytetään 2014 kerrointa 4.0 */ 
%if &y=2013 %then %do; 
ft = 'PG';
hankerias = ankerias*4.0; 
%end; 

/*lähtöaineistosta puuttuu kirjolohen hinta */ 
%if &y.=2017 %then %do; 
hkirjoloh = kirjoloh*5.63;  
%end; 

/*vuosina 2020 ja 2021 metier datassa 
-ei ole asiakastunnusta, vaan nimi_lisenssihaltija
-> käyttö identtistä, joten "nimetään uudelleen" 
-mja kk on nimellä kuukausi
-> "nimetään uudelleen */

%if &y. in (2020,2021,2022) %then %do;
asiakastunnus=nimi_lisenssinhaltija;
vessel_length = length;
/*if pituus=. then do; vessel_length='NONE'; end;*/ 
kk=put(datepart(purkupvm),MONTH.);
%end; 

/* quarter */ 
if kk  in (1,2,3)    then do; quarter=1; end;
if kk  in (4,5,6)    then do; quarter=2; end;
if kk  in (7,8,9)    then do; quarter=3; end;
if kk  in (10,11,12) then do; quarter=4; end;

if pituus=. then do; vessel_length='NONE'; end;

/* Painokorjaus tehdään vain 2013 ja 2014 */ 
%if &y. in (2013,2014) %then %do; 
keep asiakastunnus kalastaja kk quarter pituus vessel_length pyydys &rk_catch_species. kaikki
	 &rk_catch_species_prices. ices meshsizerange ft gear vessel_length metier level5 paino;
%end; 

/*muille vuosille identtinen, mutta painoa ei mukana*/ 
%if not(&y. in (2013,2014)) %then %do;
keep asiakastunnus kalastaja kk quarter pituus vessel_length pyydys &rk_catch_species. kaikki
	 &rk_catch_species_prices.
     ices meshsizerange ft gear vessel_length metier level5;
%end; 

run;





/*******************************************************************/ 

	/* Tarkistuksia*/ 

%put ********************************************************; 
%put Vuonna &y. saalislajit ja niitä vastaavat hintamuuttujat:; 
%put ; 
%put &rk_catch_species.; 
%put ; 
%put &rk_catch_species_prices.; 

%put ; 

%let specie_cnt=%eval(%sysfunc(count(%cmpres(&rk_catch_species.),%str( )))+1);
%put Saalislajeja yhteensä: &specie_cnt;

%let specieprice_cnt=%eval(%sysfunc(count(%cmpres(&rk_catch_species_prices.),%str( )))+1);
%put Saalislajeja vastaavia hintamuuttujia yhteensä: &specie_cnt;


%if not(&specie_cnt.=&specieprice_cnt.) %then %do; 
%put WARNING: Saalislajien ja niitä vastaavien hintamuuttujien määrä ei täsmää.; 
%end; 

%put ; 
%put Vuoden &y. aineistossa nollamuuttujia yhteensä &nobs_1. kpl.; 

%if &nobs_1 > 0 %then %do; 
%put Nollamuuttujat: &missing_variables.; 
%end; 








%put ********************************************************; 




/*******************************************************************/  



%if &y. in (2013,2014) %then %do; /* HUOM tässä paino mukana*/ 
/* Summataan saalis ja arvo ensin kalastajittain, jotta saadaan selville ryhmäkohtaiset kalastajamäärät */
PROC SUMMARY missing nway data=saalis&y.;
    class asiakastunnus quarter vessel_length ft gear metier level5 ices meshsizerange;
    var kalastaja &rk_catch_species. &rk_catch_species_prices.;
	id paino;
    output out=saalis2a&y. sum(&rk_catch_species. &rk_catch_species_prices.)= min(kalastaja)=;
run;

/* Lopuksi summaus ryhmittelymuuttujien mukaan */
PROC SUMMARY missing nway data=saalis2a&y.; 
    class quarter vessel_length ft gear metier level5 ices meshsizerange;
	weight paino;
    var kalastaja &rk_catch_species. &rk_catch_species_prices.;
    output out=saalis2&y. sum=;
run;
%end; 


%if not(&y. in (2013,2014)) %then %do; 
/* Summataan saalis ja arvo ensin kalastajittain, jotta saadaan selville ryhmäkohtaiset kalastajamäärät */
PROC SUMMARY missing nway data=saalis&y.;
    class asiakastunnus quarter vessel_length ft gear metier level5 ices meshsizerange;
    var kalastaja &rk_catch_species. &rk_catch_species_prices.;
    output out=saalis2a&y. sum(&rk_catch_species. &rk_catch_species_prices.)= min(kalastaja)=;
run;

/* Lopuksi summaus ryhmittelymuuttujien mukaan */
PROC SUMMARY missing nway data=saalis2a&y.; 
    class quarter vessel_length ft gear metier level5 ices meshsizerange;
    var kalastaja &rk_catch_species. &rk_catch_species_prices.;
    output out=saalis2&y. sum=;
run;
%end; 


/*nollat ja tyhjät pois */ 
data saalis2&y.;
modify saalis2&y.;
array vars{*} &rk_catch_species.; /* or _numeric_ if you want all numeric variables */
do i = 1 to dim(vars);
    if vars{i}=0 then call missing(vars{i});
end;
run;

proc transpose data=saalis2&y.(obs=0) out=vname ;
 var _all_;
run;

proc sql noprint;
 select catx(' ','n(',_name_,') as ',_name_) into : list separated by ',' from vname;
 create table temp as
  select &list from saalis2&y.;
quit;

proc transpose data=temp out=drop ;
 var _all_;
run;



proc sql noprint;
select count(*) into :marker from drop where col1=0;
select _name_ into :drop separated by ' ' from drop where col1=0;
quit;

/* jos mukanaa on tyhjiä muuttujia, niin ne pudotetaan tässä*/ 
%if &marker. > 0 %then %do; 
data saalis2&y.;
 set saalis2&y.(drop=&drop);
run;
%end; 



/*siivotaan */
proc sql noprint;
select cats("WORK.",memname) into :to_delete separated by ' '
from dictionary.tables
where libname = "WORK" and memname ne "SAALIS2&y.";
quit;

proc delete data=&to_delete.;
run;


%mend A_1_Catch_rk_YYYY_1; 







