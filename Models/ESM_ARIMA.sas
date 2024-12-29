
libname save 'C:\Users\kaffa\OneDrive\Bureau\Projet_prévision\tables' ;
/* Imporation de données */

proc import datafile='C:\Users\kaffa\OneDrive\Bureau\Projet_prévision\data_source.csv'
	out=data 
	dbms=csv replace; 
	getnames=yes; 
	delimiter='09'x;
	
run; 

data save.data; set data(keep=Time Italy);
	Date = datepart(Time);
	format Date MonYY7.;
	Yt = Italy;
run; 

data save.data ; set save.data ;
	if Date <= '31DEC2022'd;
	drop Time Italy;
run;


data save.data ; set save.data ;
taux = yt -lag1(yt) ;
run;

/* label */
data save.data;
set save.data;
label date = "Date"  Yt= "La série CPI de l'Italie" ; 
run ;

ODS RTF file='C:\Users\kaffa\Music\R\graph22.docx';
proc autoreg data= save.data ;
model yt= /
stationarity=(kpss=(kernel=nw auto)) ;
run ; quit ;
ods rtf close;

ODS RTF file='C:\Users\kaffa\Music\R\graph22.docx';
proc autoreg data= save.data ;
model taux= /
stationarity=(kpss=(kernel=nw auto)) ;
run ; quit ;
ods rtf close;


/* graph */
ODS RTF file='C:\Users\kaffa\OneDrive\Bureau\Projet_prévision\test.docx';
proc gplot data = save.data ;
plot yt*Date / OVERLAY;
symbol i=join;
symbol c=blue; 
run ; quit;
ods RTF close;





/**********************************/
/************** arma **************/
/*********************************/


proc arima data=save.data;
Identify var = Yt(1);
run;

/*Estmation ARIMA*/

ODS RTF file='C:\Users\kaffa\OneDrive\Bureau\Projet_prévision\test.doc';
proc arima data=save.data;
Identify var = Yt(1);
estimate p=2 q=6 method=ML ;
Forecast out = ARIMA lead=0 
interval= month id=Date;
run; quit; 
ods rtf close;
/*Test de normalité du résidus*/ 
ODS RTF file='C:\Users\kaffa\OneDrive\Bureau\Projet_prévision\test.doc';
proc univariate  
data = ARIMA normal; 
var RESIDUAL; 
run; quit;
ods rtf close; 
/*Test effat ARCH */
ODS RTF file='C:\Users\kaffa\OneDrive\Bureau\Projet_prévision\test.doc';
proc autoreg data = ARIMA;
model RESIDUAL= /  archtest=(qlm) ; 
run; quit;
ods rtf close; 


/*////////////////////////////////////*/
/* Correction Normalité avec ARIMA-X */
/*///////////////////////////////////*/
data save.data;
    set save.data;
    
    * Création de dum1 pour tous les outliers négatifs ;
    if _n_ in (276) then dum1 = 1;
    else dum1 = 0;
    
    * Création de dum2 pour tous les outliers positifs ;
    if _n_ in (225,237) then dum2 = 1;
    else dum2 = 0;
    
    * Création de dum3 pour les deux valeurs négatives les plus extrêmes ;
    if _n_ in (249,268) then dum3 = 1;
    else dum3 = 0;
    
    * Création de dum4 pour les deux valeurs positives les plus extrêmes ;
    if _n_ in (253) then dum4 = 1;
    else dum4 = 0;

	if _n_ in (265) then dum5 = 1;
    else dum5 = 0;

	if _n_ in (274) then dum6 = 1;
    else dum6 = 0;

	if _n_ in (262) then dum7 = 1;
    else dum7 = 0;

	if _n_ in (270) then dum8 = 1;
    else dum8 = 0;

run;


/*On estime encore une fois le modèle*/
ODS RTF file='C:\Users\kaffa\OneDrive\Bureau\Projet_prévision\test.doc';
proc arima data=save.data;
Identify var = Yt(1) Crosscorr=(dum1 dum2 dum3 dum4 /*dum5*/ dum6 dum7 /*dum8*/);
estimate p=2 q=6 method=ML input=(dum1 dum2 dum3 dum4 /*dum5*/ dum6 dum7 /*dum8*/); 
Forecast out = ARIMA_X lead=0 
interval= month id=Date;
run; quit; 
ods rtf close;
/*Test de normalité du résidus*/ 
proc univariate  
data = ARIMA_X normal; 
var RESIDUAL; 
run; quit;

/*Test effat ARCH */
proc autoreg data = ARIMA_X;
model RESIDUAL= /  archtest=(qlm) ; 
run; quit;
 
ODS RTF file='C:\Users\kaffa\OneDrive\Bureau\Projet_prévision\test.doc';
proc sgplot data=ARIMA_X;
series x=date y=yt ;
series x=date y=forecast;
xaxis values=("01JAN2000"d to "01DEC2022"d by qtr) ;
run;
ods rtf close;


ODS RTF file='C:\Users\kaffa\OneDrive\Bureau\Projet_prévision\test.doc';
proc sgplot data=ARIMA_X;
series x=date y=residual ;
xaxis values=("01JAN2000"d to "01DEC2022"d by qtr) ;
run;
ods rtf close;


/*****************************/
/***********ESM****************/
/******************************/


********************************;
/* Utilisation d'un Double ESM */
********************************;


/* Estimation du double ESM sur tout l'échantillon */
proc esm data=save.data
print=(estimates statistics) plot=(models ) OUT= table3 ;  
id date interval=month;
forecast Yt / model=double ;
run;


********************************;
/* Utilisation d'un Holt ESM */
********************************;

** ESM Holt sur tout l'échantillon;
ODS RTF file='C:\Users\kaffa\OneDrive\Bureau\Projet_prévision\test.doc';
proc esm data= save.data 
print=(estimates statistics) plot=(models) OUT=_NULL_ ;  
     
                                                               ;
id date interval=month;
forecast Yt / model=linear ;
run;
ods rtf close;

proc esm data=save.data outfor=table_out8
back=0 lead=12 plot=(forecasts)  ; 
id date interval=month;
forecast Yt / alpha =0.10 model=linear;
run;


ODS RTF file='C:\Users\kaffa\OneDrive\Bureau\Projet_prévision\test2.doc';
title "ESM Holt";
proc sgplot data=table_out8;
series x=date y=actual ;
series x=date y=predict;
xaxis values=("01JAN2000"d to "01DEC2022"d by qtr) ;
refline "01DEC2022"d / axis=x;
run;
ods rtf close;


ODS RTF file='C:\Users\kaffa\OneDrive\Bureau\Projet_prévision\test2.doc';
title "ESM Holt";
proc sgplot data=table_out8;
series x=date y=ERROR ;
xaxis values=("01JAN2002"d to "01DEC2023"d by month) ;
refline "01DEC2022"d / axis=x;
run;
ods rtf close;


**********************************************;
** DAMPTREND :    Estimations DTM             ;
**********************************************;

** ESM DTM sur tout l'échantillon;

proc esm data=save.data 
print=(estimates statistics) plot=(models) OUT=_NULL_ ; 
id date interval=month;
forecast Yt / model=DAMPTREND ;
run;



/***************************************/
/*******Comparaison des modèles*********/
/***************************************/

/* MAE et RMSE */

proc import datafile="C:\Users\kaffa\OneDrive\Bureau\Méthode de prévision\SSA\error.csv"
            out=ssa
            dbms=csv replace;
     delimiter=',';
     getnames=yes;
     datarow=2;
run;

data table_out8 ;
set table_out8 ;
err_abs_ESM = abs(error) ;
err_squared_ESM = error**2 ;
id_ligne = _n_ ;
run ;

data ARIMA_X ;
set ARIMA_X ;
err_abs_ARIMA = abs(residual) ;
err_squared_ARIMA = residual**2 ;
id_ligne = _n_ ;
run ;

data ssa ;
set ssa ;
err_abs_SSA = abs(data_ts-trend_cycle) ;
err_squared_SSA = (data_ts-trend_cycle)**2 ;
id_ligne = _n_ ;
run ;

proc means data=table_out8 sum ;
var err_abs_ESM    err_squared_ESM;
run ; * 51.4076082 25.8940109 ;

proc means data=ARIMA_X sum ;
var err_abs_ARIMA err_squared_ARIMA;
run ; *41.7862714 11.1508283 ;

proc means data=ssa sum ;
var err_abs_SSA err_squared_SSA;
run ; *23.8835132 6.2182721 ; 

data MAE_RMSE ;
MAE_ESM = 51.4076082/276 ;
MAE_ARIMA = 41.7862714/275 ;
MAE_SSA = 23.8835132/276 ;
RMSE_ESM = sqrt(25.8940109/276) ;
RMSE_ARIMA = sqrt(11.1508283/275) ;
RMSE_SSA = sqrt(6.2182721/276) ;
run ;

/* Tests de Diebold-Mariano */

data merged_table;
   merge table_out8 ARIMA_X ssa;
   by id_ligne ;
   keep date err_squared_ESM err_squared_ARIMA err_squared_SSA ESM_ARIMA ARIMA_SSA SSA_ESM;
   ESM_ARIMA = err_squared_ESM - err_squared_ARIMA ;
   ARIMA_SSA = err_squared_ARIMA - err_squared_SSA ;
   SSA_ESM = err_squared_SSA - err_squared_ESM ;
run;

data merged_table ;
set merged_table ;
where date >= "01FEB2000"d;
run ;
data merged_table;
    set merged_table; 
    if date <= "31DEC2022"d; 
run;



proc autoreg data=merged_table;
    model ESM_ARIMA = / covest=neweywest;
run;


proc autoreg data=merged_table;
    model ARIMA_SSA = / covest=neweywest;
run;


proc autoreg data=merged_table;
    model SSA_ESM = / covest=neweywest;
run;

