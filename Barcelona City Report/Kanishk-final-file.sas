
/* BUSINESS REPORTING TOOLS RETAKE 2 - KANISHK GUPTA */

/* In this project SQL is used to analyze data provided. SQL code will be written in SAS and then Tableau will be used for visualization and storyy creation */

/* First step involves creating of a library */

libname retake2 "C:\Users\kgupta\Desktop\BUSINESS REPORTING TOOLS RETAKE\New folder"; run;

/* Next step involves importing all the files on which further analysis will be done */

proc import datafile = 'C:/Users/kgupta/Desktop/BUSINESS REPORTING TOOLS RETAKE/BRT_retake_data (2)/unemployment.csv'
 out = retake2.Unemp
 dbms = CSV
 ;
run;  /* DATA RELATED TO UNEMPLOYMENT */

proc print data = retake2.Unemp;
run;

proc import datafile = 'C:/Users/kgupta/Desktop/BUSINESS REPORTING TOOLS RETAKE/BRT_retake_data (2)/transports.csv'
 out = retake2.transports
 dbms = CSV
 ;
run;   /* DATA RELATED TO TRANSPORTS */

proc print data = retake2.transports;
run;

proc import datafile = 'C:/Users/kgupta/Desktop/BUSINESS REPORTING TOOLS RETAKE/BRT_retake_data (2)/population.csv'
 out = retake2.population
 dbms = CSV
 ;
run; /* DATA RELATED TO POPULATION */

proc print data = retake2.population;
run;

proc import datafile = 'C:/Users/kgupta/Desktop/BUSINESS REPORTING TOOLS RETAKE/BRT_retake_data (2)/most_frequent_names.csv'
 out = retake2.most_frequent_names
 dbms = CSV
 ;
run;    /* DATA RELATED TO MOST FREQUENT NAMES */

proc print data = retake2.most_frequent_names;
run;


proc import datafile = 'C:/Users/kgupta/Desktop/BUSINESS REPORTING TOOLS RETAKE/BRT_retake_data (2)/most_frequent_baby_names.csv'
 out = retake2.most_frequent_baby_names
 dbms = CSV
 ;
run;    /* DATA RELATED TO MOST FREQUENT BABY NAMES */

proc print data = retake2.most_frequent_baby_names;
run;

proc import datafile = 'C:/Users/kgupta/Desktop/BUSINESS REPORTING TOOLS RETAKE/BRT_retake_data (2)/immigrants_emigrants_by_sex.csv'
 out = retake2.immigrants_emigrants_by_sex
 dbms = CSV
 ;
run;   /* DATA RELATED TO IMMIGRANTS EMMIGRANTS BY SEX */

proc print data = retake2.immigrants_emigrants_by_sex;
run;


proc import datafile = 'C:/Users/kgupta/Desktop/BUSINESS REPORTING TOOLS RETAKE/BRT_retake_data (2)/immigrants_emigrants_by_destination2.csv'
 out = retake2.immigrants_des_2
 dbms = CSV
 ;
run;    /* DATA RELATED TO IMMIGRANTS EMMIGRANTS BY DESTINATIONS 2 */

proc print data = retake2.immigrants_des_2;
run;

proc import datafile = 'C:/Users/kgupta/Desktop/BUSINESS REPORTING TOOLS RETAKE/BRT_retake_data (2)/immigrants_emigrants_by_destination.csv'
 out = retake2.immigrants_des
 dbms = CSV
 ;
run;    /*  DATA RELATED TO IMMIGRANTS EMMIGRANTS BY DESTINATIONS */

proc print data = retake2.immigrants_des;
run;


proc import datafile = 'C:/Users/kgupta/Desktop/BUSINESS REPORTING TOOLS RETAKE/BRT_retake_data (2)/immigrants_emigrants_by_age.csv'
 out = retake2.immigrants_emigrants_by_age
 dbms = CSV
 ;
run;   /*  DATA RELATED TO IMMIGRANTS EMMIGRANTS BY AGE */

proc print data = retake2.immigrants_emigrants_by_age;
run;


proc import datafile = 'C:/Users/kgupta/Desktop/BUSINESS REPORTING TOOLS RETAKE/BRT_retake_data (2)/immigrants_by_nationality.csv'
 out = retake2.immigrants_by_nationality
 dbms = CSV
 ;
run;   /* DATA RELATED TO IMMIGRANTS BY NATIONALITY */

proc print data = retake2.immigrants_by_nationality;
run;


proc import datafile = 'C:/Users/kgupta/Desktop/BUSINESS REPORTING TOOLS RETAKE/BRT_retake_data (2)/deaths.csv'
 out = retake2.deaths
 dbms = CSV
 ;
run;     /* DATA RELATED TO DEATHS */

proc print data = retake2.deaths;
run;

proc import datafile = 'C:/Users/kgupta/Desktop/BUSINESS REPORTING TOOLS RETAKE/BRT_retake_data (2)/bus_stops.csv'
 out = retake2.bus_stops
 dbms = CSV
 ;
run;   /* DATA RELATED TO BUS STOPS */

proc print data = retake2.bus_stops;
run;

proc import datafile = 'C:/Users/kgupta/Desktop/BUSINESS REPORTING TOOLS RETAKE/BRT_retake_data (2)/births.csv'
 out = retake2.births
 dbms = CSV
 ;
run;   /* DATA RELATED TO BIRTHS */

proc print data = retake2.births;
run;

proc import datafile = 'C:/Users/kgupta/Desktop/BUSINESS REPORTING TOOLS RETAKE/BRT_retake_data (2)/air_stations_Nov2017.csv'
 out = retake2.air_stations_Nov2017
 dbms = CSV
 ;
run;   /* DATA RELATED TO AIR STATIONS */

proc print data = retake2.air_stations_Nov2017;
run;


proc import datafile = 'C:/Users/kgupta/Desktop/BUSINESS REPORTING TOOLS RETAKE/BRT_retake_data (2)/air_quality_Nov2017.csv'
 out = retake2.air_quality_17
 dbms = CSV
 ;
run;  /* DATA RELATED TO AIR QUALITY */

proc print data = retake2.air_quality_17;
run;

proc import datafile = 'C:/Users/kgupta/Desktop/BUSINESS REPORTING TOOLS RETAKE/BRT_retake_data (2)/accidents_2017.csv'
 out = retake2.accidents_2017
 dbms = CSV
 ;
run;  /* DATA RELATED TO ACCIDENTS */

proc print data = retake2.accidents_2017;
run;

/* Lets start with some basic statistics */

/* Query : Births in Barcelona based on Gender*/
proc sql;
create table retake2.birth_info as
select Year,Gender, sum(Number) as Birth_Count 
from retake2.Births
group by Year, Gender
;
quit;
run;

proc print data = retake2.birth_info;
run;

/*  Query : Births in Barcelona based on districts*/
proc sql;
create table retake2.birth_info_d as
select Year,District_Name, sum(Number) as Birth_Count 
from retake2.Births
group by Year, District_Name
;
quit;
run;

proc print data = retake2.birth_info_d;
run;

/* Query : Death in Barcelona based on Age*/
proc sql;
create table retake2.death_info as
select Year,Age, sum(Number) as Death_Count 
from retake2.Deaths
group by Year, Age
;
quit;
run;

proc print data = retake2.death_info;
run;

/* Query : Death in Barcelona based on Districts */
proc sql;
create table retake2.death_info_d as
select Year,District_Name, sum(Number) as Death_Count 
from retake2.Deaths
group by Year, District_Name
;
quit;
run;

proc print data = retake2.death_info_d;
run;

/* Query : Compare birth and death in different districts */

proc sql;
create table retake2.birth_death_info as
select b.District_Name, sum(b.Number) as Birth_Count, sum(d.Number) as Death_Count  
from retake2.Births as b, retake2.Deaths as d 
group by b.District_Name
;
quit;
run;

proc print data = retake2.birth_death_info;
run;

/* Now let us discuss about some general statistics about the city Barcelona */

/* Query : Most preferred transport in the city */

proc sql;
create table retake2.pre_trans as
select Transport, count(Transport) as Most_Preferred_Transport 
from retake2.Transports
group by Transport
;
quit;
run;

proc print data = retake2.pre_trans;
run;

/* Query : TOP female names preferred in Barcelona */

proc sql;
create table retake2.names_f as
select Name, Gender, sum(Frequency) as Most_Common_Names 
from retake2.Most_frequent_names
where Gender = 'Female' and Frequency > 10000
group by Name, Gender
;
quit;
run;

proc print data = retake2.names_f;
run;

/* Query : TOP male names preferred in Barcelona */

proc sql;
create table retake2.names as
select Name, Gender, sum(Frequency) as Most_Common_Names 
from retake2.Most_frequent_names
where Gender = 'Male' and Frequency > 10000
group by Name, Gender
;
quit;
run;

proc print data = retake2.names;
run;
/* Query : TOP male baby names preferred in Barcelona */

proc sql;
create table retake2.baby_names as
select Year,Name, Gender, sum(Frequency) as Most_Common_Baby_Names 
from retake2.Most_frequent_baby_names
where Gender = 'Male' and Frequency > 150
group by Year,Name, Gender
;
quit;
run;

proc print data = retake2.baby_names;
run;

/* Query : TOP female baby names preferred in Barcelona */

proc sql;
create table retake2.baby_names_f as
select Year,Name, Gender, sum(Frequency) as Most_Common_Baby_Names 
from retake2.Most_frequent_baby_names
where Gender = 'Female' and Frequency > 150
group by Year,Name, Gender
;
quit;
run;

proc print data = retake2.baby_names_f;
run;

/* Now let us discuss about a serious problem faced by all countries i.e. unemployment */

/* Query : Unemployment per month in the city of Barcelona*/

proc sql;
create table retake2.un_per_month as
select Year,Month,sum(Number) as Unemployment_count 
from retake2.Unemp
group by Year, Month
;
quit;
run;

proc print data = retake2.un_per_month;
run;
/* Query : Unemployment per gender in the city of Barcelona*/

proc sql;
create table retake2.un_per_gender as
select Year,Gender,sum(Number) as Unemployed_Count 
from retake2.Unemp
group by Year, Gender
;
quit;
run;

proc print data = retake2.un_per_gender;
run;

/* Query : Unemployment per district in the city of Barcelona*/

proc sql;
create table retake2.un_per_districts as
select District_Name,sum(Number) as Unemployment_count 
from retake2.Unemp
group by District_Name
;
quit;
run;

proc print data = retake2.un_per_districts;
run;


/* Let us discuss about deaths based on air quality and accidents*/

/*  Query : Deaths based on accidents on different weekdays*/
proc sql;
create table retake2.acci_week as
select District_Name, Weekday, sum(Victims) as dead_people 
from retake2.Accidents_2017
group by District_Name, Weekday
;
quit;
run;

proc print data = retake2.acci_week;
run;

/*  Query : Deaths based on accidents on different months*/

proc sql;
create table retake2.acci_month as
select District_Name,Month, sum(Victims) as dead_people 
from retake2.Accidents_2017
group by District_Name, Month
;
quit;
run;

proc print data = retake2.acci_month;
run;

/*  Query : Deaths based on accidents on different parts of day*/

proc sql;
create table retake2.acci_part_day as
select District_Name,Part_of_the_day, sum(Victims) as dead_people 
from retake2.Accidents_2017
group by District_Name, Part_of_the_day
;
quit;
run;

proc print data = retake2.acci_part_day;
run;

/*  Query : Serious injuries based on accidents on different weekdays*/

proc sql;
create table retake2.ser_inj_week as
select District_Name, Weekday, sum(Serious_injuries) as seriously_injured_people 
from retake2.Accidents_2017
group by District_Name, Weekday
;
quit;
run;

proc print data = retake2.ser_inj_week;
run;

/*  Query : Serious injuries based on accidents on different months*/

proc sql;
create table retake2.ser_inj_month as
select District_Name,Month, sum(Serious_injuries) as seriously_injured_people  
from retake2.Accidents_2017
group by District_Name, Month
;
quit;
run;

proc print data = retake2.ser_inj_month;
run;

/*  Query : Serious injuries based on accidents on different parts of the day*/


proc sql;
create table retake2.ser_inj_part_day as
select District_Name,Part_of_the_day, sum(Serious_injuries) as seriously_injured_people 
from retake2.Accidents_2017
group by District_Name, Part_of_the_day
;
quit;
run;

proc print data = retake2.ser_inj_part_day;
run;

/*  Query : mild injuries based on accidents on different weekdays*/

proc sql;
create table retake2.mild_inj_week as
select District_Name, Weekday, sum(Mild_injuries) as mildly_injured_people 
from retake2.Accidents_2017
group by District_Name, Weekday
;
quit;
run;

proc print data = retake2.mild_inj_week;
run;

/*  Query : mild injuries based on accidents on different months*/

proc sql;
create table retake2.mild_inj_month as
select District_Name,Month, sum(Mild_injuries) as mildly_injured_people   
from retake2.Accidents_2017
group by District_Name, Month
;
quit;
run;

proc print data = retake2.mild_inj_month;
run;

/*  Query : mild injuries based on accidents on different parts of the day*/


proc sql;
create table retake2.mild_inj_part_day as
select District_Name,Part_of_the_day, sum(Mild_injuries) as mildly_injured_people 
from retake2.Accidents_2017
group by District_Name, Part_of_the_day
;
quit;
run;

proc print data = retake2.mild_inj_part_day;
run;


/* Lets discuss about the deaths which are caused due to various types of air quality. */

/* First let's join Air quality and Air station table using inner join */

proc sql;
create table retake2.Final_air_quality_table as
SELECT * 
FROM retake2.Air_quality_17
INNER JOIN retake2.Air_stations_nov2017
ON Air_quality_17.Station = Air_stations_nov2017.Station;
quit;
run;

proc print data = retake2.Final_air_quality_table;
run;

/* deaths based on NO2 Quality*/

proc sql;
create table retake2.NO2_deaths as
SELECT f.District_Name, f.NO2_Quality, sum(d.Number) as total_deaths
FROM retake2.Final_air_quality_table as f
INNER JOIN retake2.Deaths as d
ON f.District_Name = d.DIstrict_Name
group by f.District_Name,f.NO2_Quality;
quit;
run;

proc print data = retake2.NO2_deaths;
run;

/* deaths based on O3 Quality*/

proc sql;
create table retake2.O3_deaths as
SELECT f.District_Name, f.O3_Quality, sum(d.Number) as total_deaths
FROM retake2.Final_air_quality_table as f
INNER JOIN retake2.Deaths as d
ON f.District_Name = d.DIstrict_Name
group by f.District_Name,f.O3_Quality;
quit;
run;

proc print data = retake2.O3_deaths;
run;

/* deaths based on Air Quality*/

proc sql;
create table retake2.Air_Quality_deaths as
SELECT f.District_Name, f.Air_Quality, sum(d.Number) as total_deaths
FROM retake2.Final_air_quality_table as f
INNER JOIN retake2.Deaths as d
ON f.District_Name = d.DIstrict_Name
group by f.District_Name,f.Air_Quality;
quit;
run;

proc print data = retake2.Air_Quality_deaths;
run;

/* Lets now discuss about the immigrants and emmigrants in Barcelona */

/* Let us join two tables i.e. Immigrants by destination and Immigrants by destination 2 */

proc sql;
create table retake2.Final_immigration as
SELECT *
FROM retake2.Immigrants_des as I1
INNER JOIN retake2.Immigrants_des_2 as I2
ON I1.from = I2.from;
quit;
run;

proc print data = retake2.Final_immigration;
run;

/* Query : Grouping Immigrants based on states or countries from which they came to barcelona */

proc sql;
create table retake2.out_immi as
SELECT to, from, sum(weight)
FROM retake2.Final_immigration 
group by to,from;
quit;
run;

proc print data = retake2.out_immi;
run;

/* Query : Most common nationality of the immigrants in Barcelona */

proc sql;
create table retake2.nati_immi as
SELECT Nationality, sum(Number) as top_nationality
FROM retake2.Immigrants_by_nationality
group by Nationality
having sum(Number) > 5000;
quit;
run;

proc print data = retake2.nati_immi;
run;

/* Lets have an age comparison for age of immigrants and emmigrants in Barcelona */

proc sql;
create table retake2.age_immi_emmi as
SELECT Age, sum(Immigrants) as immigrants_age_category, sum(Emigrants) as Emigrants_age_category
FROM retake2.Immigrants_emigrants_by_age
group by Age
;
quit;
run;

proc print data = retake2.age_immi_emmi;
run;

/* Finally let us compare Immigrants and Emigrants based on Gender*/

proc sql;
create table retake2.Gender_immi_emmi as
SELECT Gender, sum(Immigrants) as immigrants_Gender, sum(Emigrants) as Emigrants_Gender
FROM retake2.Immigrants_emigrants_by_sex
group by Gender
;
quit;
run;

proc print data = retake2.Gender_immi_emmi;
run;

/* Link of published story */

/*https://prod-apsoutheast-a.online.tableau.com/t/kanishkguptaatiesegfr/views/Kanishk_Final/DETAILED_CITY_STORY_REPORT/kanishk.gupta@ieseg.fr/e5c6252d-0cab-4b62-bb2e-b4b2fe9ac2a6?:display_count=n&:showVizHome=n&:origin=viz_share_link  */