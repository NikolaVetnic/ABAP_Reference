# j&s-soft Balkan 20221017 - Tasks

## 1 Introduction to HCM

Original meeting notes (in Serbian)
> Infotype : 2 (personal data), klik na "slicicu" (IMG ikonica) - Overview, vidi se slog sa podacima o radniku
> 
> Postoji Start / End - to je vreme vazenja podataka (period u kom su podaci vazeci). Pogledati koje vrste infotipova postoje - postoje tipovi gde se slogovi nadogradjuju jedni na druge bez rupa (ne mozes da u momentu vremena NEMAS ime i prezime), a postoje i oni "sa rupama" (npr. neka dopunska funkcija, tipa pomocnik ili savetnik ili nesto), takodje postoje i oni sa preklapanjima kada vazi vise slogova (recimo takodje funkcije - neko je i savetnik ali i direktor).
> 
> Od Kapitella 9, 10 ili 11 pocinje HCM pa to treba pogledati (rec je o dodatnoj dokumentaciji za koju mi je odobren pristup pre nekog vremena). Postoji glava sa infotipovima.
> 
> Svako bi mogao da kreira svoj infotip. Transkaciju treba naci solo (Create Infotype), moguce da je PM01 (/opm01 da se otvori u novom prozoru). Transakcija gde je moguce kreirati svoj infotip. Mora se kreirati odgovarajuca DB tabela, cak mozda i odgovarajucu strukturu (ali moguce i samo tabela), transakcija ce voditi korak po korak. 
> 
> Treba naci slobodne brojeve za infotip za sve nas... Otvara se DB explorer (SE11), odgovarajuci database je PA pa broj tipa, recimo PA0002 je za Personnel Data - dobija se odgovarajuca tabela. Ikonica levo od Tech Settings (Contents, CTRL+SHIFT+F10).
> 
> Svaki infotip ima dynpro pa treba i dynpro da se napise.
> 
> Moguce je da do ponedeljka nece biti gotovo ali treba raditi.

### 1.1 Solution

Infotype (IT) `9900` has been created, steps taken can be found [here](https://github.com/NikolaVetnic/ABAP_Reference/blob/master/xx_Reference/Infotypes.md).

## 2 `EMPLOYEE` cont'd

Original meeting notes (in Serbian):
> 1) promeniti naziv polja i pogledati sta se desava sa podacima
> 2) promeniti data element za nesto (za departman), umesto da pise recimo HR/MAN/MARK da se promeni element u CHAR duzine 3 ili nesto krace pa da se vidi sta se desava sa slogovima, posle ukloniti jedno polje iz DB i videti sta se desava
> 3) promeniti duzinu nekog polja da bude duze, npr. umesto CHAR duzine 10 da bude duzina 20

### 2.1 Solution

Results of actions taken are as follows:
1. upon changing the field name (`LASTNAME` to `SURNAME` in my case) table can be saved & activated but the data there is lost, in addition all the code using the old field name will be invalid - the most elegant solution is to simply change the Field Labels of the domain of the field in question
2. this subtask consists of two examples:
	* upon changing the length of the domain of `ZNV_TASK1_DEPARTMENT` field from `20` to `5`, saving brought up the following ["Take note" dialogue](http://nikolapacekvetnic.rs/wp-content/uploads/2022/10/Screenshot-2022-10-21-at-12.19.16.jpg), which then causes errors which prevent database database activation; solution to the problem was found [here](https://answers.sap.com/questions/7835363/increasing-the-length-of-domain.html), namely "...while doing changes make sure that you go to `se14` (database utility) to activate and adjust database..." - it is necessary to go to `SE14`, type in the name of the table and then scroll down to click on **Activate and Adjust the Database** button, after which it is possible to activate the database again
	* upon changing the length of the domain of `ZNV_TASK1_DEPARTMENT` field from `5` to `3`, the following errors were caused: `Length of fixed value MARK > maximum number of characters (3)`, i.e. it is impossible to reduce the number of characters further due to the existence of fixed values whose length is greater than the new domain length
3. changing the domain length (again, of `ZNV_TASK1_DEPARTMENT`) to a greater value brought up the "Take note" dialogue but otherwise it worked without a problem

## 3 Database Settings

Original meeting notes (in Serbian):
> Ukljuciti u Tech Settings Protokolirung: Goto / Technical Settings / ...
> Ukljuciti Log Changes u Tech Settings (Dictionary / Display Technical Settings) kod prikaza baze.

### 3.1 Solution

Results of actions taken are as follows:
1. within `SE11` I selected the `ZNV_TASK1_EMPL` table, displayed it, went to **Technical Settings** and changed the mode to *Change*, scrolled down and checked **Log Changes**

## 4 Maintenance View

Original meeting notes (in Serbian):
> TCode - SM30, popunjavac DB-a. Ako potrazimo tabelu necemo moci da je nadjemo (tj. tu tabelu koju smo mi kreirali), a sada fali da svi kreiramo Maintenance view (pogledati kako se to radi), hint: kad se udje u DB gore u Utilities / Table Maintenance Generator... U SM30 ce tada biti moguce rucno unositi slogove.

## 4.1 Solution
Results of actions taken are as follows:
1. used the `SM30` tcode to go to **Edit Table Views: Initial Screen** transaction, trying to display `ZNV_TASK1_EMPL` table caused the following error: `The maintenance dialog for ZNV... is incomplete or not defined`
2. go to `SE11` tcode, display `ZNV_TASK1_EMPL` table, head over to **Utilities / Table Maintenance Generator**, got up to [this dialogue](http://nikolapacekvetnic.rs/wp-content/uploads/2022/10/Screenshot-2022-10-21-at-12.52.53-scaled.jpg) and stopped there for the time until I research (or ask about) the options needed to be punched in here

## 5 View Cluster

Original meeting notes (in Serbian):
> Sledeci korak bi bio da se kreira View Cluster - ako bismo dodali dodali jos objekata/tabela u nas sistem, ako bismo zeleli sve da ih imamo u jednom app-u zapakujemo ih u View Cluster (svaki Generator zavrsi u View Cluster-u gde se ustvari podaci mogu kreirati bez da se izvrsava program).

## 5.1 Solution

Solution is pending until the parameters for the Maintenance View are filled out.
