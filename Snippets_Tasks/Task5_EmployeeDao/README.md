# Object-oriented `EMPLOYEE` table task implementation utilizing DAOs and DB management class

Original meeting notes (in Serbian):
> sta da bude potpuno staticki, sta ima smisla da bude u instanci (da iinstanciramo zaposlenog), show nema smisla da bude instanca - to je potpuno staticki, mozda delete i update da budu i u instanci  
> za sve radnje mora da se selektuju data iz DB, mozda u ovom momentu bi imalo smisla da se koriste DAO klase, pojam buffering-a (buffer-ujemo, kad se krene rad sa programom da se ucita u internu tabelu da se ucita DB i da je uvek ucitan, da ne radimo uvek select) i da se uvek koristi ta internat tabela i na kraju se ona sacuva - dobar best practice  
> u samoj logici programa, kao bafering da postoji globalna tabela koja se ucita pri pokretanju i onda se uvek pita da li je sadrzaj ucitan, poziva se metoda iz DAO-a, a ako tabela ima podatke nista, sve imamo, sve kasnije radimo sa DAO  
> LOOP table i ako su slogovi isti u obe preskoce se  
> DAO dakle sadrzi celu tabelu, sve podatke iz DB  

Following is the reason behind the proposed solution.

## Static Vs Non-Static Methods

Approach taken creates a DB management class `znv_cl_task1_empl_db_man` which takes in appropriate parameters for performing selected tasks. 

Two groups of methods are observed: those that require the entire table row (`update`) and those that do not (all others). When planning the `znv_cl_task1_employee_dao` class a question of which of the two groups, if any, is appropriate for inclusion. It seemed most appropraite to extract all the DB manipulation tools to a self-contained class which would then be fed the required data.

In that sense, this DB management class contains only static methods and takes in required parameters - an entire `znv_cl_task1_employee_dao` object, just an `ID`, or none (as is the case with `destroy` method).

On the other hand, the `znv_cl_task1_employee_dao` class contains all the employee data, `get`/`set` methods for all of those, a `constructor` method as well as a `get_employee_as_row` so that the format required by the DB management class is easily accessible.

## Buffering

After consulting [this article](https://gocoding.org/table-buffering-in-sap-abap/) and checking out the DB settings (`SE11`, Database Table, Technical Settings, (scroll down to) Buffering), I have learned that the DB buffering should obviously be done thusly, and not manually. There is danger of compromising data consistency so it all comes down to how often the data is accessed and in what way.

## DAO

Finally, DAO in my solution is a method of transport for the parameters picked up from the input form and composited into an instance of the appropriate class. In case of actions that only require `ID` it seemed redundant to create DAOs.
