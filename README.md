# ratdbtools package overview

This is a package of functions for use with the RATTUS project database. Currently it is focused on importing and exporting tables between different formats (Google, MySQL, CSV dump) but should expand to cover typical queries, map outputs, etc.

**Scroll to the bottom for the generic bitbucket readme instructions on using the repo**

### Installing ratdbtools

Since the repo is structured as a package, you can install from inside R using the **devtools** package:

(NB this can all be done through github now, use install_github("York-Rattus/ratdbtools")

1. Install devtools: `install.packages("devtools")`
2. Load the devtools package: `library(devtools)`
3. Use it to install ratdbtools: `install_github("York-Rattus/ratdbtools")`

## Functions

### 'Pull' functions
These import a version of the RATTUS database from the specified location, and save it to the working environment as a list of data.tables.

#### rattusPull - import database from MySQL
`x <- rattusPull(connection)`

* **connection** MySQLConnection object defining the database connection to read from. Use the `dbConnect` function to create a connecton object using the RATTUS project credentials.

* **bestDate** Should a set of columns be added to SPECIMEN to show the best available dating info drawn from across all tables? (Defaults to TRUE)
* **bestSpecies** Should a set of columns be added to SPECIMEN to show the most confident species ID available? (Defaults to TRUE)
* **prepMetrics** Should the fusion status of the relevant end(s) for each metric value be worked out and appended to the METRICS table? (Defaults to FALSE)

#### rattusDecode - decode lookup columns in an R copy of the database
`x <- rattusDecode(db)`

* **db** An R copy of the database, as created by one of the rattusPull functions.
* **lookups** Should true lookup fields be decoded? True by default
* **keyfields** Character vector of tables (from PHASE, SPECIMEN, ASSEMBLAGE) into which key fields should be merged.
  
#### rattusPullGoogle - import database from Google Sheets version (now unlikely to be used)
`x <- rattusPull(litMod, specMod)`

* **litMod** URI for the site and literature database module (default provided).
* **specMod** URI for the specimen and sample database module (default provided).

#### rattusPullFrom Disk - import database from .csv dump
`x <- rattusPullFromDisk(path)`

* **path** path to folder in which the database dump (i.e. set of .csv files) is stored.

### rattusPush - write a version of the database from the R environment to an existing MySQL instance
Nb. this will only work with a local version; the UoY server won't allow it.
`rattusPush(db, connection, prompt = T, report = T, returnDroppedRows = T)`

* **db** a RATTUS database object, i.e. a list of data.tables as produced by `rattusPull` or similar.
* **connection** an object of class MySQLConnection (or MariaDBConnection).
* **prompt** if TRUE, will check with user before overwriting any non-empty tables.
* **report** if TRUE, will report the number of rows written to each table as the function runs.
* **returnDroppedRows** if TRUE, function returns a list of data.tables (potentially empty) representing the rows from each table which were not succesfully written to the SQL database.

### rattusDump - save a version of the database to disk as .csv files
(NB - this is not clean due to comma use in the data)

`rattusDump(db, path = NULL)`

* **db** a RATTUS database object, i.e. a list of data.tables as produced by `rattusPull` or similar.
* **path** the name (and if needed path) to a folder to save the tables in. If the folder doesn't exist it will be created. If NULL, the system date is used for the folder name instead.

---
