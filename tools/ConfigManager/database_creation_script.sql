/********************* ROLES **********************/
/********************* UDFS ***********************/
/****************** SEQUENCES ********************/
CREATE SEQUENCE GENERIC;
/******************** DOMAINS *********************/
/******************* PROCEDURES ******************/
/******************** TABLES **********************/
CREATE TABLE COMPILERS
(
  COMPILER_ID integer NOT NULL,
  NAME varchar(255) NOT NULL,
  CONSTRAINT COMPILERS_PK PRIMARY KEY (COMPILER_ID)
);
CREATE TABLE PAIRING
(
  COMPILER_ID integer NOT NULL,
  SUITE_ID integer NOT NULL,
  ID integer NOT NULL,
  CONSTRAINT PAIRING_PK PRIMARY KEY (ID)
);
CREATE TABLE SUITES
(
  SUITE_ID integer NOT NULL,
  SUITENAME varchar(50) NOT NULL,
  PROTOCOL varchar(20),
  HOST varchar(50),
  PORT integer,
  "DATABASE" varchar(255),
  "PASSWORD" varchar(255),
  DO_REBUILD char(3),
  USERNAME varchar(255),
  DELIMITER_TYPE varchar(20),
  DELIMITER char(1),
  PROPERTIES blob sub_type 1,
  CREATE_SCRIPTS blob sub_type 1,
  DROP_SCRIPTS blob sub_type 1,
  SHORTNAME varchar(255),
  CONSTRAINT SUITES_PK PRIMARY KEY (SUITE_ID),
  CONSTRAINT SUITES_UK1 UNIQUE (SHORTNAME),
  CONSTRAINT SUITES_UK2 UNIQUE (SUITENAME)
);
/********************* VIEWS **********************/
/******************* EXCEPTIONS *******************/
/******************** TRIGGERS ********************/
ALTER TABLE PAIRING ADD CONSTRAINT PAIRING_COMPILERS_FK
  FOREIGN KEY (COMPILER_ID) REFERENCES COMPILERS (COMPILER_ID);
ALTER TABLE PAIRING ADD CONSTRAINT PAIRING_SUITES_FK
  FOREIGN KEY (SUITE_ID) REFERENCES SUITES (SUITE_ID);
GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON COMPILERS TO  SYSDBA WITH GRANT OPTION;
GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON PAIRING TO  SYSDBA WITH GRANT OPTION;
GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON SUITES TO  SYSDBA WITH GRANT OPTION;