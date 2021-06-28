       IDENTIFICATION DIVISION.
       PROGRAM-ID. FPPMAIN.
      *AUTOR. MARCOS THADEU OLIVEIRA DA SILVA.
      *******************************************
      *  PROGRAMA MENU  *
      *******************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-ACT           PIC 9(02) VALUE ZEROS.
       01 MENU            PIC X(01) VALUE SPACES.
       SCREEN SECTION.
       01  TELAMENU.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                       -- MENU --".
           05  LINE 04  COLUMN 01 
               VALUE  "F1 - FPP001.COB".
           05  LINE 06  COLUMN 01 
               VALUE  "F2 - FPP002.COB".
           05  LINE 08  COLUMN 01
               VALUE  "F3 - FPP003.COB".
           05  LINE 10  COLUMN 01 
               VALUE  "F4 - FPP004.COB".
           05  LINE 12  COLUMN 01 
               VALUE  "F5 - FPP011.COB".
           05  LINE 14  COLUMN 01 
               VALUE  "F6 - FPP012.COB".
           05  LINE 16  COLUMN 01
               VALUE  "F7 - FPP013.COB".
           05  LINE 18  COLUMN 01 
               VALUE  "F8 - FPP014.COB".
           05  LINE 20  COLUMN 01
               VALUE  "F9 - CADCEP.COB".
           05  LINE 22  COLUMN 01 
               VALUE  "F10 - FPPREL.COB".




           
           05  TMENU
               LINE 01  COLUMN 01  PIC X(01)
               USING  MENU
               HIGHLIGHT.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
           DISPLAY  TELAMENU.
           ACCEPT TMENU
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
                   CALL "FPP001.COB".
           IF W-ACT = 03
                   CALL "FPP002.COB".
           IF W-ACT = 04
                   CALL "FPP003.COB".
           IF W-ACT = 05
                   CALL "FPP004.COB".
           IF W-ACT = 06
                   CALL "FPP011.COB".
           IF W-ACT = 07
                   CALL "FPP012.COB".
           IF W-ACT = 08
                   CALL "FPP013.COB".
           IF W-ACT = 09
                   CALL "FPP014.COB".
           IF W-ACT = 10
                   CALL "CADCEP.COB".
           IF W-ACT = 11
                   CALL "FPPREL.COB".
