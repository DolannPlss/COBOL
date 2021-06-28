       IDENTIFICATION DIVISION.
       PROGRAM-ID. FPP011.
      *AUTHOR. MARCOS THADEU OLIVEIRA DA SILVA
      ***************************************************
      * CONSULTA DE ENDERECAMENTO CEP *
      ***************************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *
       SELECT CADDEP ASSIGN TO DISK
                      ORGANIZATION IS INDEXED
                      ACCESS MODE  IS DYNAMIC
                      RECORD KEY   IS CODDEP
                      ALTERNATE RECORD KEY IS DENDEP
                      WITH DUPLICATES
                      FILE STATUS  IS ST-ERRO.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADDEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQDEP.DAT". 
       01 REGDEP.
          03 CODDEP                   PIC 9(03).
          03 DENDEP                   PIC X(25).
          03 NVL                      PIC 9(02).
          03 STAT                     PIC X(01).
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01 W-SEL             PIC 9(01) VALUE ZEROS.
       01 W-CONT         PIC 9(04) VALUE ZEROS.
       01 W-OPCAO      PIC X(01) VALUE SPACES.
       01 ST-ERRO        PIC X(02) VALUE "00".
       01 W-ACT            PIC 9(02) VALUE ZEROS.
       01 MENS             PIC X(50) VALUE SPACES.
       01 LIMPA             PIC X(55) VALUE SPACES.
       01 SOLIC             PIC X(20) VALUE SPACES.
       01 CONLIN          PIC 9(03) VALUE 001.
       01 CODENTER      PIC 9(03) VALUE ZEROS.
       01 DENENTER    PIC X(25) VALUE SPACES.
       01 IND         PIC 9(02) VALUE 1.

       01 TABNIVEL.
          03 FILLER PIC X(28) VALUE "01CONSELHO CONSULTIVO".
          03 FILLER PIC X(28) VALUE "02CONSELHO FISCAL".
          03 FILLER PIC X(28) VALUE "03PRESIDENCIA".
          03 FILLER PIC X(28) VALUE "04DIRETORIA ADMINISTRATIVA".
          03 FILLER PIC X(28) VALUE "05DIRETORIA INDUSTRIAL".
          03 FILLER PIC X(28) VALUE "06DIRETORIA COMERCIAL".
          03 FILLER PIC X(28) VALUE "07DIRETORIA MARKETING".
          03 FILLER PIC X(28) VALUE "08DIRETORIA JURIDICA".
          03 FILLER PIC X(28) VALUE "09CONSULTORIA".

          
       01 TABNIVELX   REDEFINES TABNIVEL.
          03 TBNIVEL  PIC X(28) OCCURS 9 TIMES.
      * 
       01 TXTNIVEL.
          03 TXTNIVEL1 PIC x(02) VALUE SPACES.
          03 TXTNIVEL2 PIC X(26) VALUE SPACES. 

       01 TABSTATUS.
          03 FILLER PIC X(13) VALUE "AATIVA".
          03 FILLER PIC X(13) VALUE "SSUSPENSA".
          03 FILLER PIC X(13) VALUE "DDESATIVADA".

          
       01 TABSTAUTSX   REDEFINES TABSTATUS.
          03 TBSTATUS  PIC X(13) OCCURS 3 TIMES.
      * 
       01 TXTSTATUS.
          03 TXTSTATUS1 PIC x(01) VALUE SPACES.
          03 TXTSTATUS2 PIC X(12) VALUE SPACES. 

        
      *
      *************************
      * REPORT SECTION *
      *************************
      *
       01 DETALHE.
          03 DETCOD        PIC 999.
          03 FILLER        PIC X(06) VALUE SPACES.
          03 DETDEN        PIC X(25) VALUE SPACES.
          03 FILLER        PIC X(01) VALUE SPACES.
          03 DETNIVEL      PIC 9(02) VALUE ZEROS.
          03 FILLER        PIC X(01) VALUE SPACES.
          03 DETTXTNIVEL   PIC X(26) VALUE SPACES.
          03 FILLER        PIC X(01) VALUE SPACES.
          03 DETSTATUS     PIC X(01) VALUE SPACES.
          03 FILLER        PIC X(01) VALUE SPACES.
          03 DETTXTSTATUS  PIC X(12) VALUE SPACES.
      *
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  TELACONSULTA.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                       -- CONSULTA DE DE".
           05  LINE 02  COLUMN 41 
               VALUE  "PARTAMENTOS --".
           05  LINE 04  COLUMN 01 
               VALUE  "CODIGO      DEPARTAMENTO             NIV".
           05  LINE 04  COLUMN 41 
               VALUE  "EL HIERARQUICO              STATUS".
           
           05  TCODENTER
               LINE 06  COLUMN 01  PIC 9(03)
               USING  CODENTER
               HIGHLIGHT.
           05  TDENENTER
               LINE 06  COLUMN 08  PIC X(25)
               USING  DENENTER
               HIGHLIGHT.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP1.
           OPEN INPUT  CADDEP
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "ARQUIVO DE CADDEP NAO EXISTE" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2
              ELSE
                IF ST-ERRO = "95"
                    MOVE "ISAM NAO CARREGADO" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ROT-FIM2
                ELSE
                  MOVE "ERRO ABERTURA DO ARQUIVO CADDEP"  TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM2.
      *
       INC-001.
           MOVE ZEROS TO CODENTER
           MOVE SPACES TO DENENTER
           DISPLAY  TELACONSULTA.
       INC-001A.
           DISPLAY (23, 13)
            "SOLIC.=> ESC=ENCERRA CONSULTA F1=CONSULTA P/DENOMINACAO"
           ACCEPT TCODENTER
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                      CLOSE CADDEP
                      GO TO ROT-FIM.
           IF W-ACT = 02
                      GO TO INC-001B.
           IF W-ACT > 02
                      MOVE "*** FUNCAO NAO DEFINIDA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001A.
           MOVE CODENTER TO CODDEP
           START CADDEP KEY IS NOT LESS CODDEP INVALID KEY
                 MOVE "*** CHAVE NAO ENCONTRADA ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001.
           GO TO INC-RD2.
       INC-001B.
           DISPLAY (23, 13)
            "SOLIC.=> ESC=ENCERRA CONSULTA   F1=CONSULTA P/CODIGO "
           ACCEPT TDENENTER
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                      CLOSE CADDEP
                      GO TO ROT-FIM.
           IF W-ACT = 02
                      GO TO INC-001A.
           IF W-ACT > 02
                      MOVE "*** FUNCAO NAO DEFINIDA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001A.
           MOVE DENENTER TO DENDEP
           START CADDEP KEY IS NOT LESS DENDEP INVALID KEY
                 MOVE "*** DENOMINACAO NAO ENCONTRADA ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001B.
       INC-RD2.
           DISPLAY (23, 13) LIMPA
           READ CADDEP NEXT
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "10"
                 MOVE "*** FIM DO ARQUIVO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO NOV-SOL
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO CADDEP"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
              NEXT SENTENCE.

       R3.
           MOVE TBNIVEL(IND) TO TXTNIVEL
           IF TXTNIVEL1 NOT = NVL
              ADD 1 TO IND
              IF IND < 10
                 GO TO R3
              ELSE
                 MOVE 1 TO IND
           ELSE
               MOVE 1 TO IND
               MOVE TXTNIVEL2 TO DETTXTNIVEL.

       R4.
           MOVE TBSTATUS(IND) TO TXTSTATUS
           IF TXTSTATUS1 NOT = STAT
              ADD 1 TO IND
              IF IND < 4
                 GO TO R4
              ELSE
                 MOVE 1 TO IND
           ELSE
               MOVE 1 TO IND
               MOVE TXTSTATUS2 TO DETTXTSTATUS.
       R5.




       ROT-MONTAR.
           MOVE CODDEP TO DETCOD
           MOVE DENDEP TO DETDEN
           MOVE NVL TO DETNIVEL
           MOVE STAT TO DETSTATUS
           PERFORM R3 THRU R5
           COMPUTE LIN = CONLIN + 5
           DISPLAY (LIN, 01) DETALHE
           ADD 1 TO CONLIN
           IF CONLIN < 16
                  GO TO INC-RD2.
      *



       ROT-SOL.
           DISPLAY (23, 13) " *** CONTINUA CONSULTA : . ***"
           MOVE "S" TO W-OPCAO
           ACCEPT  (23, 38) W-OPCAO  WITH UPDATE
           IF W-OPCAO = "S"
                  MOVE 01 TO CONLIN
                  DISPLAY TELACONSULTA
                  GO TO INC-RD2.
           IF W-OPCAO NOT = "N"
                  MOVE "*** DIGITE S=SIM   N=NAO ***" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-SOL.
           MOVE 01 TO CONLIN.
           GO TO INC-001.
      *
       NOV-SOL.
           DISPLAY (23, 13) " *** NOVA CONSULTA : . ***"
           MOVE "S" TO W-OPCAO
           ACCEPT  (23, 34) W-OPCAO WITH UPDATE
           IF W-OPCAO = "S"
                  MOVE 01 TO CONLIN
                  GO TO INC-001.
           IF W-OPCAO NOT = "N"
                  MOVE "*** DIGITE S=SIM   N=NAO ***" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO NOV-SOL.
      *
      **********************
      * ROTINA DE FIM      *
      **********************
      *
       ROT-FIM.
           DISPLAY (01 01) ERASE.
           CLOSE CADDEP.
       ROT-FIM2.
           EXIT PROGRAM.
       ROT-FIM3.
           STOP RUN.
      *
      **********************
      * ROTINA DE MENSAGEM *
      **********************
      *
       ROT-MENS.
           MOVE ZEROS TO W-CONT.
       ROT-MENS1.
           DISPLAY (23, 13) MENS.
       ROT-MENS2.
           ADD 1 TO W-CONT
           IF W-CONT < 1000
              GO TO ROT-MENS2
           ELSE
              DISPLAY (23, 12) LIMPA.
       ROT-MENS-FIM.
           EXIT.
       FIM-ROT-MENS.
      *
