       IDENTIFICATION DIVISION.
       PROGRAM-ID. FPP014.
      *AUTHOR. MARCOS THADEU OLIVEIRA DA SILVA
      ********************************************************
      * CONSULTA DE FUNCIONARIOS *
      ********************************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADFUNC ASSIGN TO DISK
                      ORGANIZATION IS INDEXED
                      ACCESS MODE  IS DYNAMIC
                      RECORD KEY   IS CODFUNC
                      ALTERNATE RECORD KEY IS NOME
                      WITH DUPLICATES
                      ALTERNATE RECORD KEY IS DATANASC
                      WITH DUPLICATES
                      ALTERNATE RECORD KEY IS FUNCCEP
                      WITH DUPLICATES
                      FILE STATUS  IS ST-ERRO.
      *
           SELECT CADCAR ASSIGN TO DISK
                      ORGANIZATION IS INDEXED
                      ACCESS MODE  IS DYNAMIC
                      RECORD KEY   IS CODCAR
                      ALTERNATE RECORD KEY IS DENCAR
                      WITH DUPLICATES
                      FILE STATUS  IS ST-ERRO.
      
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADFUNC
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQFUNC.DAT". 
       01 REGFUNC.
          03 CODFUNC                  PIC 9(06).
          03 NOME                     PIC X(30).
          03 DATANASC                 PIC X(08).
          03 SEXO                     PIC X(01).
          03 GENERO                   PIC X(01).
          03 FUNCDEP                  PIC 9(03).
          03 FUNCCAR                  PIC 9(03).
          03 NVLSAL                   PIC 9(02).
          03 IMPRENDA                 PIC X(01).
          03 QUANTDEP                 PIC 9(01).
          03 FUNCCEP                  PIC 9(08).
          03 NUMRES                   PIC 9(04).
          03 COMPRES                  PIC X(12).
          03 FUNCSTAT                 PIC X(01).
      *-----------------------------------------------------------------
       FD CADCAR
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQCAR.DAT". 
       01 REGCAR.
          03 CODCAR                   PIC 9(03).
          03 DENCAR                   PIC X(25).
          03 TIPOSAL                  PIC X(01).
          03 SALBASE                  PIC 9(06)V99.
          03 STAT                     PIC X(01).
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
       01 CODENTER      PIC 9(06) VALUE ZEROS.
       01 NOMEENTER    PIC X(30) VALUE SPACES.
       01 IND         PIC 9(02) VALUE 1.

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

       01 TABTIPO.
          03 FILLER PIC X(13) VALUE "HHORISTA".
          03 FILLER PIC X(13) VALUE "DDIARISTA".
          03 FILLER PIC X(13) VALUE "MMENSALISTA".

          
       01 TABTIPOX   REDEFINES TABTIPO.
          03 TBTIPO  PIC X(13) OCCURS 3 TIMES.
      * 
       01 TXTTIPO.
          03 TXTTIPO1 PIC x(01) VALUE SPACES.
          03 TXTTIPO2 PIC X(12) VALUE SPACES.  
       
       01  DDMMAAAA.
          03 DDMMAAAA1 PIC X(01) VALUE SPACES.
          03 DDMMAAAA2 PIC X(03) VALUE SPACES.
          03 DDMMAAAA3 PIC X(05) VALUE SPACES.

       01  AAAAMMDD.
          03 AAAAMMDD1 PIC X(01) VALUE SPACES.
          03 AAAAMMDD2 PIC X(05) VALUE SPACES.
          03 AAAAMMDD3 PIC X(07) VALUE SPACES.

        
      *
      *************************
      * REPORT SECTION *
      *************************
      *
       01 DETALHE.
          03 DETCOD        PIC 999999.
          03 FILLER        PIC X(01) VALUE SPACES.
          03 DETNOME       PIC X(30) VALUE SPACES.
          03 FILLER        PIC X(01) VALUE SPACES.
          03 DETTIPO       PIC X(01) VALUE ZEROS.
          03 FILLER        PIC X(01) VALUE SPACES.
          03 DETTXTTIPO    PIC X(13) VALUE SPACES.
          03 FILLER        PIC X(01) VALUE SPACES.
          03 DETSALBASE    PIC 9(06)V99 VALUE ZEROS.
          03 FILLER        PIC X(05) VALUE SPACES.
          03 DETSTATUS     PIC X(01) VALUE SPACES.
          03 FILLER        PIC X(01) VALUE SPACES.
          03 DETTXTSTATUS  PIC X(12) VALUE SPACES.
      *
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  TELACONSULTA.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                       -- CONSULTA DE FU".
           05  LINE 02  COLUMN 41 
               VALUE  "NCIONARIOS --".
           05  LINE 04  COLUMN 01 
               VALUE  "CODIGO          NOME                 TIP".
           05  LINE 04  COLUMN 41 
               VALUE  "O SALARIO     SALBASE        STATUS".
           
           05  TCODENTER
               LINE 06  COLUMN 01  PIC 9(06)
               USING  CODENTER
               HIGHLIGHT.
           05  TNOMEENTER
               LINE 06  COLUMN 08  PIC X(25)
               USING  NOMEENTER
               HIGHLIGHT.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
           OPEN INPUT  CADFUNC 
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "ARQUIVO DE CADFUNC NAO EXISTE" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2
              ELSE
                IF ST-ERRO = "95"
                    MOVE "ISAM NAO CARREGADO" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ROT-FIM2
                ELSE
                  MOVE "ERRO ABERTURA DO ARQUIVO CADFUNC"  TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM2.
           OPEN INPUT  CADCAR
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "ARQUIVO DE CADFUNC NAO EXISTE" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2
              ELSE
                IF ST-ERRO = "95"
                    MOVE "ISAM NAO CARREGADO" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ROT-FIM2
                ELSE
                  MOVE "ERRO ABERTURA DO ARQUIVO CADFUNC"  TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM2.



      *
       INC-001.
           MOVE ZEROS TO CODENTER
           MOVE SPACES TO NOMEENTER
           DISPLAY  TELACONSULTA.
       INC-001A.
           DISPLAY (23, 13)
            "SOLIC.=> ESC=ENCERRA CONSULTA F1=CONSULTA P/NOME"
           ACCEPT TCODENTER
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                      CLOSE CADCAR CADFUNC
                      GO TO ROT-FIM.
           IF W-ACT = 02
                      GO TO INC-001B.
           IF W-ACT > 02
                      MOVE "*** FUNCAO NAO DEFINIDA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001A.
           MOVE CODENTER TO CODFUNC
           START CADFUNC KEY IS NOT LESS CODFUNC INVALID KEY
                 MOVE "*** CHAVE NAO ENCONTRADA ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001.
           GO TO INC-RD2.
       INC-001B.
           DISPLAY (23, 13)
            "SOLIC.=> ESC=ENCERRA CONSULTA   F1=CONSULTA P/CODIGO "
           ACCEPT TNOMEENTER
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                      CLOSE CADFUNC CADCAR
                      GO TO ROT-FIM.
           IF W-ACT = 02
                      GO TO INC-001A.
           IF W-ACT > 02
                      MOVE "*** FUNCAO NAO DEFINIDA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001A.
           MOVE NOMEENTER TO NOME
           START CADFUNC KEY IS NOT LESS NOME INVALID KEY
                 MOVE "*** NOME NAO ENCONTRADO ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001B.
       INC-RD2.

           DISPLAY (23, 13) LIMPA
           READ CADFUNC NEXT
           IF ST-ERRO NOT = "00"

              IF ST-ERRO = "10"
                 MOVE "*** FIM DO ARQUIVO  1***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO NOV-SOL
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO CADFUNC"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
              NEXT SENTENCE.

       R3.
           MOVE TBTIPO(IND) TO TXTTIPO
           IF TXTTIPO1 NOT = TIPOSAL
              ADD 1 TO IND
              IF IND < 4
                 GO TO R3
              ELSE
                 MOVE 1 TO IND
           ELSE
               MOVE 1 TO IND
               MOVE TXTTIPO2 TO DETTXTTIPO.

       R4.
           MOVE TBSTATUS(IND) TO TXTSTATUS
           IF TXTSTATUS1 NOT = FUNCSTAT
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
           MOVE FUNCCAR TO CODCAR.
           READ CADCAR
           MOVE CODFUNC TO DETCOD
           MOVE NOME TO DETNOME
           MOVE TIPOSAL TO DETTIPO
           MOVE SALBASE TO DETSALBASE
           MOVE FUNCSTAT TO DETSTATUS
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
           CLOSE CADFUNC CADCAR.
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