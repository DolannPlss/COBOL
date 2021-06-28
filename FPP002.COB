       IDENTIFICATION DIVISION.
       PROGRAM-ID. FPP002.
      *AUTHOR. MARCOS THADEU OLIVEIRA DA SILVA
      ********************************************************
      * CADASTRO DE CARGOS *
      ********************************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
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
       FD CADCAR
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQCAR.DAT". 
       01 REGCAR.
          03 CODCAR                   PIC 9(03).
          03 DENCAR                   PIC X(25).
          03 TIPOSAL                  PIC X(01).
          03 SALBASE                  PIC 9(06)V99.
          03 STAT                     PIC X(01).
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-CONT          PIC 9(06) VALUE ZEROS.
       77 W-OPCAO         PIC X(01) VALUE SPACES.
       77 W-ACT           PIC 9(02) VALUE ZEROS.
       77 MENS            PIC X(50) VALUE SPACES.
       77 LIMPA           PIC X(50) VALUE SPACES. 
       01 ST-ERRO         PIC X(02) VALUE "00".
       01 W-SEL           PIC 9(01) VALUE ZEROS.
       01 IND             PIC 9(02) VALUE 1.

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
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  TELAPRINCIPAL.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                         --  CADATRO DE ".
           05  LINE 02  COLUMN 41 
               VALUE  "CARGOS --".
           05  LINE 06  COLUMN 01 
               VALUE  " CODIGO DO CARGO:".
           05  LINE 08  COLUMN 01 
               VALUE  " DENOMINACAO DO CARGO:".
           05  LINE 10  COLUMN 01 
               VALUE  " TIPO DE SALARIO:".
           05  LINE 12  COLUMN 01 
               VALUE  " SALARIO BASE:".
           05  LINE 14  COLUMN 01 
               VALUE  " STATUS:".
           05  LINE 23  COLUMN 01 
               VALUE  "MENSAGEM:".

           05  TCODCAR
               LINE 06  COLUMN 20  PIC 999
               USING  CODCAR
               HIGHLIGHT.
           05  TDENCAR
               LINE 08  COLUMN 24  PIC x(25)
               USING  DENCAR
               HIGHLIGHT.
           05  TTIPOSAL
               LINE 10  COLUMN 19  PIC x(01)
               USING  TIPOSAL
               HIGHLIGHT.
           05  TSALBASE
               LINE 12  COLUMN 17  PIC 9(06)V99
               USING  SALBASE
               HIGHLIGHT.
           05  TSTAT
               LINE 14  COLUMN 10  PIC X(01)
               USING  STAT
               HIGHLIGHT.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
           
       R0.
           OPEN I-O CADCAR
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADCAR
                 CLOSE CADCAR
                 MOVE "*** ARQUIVO CADCAR FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                IF ST-ERRO = "95"
                    MOVE "*** ISAM NAO EXCUTADO NO INICIO! **" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO ROT-FIM
                ELSE
                    MOVE "ERRO NA ABERTURA DO ARQUIVO CADCAR" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
      * 
      *------------[ INICIALIZACAO DAS VARIAVEIS ]----------------------               
       R1A.
           MOVE SPACES TO DENCAR TIPOSAL STAT 
           MOVE ZEROS TO CODCAR SALBASE W-SEL
      *-------------[VISUALIZACAO DA TELA]--------------------
           DISPLAY TELAPRINCIPAL.
       R1.
           ACCEPT TCODCAR
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO ROT-FIM.

           IF CODCAR < 1
              MOVE "CODCAR INVALIDO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R1.
       LER-CADCAR.
           READ CADCAR
           IF ST-ERRO NOT = "23"
              IF ST-ERRO = "00"
                DISPLAY TELAPRINCIPAL
                MOVE "*** CARGO JA CADASTRADO ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R1
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO CADCAR" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                MOVE "*** CARGO NAO CADASTRADO ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM.
           

       R2.
           ACCEPT TDENCAR
           IF DENCAR = SPACES
              MOVE "CAMPO VAZIO!" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R2.
       R3.
           ACCEPT TTIPOSAL.
       R3A.
           MOVE TBTIPO(IND) TO TXTTIPO
           IF TXTTIPO1 NOT = TIPOSAL
              ADD 1 TO IND
              IF IND < 4
                 GO TO R3A
              ELSE
                 DISPLAY (23, 14) "TIPO INVALIDO"
                 MOVE 1 TO IND
                 GO TO R3
           ELSE
               MOVE 1 TO IND
               DISPLAY (10, 19) TXTTIPO2.
       R4.
           ACCEPT TSALBASE

           IF TSALBASE < 1
              MOVE "SALARIO INVALIDO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R4.
       R5.
           ACCEPT TSTAT.
       R5A.
           MOVE TBSTATUS(IND) TO TXTSTATUS
           IF TXTSTATUS1 NOT = STAT
              ADD 1 TO IND
              IF IND < 4
                 GO TO R5A
              ELSE
                 DISPLAY (23, 14) "STATUS INVALIDO"
                 MOVE 1 TO IND
                 GO TO R5
           ELSE
               MOVE 1 TO IND
               DISPLAY (14, 10) TXTSTATUS2.
      * ------------- VERICAR SE E ALTERACAO -----------------

       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGCAR
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO R1.
                IF ST-ERRO = "22"
                  MOVE "* CARGO EXISTE,DADOS NAO GRAVADOS *" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO R1
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO CADCAR" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.
      *
      *----------------------------------------------------------------
       ROT-FIM.
           CLOSE CADCAR.
           STOP RUN.

      *---------[ ROTINA DE MENSAGEM ]---------------------   
       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (23, 12) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
                   MOVE SPACES TO MENS
                   DISPLAY (23, 12) MENS.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.



