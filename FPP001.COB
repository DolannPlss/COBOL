       IDENTIFICATION DIVISION.
       PROGRAM-ID. FPP001.
      *AUTHOR. MARCOS THADEU OLIVEIRA DA SILVA
      ********************************************************
      * CADASTRO DE DEPARTAMENTO  *
      ********************************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
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
       77 W-CONT          PIC 9(06) VALUE ZEROS.
       77 W-OPCAO         PIC X(01) VALUE SPACES.
       77 W-ACT           PIC 9(02) VALUE ZEROS.
       77 MENS            PIC X(50) VALUE SPACES.
       77 LIMPA           PIC X(50) VALUE SPACES. 
       01 ST-ERRO         PIC X(02) VALUE "00".
       01 W-SEL           PIC 9(01) VALUE ZEROS.
       01 IND             PIC 9(02) VALUE 1.

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

        
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  TELAPRINCIPAL.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                         -- CADATRO DE D".
           05  LINE 02  COLUMN 41 
               VALUE  "EPARTAMENTO --".
           05  LINE 06  COLUMN 01 
               VALUE  " CODIGO DO DEPARTAMENTO:".
           05  LINE 08  COLUMN 01 
               VALUE  " DENOMINACAO DO DEPARTAMENTO:".
           05  LINE 10  COLUMN 01 
               VALUE  " NIVEL HIERARQUICO:".
           05  LINE 12  COLUMN 01 
               VALUE  " STATUS:".
           05  LINE 23  COLUMN 01 
               VALUE  "MENSAGEM:".

           05  TCODDEP
               LINE 06  COLUMN 26  PIC 999
               USING  CODDEP
               HIGHLIGHT.
           05  TDENDEP
               LINE 08  COLUMN 31  PIC x(25)
               USING  DENDEP
               HIGHLIGHT.
           05  TNVL
               LINE 10  COLUMN 21  PIC 99
               USING  NVL
               HIGHLIGHT.
           05  TSTAT
               LINE 12  COLUMN 10  PIC X(01)
               USING  STAT
               HIGHLIGHT.





      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
           
       R0.
           OPEN I-O CADDEP
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADDEP
                 CLOSE CADDEP
                 MOVE "*** ARQUIVO CADDEP FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                IF ST-ERRO = "95"
                    MOVE "*** ISAM NAO EXCUTADO NO INICIO! **" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO ROT-FIM
                ELSE
                    MOVE "ERRO NA ABERTURA DO ARQUIVO CADDEP" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
      * 
      *------------[ INICIALIZACAO DAS VARIAVEIS ]----------------------               
       R1A.
           MOVE SPACES TO DENDEP STAT 
           MOVE ZEROS TO CODDEP NVL W-SEL
      *-------------[VISUALIZACAO DA TELA]--------------------
           DISPLAY TELAPRINCIPAL.
       R1.
           ACCEPT TCODDEP
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO ROT-FIM.

           IF CODDEP < 1
              MOVE "CODDEP INVALIDO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R1.
       LER-CADDEP.
           READ CADDEP
           IF ST-ERRO NOT = "23"
              IF ST-ERRO = "00"
                DISPLAY TELAPRINCIPAL
                MOVE "*** DEPARTAMENTO JA CADASTRADO ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R1
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO CADDEP" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                MOVE "*** DEPARTAMENTO NAO CADASTRADO ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM.
           

       R2.
           ACCEPT TDENDEP
           IF DENDEP = SPACES
              MOVE "CAMPO DENDEP VAZIO!" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R2.
       R3.
           ACCEPT TNVL.
       R3A.
           MOVE TBNIVEL(IND) TO TXTNIVEL
           IF TXTNIVEL1 NOT = NVL
              ADD 1 TO IND
              IF IND < 10
                 GO TO R3A
              ELSE
                 DISPLAY (23, 14) "NIVEL HIERARQUICO INVALIDO"
                 MOVE 1 TO IND
                 GO TO R3
           ELSE
               MOVE 1 TO IND
               DISPLAY (10, 23) TXTNIVEL2.
       R4.
           ACCEPT TSTAT.
       R4A.
           MOVE TBSTATUS(IND) TO TXTSTATUS
           IF TXTSTATUS1 NOT = STAT
              ADD 1 TO IND
              IF IND < 4
                 GO TO R4A
              ELSE
                 DISPLAY (23, 14) "STATUS INVALIDO"
                 MOVE 1 TO IND
                 GO TO R4
           ELSE
               MOVE 1 TO IND
               DISPLAY (12, 10) TXTSTATUS2.
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
                WRITE REGDEP
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO R1.
                IF ST-ERRO = "22"
                  MOVE "* DEP EXISTE,DADOS NAO GRAVADOS *" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO R1
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO CADDEP" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.
      *
      *----------------------------------------------------------------
       ROT-FIM.
           CLOSE CADDEP.
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