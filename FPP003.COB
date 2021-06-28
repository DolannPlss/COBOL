       IDENTIFICATION DIVISION.
       PROGRAM-ID. FPP003.
      *AUTHOR. MARCOS THADEU OLIVEIRA DA SILVA
      ********************************************************
      * CADASTRO DE DEPENDENTES *
      ********************************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADDEPEN ASSIGN TO DISK
                      ORGANIZATION IS INDEXED
                      ACCESS MODE  IS DYNAMIC
                      RECORD KEY   IS DEPENKEY
                      ALTERNATE RECORD KEY IS DEPENCODFUNC
                      WITH DUPLICATES
                      ALTERNATE RECORD KEY IS NOMEDEPEN
                      WITH DUPLICATES
                      ALTERNATE RECORD KEY IS DEPENDATANASC
                      WITH DUPLICATES
                      FILE STATUS  IS ST-ERRO.
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
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
      *-----------------------------------------------------------------
       FD CADDEPEN
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQDEPEN.DAT". 
       01 REGDEPEN.   
          03 NOMEDEPEN                PIC X(30).
          03 GRAUPAREN                PIC 9(01).
          03 DEPENDATANASC            PIC 9(08).
          03 SEXODEPEN                PIC X(01).
          03 DEPENINSS                PIC X(01).
          03 DEPENIMPRENDA            PIC X(01).
          03 DEPENSTAT                PIC X(01).
          03 DEPENKEY.                
              05 DEPENCODFUNC         PIC 9(06).
              05 DEPENSEQ             PIC 9(01).

      *-----------------------------------------------------------------
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
       WORKING-STORAGE SECTION.
       77 W-CONT          PIC 9(06) VALUE ZEROS.
       77 W-OPCAO         PIC X(01) VALUE SPACES.
       77 W-ACT           PIC 9(02) VALUE ZEROS.
       77 MENS            PIC X(50) VALUE SPACES.
       77 LIMPA           PIC X(50) VALUE SPACES. 
       01 ST-ERRO         PIC X(02) VALUE "00".
       01 W-SEL           PIC 9(01) VALUE ZEROS.
       01 IND             PIC 9(02) VALUE 1.

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


       01 TABPAREN.
          03 FILLER PIC X(19) VALUE "0ESPOSA".
          03 FILLER PIC X(19) VALUE "1FILHO(A)".
          03 FILLER PIC X(19) VALUE "2ENTEADO(A)".
          03 FILLER PIC X(19) VALUE "3PAI".
          03 FILLER PIC X(19) VALUE "4MAE".
          03 FILLER PIC X(19) VALUE "5AVOS".
          03 FILLER PIC X(19) VALUE "6NETO".
          03 FILLER PIC X(19) VALUE "7AGREGADO".
          03 FILLER PIC X(19) VALUE "8RESPONSAVEL LEGAL".
          03 FILLER PIC X(19) VALUE "9OUTROS".


          
       01 TABSTAUTSX   REDEFINES TABPAREN.
          03 TBPAREN  PIC X(19) OCCURS 3 TIMES.
      * 
       01 TXTPAREN.
          03 TXTPAREN1 PIC x(01) VALUE SPACES.
          03 TXTPAREN2 PIC X(18) VALUE SPACES.

       01  DDMMAAAA.
          03 DDMMAAAA1 PIC X(02) VALUE SPACES.
          03 DDMMAAAA2 PIC X(02) VALUE SPACES.
          03 DDMMAAAA3 PIC X(04) VALUE SPACES.

       01  AAAAMMDD.
          03 AAAAMMDD1 PIC X(04) VALUE SPACES.
          03 AAAAMMDD2 PIC X(02) VALUE SPACES.
          03 AAAAMMDD3 PIC X(02) VALUE SPACES.
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  TELAPRINCIPAL.
           05  LINE 02  COLUMN 01 
               VALUE  "                       -- CADASTRO DE DE".
           05  LINE 02  COLUMN 41 
               VALUE  "PENDENTES --".
           05  LINE 04  COLUMN 01 
               VALUE  "CHAPA FUNCIONARIO:          SEQUENCIA DE".
           05  LINE 04  COLUMN 41 
               VALUE  "PENTENTE:".
           05  LINE 06  COLUMN 01 
               VALUE  "NOME DEPENDENTE:".
           05  LINE 08  COLUMN 01 
               VALUE  "GRAU PARENTESCO:".
           05  LINE 10  COLUMN 01 
               VALUE  "DATA NASCIMENTO:".
           05  LINE 12  COLUMN 01 
               VALUE  "SEXO:".
           05  LINE 14  COLUMN 01 
               VALUE  "DEPENDENTE INSS:".
           05  LINE 16  COLUMN 01 
               VALUE  "DEPENDENTE IMP.REDA:".
           05  LINE 18  COLUMN 01 
               VALUE  "STATUS:".
           05  LINE 23  COLUMN 01 
               VALUE  "MENSAGEM:".



           05  TDEPENCODFUNC
               LINE 04  COLUMN 20  PIC 9(06)
               USING  DEPENCODFUNC
               HIGHLIGHT.
           05  TDEPENSEQ
               LINE 04  COLUMN 51  PIC 9(01)
               USING  DEPENSEQ
               HIGHLIGHT.
           05  TNOMEDEPEN
               LINE 06  COLUMN 18  PIC X(30)
               USING  NOMEDEPEN
               HIGHLIGHT.
           05  TGRAUPAREN
               LINE 08  COLUMN 18  PIC 9(01)
               USING  GRAUPAREN
               HIGHLIGHT.
           05  TDEPENDATANASC
               LINE 10  COLUMN 18  PIC X(08)
               USING  DEPENDATANASC
               HIGHLIGHT.
           05  TSEXODEPEN
               LINE 12  COLUMN 07  PIC X(01)
               USING  SEXODEPEN
               HIGHLIGHT.
           05  TDEPENINSS
               LINE 14  COLUMN 18  PIC X(01)
               USING  DEPENINSS
               HIGHLIGHT.
           05  TDEPENIMPRENDA
               LINE 16  COLUMN 22  PIC X(01)
               USING  DEPENIMPRENDA
               HIGHLIGHT.
           05  TDEPENSTAT
               LINE 18  COLUMN 09  PIC X(01)
               USING  DEPENSTAT
               HIGHLIGHT.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO. 
       R0.
           OPEN I-O CADDEPEN
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADDEPEN
                 CLOSE CADDEPEN
                 MOVE "*** ARQUIVO CADDEPEN FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                IF ST-ERRO = "95"
                    MOVE "*** ISAM NAO EXCUTADO NO INICIO! **" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO ROT-FIM
                ELSE
                    MOVE "ERRO NA ABERTURA DO ARQUIVO CADDEPEN" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
       R0A.
           OPEN INPUT CADFUNC
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 MOVE "*** ARQUIVO DE CADFUNC NAO ENCONTRADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO DE CADFUNC " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE. 
      *------------[ INICIALIZACAO DAS VARIAVEIS ]---------------------- 
           MOVE SPACES TO NOMEDEPEN SEXODEPEN DEPENSTAT
           MOVE SPACES TO DEPENINSS DEPENIMPRENDA 
           MOVE ZEROS TO DEPENCODFUNC DEPENDATANASC
           MOVE ZEROS TO DEPENSEQ GRAUPAREN CODFUNC
      *-------------[VISUALIZACAO DA TELA]--------------------------------
           DISPLAY TELAPRINCIPAL.
       R1.
           ACCEPT TDEPENCODFUNC
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO ROT-FIM.
           IF DEPENCODFUNC = 0 
                 MOVE "*** CODIGO NÃO INFORMADO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R1.
       R1A.
           MOVE DEPENCODFUNC TO CODFUNC.
           READ CADFUNC
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "23"
                   MOVE "CODIGO DIGITADO NAO ENCONTRADO" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1
               ELSE
                  MOVE "ERRO NA LEITURA ARQUIVO CADFUNC" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM
           ELSE
                DISPLAY TELAPRINCIPAL.



       R2.
           ACCEPT TDEPENSEQ

           IF DEPENSEQ < 1
              MOVE "DEPENSEQ INVALIDO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R2.


       LER-CADFUNC.
           READ CADDEPEN
           IF ST-ERRO NOT = "23"
              IF ST-ERRO = "00"
                DISPLAY TELAPRINCIPAL
                MOVE "*** DEPEN JA CADASTRADO ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R1
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO CADDEPEN" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                MOVE "*** DEPEN NAO CADASTRADO ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM.




       R3.
           ACCEPT TNOMEDEPEN
           IF NOMEDEPEN = SPACES
              MOVE "CAMPO VAZIO!" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R3.
       R4.
           ACCEPT TGRAUPAREN.
       R4A.
           MOVE TBPAREN(IND) TO TXTPAREN
           IF TXTPAREN1 NOT = GRAUPAREN
              ADD 1 TO IND
              IF IND < 11
                 GO TO R4A
              ELSE
                 DISPLAY (23, 14) "GRAUPAREN INVALIDO"
                 MOVE 1 TO IND
                 GO TO R4
           ELSE
               MOVE 1 TO IND
               DISPLAY (23, 14) TXTPAREN2.
       R5.

           ACCEPT TDEPENDATANASC.
           MOVE DEPENDATANASC TO DDMMAAAA
           MOVE DDMMAAAA1 TO AAAAMMDD3
           MOVE DDMMAAAA2 TO AAAAMMDD2
           MOVE DDMMAAAA3 TO AAAAMMDD1
           MOVE AAAAMMDD TO DEPENDATANASC.
       R6.
           ACCEPT TSEXODEPEN
           IF SEXODEPEN = SPACES
              MOVE "CAMPO VAZIO!" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R6.
           IF SEXODEPEN = "M"
              MOVE "*** MASCULINO ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R7.
                  IF SEXODEPEN = "F"
                     MOVE "*** FEMININO ***" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     GO TO R7
                  ELSE
                     MOVE "*** CARACTER INVALIDO ***" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     GO TO R6.
       R7.
           ACCEPT TDEPENINSS
           IF DEPENINSS = SPACES
              MOVE "CAMPO VAZIO!" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R7.
           IF DEPENINSS = "S" 
              MOVE "*** SIM ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R8.
                  IF DEPENINSS = "N"
                     MOVE "*** NAO ***" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     GO TO R8.
                     IF DEPENINSS = "E"
                        MOVE "*** ESPECIAL ***" TO MENS
                        PERFORM ROT-MENS THRU ROT-MENS-FIM
                        GO TO R8
                     ELSE
                        MOVE "*** CARACTER INVALIDO ***" TO MENS
                        PERFORM ROT-MENS THRU ROT-MENS-FIM
                        GO TO R7.
       R8.
           ACCEPT TDEPENIMPRENDA
           IF DEPENIMPRENDA = SPACES
              MOVE "CAMPO VAZIO!" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R8.
           IF DEPENIMPRENDA = "S" 
              MOVE "*** SIM ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R9.
                  IF DEPENIMPRENDA = "N"
                     MOVE "*** NAO ***" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     GO TO R9.
                     IF DEPENIMPRENDA = "E"
                        MOVE "*** ESPECIAL ***" TO MENS
                        PERFORM ROT-MENS THRU ROT-MENS-FIM
                        GO TO R9
                     ELSE
                        MOVE "*** CARACTER INVALIDO ***" TO MENS
                        PERFORM ROT-MENS THRU ROT-MENS-FIM
                        GO TO R8.
       R9.
           ACCEPT TDEPENSTAT.
       R9A.
           MOVE TBSTATUS(IND) TO TXTSTATUS
           IF TXTSTATUS1 NOT = DEPENSTAT
              ADD 1 TO IND
              IF IND < 4
                 GO TO R9A
              ELSE
                 DISPLAY (23, 14) "STATUS INVALIDO"
                 MOVE 1 TO IND
                 GO TO R9
           ELSE
               MOVE 1 TO IND
               DISPLAY (23, 14) TXTSTATUS2.
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
                WRITE REGDEPEN
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO R1.
                IF ST-ERRO = "22"
                  MOVE "* DEP EXISTE,DADOS NAO GRAVADOS *" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO R1
                ELSE
                      MOVE "ERRO NA GRAVACAO D ARQUIVO CADDEPEN" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.
      *
      *----------------------------------------------------------------
       ROT-FIM.
           CLOSE CADFUNC CADDEPEN.
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

		

