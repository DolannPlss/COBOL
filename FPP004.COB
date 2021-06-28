       IDENTIFICATION DIVISION.
       PROGRAM-ID. FPP004.
      *AUTHOR. MARCOS THADEU OLIVEIRA DA SILVA
      ********************************************************
      * CADASTRO DE FUNCIONARIOS *
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
           SELECT CADDEP ASSIGN TO DISK
                      ORGANIZATION IS INDEXED
                      ACCESS MODE  IS DYNAMIC
                      RECORD KEY   IS CODDEP
                      ALTERNATE RECORD KEY IS DENDEP
                      WITH DUPLICATES
                      FILE STATUS  IS ST-ERRO.
      *
           SELECT CADCEP ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CEP-NUMCEP
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS CEP-LOGRA
                                   WITH DUPLICATES.
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
       FD CADDEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQDEP.DAT". 
       01 REGDEP.
          03 CODDEP                   PIC 9(03).
          03 DENDEP                   PIC X(25).
          03 NVL                      PIC 9(02).
          03 STAT                     PIC X(01).
      *-----------------------------------------------------------------
       FD CADCEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCEP.DAT". 
       01 REGCEP.
                03 CEP-NUMCEP        PIC 9(08).
                03 CEP-LOGRA         PIC X(30).
                03 CEP-BAIRRO        PIC X(20).
                03 CEP-CIDADE        PIC X(20).
                03 CEP-UF            PIC X(02).
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

       01 TABGENERO.
          03 FILLER PIC X(24) VALUE "HHETEROSEXUAL".
          03 FILLER PIC X(24) VALUE "HHOMOSEXUAL".
          03 FILLER PIC X(24) VALUE "BBISEXUAL".
          03 FILLER PIC X(24) VALUE "TTRANSSEXUAL".
          03 FILLER PIC X(24) VALUE "PPANSEXUAL".
          03 FILLER PIC X(24) VALUE "OOUTROS ".
       01 TABGENEROX REDEFINES TABGENERO.
          03 TBGENERO  PIC X(24) OCCURS 3 TIMES.
      * 
       01 TXTGENERO.
          03 TXTGENERO1 PIC x(01) VALUE SPACES.
          03 TXTGENERO2 PIC X(23) VALUE SPACES.
       
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
           05  LINE 01  COLUMN 01 
               VALUE  "                    -- CADASTRO DE FUNCI".
           05  LINE 01  COLUMN 41 
               VALUE  "ONARIOS --".
           05  LINE 03  COLUMN 01 
               VALUE  "CHAPA FUNCIONARIO:        NOME:".
           05  LINE 05  COLUMN 01 
               VALUE  "DATA NASCIMENTO:          SEXO:".
           05  LINE 07  COLUMN 01 
               VALUE  "GENERO:".
           05  LINE 09  COLUMN 01 
               VALUE  "CODIGO DEPARTAMENTO:".
           05  LINE 11  COLUMN 01 
               VALUE  "CODIGO CARGO:".
           05  LINE 13  COLUMN 01 
               VALUE  "NIVEL SALARIAL:".
           05  LINE 15  COLUMN 01 
               VALUE  "IMP.RENDA:".
           05  LINE 17  COLUMN 01 
               VALUE  "QUANTIDADE DE DEPENDENTES:".
           05  LINE 19  COLUMN 01 
               VALUE  "CEP DO FUNCIONARIO:".
           05  LINE 21  COLUMN 01 
               VALUE  "NUMERO:      COMPL:              STATUS".
           05  LINE 21  COLUMN 41 
               VALUE  ":".
           05  LINE 23  COLUMN 01 
               VALUE  "MENSAGEM:".



           05  TCODFUNC
               LINE 03  COLUMN 20  PIC 999999
               USING  CODFUNC
               HIGHLIGHT.
           05  TNOME
               LINE 03  COLUMN 33  PIC X(30)
               USING  NOME
               HIGHLIGHT.
           05  TDATA
               LINE 05  COLUMN 18  PIC X(08)
               USING  DATANASC
               HIGHLIGHT.
           05  TSEXO
               LINE 05  COLUMN 33  PIC X(01)
               USING  SEXO
               HIGHLIGHT.
           05  TGENERO
               LINE 07  COLUMN 09  PIC X(01)
               USING  GENERO
               HIGHLIGHT.
           05  TFUNCDEP
               LINE 09  COLUMN 22  PIC 9(03)
               USING  FUNCDEP
               HIGHLIGHT.
           05  TDENDEP
               LINE 09  COLUMN 26  PIC X(30)
               USING  DENDEP
               HIGHLIGHT.
           05  TFUNCCAR
               LINE 11  COLUMN 15  PIC 9(03)
               USING  FUNCCAR
               HIGHLIGHT.
           05  TDENCAR
               LINE 11  COLUMN 19  PIC X(25)
               USING  DENCAR
               HIGHLIGHT.
           05  TNVLSAL
               LINE 13  COLUMN 17  PIC 9(02)
               USING  NVLSAL
               HIGHLIGHT.
           05  TIMPRENDA
               LINE 15  COLUMN 12  PIC X(01)
               USING  IMPRENDA
               HIGHLIGHT.
           05  TQUANTDEP
               LINE 17  COLUMN 28  PIC 9(01)
               USING  QUANTDEP
               HIGHLIGHT.
           05  TFUNCCEP
               LINE 19  COLUMN 21  PIC 9(08)
               USING  FUNCCEP
               HIGHLIGHT.
           05  TLOGRA
               LINE 19  COLUMN 30  PIC X(30)
               USING  CEP-LOGRA
               HIGHLIGHT.
           05  TNUMRES
               LINE 21  COLUMN 09  PIC 9(04)
               USING  NUMRES
               HIGHLIGHT.
           05  TCOMPRES
               LINE 21  COLUMN 21  PIC X(12)
               USING  COMPRES
               HIGHLIGHT.
           05  TFUNCSTAT
               LINE 21  COLUMN 43  PIC X(01)
               USING  FUNCSTAT
               HIGHLIGHT.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO. 
       R0.
           OPEN I-O CADFUNC
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADFUNC
                 CLOSE CADFUNC
                 MOVE "*** ARQUIVO CADFUNC FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                IF ST-ERRO = "95"
                    MOVE "*** ISAM NAO EXCUTADO NO INICIO! **" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO ROT-FIM
                ELSE
                    MOVE "ERRO NA ABERTURA DO ARQUIVO CADFUNC" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
       R0A.
           OPEN INPUT CADDEP
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 MOVE "*** ARQUIVO DE CADDEP NAO ENCONTRADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO DE CADDEP " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.      
       R0B.
           OPEN INPUT CADCAR
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 MOVE "*** ARQUIVO DE CADCAR NAO ENCONTRADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO DE CADCAR " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
       R0C.
           OPEN INPUT CADCEP
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 MOVE "*** ARQUIVO DE CEP NAO ENCONTRADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO DE CEP " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
      * 
      *------------[ INICIALIZACAO DAS VARIAVEIS ]---------------------- 
       R1A.
           MOVE SPACES TO  NOME SEXO GENERO IMPRENDA
           MOVE SPACES TO COMPRES FUNCSTAT DATANASC
           MOVE SPACES TO DENCAR DENDEP CEP-LOGRA
           MOVE ZEROS TO CEP-NUMCEP
           MOVE ZEROS TO CODFUNC FUNCDEP FUNCCAR NVLSAL
           MOVE ZEROS TO QUANTDEP FUNCCEP NUMRES CODCAR CODDEP 
      *-------------[VISUALIZACAO DA TELA]--------------------------------
           DISPLAY TELAPRINCIPAL.
       R1.
           ACCEPT TCODFUNC
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO ROT-FIM.

           IF CODFUNC < 1
              MOVE "CODFUNC INVALIDO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R1.
       LER-CADFUNC.
           READ CADFUNC
           IF ST-ERRO NOT = "23"
              IF ST-ERRO = "00"
                DISPLAY TELAPRINCIPAL
                MOVE "*** FUNCIONARIO JA CADASTRADO ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R1
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO CADFUNC" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                MOVE "*** FUNCIONARIO NAO CADASTRADO ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM.
       R2.
           ACCEPT TNOME
           IF NOME = SPACES
              MOVE "CAMPO VAZIO!" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R2.
       R3.

           ACCEPT TDATA.
           MOVE DATANASC TO DDMMAAAA
           MOVE DDMMAAAA3 TO AAAAMMDD1
           MOVE DDMMAAAA2 TO AAAAMMDD2
           MOVE DDMMAAAA1 TO AAAAMMDD3
           MOVE AAAAMMDD TO DATANASC.

       R4.
           ACCEPT TSEXO
           IF SEXO = SPACES
              MOVE "CAMPO VAZIO!" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R4.
           IF SEXO = "M"
              MOVE "*** MASCULINO ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R5.
                  IF SEXO = "F"
                     MOVE "*** FEMININO ***" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     GO TO R5
                  ELSE
                     MOVE "*** CARACTER INVALIDO ***" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     GO TO R4.
       R5.
           ACCEPT TGENERO.
       R5A.
           MOVE TBGENERO(IND) TO TXTGENERO
           IF TXTGENERO1 NOT = GENERO
              ADD 1 TO IND
              IF IND < 7
                 GO TO R5A
              ELSE
                 DISPLAY (23, 14) "GENERO INVALIDO"
                 MOVE 1 TO IND
                 GO TO R5
           ELSE
               MOVE 1 TO IND
               DISPLAY (23, 14) TXTGENERO2.
       R6.
           ACCEPT TFUNCDEP
           IF FUNCDEP = 0 
                 MOVE "*** CODIGO NÃO INFORMADO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R6.
       R6A.
           MOVE FUNCDEP TO CODDEP.
           READ CADDEP
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "23"
                   MOVE "DEPARTAMENTO DIGITADO NAO ENCONTRADO" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R6
               ELSE
                  MOVE "ERRO NA LEITURA ARQUIVO CADDEP" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM
           ELSE
                DISPLAY TELAPRINCIPAL.
       R7.
           ACCEPT TFUNCCAR
           IF FUNCCAR = 0 
                 MOVE "*** CODIGO INFORMADO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R7.
       R7A.
           MOVE FUNCCAR TO CODCAR.
           READ CADCAR
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "23"
                   MOVE "*** CARGO DIGITADO NAO ENCONTRADO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R7
               ELSE
                  MOVE "ERRO NA LEITURA ARQUIVO CADCAR" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM
           ELSE
                DISPLAY TELAPRINCIPAL.
       R8.
           ACCEPT TNVLSAL
           IF NVLSAL > 20 OR NVLSAL < 0
                   MOVE "*** NIVEL INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R8.
       R9.
           ACCEPT TIMPRENDA
           IF IMPRENDA = SPACES
              MOVE "CAMPO VAZIO!" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R9.
           IF IMPRENDA = "S" 
              MOVE "*** SIM ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R10.
                  IF IMPRENDA = "N"
                     MOVE "*** NAO ***" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     GO TO R10.
                     IF IMPRENDA = "E"
                        MOVE "*** ESPECIAL ***" TO MENS
                        PERFORM ROT-MENS THRU ROT-MENS-FIM
                        GO TO R10
                     ELSE
                        MOVE "*** CARACTER INVALIDO ***" TO MENS
                        PERFORM ROT-MENS THRU ROT-MENS-FIM
                        GO TO R9.
       R10.
           ACCEPT TQUANTDEP.
       R11.
           ACCEPT TFUNCCEP
           IF FUNCCEP = 0 
                 MOVE "*** CODIGO NÃO INFORMADO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R11.
       R11A.
           MOVE FUNCCEP TO CEP-NUMCEP.
           READ CADCEP
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "23"
                   MOVE "*** CEP DIGITADO NAO ENCONTRADO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R11
               ELSE
                  MOVE "ERRO NA LEITURA ARQUIVO CADCEP" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM
           ELSE
                DISPLAY TELAPRINCIPAL.
       R12.
           ACCEPT TNUMRES
           IF NUMRES< 1
              MOVE "NUMERO INVALIDO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R12.
       R13.
           ACCEPT TCOMPRES.
       R14.
           ACCEPT TFUNCSTAT.
       R14A.
           MOVE TBSTATUS(IND) TO TXTSTATUS
           IF TXTSTATUS1 NOT = FUNCSTAT
              ADD 1 TO IND
              IF IND < 4
                 GO TO R14A
              ELSE
                 DISPLAY (23, 14) "STATUS INVALIDO"
                 MOVE 1 TO IND
                 GO TO R14
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
                WRITE REGFUNC
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
           CLOSE CADFUNC CADCAR CADDEP CADCEP.
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

		
