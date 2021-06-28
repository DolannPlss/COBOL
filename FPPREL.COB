       IDENTIFICATION DIVISION.
       PROGRAM-ID. FPPREL.
       AUTHOR. MARCOS THADEU OLIVEIRA DA SILVA
      ***************************
      * RELATORIO DE PAGAMENTO  *
      ***************************
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
           SELECT CADPAGTX ASSIGN TO DISK
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
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
       FD CADPAGTX
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADPAGRL.DOC".
       01 REGPAGTX    PIC X(100).
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
       77 W-CONT          PIC 9(06) VALUE ZEROS.
       77 W-OPCAO         PIC X(01) VALUE SPACES.
       77 W-ACT           PIC 9(02) VALUE ZEROS.
       77 MENS            PIC X(50) VALUE SPACES.
       77 LIMPA           PIC X(50) VALUE SPACES. 
       01 ST-ERRO         PIC X(02) VALUE "00".
       01 W-SEL           PIC 9(01) VALUE ZEROS.
       01 IND             PIC 9(02) VALUE 1.
       01 SALBRUTO        PIC 9(06)V99 VALUE ZEROS.
       01 INSS            PIC 9(06)V99 VALUE ZEROS.
       01 IMPREN          PIC 9(06)V99 VALUE ZEROS.
       01 SALLIQ          PIC 9(06)V99 VALUE ZEROS.


       01 PAGTX.
                03 CODFUNCTX      PIC 999999.
                03 FILLER            PIC X(01) VALUE " ".
                03 NOMETX       PIC X(25).
                03 FUNCCARTX      PIC 9(03).
                03 FILLER            PIC X(01) VALUE " ".

                03 FILLER            PIC X(01) VALUE " ".
                03 SALBRUTOTX          PIC 9(06)V99.
                03 FILLER            PIC X(01) VALUE " ".
                03 INSSTX          PIC 9(06)V99.
                03 FILLER            PIC X(01) VALUE " ".
                03 IMPRENTX          PIC 9(06)V99.
                03 FILLER            PIC X(01) VALUE " ".
                03 SALLIQTX          PIC 9(06)V99.
       01 CAB0.
                03 FILLER      PIC X(20) VALUE "FATEC ZL".
                03 FILLER      PIC X(30) VALUE
                        "   *** RELACAO DE PAGAMENTO ***".

       01 CAB1.
                03 CAB-CODFUNCTX     PIC X(06) VALUE "COD".
                03 FILLER            PIC X(01) VALUE " ".
                03 CAB-NOMETX        PIC X(15) VALUE "   NOME FUNC   ".
                03 FILLER            PIC X(10) VALUE " ".
                03 CAB-FUNCCARTX     PIC X(05) VALUE "CARGO".
                03 FILLER            PIC X(01) VALUE " ".
                03 CAB-SALBRUTOTX    PIC X(07) VALUE "SAL.BRU".
                03 FILLER            PIC X(01) VALUE " ".
                03 CAB-INSSTX        PIC X(04) VALUE "INSS".
                03 FILLER            PIC X(05) VALUE " ".
                03 CAB-IMPRENTX      PIC X(09) VALUE "IMP.RENDA".
                03 FILLER            PIC X(01) VALUE " ".
                03 CAB-SALLIQTX      PIC X(07) VALUE "SAL.LIQ".
       01 CAB2.
                03 CAB-CODFUNCTX     PIC X(06) VALUE "------".
                03 FILLER            PIC X(01) VALUE " ".
                03 CAB-NOMETX        PIC X(15) VALUE "---------------".
                03 FILLER            PIC X(10) VALUE " ".
                03 CAB-FUNCCARTX     PIC X(05) VALUE "-----".
                03 FILLER            PIC X(01) VALUE " ".
                03 CAB-SALBRUTOTX    PIC X(07) VALUE "-------".
                03 FILLER            PIC X(01) VALUE " ".
                03 CAB-INSSTX        PIC X(04) VALUE "----".
                03 FILLER            PIC X(05) VALUE " ".
                03 CAB-IMPRENTX      PIC X(09) VALUE "---------".
                03 FILLER            PIC X(01) VALUE " ".
                03 CAB-SALLIQTX      PIC X(07) VALUE "-------".
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  TELAPRINCIPAL.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "        *** IMPRESSAO DE RELATORIO PAG  ***".
           05  LINE 02  COLUMN 41 
               VALUE  " ".
           05  LINE 12  COLUMN 01 
               VALUE  "                         CONFIRMA IMPRES".
           05  LINE 12  COLUMN 41 
               VALUE  "SAO (S ou N ) :".
           05  LINE 24  COLUMN 01 
               VALUE  " MENSAGEM :".
           05  TW-OPCAO
               LINE 12  COLUMN 57  PIC X(01)
               USING  W-OPCAO
               HIGHLIGHT.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO. 
      *
       INC-001.
                MOVE SPACES TO NOME DENCAR TIPOSAL
                MOVE ZEROS TO CODFUNC FUNCCAR 
                MOVE ZEROS TO SALBASE SALBRUTO INSS
                MOVE ZEROS TO IMPREN SALLIQ
                DISPLAY TELAPRINCIPAL.
       INC-OPC.
                ACCEPT TW-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "* DADOS RECUSADOS PELO OPERADOR *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ROT-FIM.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-OP0.
           OPEN INPUT CADFUNC
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      MOVE "* ARQUIVO FUNC NAO EXISTE *" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
               ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO FUNC" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
           ELSE
                    NEXT SENTENCE.
           OPEN INPUT CADCAR
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      MOVE "* ARQUIVO CAR NAO EXISTE *" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
               ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO FUNC" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
           ELSE
                    NEXT SENTENCE.
      *
           OPEN OUTPUT CADPAGTX
           IF ST-ERRO NOT = "00"
                      MOVE "ERRO ABERTURA DO ARQUIVO CADPAGTX" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.
       ROT-CABEC.
           WRITE REGPAGTX FROM CAB0.
           MOVE SPACES TO REGPAGTX
           WRITE REGPAGTX.
           WRITE REGPAGTX FROM CAB1.
           WRITE REGPAGTX FROM CAB2.
      *
       LER-FUNC01.
                READ CADFUNC NEXT
                IF ST-ERRO NOT = "00"
                   IF ST-ERRO = "10"
                      MOVE "*** FIM DO CADFUNC ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                   ELSE
                      MOVE "ERRO NA LEITURA CADFUNC"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       INC-003.


           MOVE CODFUNC TO CODFUNCTX.
           MOVE NOME  TO NOMETX.
           MOVE FUNCCAR TO FUNCCARTX.

           

           MOVE FUNCCAR TO CODCAR.
           READ CADCAR


           IF TIPOSAL = "H"
                   COMPUTE SALBRUTO = SALBASE * 220.
           IF TIPOSAL = "D"
                   COMPUTE SALBRUTO = SALBASE * 30.
           IF TIPOSAL = "M"
                   COMPUTE SALBRUTO = SALBASE * 1.
           
           MOVE SALBRUTO TO SALBRUTOTX.

           IF SALBRUTO < 2300
                   COMPUTE INSS = SALBRUTO * 0,08.
           IF SALBRUTO > 2300,01 AND SALBRUTO < 4500
                   COMPUTE INSS = SALBRUTO * 0,09.
           IF SALBRUTO > 4500
                   COMPUTE INSS = SALBRUTO * 0,10.

           MOVE INSS TO INSSTX.

           IF (SALBRUTO - INSS) < 3400
                   COMPUTE IMPREN = (SALBRUTO - INSS) * 0.

           IF (SALBRUTO - INSS) > 3400,01 AND (SALBRUTO - INSS)<6000
                   COMPUTE IMPREN = (SALBRUTO - INSS) * 0,12.

           IF (SALBRUTO - INSS) > 6000,01 AND (SALBRUTO - INSS)<9500
                   COMPUTE IMPREN = (SALBRUTO - INSS) * 0,15.

           IF (SALBRUTO - INSS) > 9500
                   COMPUTE IMPREN = (SALBRUTO - INSS) * 0,2.

           MOVE IMPREN TO IMPRENTX.

           COMPUTE SALLIQ = SALBRUTO - INSS - IMPREN
           MOVE SALLIQ TO SALLIQTX.
           
       INC-WR1.
                WRITE REGPAGTX FROM PAGTX
                IF ST-ERRO = "00" 
                      DISPLAY (20, 50) CODFUNCTX
                      MOVE "*** DADOS IMPRESSOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO LER-FUNC01
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO CADPAGTX"
                                                       TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.
      *
      **********************
      * ROTINA DE FIM      *
      **********************
      *
       ROT-FIM.
                DISPLAY (01, 01) ERASE
                CLOSE CADFUNC CADCAR CADPAGTX.
       ROT-FIMP.
                EXIT PROGRAM.

       ROT-FIMS.
                STOP RUN.
      *
      **********************
      * ROTINA DE MENSAGEM *
      **********************
      *
       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (23, 12) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
                   DISPLAY (23, 12) LIMPA.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.




