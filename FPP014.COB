       IDENTIFICATION DIVISION.
       PROGRAM-ID. FPP014.
      *AUTOR: DIEGO TOSHIO TAKEDA KOGA              *
      ***********************************************
      *             CONSULTA DE FUNCIONARIO         *
      ***********************************************
      *----------------------------------------------------------------	
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *---------------------ARQUIVO DO FUNCIONARIO--------------------
           SELECT ARQFUNC ASSIGN TO DISK
                      ORGANIZATION IS INDEXED
                      ACCESS MODE IS DYNAMIC
                      RECORD KEY IS CHAPA
                      ALTERNATE RECORD KEY IS NOME WITH DUPLICATES
                      FILE STATUS IS ST-ERRO.
      *---------------------ARQUIVO DO CARGO---------------------------	
           SELECT ARQCARG ASSIGN TO DISK
                      ORGANIZATION IS INDEXED
                      ACCESS MODE  IS DYNAMIC
                      RECORD KEY   IS CODIGO
                      FILE STATUS IS ST-ERRO.
      *----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
      *---------------------ARQUIVO DO FUNCIONARIO--------------------	   
       FD ARQFUNC
                 LABEL RECORD IS STANDARD
                 VALUE OF FILE-ID IS "ARQFUNC.DAT".
       01 REGFUNC.
           03 CHAPA           PIC 9(06).
           03 NOME            PIC X(30).
           03 DATANASC        PIC 9(08).
		   03 DT REDEFINES DATANASC.
		       05 DIA         PIC 9(02). 
			   05 MES         PIC 9(02).
			   05 ANO         PIC 9(04).
		   03 SEXO            PIC X(01).
		   03 GENERO          PIC X(01).
           03 CODPTO          PIC 9(03).
           03 CODCARG         PIC 9(03).		   
		   03 NVSALARIAL      PIC 9(02).
		   03 IMPOSTORENDA    PIC X(01).
		   03 DEPENDENTES     PIC 9(01).
           03 CEP-FUNC        PIC 9(08).
		   03 NUMERO-FUNC     PIC 9(04).
		   03 COMPLEMENTO-FUNC PIC X(12).
           03 STATUS1         PIC X(01).
      *------------------------ARQUIVO DO CARGO------------------------
       FD ARQCARG
              LABEL RECORD IS STANDARD
              VALUE OF FILE-ID IS "ARQCARG.DAT".
       01 REGCARG.
           03 CODIGO          PIC 9(03).
           03 DENOMINACAO  PIC X(25).
           03 TIPOSALARIO  PIC X(01).
           03 SALARIOBASE  PIC 9(06)V99.
           03 STATUS2        PIC X(01).
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01 W-SEL        PIC 9(01) VALUE ZEROS.
       01 W-CONT       PIC 9(04) VALUE ZEROS.
       01 W-OPCAO      PIC X(01) VALUE SPACES.
       01 ST-ERRO      PIC X(02) VALUE "00".
       01 W-ACT        PIC 9(02) VALUE ZEROS.
       01 MENS         PIC X(50) VALUE SPACES.
       01 LIMPA        PIC X(55) VALUE SPACES.
       01 SOLIC        PIC X(20) VALUE SPACES.
       01 CONLIN       PIC 9(03) VALUE 001.
       01 CHAPAENTR    PIC 9(06) VALUE ZEROS.
       01 NOMENTR      PIC X(30) VALUE SPACES.
       01 IND          PIC 9(02) VALUE ZEROS.
       01 INDSALARIO   PIC 9(01) VALUE ZEROS.

       01 TABSALARIO.
           03 TBSALARIO      PIC X(20) OCCURS 3 TIMES.

       01 TABSTATUS.
           03 FILLER PIC X(20) VALUE "AATIVA".
           03 FILLER PIC X(20) VALUE "SSUSPENSA".
           03 FILLER PIC X(20) VALUE "DDESATIVADA".

       01 TABSTATUSX      REDEFINES TABSTATUS.
           03 TBSTATUS    PIC X(15) OCCURS 3 TIMES.

       01 TXTSTATUS.
           03 TXTSTATUS2 PIC X(14) VALUE SPACES.
      *-----------------------------------------------------------------
       01 LISTA.
          03 FILLER          PIC X(02) VALUE SPACES.
          03 DETCHAPA        PIC 999999.
          03 FILLER          PIC X(02) VALUE SPACES.
          03 DETNOME         PIC X(29) VALUE SPACES.
          03 FILLER          PIC X(02) VALUE SPACES.
          03 DETTPSALARIO    PIC X(17) VALUE SPACES.
          03 FILLER          PIC X(02) VALUE SPACES.
          03 DETSALARIOBASE  PIC 9(06)V99.
          03 FILLER          PIC X(02) VALUE SPACES.
          03 DETSTATUS       PIC X(20) VALUE SPACES.

      *-----------------------------------------------------------------  
      *                         ROTINA DE TELA                         *
      *-----------------------------------------------------------------  
       SCREEN SECTION.
       01  TELA.
           05 BLANK SCREEN.
           05  LINE 03  COLUMN 01
               VALUE  "                          CONSULTA DE".
           05  LINE 03  COLUMN 41
               VALUE  "FUNCIONARIOS                          ".
           05  LINE 05  COLUMN 01
               VALUE  "________________________________________".
           05  LINE 05  COLUMN 41
               VALUE  "________________________________________".			   
           05  LINE 09  COLUMN 01
               VALUE  "  CHAPA |            NOME             ".
           05  LINE 09  COLUMN 41
               VALUE  "  ".
           05  LINE 09  COLUMN 41
               VALUE  "| TIPO SALARIO | SALARIO BASE | STATUS ".
           05  LINE 10  COLUMN 01
               VALUE  "________________________________________".		
           05  LINE 10  COLUMN 41
               VALUE  "________________________________________".           
		   05  LINE 21  COLUMN 01
               VALUE  "________________________________________".
           05  LINE 21  COLUMN 41
               VALUE  "________________________________________".		   
           05  LINE 23  COLUMN 01
               VALUE  "  MENSAGEM:".

           05  TCHAPAENTR
               LINE 11  COLUMN 03  PIC 9(06)
               USING CHAPAENTR.
           05  TNOMENTR
               LINE 11  COLUMN 06  PIC X(30)
               USING NOMENTR.
           

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.

           MOVE "HORISTA"      TO TBSALARIO(1)
           MOVE "DIARISTA"     TO TBSALARIO(2)
           MOVE "MENSALISTA"   TO TBSALARIO(3).


       INC-OP1.
           OPEN INPUT ARQFUNC
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "ARQUIVO ARQFUNC NAO EXISTE" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2
              ELSE
                 MOVE "ERRO ABERTURA DO ARQUIVO ARQFUNC"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2.

       INC-OP2.
           OPEN INPUT ARQCARG
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "*** ARQUIVO ARQCARG NAO ENCONTRADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO ARQCARG" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.

      *-----------------------------------------------------------------  
      *                   INICIALIZACAO DAS VARIAVEIS                  *
      *----------------------------------------------------------------- 	
       INC-001.
           MOVE SPACES TO NOMENTR TIPOSALARIO
           MOVE ZEROS TO CHAPAENTR SALARIOBASE

      *----------------------------------------------------------------- 
           DISPLAY  TELA.

       INC-001A.
           DISPLAY (23, 13)
            "SOLIC.=> F1=ENCERRA CONSULTA   F2=CONSULTA P/DENOM."
           ACCEPT TCHAPAENTR
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
                      CLOSE ARQFUNC
                      GO TO ROT-FIM.
           IF W-ACT = 03
                      GO TO INC-001B.
           IF W-ACT > 03
                      MOVE "*** FUNCAO NAO DEFINIDA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001A.
           MOVE CHAPAENTR TO CHAPA
           START ARQFUNC KEY IS NOT LESS CHAPA INVALID KEY
                 MOVE "*** CHAVE NAO ENCONTRADA ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001.
           GO TO INC-RD2.

       INC-001B.
           DISPLAY (23, 13)
            "SOLIC.=> F1=ENCERRA CONSULTA     F2=CONSULTA P/COD CARGO "
           ACCEPT TNOMENTR
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
                      CLOSE ARQFUNC
                      GO TO ROT-FIM.
           IF W-ACT = 03
                      GO TO INC-001B.
           IF W-ACT > 03
                      MOVE "*** FUNCAO NAO DEFINIDA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001B.
           MOVE NOMENTR TO NOME
           START ARQFUNC KEY IS NOT LESS NOME INVALID KEY
                 MOVE "*** CHAVE NAO ENCONTRADA ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001B.
       INC-RD2.
           DISPLAY (23, 13) LIMPA
           READ ARQFUNC NEXT
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "10"
                 MOVE "*** FIM DO ARQUIVO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO NOV-SOL
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO ARQFUNC"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
              NEXT SENTENCE.

       
       LER-ARQCARG.
           READ ARQCARG NEXT
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "10"
                 PERFORM ROT-MONTAR
                 MOVE "*** FIM DO ARQUIVO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO NOV-SOL
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO ARQCARG"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
              NEXT SENTENCE.

       ROT-SAL.
           IF TIPOSALARIO = "H"
               MOVE 1 TO INDSALARIO
           ELSE
               IF TIPOSALARIO = "D"
                  MOVE 2 TO INDSALARIO
               ELSE
                   IF TIPOSALARIO = "M"
                      MOVE 3 TO INDSALARIO.

       ROT-STATUS.

           IF STATUS1 = "A"
               MOVE "ATIVA" TO TXTSTATUS
           ELSE
               IF STATUS1 = "S"
                  MOVE "SUSPENSA" TO TXTSTATUS
               ELSE
                   IF STATUS1 = "D"
                      MOVE "DESATIVADA" TO TXTSTATUS.


       ROT-MONTAR.
           MOVE CHAPA  TO DETCHAPA
           MOVE NOME TO DETNOME
           MOVE TBSALARIO(INDSALARIO) TO DETTPSALARIO
           MOVE SALARIOBASE TO DETSALARIOBASE
           MOVE TXTSTATUS2  TO DETSTATUS
           IF CONLIN < 21
           COMPUTE LIN = CONLIN + 10
           DISPLAY (LIN, 02) LISTA
           ADD 1 TO CONLIN
                  GO TO INC-RD2.

       ROT-SOL.
           DISPLAY (23, 13) " *** CONTINUA CONSULTA : . ***"
           MOVE "S" TO W-OPCAO
           ACCEPT  (23, 38) W-OPCAO  WITH UPDATE
           IF W-OPCAO = "S"
                  MOVE 01 TO CONLIN
                  DISPLAY TELA
                  GO TO INC-RD2.
           IF W-OPCAO NOT = "N"
                  MOVE "*** DIGITE S=SIM   N=NAO ***" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-SOL.
           MOVE 01 TO CONLIN.
           GO TO INC-001.

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
      *-----------------------[ ROTINA DE FIM ]------------------------
       ROT-FIM.
           DISPLAY (01 01) ERASE.
           CLOSE ARQFUNC ARQCARG.
       ROT-FIM2.
           EXIT PROGRAM.
       ROT-FIM3.
           STOP RUN.
      *---------------------[ ROTINA DE MENSAGEM ]---------------------
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

      *---------------------*** FIM DE PROGRAMA ***--------------------*
