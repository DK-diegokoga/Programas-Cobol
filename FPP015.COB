       IDENTIFICATION DIVISION.
       PROGRAM-ID. FPP015.
      *AUTOR: DIEGO TOSHIO TAKEDA KOGA              *
      ***********************************************
      *         ARQUIVO DE FOLHA DE PAGAMENTO       *
      ***********************************************
      *-----------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                     DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *---------------------ARQUIVO DO FUNCIONARIO---------------- 		   
           SELECT ARQFUNC ASSIGN TO DISK
                      ORGANIZATION IS INDEXED
                      ACCESS MODE IS DYNAMIC
                      RECORD KEY IS CHAPA
                      FILE STATUS IS ST-ERRO
                      ALTERNATE RECORD KEY IS NOME
                      WITH DUPLICATES.
      *---------------------ARQUIVO DO CARGO----------------------
           SELECT ARQCARG ASSIGN TO DISK
                      ORGANIZATION IS INDEXED
                      ACCESS MODE IS DYNAMIC
                      RECORD KEY IS CODIGO
                      FILE STATUS IS ST-ERRO
                      ALTERNATE RECORD KEY IS DENOMINACAO
                      WITH DUPLICATES.
      *---------------------GERAR ARQUIVO FUNCIONARIO------------- 
           SELECT TARQFUNC ASSIGN TO DISK
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS ST-ERRO.
      *
      *-----------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
      *---------------------ARQUIVO DO FUNCIONARIO---------------- 	   
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

      *---------------------ARQUIVO DO CARGO----------------------
       FD ARQCARG
              LABEL RECORD IS STANDARD
              VALUE OF FILE-ID IS "ARQCARG.DAT".
       01 REGCARG.
           03 CODIGO          PIC 9(03).
           03 DENOMINACAO     PIC X(25).
           03 TIPOSALARIO     PIC X(01).
		   03 SALARIOBASE     PIC 9(06)V99.
           03 STATUS1         PIC X(01).
      *---------------------GERAR ARQUIVO FUNCIONARIO------------- 
       FD TARQFUNC
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQUIVO2.DOC".
       01 REGARQFUNC    PIC X(100).		   
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-SEL      PIC 9(01) VALUE ZEROS.
       77 W-CONT     PIC 9(06) VALUE ZEROS.
       77 W-OPCAO    PIC X(01) VALUE SPACES.
       77 ST-ERRO    PIC X(02) VALUE "00".
       77 W-ACT      PIC 9(02) VALUE ZEROS.
       77 MENS       PIC X(50) VALUE SPACES.
       77 LIMPA      PIC X(50) VALUE SPACES.
       01 CONLIN     PIC 9(03) VALUE ZEROS.
       01 CONLIN1    PIC 9(04) VALUE ZEROS.
       01 CONPAG     PIC 9(03) VALUE ZEROS.
       01 INSS       PIC 9(06)V99 VALUE ZEROS.
       01 TOTAL      PIC ZZZ9 VALUE ZEROS.
       01 LIQUIDO    PIC 9(06)V99 VALUE ZEROS.
       01 BRUTO      PIC 9(06)99 VALUE ZEROS.
	   01 IMPOSTO    PIC 9(06)V99 VALUE ZEROS.
      *

       01  CAB1.
           05  FILLER                 PIC X(046) VALUE
           "EMPRESA FATEC - ZL    ***FOLHA DE PAGAMENTO***".
           05  FILLER                 PIC X(008) VALUE
           "PAGINA:".
           05  NUMPAG   VALUE ZEROS  PIC Z99.
           05  FILLER                 PIC X(008) VALUE
           "   DATA:".
           05  DATAREL  VALUE ZEROS  PIC 99.99.99.

       01  CAB2.
           05  FILLER                 PIC X(037) VALUE
           "CODIGO NOME DO FUNCIONARIO    CARGO".
           05  FILLER                 PIC X(036) VALUE
           "    S.BRU   INSS  I.RENDA  S.LIQUIDO".

       01  CAB3.
           05  FILLER                 PIC X(057) VALUE
           "---------------------------------------------------------".
           05  FILLER                 PIC X(016) VALUE
           "----------------".


       01  CABBLANK.
           05  FILLER                 PIC X(083) VALUE
           "                                                          ".

       01  CAB4.
           05  FILLER              PIC X(057) VALUE
           "TOTAL DA FOLHA DE PAGAMENTO..........:                 ".

       01  CAB5.
           05  FILLER              PIC X(057) VALUE
           "TOTAL DE FUNCIONARIOS:     ".

       01  DET.
           05  1TX     PIC 99999 VALUE ZEROS.
           05  FILLER          PIC X(02) VALUE SPACES.
           05  2TX       PIC X(017) VALUE SPACES.
           05  FILLER          PIC X(04) VALUE SPACES.
           05  3TX     PIC 999 VALUE ZEROS.
           05  FILLER                 PIC X(001) VALUE SPACES.
           05  4TX  PIC X(08) VALUE SPACES.
           05  FILLER                 PIC X(02) VALUE "  ".
           05  5TX   PIC 9(06)V99.
           05  FILLER                 PIC X(02) VALUE "  ".
           05  6TX        PIC ZZZZ9.
           05  FILLER                 PIC X(02) VALUE "  ".
           05  7TX   PIC 9(06)V99.  
       01  LINTOTAIS.
           05  FILLER                 PIC X(073) VALUE
           "***TOTAL FOLHA DE PAGAMENTO..........:             ".
       01  LINTOT.
           05  FILLER                 PIC X(051) VALUE
           "                       ***TOTAL DE FUNCIONARIOS : ".
           05  TOTFUNC   VALUE ZEROS  PIC ZZZ9.
           05  FILLER                 PIC X(004) VALUE
           " ***".
       01  LINHABR                    PIC X(150) VALUE SPACES.
      *-----------------------------------------------------------------  
      *                         ROTINA DE TELA                         *
      *----------------------------------------------------------------- 
       SCREEN SECTION.
       01  TELA.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01
               VALUE  "                  *** RELATORIO DE FOLHA".
           05  LINE 02  COLUMN 41
               VALUE  " DE PAGAMENTO ***".
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
                MOVE SPACES TO NOME DENOMINACAO TIPOSALARIO
                MOVE ZEROS TO CODIGO CHAPA SALARIOBASE INSS IMPOSTORENDA
                MOVE ZEROS TO LIQUIDO TOTAL BRUTO
                DISPLAY TELA.
       INC-OPC.
                ACCEPT TW-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "* DADOS RECUSADOS PELO USUARIO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ROT-FIM.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
      *
       INC-OP0.
           OPEN INPUT ARQFUNC
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      MOVE "* ARQUIVO FUNCIONARIOS NAO EXISTE *" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
               ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO FUNCIONARIOS"
                      TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
           ELSE
                    NEXT SENTENCE.
      *
           OPEN OUTPUT TARQFUNC
           IF ST-ERRO NOT = "00"
                      MOVE "ERRO ABERTURA DO ARQUIVO ARQFUNCTX" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.

       INC-OP1.
           OPEN INPUT ARQCARG
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      MOVE "* ARQUIVO DE CARGO NAO EXISTE *" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
               ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO DE CARGO"
                      TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
           ELSE
                    NEXT SENTENCE.


       ROT-CABEC.
           ADD 1 TO CONPAG
           MOVE CONPAG TO NUMPAG
           WRITE REGARQFUNC FROM CAB1.
           WRITE REGARQFUNC FROM CAB3.
           MOVE SPACES TO REGARQFUNC
           WRITE REGARQFUNC FROM CAB2.
           WRITE REGARQFUNC FROM CAB3.

      *
       LER-FUNC.
                READ ARQFUNC NEXT
                IF ST-ERRO NOT = "00"
                   IF ST-ERRO = "10"
                      WRITE REGARQFUNC FROM LINHABR
                      MOVE CONLIN1 TO TOTFUNC
                      WRITE REGARQFUNC FROM LINTOTAIS
                      WRITE REGARQFUNC FROM LINTOT
                      MOVE "*** FIM DO ARQFUNC ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                   ELSE
                      MOVE "ERRO NA LEITURA ARQFUNC"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.

       LER-CARG.
                READ ARQCARG NEXT
                IF ST-ERRO NOT = "00"
                   IF ST-ERRO = "10"
                      MOVE "*** FIM DO ARQCARG ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                   ELSE
                      MOVE "ERRO NA LEITURA ARQCARG"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.

       ROT-CALC-1.

           IF TIPOSALARIO = "H"
               MULTIPLY SALARIOBASE BY 220 GIVING BRUTO
           ELSE
               IF TIPOSALARIO = "D"
                   MULTIPLY SALARIOBASE BY 30 GIVING BRUTO
               ELSE
                   IF TIPOSALARIO = "M"
                      MOVE SALARIOBASE TO BRUTO.

           IF BRUTO NOT GREATER 2300
              MULTIPLY BRUTO BY 0,08 GIVING INSS
              GO TO INC-003.

           IF BRUTO > 2300 AND BRUTO NOT GREATER 4500
              MULTIPLY BRUTO BY 0,09 GIVING INSS
              GO TO INC-003.

           IF BRUTO > 4500
              MULTIPLY BRUTO BY 0,1 GIVING INSS
              GO TO INC-003.
           

       INC-003.
           MOVE CHAPA       TO 1TX.
           MOVE NOME        TO 2TX.
           MOVE CODIGO      TO 3TX.
           MOVE DENOMINACAO TO 4TX.
           MOVE BRUTO       TO 5TX.
           MOVE INSS        TO 6TX.
           IF IMPOSTO = BRUTO - INSS
		      IF IMPOSTO = 3400 AND IMPOSTO < 3400
                   MULTIPLY IMPOSTO BY 0 GIVING LIQUIDO
                   MOVE LIQUIDO TO 7TX.

			  IF IMPOSTO = BRUTO - INSS
			       IF IMPOSTO > 3400 AND IMPOSTO < 6000
			       MULTIPLY IMPOSTO BY 0,12 GIVING LIQUIDO
                   MOVE LIQUIDO TO 7TX.

			  IF IMPOSTO = BRUTO - INSS 
			       IF IMPOSTO > 6000 AND IMPOSTO < 9500
			       MULTIPLY IMPOSTO BY 0,15 GIVING LIQUIDO
			       MOVE LIQUIDO TO 7TX.
 
			  IF IMPOSTO = BRUTO - INSS 
			       IF IMPOSTO > 9500
                   MULTIPLY IMPOSTO BY 0,20 GIVING LIQUIDO
				   MOVE LIQUIDO TO 7TX.


       INC-WR1.
           WRITE REGARQFUNC FROM DET
           IF ST-ERRO NOT = "00"
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO ARQFUNCTX"
                                                       TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.

           WRITE REGARQFUNC FROM CABBLANK.
      *     WRITE REGARQFUNCTX FROM CABBLANK.


                ADD 1 TO CONLIN CONLIN1
                DISPLAY (20, 40) CONLIN1
                IF CONLIN < 34
                      GO TO LER-FUNC
                ELSE
                      MOVE ZEROS TO CONLIN
                      MOVE SPACES TO REGARQFUNC

                      WRITE REGARQFUNC FROM CAB4.
                      WRITE REGARQFUNC FROM CABBLANK.
                      WRITE REGARQFUNC FROM CAB5.
                      WRITE REGARQFUNC FROM LINHABR
                      GO TO ROT-CABEC.

      *
      **********************
      * ROTINA DE FIM      *
      **********************
       ROT-FIM.
                DISPLAY (01, 01) ERASE
                CLOSE ARQCARG ARQFUNC TARQFUNC.
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
