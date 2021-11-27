       IDENTIFICATION DIVISION.
       PROGRAM-ID. FPP016.
      *AUTOR: DIEGO TOSHIO TAKEDA KOGA              *
      ***********************************************
      *           ARQUIVO DE CARGOS                 *
      ***********************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                     DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *---------------------ARQUIVO DO CARGO----------------------	   
           SELECT ARQCARG ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODIGO
                    ALTERNATE RECORD KEY IS DENOMINACAO WITH DUPLICATES
                    FILE STATUS  IS ST-ERRO.
      *---------------------GERAR ARQUIVO CARGO------------- 					
           SELECT TX ASSIGN TO DISK
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  FILE STATUS  IS ST-ERRO.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
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
      *---------------------GERAR ARQUIVO CARGO-------------
       FD TX
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQUIVO1.DOC".
       01 ARQTX    PIC X(100).
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
      *

       01  CAB1.
           05  FILLER                 PIC X(057) VALUE
           "﻿FATEC - ZONA LESTE       ***  GERANDO ARQUIVO  ***".
           05  FILLER                 PIC X(019) VALUE
           "       PAG. : ".
           05  NUMPAG   VALUE ZEROS  PIC 999.


       01  CAB2.
           05  FILLER                 PIC X(057) VALUE
           "CODIGO         DENOMINACAO     TIPO SALARIO     SALARIO B".
           05  FILLER                 PIC X(015) VALUE
           "ASE      STATUS".

       01  CAB3.
           05  FILLER                 PIC X(057) VALUE
           "---------      ------------   --------------    ---------".
           05  FILLER                 PIC X(016) VALUE
           "----    --------".

       01  DET.
           05  1TX  VALUE ZEROS  PIC 9(03).
           05  FILLER                 PIC X(012) VALUE 
           "    ".
           05  2TX   VALUE SPACES PIC X(25).
           05  FILLER                 PIC X(004) VALUE
           "    ".
           05  3TX  VALUE SPACES PIC X(001).
           05  FILLER                 PIC X(004) VALUE
           "    ".
           05  4TX  VALUE ZEROS PIC ZZZ.ZZ9,99.
           05  FILLER                 PIC X(009) VALUE
           "    ".
           05  5TX  VALUE SPACES PIC X(001).

       01  LINTOT.
           05  FILLER                 PIC X(051) VALUE
           "                          *** TOTAL DE REGISTROS : ".
           05  TOTREG   VALUE ZEROS  PIC ZZZ9.
           05  FILLER                 PIC X(004) VALUE
           " ***".

      *-----------------------------------------------------------------  
      *                         ROTINA DE TELA                         *
      *----------------------------------------------------------------- 
       SCREEN SECTION.
       01  TELA.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                *** CONVERSAO DE ARQUIVO".
           05  LINE 02  COLUMN 41 
               VALUE  " FPP002 PARA TXT ***".
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
                MOVE SPACES TO DENOMINACAO STATUS1.
                MOVE SPACES TO TIPOSALARIO. 
                MOVE ZEROS  TO CODIGO SALARIOBASE.
				MOVE ZEROS  TO W-SEL.
                DISPLAY TELA.
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
      *
       INC-OP0.
           OPEN INPUT ARQCARG
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      MOVE "* ARQUIVO NAO EXISTE *" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
               ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
           ELSE
                    NEXT SENTENCE.
      *
           OPEN OUTPUT TX
           IF ST-ERRO NOT = "00"
                      MOVE "ERRO ABERTURA DO ARQUIVO" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.
       ROT-CABEC. 
           ADD 1 TO CONPAG
           MOVE CONPAG TO NUMPAG
           WRITE ARQTX FROM CAB1.
           MOVE SPACES TO ARQTX
           WRITE ARQTX.
           WRITE ARQTX FROM CAB2.
           WRITE ARQTX FROM CAB3.
      *
       LER-CEP01.
                READ ARQCARG NEXT
                IF ST-ERRO NOT = "00"
                   IF ST-ERRO = "10"
                      MOVE SPACES TO ARQTX
                      WRITE ARQTX
                      MOVE CONLIN1 TO TOTREG
                      WRITE ARQTX FROM LINTOT                     
                      MOVE "*** FIM ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                   ELSE
                      MOVE "ERRO NA LEITURA "   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       INC-003.
           MOVE CODIGO       TO 1TX.
           MOVE DENOMINACAO  TO 2TX.
           MOVE TIPOSALARIO  TO 3TX.
           MOVE SALARIOBASE  TO 4TX.
           MOVE STATUS1      TO 5TX.

       INC-WR1.
           WRITE ARQTX FROM DET
                IF ST-ERRO NOT = "00"
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO CADCEPTX"
                                                       TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.

                ADD 1 TO CONLIN CONLIN1
                DISPLAY (20, 40) CONLIN1
                DISPLAY (20, 50) 1TX
                IF CONLIN < 34
                      GO TO LER-CEP01
                ELSE
                      MOVE ZEROS TO CONLIN
                      GO TO ROT-CABEC.
      *
      **********************
      * ROTINA DE FIM      *
      **********************
       ROT-FIM.
                DISPLAY (01, 01) ERASE
                CLOSE ARQCARG TX.
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
