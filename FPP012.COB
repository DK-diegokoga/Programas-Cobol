       IDENTIFICATION DIVISION.
       PROGRAM-ID. FPP012.
      *AUTOR: DIEGO TOSHIO TAKEDA KOGA              *
      ***********************************************
      *             CONSULTA DE CARGOS              *
      ***********************************************
      *----------------------------------------------------------------	   
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *---------------------ARQUIVO DO CARGO---------------------------		   
           SELECT ARQCARG ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODIGO
                    ALTERNATE RECORD KEY IS DENOMINACAO WITH DUPLICATES
                    FILE STATUS  IS ERRO.
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
      *---------------------ARQUIVO DO CARGO---------------------------		   
       FD ARQCARG
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQCARG.DAT".

       01 REGCARG.
           03 CODIGO       PIC 9(03).
           03 DENOMINACAO  PIC X(25).
           03 TIPOSALARIO  PIC X(01).
           03 SALARIOBASE  PIC 9(06)V99.
           03 STATUS1        PIC X(01).
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 ERRO                PIC X(02) VALUE "00".
       01 MENSAGEM-CONTADOR   PIC 9(04).
       01 MENSAGEM            PIC X(50).
       01 ESCAPE-KEY          PIC 9(02).
       01 AUX                 PIC 9(02).
       01 CLEARLINE           PIC X(70) VALUE SPACES.
      *-----------------------------------------------------------------
       01 BUFFER.
           03 FILLER PIC X(11) VALUE "HHORISTA   ".
           03 FILLER PIC X(11) VALUE "DDIARISTA  ".
           03 FILLER PIC X(11) VALUE "MMENSALISTA".
      *
       01 FILLER REDEFINES BUFFER.
           03 VECTOR-TIPOS PIC X(11) OCCURS 3 TIMES.
      *
       01 VIEW-TIPO.
           03 VIEW-TIPO-SIGLA  PIC X(01).
           03 VIEW-TIPO-EXT    PIC X(10).
      *
       01 BUFFER-STATS.
           03 FILLER PIC X(11) VALUE "AATIVA     ".
           03 FILLER PIC X(11) VALUE "SSUSPENSA  ".
           03 FILLER PIC X(11) VALUE "DDESATIVADA".
      *
       01 FILLER REDEFINES BUFFER-STATS.
           03 VECTOR-STATS PIC X(11) OCCURS 3 TIMES.
      *
       01 VIEW-STAT.
           03 VIEW-STAT-SIGLA  PIC X(01).
           03 VIEW-STAT-EXT    PIC X(10).
      *-----------------------------------------------------------------  
      *                         ROTINA DE TELA                         *
      *----------------------------------------------------------------- 
       SCREEN SECTION.
       01  TELA.
           05  BLANK SCREEN.
           05  LINE 03  COLUMN 01
               VALUE  "                              CONSULTA D".
           05  LINE 03  COLUMN 41
               VALUE  "E CARGOS".
           05  LINE 05  COLUMN 01
               VALUE  "________________________________________".
           05  LINE 05  COLUMN 41
               VALUE  "________________________________________".
           05  LINE 07  COLUMN 20
               VALUE  "   INSIRA O CODIGO:".
           05  LINE 08  COLUMN 01
               VALUE  "________________________________________".		
           05  LINE 08  COLUMN 41
               VALUE  "________________________________________".			   
           05  LINE 09  COLUMN 01
               VALUE  "CODIGO | DENOMINACAO              |SALAR".
           05  LINE 09  COLUMN 41
               VALUE  "IO   |TIPO SALARIO    | STATUS        ".
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
           05  TCODIGO
               LINE 07  COLUMN 42  PIC 9(03)
               USING  CODIGO.
           05  TMENSAGEM
               LINE 23  COLUMN 13  PIC X(50)
               USING  MENSAGEM.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.

       CREATE-FILES.
           PERFORM OPEN-ARQCARG.

       INICIO.
           PERFORM R0.
           DISPLAY TELA.

       INPUT-CODIGO.
           ACCEPT TCODIGO.
           ACCEPT ESCAPE-KEY FROM ESCAPE KEY.

           IF ESCAPE-KEY = 01 GO TO FIM.

           PERFORM FIND.
           GO TO INPUT-CODIGO.

       FIM.
           CLOSE ARQCARG.
           STOP RUN.

       FIND.
           START ARQCARG KEY IS NOT LESS CODIGO
           MOVE 11 TO LIN.
           PERFORM FIND-1.

       FIND-1.
           IF LIN < 21
           READ ARQCARG NEXT
           IF ERRO = "00"
               PERFORM CHECK-TIPO
               PERFORM CHECK-STATS
               DISPLAY (LIN, 04) CODIGO " |" DENOMINACAO " |" 
			   SALARIOBASE "  |" TIPOSALARIO "  " 
			   VIEW-TIPO-EXT "   |" 
			   STATUS1 " " VIEW-STAT-EXT
               ADD 1 TO LIN
               GO TO FIND-1

           ELSE IF ERRO = "10" DISPLAY (LIN, 04) CLEARLINE
               ADD 1 TO LIN
               GO TO FIND-1

           ELSE MOVE ERRO TO MENSAGEM PERFORM SHOW-MESSAGE.

      *-----------------------------------------------------------------  
      *                   INICIALIZACAO DAS VARIAVEIS                  *
      *----------------------------------------------------------------- 
       R0.
           MOVE SPACES TO DENOMINACAO TIPOSALARIO STATUS1.
		   MOVE SPACES TO VIEW-STAT VIEW-TIPO.
           MOVE ZEROS  TO CODIGO SALARIOBASE.

      *-----------------------------------------------------------------*
       CHECK-TIPO.
           MOVE ZEROS TO AUX
           PERFORM CHECK-TIPO-1.

       CHECK-TIPO-1.
           ADD 1 TO AUX.
           MOVE VECTOR-TIPOS(AUX) TO VIEW-TIPO.
           IF VIEW-TIPO-SIGLA = TIPOSALARIO MOVE 4 TO AUX
           ELSE MOVE SPACES TO VIEW-TIPO.
           IF AUX < 4 GO TO CHECK-TIPO-1.

       CHECK-STATS.
           MOVE ZEROS TO AUX
           PERFORM CHECK-STATS-1.

       CHECK-STATS-1.
           ADD 1 TO AUX.
           MOVE VECTOR-STATS(AUX) TO VIEW-STAT.
           IF VIEW-STAT-SIGLA = STATUS1 MOVE 4 TO AUX
           ELSE MOVE SPACES TO VIEW-STAT.
           IF AUX < 4 GO TO CHECK-STATS-1.

      *---------------------[ ROTINA DE ABERTURA ]---------------------
       OPEN-ARQCARG.
           OPEN INPUT ARQCARG.
           IF ERRO NOT = "00"
               IF ERRO = "30" PERFORM ERROR-FILE-NOT-EXIST 
			   GO TO FIM
               ELSE IF ERRO = "95"
                   PERFORM ERROR-ISAM-NOT-LOADED
                   GO TO FIM
               ELSE PERFORM ERROR-FILE-NOT-OPENED
			   GO TO FIM.
      *---------------------[ ROTINA DE MENSAGEM ]---------------------
       ERROR-EMPTY.
           MOVE "O CAMPO PRECISA SER INFORMADO" TO MENSAGEM
           PERFORM SHOW-MESSAGE.

       ERROR-FILE-NOT-OPENED.
           MOVE "ERRO NA ABERTURA DO ARQUIVO" TO MENSAGEM
           PERFORM SHOW-MESSAGE.

       ERROR-FILE-NOT-EXIST.
           MOVE "ERRO O ARQUIVO NAO EXISTE" TO MENSAGEM
           PERFORM SHOW-MESSAGE.

       ERROR-ISAM-NOT-LOADED.
           MOVE "ERRO O ISAM NAO FOI CARREGADO" TO MENSAGEM
           PERFORM SHOW-MESSAGE.

      ************************************************************	
       SHOW-MESSAGE.
           MOVE ZEROS TO MENSAGEM-CONTADOR.
           DISPLAY TMENSAGEM.

           PERFORM SHOW-MESSAGE-1.

           MOVE SPACES TO MENSAGEM.
           DISPLAY TMENSAGEM.

       SHOW-MESSAGE-1.
           ADD 1 TO MENSAGEM-CONTADOR.
           IF MENSAGEM-CONTADOR < 3000
		   GO TO SHOW-MESSAGE-1.
