       IDENTIFICATION DIVISION.
       PROGRAM-ID. FPP011.
      *AUTOR: DIEGO TOSHIO TAKEDA KOGA              *
      ***********************************************
      *           CONSULTA DE DEPARTAMENTO          *
      ***********************************************
      *----------------------------------------------------------------	   
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *---------------------ARQUIVO DO DEPARTAMENTO--------------------    	   
           SELECT ARQDEP ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           RECORD KEY   IS CODIGO1
           ALTERNATE RECORD KEY IS DENOMINACAO WITH DUPLICATES
           FILE STATUS  IS ERRO.
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
      *---------------------ARQUIVO DO DEPARTAMENTO--------------------	 	   
       FD ARQDEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQDEP.DAT".
       01 REGDEP.
           03 CODIGO1        PIC 9(03).
           03 DENOMINACAO    PIC X(25).
           03 HIERARQUIA     PIC 9(02).
           03 STATUS1        PIC X(01).
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 ERRO                PIC X(02) VALUE "00".
       01 MENSAGEM-CONTADOR   PIC 9(04).
       01 MENSAGEM            PIC X(50).
       01 ESCAPE-KEY          PIC 9(02).
       01 AUX                 PIC 9(02).
       01 CLEARLINE           PIC X(73) VALUE SPACES.

       01 BUFFER.
           03 FILLER PIC X(24) VALUE "CONSELHO CONSULTIVO     ".
           03 FILLER PIC X(24) VALUE "CONSELHO FISCAL         ".
           03 FILLER PIC X(24) VALUE "PRESIDENCIA             ".
           03 FILLER PIC X(24) VALUE "DIRETORIA ADMINISTRATIVA".
           03 FILLER PIC X(24) VALUE "DIRETORIA INDUSTRIAL    ".
           03 FILLER PIC X(24) VALUE "DIRETORIA COMERCIAL     ".
           03 FILLER PIC X(24) VALUE "DIRETORIA MARKETING     ".
           03 FILLER PIC X(24) VALUE "DIRETORIA JURIDICA      ".
           03 FILLER PIC X(24) VALUE "CONSULTORIA             ".
      *-----------------------------------------------------------------
       01 FILLER REDEFINES BUFFER.
           03 VECTOR-NIVEL PIC X(24) OCCURS 9 TIMES.
      *
       01 VIEW-NIVEL.
           03 VIEW-NIVEL-EXT PIC X(24).
      *
       01 BUFFER-STATUS.
           03 FILLER PIC X(11) VALUE "AATIVA     ".
           03 FILLER PIC X(11) VALUE "SSUSPENSA  ".
           03 FILLER PIC X(11) VALUE "DDESATIVADA".
      *
       01 FILLER REDEFINES BUFFER-STATUS.
           03 VECTOR-STATUS PIC X(11) OCCURS 3 TIMES.
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
               VALUE  "                            CONSULTA  DE".
           05  LINE 03  COLUMN 41
               VALUE  "PARTAMENTOS".
           05  LINE 05  COLUMN 01
               VALUE  "________________________________________".
           05  LINE 05  COLUMN 41
               VALUE  "________________________________________".
           05  LINE 07  COLUMN 20
               VALUE  "   INSIRA O CODIGO: ".
           05  LINE 08  COLUMN 01
               VALUE  "________________________________________".		
           05  LINE 08  COLUMN 41
               VALUE  "________________________________________".           
		   05  LINE 09  COLUMN 01
               VALUE  "CODIGO  |DENOMINACAO              |".
           05  LINE 09  COLUMN 36
               VALUE  "NIVEL HIERARQUICO           |STATUS    ".
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
           05  CODIGO
               LINE 07  COLUMN 42  PIC 9(03)
               USING  CODIGO1.
           05  TMENSAGEM
               LINE 23  COLUMN 13  PIC X(50)
               USING  MENSAGEM.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
	   
       CREATE-FILES.
           PERFORM OPEN-ARQDEP.

       INICIO.
           PERFORM R0.
           DISPLAY TELA.

       INPUT-CODIGO.
           ACCEPT CODIGO.
           ACCEPT ESCAPE-KEY FROM ESCAPE KEY.

           IF ESCAPE-KEY = 01 GO TO FIM.

           PERFORM FIND.
           GO TO INPUT-CODIGO.

       FIM.
           CLOSE ARQDEP.
           STOP RUN.

       FIND.
           START ARQDEP KEY IS NOT LESS CODIGO1
           MOVE 11 TO LIN.
           PERFORM FIND-1.

       FIND-1.
           IF LIN < 21
           READ ARQDEP NEXT
           IF ERRO = "00"
               PERFORM CHECK-HIERARQUIA
               PERFORM CHECK-STATUS
               DISPLAY (LIN, 04) CODIGO1 "  |" DENOMINACAO "|" 
			   HIERARQUIA " " VIEW-NIVEL-EXT " |" STATUS1 " " 
               VIEW-STAT-EXT
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
           MOVE SPACES TO DENOMINACAO STATUS1 VIEW-NIVEL VIEW-STAT.
           MOVE ZEROS  TO CODIGO1 HIERARQUIA.


      *-----------------------------------------------------------------*
       CHECK-HIERARQUIA.
           MOVE SPACES TO VIEW-NIVEL.
           IF HIERARQUIA > 0 AND HIERARQUIA < 10
               MOVE VECTOR-NIVEL(HIERARQUIA) TO VIEW-NIVEL.

       CHECK-STATUS.
           MOVE ZEROS TO AUX
           PERFORM CHECK-STATUS-1.

       CHECK-STATUS-1.
           ADD 1 TO AUX.
           MOVE VECTOR-STATUS(AUX) TO VIEW-STAT.
           IF VIEW-STAT-SIGLA = STATUS1 MOVE 4 TO AUX
           ELSE MOVE SPACES TO VIEW-STAT.
           IF AUX < 4 GO TO CHECK-STATUS-1.

      *---------------------[ ROTINA DE ABERTURA ]---------------------
       OPEN-ARQDEP.
           OPEN INPUT ARQDEP.
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
