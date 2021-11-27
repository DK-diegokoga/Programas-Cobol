       IDENTIFICATION DIVISION.
       PROGRAM-ID. FPP013.
      *AUTOR: DIEGO TOSHIO TAKEDA KOGA              *
      ***********************************************
      *             CONSULTA DE DEPENDENTES         *
      ***********************************************
      *----------------------------------------------------------------		   
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *---------------------ARQUIVO DO DEPENDENTE----------------
           SELECT ARQDEPE ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS DEPEN-KEY
                    ALTERNATE RECORD KEY IS NOME WITH DUPLICATES					
                    FILE STATUS  IS ERRO.
      *---------------------ARQUIVO DO FUNCIONARIO---------------- 					
           SELECT ARQFUNC ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CHAPA
                    ALTERNATE RECORD KEY IS NOME1 WITH DUPLICATES					
                    FILE STATUS  IS ERRO.
      *---------------------ARQUIVO DO DEPARTAMENTO---------------
           SELECT ARQDEP ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODIGO1
                    ALTERNATE RECORD KEY IS DENOMINACAO WITH DUPLICATES					
                    FILE STATUS  IS ERRO.
      *---------------------ARQUIVO DO CARGO----------------------
           SELECT ARQCARG ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODIGO
                    ALTERNATE RECORD KEY IS DENOMINACAO WITH DUPLICATES					
                    FILE STATUS  IS ERRO.

       DATA DIVISION.
       FILE SECTION.

      *---------------------ARQUIVO DO DEPENDENTE----------------
       FD ARQDEPE
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQDEPE.DAT".

       01 REGDEPE.
           03 DEPEN-KEY.
            05 DEPEN-CHAPA        PIC 9(06).
            05 DEPEN-SEQ          PIC 9(01).
           03 NOME                PIC X(30).
           03 PARENTESCO          PIC 9(02).		   
           03 DEPEN-DATANASC      PIC 9(08).
           03 DTNASCIMENTO REDEFINES DEPEN-DATANASC.
               05 DIA                 PIC 9(02).
               05 MES                 PIC 9(02).
               05 ANO                 PIC 9(04).
           03 FILLER           PIC X(01).
           03 FILLER           PIC X(01).
           03 FILLER           PIC X(01).
           03 STATUS1            PIC X(01).
      *---------------------ARQUIVO DO FUNCIONARIO----------------		   
       FD ARQFUNC
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQFUNC.DAT".

       01 REGFUNC.
           03 CHAPA            PIC 9(06).
           03 NOME1             PIC X(30).
           03 FILLER           PIC 9(08).
           03 FILLER           PIC X(01).
           03 FILLER           PIC X(01).
           03 CODPTO           PIC 9(03).
           03 CODCARG          PIC 9(03).
           03 FILLER           PIC 9(02).
           03 FILLER           PIC X(01).
           03 FILLER           PIC 9(01).
           03 FILLER           PIC X(01).
      *---------------------ARQUIVO DO DEPARTAMENTO---------------		   
       FD ARQDEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQDEP.DAT".

       01 REGDEP.
           03 CODIGO1      PIC 9(03).
           03 DENOMINACAO1  PIC X(25).
           03 FILLER       PIC 9(02).
           03 FILLER       PIC X(01).
      *---------------------ARQUIVO DO CARGO----------------------
       FD ARQCARG
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQCARG.DAT".

       01 REGCARG.
           03 CODIGO       PIC 9(03).
           03 DENOMINACAO  PIC X(25).
           03 FILLER       PIC X(01).
           03 FILLER       PIC 9(06)V99.
           03 FILLER       PIC X(01).
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 ERRO                PIC X(02) VALUE "00".
       01 MENSAGEM-CONTADOR   PIC 9(04).
       01 MENSAGEM            PIC X(50).
       01 ESCAPE-KEY          PIC 9(02).
       01 ITERATOR            PIC 9(02).
       01 CLEARLINE           PIC X(78) VALUE SPACES.

       01 BUFFER-GRAUS.
           03 FILLER PIC X(17) VALUE "ESPOSA           ".
           03 FILLER PIC X(17) VALUE "FILHO(A)         ".
           03 FILLER PIC X(17) VALUE "ENTEADO(A)       ".
           03 FILLER PIC X(17) VALUE "PAI              ".
           03 FILLER PIC X(17) VALUE "MÃE              ".
           03 FILLER PIC X(17) VALUE "AVOS             ".
           03 FILLER PIC X(17) VALUE "NETO(A)          ".
           03 FILLER PIC X(17) VALUE "AGREGADO         ".
           03 FILLER PIC X(17) VALUE "RESPONSAVEL LEGAL".
           03 FILLER PIC X(17) VALUE "OUTROS           ".

       01 FILLER REDEFINES BUFFER-GRAUS.
           03 VECTOR-GRAUS PIC X(17) OCCURS 10 TIMES.

       01 VIEW-GRAU.
           03 VIEW-GRAU-EXT PIC X(17).

       01 BUFFER-STATS.
           03 FILLER PIC X(14) VALUE "AATIVA        ".
           03 FILLER PIC X(14) VALUE "SSUSPENSA     ".
           03 FILLER PIC X(14) VALUE "DDESATIVADA   ".
           03 FILLER PIC X(14) VALUE "EEX DEPENDENTE".

       01 FILLER REDEFINES BUFFER-STATS.
           03 VECTOR-STATS PIC X(14) OCCURS 4 TIMES.

       01 VIEW-STAT.
           03 VIEW-STAT-SIGLA  PIC X(01).
           03 VIEW-STAT-EXT    PIC X(13).
      *-----------------------------------------------------------------  
      *                         ROTINA DE TELA                         *
      *----------------------------------------------------------------- 
       SCREEN SECTION.
       01  TELA.
           05  BLANK SCREEN.	   
           05  LINE 02  COLUMN 01
               VALUE  "                          CONSULTA DE DE".
           05  LINE 02  COLUMN 41
               VALUE  "PENDENTES".
           05  LINE 03  COLUMN 01
               VALUE  "________________________________________".
           05  LINE 03  COLUMN 41
               VALUE  "________________________________________".
           05  LINE 05  COLUMN 01
               VALUE  "   CODIGO:".
           05  LINE 06  COLUMN 01
               VALUE  "   DEPARTAMENTO:".
           05  LINE 07  COLUMN 01
               VALUE  "   NOME:".		   
           05  LINE 08  COLUMN 01
               VALUE  "   CARGO:".
           05  LINE 09  COLUMN 01
               VALUE  "________________________________________".		
           05  LINE 09  COLUMN 41
               VALUE  "________________________________________". 			   
           05  LINE 10  COLUMN 01
               VALUE  "   SEQUENCIA   NOME  DEPENDENTE       GR".
           05  LINE 10  COLUMN 41
               VALUE  "AU          NASCIMENTO       STATUS     ".
           05  LINE 11  COLUMN 01
               VALUE  "________________________________________".		
           05  LINE 11  COLUMN 41
               VALUE  "________________________________________". 			   
           05  LINE 21  COLUMN 01
               VALUE  "________________________________________".
           05  LINE 21  COLUMN 41
               VALUE  "________________________________________".
           05  LINE 23  COLUMN 01
               VALUE  "  MENSAGEM:".
           05  SCREEN-CHAPA
               LINE 05  COLUMN 13  PIC 9(06)
               USING  CHAPA.
           05  SCREEN-DEP-CODIGO
               LINE 06  COLUMN 18  PIC 9(03)
               USING  CODPTO.
           05  SCREEN-DEP-CODIGO-EXT
               LINE 06  COLUMN 22  PIC X(25)
               USING  DENOMINACAO1.
           05  SCREEN-NOME
               LINE 07  COLUMN 37  PIC X(25)
               USING  NOME.			   
           05  SCREEN-CARGO-CODIGO
               LINE 08  COLUMN 11  PIC 9(03)
               USING  CODCARG.
           05  SCREEN-CARGO-CODIGO-EXT
               LINE 08  COLUMN 15  PIC X(25)
               USING  DENOMINACAO.
           05  SCREEN-MENSAGEM
               LINE 23  COLUMN 13  PIC X(50)
               USING  MENSAGEM.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       CREATE-FILES.
           PERFORM OPEN-CADFUNC.
           PERFORM OPEN-CADDEPE.
           PERFORM OPEN-CADDEP.
           PERFORM OPEN-CADCARGO.

       INICIO.
           PERFORM CLEAR-REGFUNC.
           PERFORM CLEAR-REGDEPE.
           PERFORM CLEAR-REGDEP.
           PERFORM CLEAR-REGCARGO.
           DISPLAY TELA.

       INPUT-CHAPA.
           ACCEPT SCREEN-CHAPA.
           ACCEPT ESCAPE-KEY FROM ESCAPE KEY.

           IF ESCAPE-KEY = 01 GO TO FIM.

           PERFORM READ-CADFUNC.
           IF ERRO NOT = "00"
               GO TO INICIO.

           PERFORM READ-CADDEP.
           PERFORM READ-CADCARGO.
           DISPLAY TELA.

           PERFORM FIND.
           GO TO INPUT-CHAPA.
      *-----------------------[ ROTINA DE FIM ]------------------------
       FIM.
           CLOSE ARQFUNC.
           CLOSE ARQDEPE.
           CLOSE ARQDEP.
           CLOSE ARQCARG.
           STOP RUN.

       FIND.
           MOVE CHAPA TO DEPEN-CHAPA
           MOVE ZEROS TO DEPEN-SEQ
           START ARQDEPE KEY IS NOT LESS DEPEN-KEY
           MOVE 11 TO LIN.
           PERFORM FIND-1.

       FIND-1.
           IF LIN < 21
           READ ARQDEPE NEXT
           IF CHAPA NOT = DEPEN-CHAPA DISPLAY (LIN, 02) CLEARLINE
               ADD 1 TO LIN
               GO TO FIND-1

           ELSE IF ERRO = "00"
               PERFORM CHECK-GRAU
               PERFORM CHECK-STATS
               DISPLAY (LIN, 02) DEPEN-SEQ "  " DENOMINACAO "  " 
               PARENTESCO " " VIEW-GRAU-EXT " " DIA "/" MES "/" 
			   ANO "  " STATUS1 " " VIEW-STAT-EXT
               ADD 1 TO LIN
               GO TO FIND-1

           ELSE IF ERRO = "10" DISPLAY (LIN, 02) CLEARLINE
               ADD 1 TO LIN
               GO TO FIND-1

           ELSE MOVE ERRO TO MENSAGEM PERFORM SHOW-MESSAGE.

      *-----------------------------------------------------------------*
       CHECK-GRAU.
           MOVE SPACES TO VIEW-GRAU.
           IF PARENTESCO > 0 AND PARENTESCO < 10
               MOVE VECTOR-GRAUS(PARENTESCO) TO VIEW-GRAU.

       CHECK-STATS.
           MOVE ZEROS TO ITERATOR
           PERFORM CHECK-STATS-1.

       CHECK-STATS-1.
           ADD 1 TO ITERATOR.
           MOVE VECTOR-STATS(ITERATOR) TO VIEW-STAT.
           IF VIEW-STAT-SIGLA = STATUS1 MOVE 5 TO ITERATOR
           ELSE MOVE SPACES TO VIEW-STAT.
           IF ITERATOR < 5 GO TO CHECK-STATS-1.

      *-----------------------------------------------------------------  
      *                   INICIALIZACAO DAS VARIAVEIS                  *
      *----------------------------------------------------------------- 
       CLEAR-REGFUNC.
           MOVE SPACES TO NOME
           MOVE ZEROS  TO CHAPA CODCARG CODPTO.

       CLEAR-REGDEPE.
           MOVE SPACES TO NOME STATUS1.
           MOVE ZEROS TO DEPEN-SEQ PARENTESCO DEPEN-DATANASC.

       CLEAR-REGDEP.
           MOVE SPACES TO DENOMINACAO1.
           MOVE ZEROS TO CODIGO1.

       CLEAR-REGCARGO.
           MOVE SPACES TO DENOMINACAO.
           MOVE ZEROS TO CODIGO.


      *---------------------[ ROTINA DE ABERTURA ]---------------------
       OPEN-CADFUNC. OPEN INPUT ARQFUNC PERFORM CHECK-OPEN.
       OPEN-CADDEPE. OPEN INPUT ARQDEPE PERFORM CHECK-OPEN.
       OPEN-CADDEP. OPEN INPUT ARQDEP PERFORM CHECK-OPEN.
       OPEN-CADCARGO. OPEN INPUT ARQCARG PERFORM CHECK-OPEN.

       CHECK-OPEN.
           IF ERRO NOT = "00"
              IF ERRO = "30" PERFORM ERROR-FILE-NOT-EXIST GO TO FIM
              ELSE IF ERRO = "95"
                  PERFORM ERROR-ISAM-NOT-LOADED GO TO FIM
              ELSE PERFORM ERROR-FILE-NOT-OPENED GO TO FIM.

       CHECK-READ.
           IF ERRO NOT = "23" AND ERRO NOT = "00"
           PERFORM ERROR-FILE-NOT-READED GO TO FIM.

       READ-CADFUNC. READ ARQFUNC. PERFORM CHECK-READ.
       READ-CADDEP.
           MOVE CODPTO TO CODIGO1.
           READ ARQDEP. PERFORM CHECK-READ.

       READ-CADCARGO.
           MOVE CODCARG TO CODIGO.
           READ ARQCARG. PERFORM CHECK-READ.

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

       ERROR-FILE-NOT-READED.
           MOVE "ERRO NA LEITURA DO ARQUIVO" TO MENSAGEM
           PERFORM SHOW-MESSAGE.
		   
      ************************************************************
       SHOW-MESSAGE.
           MOVE ZEROS TO MENSAGEM-CONTADOR.
           DISPLAY SCREEN-MENSAGEM.

           PERFORM SHOW-MESSAGE-1.

           MOVE SPACES TO MENSAGEM.
           DISPLAY SCREEN-MENSAGEM.

       SHOW-MESSAGE-1.
           ADD 1 TO MENSAGEM-CONTADOR.
           IF MENSAGEM-CONTADOR < 3000 
		   GO TO SHOW-MESSAGE-1.
