       IDENTIFICATION DIVISION.
       PROGRAM-ID. FPP002.
      *AUTOR: DIEGO TOSHIO TAKEDA KOGA              *
      ***********************************************
      *             CADASTRO DE CARGOS              *
      ***********************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *---------------------ARQUIVO DO CARGO---------------------------	  
           SELECT ARQCARG ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODIGO
                    ALTERNATE RECORD KEY IS DENOMINACAO WITH DUPLICATES
                    FILE STATUS  IS ST-ERRO.							
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
      *---------------------ARQUIVO DO FUNCIONARIO----------------------	  	   
       FD ARQCARG
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQCARG.DAT".
       01 REGCARG.
           03 CODIGO          PIC 9(03).
           03 DENOMINACAO     PIC X(25).
           03 TIPOSALARIO     PIC X(01).
		   03 SALARIOBASE     PIC 9(06)V99.
           03 STATUS1         PIC X(01).
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01 W-CONT             PIC 9(04) VALUE ZEROS.
       77 W-ACT              PIC 9(02) VALUE ZEROS.	   
       01 W-OPCAO            PIC X(01) VALUE SPACES.
	   01 OPCAO              PIC 9(02) VALUE ZEROS.
       01 W-SEL              PIC 9(01) VALUE ZEROS.	   
       01 ST-ERRO            PIC X(02) VALUE "00".
       01 MENS               PIC X(50) VALUE SPACES.
       01 LIMPA              PIC X(50) VALUE SPACES.
       01 W-PROGR            PIC X(07) VALUE SPACES.
       01 AUX                PIC 9(02) VALUE ZEROS.
	   01 AUX2               PIC 9(02) VALUE ZEROS.	   
       01 TXTAPLICACAO       PIC X(25) VALUE SPACES.	   	   
	   01 DISAUX             PIC X(25) VALUE SPACES.	    
      *-----------------------------------------------------------------
       01 TIPO-SALARIO.
          03 FILLER     PIC X(13) VALUE "HHORISTA".
		  03 FILLER     PIC X(13) VALUE "DDIARISTA".
		  03 FILLER     PIC X(13) VALUE "MMENSALISTA".
      *
       01 TABSALARIO REDEFINES TIPO-SALARIO.
          03 TBSALARIO   PIC X(13) OCCURS 3 TIMES.
      *
       01 TXTSALARIO.
          03 TXT3 PIC X(01) VALUE SPACES.
          03 TXT4 PIC X(11) VALUE SPACES.   
      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++	  
	  
       01 TABELA-STATUS.
          03 FILLER     PIC X(13) VALUE "AATIVA".
		  03 FILLER     PIC X(13) VALUE "SSUSPENSA".
		  03 FILLER     PIC X(13) VALUE "DDESATIVADA".
      *
       01 TABELA REDEFINES TABELA-STATUS.
          03 TBSTATUS  PIC X(13) OCCURS 3 TIMES.
      *
       01 TXT.
          03 TXT1 PIC X(01) VALUE SPACES.
          03 TXT2 PIC X(12) VALUE SPACES. 	  
	  
      *
      *-----------------------------------------------------------------  
      *                         ROTINA DE TELA                         *
      *-----------------------------------------------------------------       
       SCREEN SECTION.	   
	   
       01  TELA.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "            SISTEMA DE CADASTRO DE CARGOS".
           05  LINE 05  COLUMN 01 
               VALUE  "            CODIGO:".
           05  LINE 07  COLUMN 01 
               VALUE  "            DENOMINACAO:".
		   05  LINE 09  COLUMN 01
		       VALUE  "            TIPO DE SALARIO:".
		   05  LINE 11  COLUMN 01
		       VALUE  "            SALARIO BASE:".
           05  LINE 13  COLUMN 01 
               VALUE  "            STATUS :".           
		   05  LINE 23  COLUMN 01 
               VALUE  " MENSAGEM :".	
      *---------------------[ CODIGO ]-----------------------------------	   
           05  TCODIGO
               LINE 05  COLUMN 26  PIC 9(03)
               USING  CODIGO.
      *---------------------[ DENOMINACAO ]------------------------------	 			   
           05  TDENOMINACAO
               LINE 07  COLUMN 26  PIC X(25)
               USING  DENOMINACAO.
      *---------------------[ TIPO SALARIO ]-----------------------------	 			   
		   05  TSALARIO
		       LINE 09  COLUMN 30  PIC X(01)
			   USING TIPOSALARIO.
           05  TSALARIO2
               LINE 09  COLUMN 33 PIC X(11)
               USING TXT4.
      *---------------------[ SALARIO BASE ]-----------------------------			   
		   05  TSALARIOBASE
		       LINE 11  COLUMN 28 PIC ZZZ.ZZ9,99
			   USING SALARIOBASE.
      *---------------------[ STATUS ]-----------------------------------	   
           05  TSTATUS1
		       LINE 13  COLUMN 26 PIC X(01)
			   USING STATUS1.
		   05  TSTATUS2
		       LINE 13  COLUMN 29  PIC X(11)
			   USING TXT2.

      *---------------------[ TABELA DE TIPO DE SALARIO ]----------------
       01  TELA-TIPO-SALARIO.
           05  LINE 13  COLUMN 50
		       VALUE  "H-HORISTA".
           05  LINE 14  COLUMN 50
		       VALUE  "D-DIARISTA".
           05  LINE 15  COLUMN 50
		       VALUE  "M-MENSALISTA".
      *---------------------[ TABELA DE STATUS ]-------------------------
       01  TELA-STATUS.
           05  LINE 13  COLUMN 50
		       VALUE  "A-ATIVA".
           05  LINE 14  COLUMN 50
		       VALUE  "S-SUSPENSA".
           05  LINE 15  COLUMN 50
		       VALUE  "D-DESATIVADA".
			   
      *------------------------------------------------------------------
       PROCEDURE DIVISION.

       CREATE-FILES.
	       PERFORM I-O-ARQCARG.	   
      *-----------------------------------------------------------------  
      *                   INICIALIZACAO DAS VARIAVEIS                  *
      *----------------------------------------------------------------- 	   
       R0.
                MOVE ZEROS  TO CODIGO SALARIOBASE.
				MOVE ZEROS  TO W-SEL.
                MOVE SPACES TO DENOMINACAO STATUS1 TXT.
                MOVE SPACES TO TIPOSALARIO TXTSALARIO.
      *-----------------------------------------------------------------  
      *                        ROTINA DE SELECAO                       *
      *----------------------------------------------------------------- 
      *
               DISPLAY TELA.
	  
       R1.
           ACCEPT TCODIGO.
           ACCEPT W-ACT FROM ESCAPE KEY.
           IF W-ACT = 01 
		      GO TO ROT-FIM.
           IF CODIGO = ZEROS
              MOVE "*** DADO NAO PODE FICAR VAZIO **" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
			  GO TO R1.
                           
              PERFORM READ-ARQCARG.
           IF ST-ERRO = "00" 
		        MOVE "*** CODIGO JA CADASTRAD0 ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ACE-001.

       R2. 
                ACCEPT TDENOMINACAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 
                   GO TO R1.
                IF DENOMINACAO  = SPACES
                   MOVE " ESPACO NAO PODE FICAR EM BRANCO " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R2.  	

       R3.
	       DISPLAY TELA-TIPO-SALARIO
           MOVE 1 TO AUX
           ACCEPT TSALARIO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R2.
		   IF TIPOSALARIO = SPACES
	          MOVE "ESPACO NAO PODE FICAR EM BRANCO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R3.
       R3A.
           MOVE TBSALARIO(AUX) TO TXTSALARIO
           IF TXT3 NOT = TIPOSALARIO
              ADD 1 TO AUX
              IF AUX < 4
                 GO TO R3A
              ELSE
                 MOVE "*** TIPO INCORRETO***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R3
           ELSE
               DISPLAY TSALARIO2.
			   DISPLAY TELA.
	   
       R4.
	       ACCEPT TSALARIOBASE
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                 GO TO R3.
	          IF SALARIOBASE = ZEROS
	          MOVE "ESPACO NAO PODE FICAR EM BRANCO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R4.		   
	   
       R5.
	       DISPLAY TELA-STATUS
           MOVE 1 TO AUX2
           ACCEPT TSTATUS1.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R4.
		   IF STATUS1 = SPACES
	          MOVE "ESPACO NAO PODE FICAR EM BRANCO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R5.	
       R5A.
           MOVE TBSTATUS(AUX2) TO TXT
           IF TXT1 NOT = STATUS1
              ADD 1 TO AUX2
              IF AUX < 4
                 GO TO R5A
              ELSE
                 MOVE "*** TIPO STATUS INCORRETA***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R5
           ELSE
               DISPLAY TSTATUS2.
			   DISPLAY TELA.	  
	  

      *-----------------------------------------------------------------  
      *             ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO              *
      *----------------------------------------------------------------- 
 			
       SALVAR.
           MOVE "VOCE QUER GRAVAR OS DADOS? (S/N)" TO MENS
           DISPLAY (23, 12) MENS.
           ACCEPT W-OPCAO.
           MOVE SPACES TO MENS.
           DISPLAY (23, 12) MENS.

           IF W-ACT = 01 GO TO R5.

           IF W-OPCAO = "S" or "s"
			   PERFORM WRITE-ARQCARG
           ELSE IF W-OPCAO = "N" or "n" GO TO R0
           ELSE PERFORM ERROR-VALID GO TO SALVAR.
           GO TO R0.

      *
       DELETAR.
           MOVE "VOCE QUER EXCLUIR OS DADOS? (S/N)" TO MENS
           DISPLAY (23 12) MENS.
           ACCEPT W-OPCAO.
           MOVE SPACES TO MENS.
           DISPLAY (23, 12) MENS.

           IF W-ACT = 01 GO TO R5.

           IF W-OPCAO = "S" or "s"
               PERFORM DELETE-ARQCARG
               GO TO R0
           ELSE IF W-OPCAO = "N" or "n" NEXT SENTENCE
           ELSE PERFORM ERROR-VALID GO TO DELETAR.
      * 
       ACE-001.
                DISPLAY (23, 12)
                     "N=NOVO REGISTRO   A=ALTERAR   E=EXCLUIR"
                ACCEPT (23, 55) W-OPCAO
                IF W-OPCAO NOT = "N" AND W-OPCAO NOT = "A" 
                    AND W-OPCAO NOT = "E" GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY (23, 12) MENS
                IF W-OPCAO = "N"
                   GO TO R0  
                ELSE
                   IF W-OPCAO = "A"
                      MOVE 1 TO W-SEL
                      GO TO R3
			    ELSE
				   IF W-OPCAO = "E"
				      GO TO DELETAR.
      * 				
      *---------------------[ ROTINA DE ABERTURA ]---------------------
       I-O-ARQCARG.
	       OPEN I-O ARQCARG
		   IF ST-ERRO NOT = "00"
		      IF ST-ERRO = "30"
			   OPEN OUTPUT ARQCARG
			   CLOSE ARQCARG
			   PERFORM LOG-FILE-CREATED
			   GO TO I-O-ARQCARG
		      ELSE
			     IF ST-ERRO = "95"
                    MOVE "*** ISAM NAO EXECUTADO ***" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
					GO TO ROT-FIM
			    ELSE 
				    PERFORM ERROR-FILE-NOT-OPENED
					GO TO ROT-FIM.
      *---------------------[ ROTINA DE VERIFICACAO ]------------------
       CHECK-WRITE.
           IF ST-ERRO = "00" OR "02" 
		      PERFORM LOG-DATA-SAVED
           ELSE 
			  PERFORM ERROR-DATA-NOT-SAVED 
			  GO TO ROT-FIM.

       CHECK-READ.
           IF ST-ERRO NOT = "23"
               IF ST-ERRO = "00" 
			   PERFORM R3A
			   PERFORM R5A
 			   DISPLAY TELA		
			
           ELSE 
              PERFORM ERROR-FILE-NOT-READED 
			  GO TO ROT-FIM.

       CHECK-DELETE.
           IF ST-ERRO = "00" 
		      PERFORM LOG-DATA-DELETED
           ELSE 
		      PERFORM ERROR-DATA-NOT-DELETED 
			  GO TO ROT-FIM.
      *---------------------[ ROTINA DE ESCRITA ]----------------------
       WRITE-ARQCARG.
           WRITE REGCARG.
           IF ST-ERRO = "00" OR "02" 
		      PERFORM LOG-DATA-SAVED
           ELSE IF 
		      ST-ERRO = "22" 
              PERFORM REWRITE-ARQCARG
           ELSE PERFORM ERROR-DATA-NOT-SAVED 
		      GO TO ROT-FIM.
      *---------------------[ ROTINA DE REESCRITA ]---------------------
       REWRITE-ARQCARG.
     	   REWRITE REGCARG. 
		   PERFORM CHECK-WRITE.	
      *---------------------[ ROTINA DE LEITURA ]-----------------------
       READ-ARQCARG.
     	   READ ARQCARG. 
		   PERFORM CHECK-READ.
           IF ST-ERRO = "00" 
           DISPLAY TELA.
      *--------------------[ ROTINA DE EXCLUSAO ]-----------------------
       DELETE-ARQCARG. 
	       DELETE ARQCARG RECORD. 
		   PERFORM CHECK-DELETE.

      *-----------------------[ ROTINA DE FIM ]------------------------
       ROT-FIM.
           CLOSE ARQCARG
           STOP RUN.
           EXIT PROGRAM.
      *---------[ ROTINA DE MENSAGEM ]---------------------
       ERROR-VALID.
           MOVE "OPCAO NAO VALIDA" TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM.

       ERROR-EMPTY.
           MOVE " ESPACO NAO PODE FICAR EM BRANCO " TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM.

       ERROR-FILE-NOT-OPENED.
           MOVE "ERRO NA ABERTURA DO ARQUIVO" TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM.

       ERROR-DATA-NOT-SAVED.
           MOVE "DADOS NAO GRAVADOS" TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM.

       ERROR-DATA-NOT-FOUND.
           MOVE "DADOS NAO ENCONTRADOS" TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM.

       ERROR-FILE-NOT-READED.
           MOVE "ERRO NA LEITURA DO ARQUIVO" TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM.

       ERROR-DATA-NOT-DELETED.
           MOVE "DADOS NAO DELETADOS" TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM.

       LOG-FILE-CREATED.
           MOVE "ARQUIVO CRIADO" TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM.

       LOG-DATA-SAVED.
           MOVE "DADOS GRAVADOS" TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM.

       LOG-DATA-DELETED.
           MOVE "DADOS DELETADOS" TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM.	  
      ************************************************************		  
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
