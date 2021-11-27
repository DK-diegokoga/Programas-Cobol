       IDENTIFICATION DIVISION.
       PROGRAM-ID. FPP003.
      *AUTOR: DIEGO TOSHIO TAKEDA KOGA              *
      ***********************************************
      *           CADASTRO DE DEPENDENTES           *
      ***********************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                     DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *---------------------ARQUIVO DO DEPENDENTE----------------
           SELECT ARQDEPE ASSIGN TO DISK
                      ORGANIZATION IS INDEXED
                      ACCESS MODE IS DYNAMIC
                      RECORD KEY IS DEPEN-KEY
                      ALTERNATE RECORD KEY IS NOME
                      WITH DUPLICATES
                      FILE STATUS IS ST-ERRO.
      *---------------------ARQUIVO DO FUNCIONARIO----------------        					  
           SELECT ARQFUNC ASSIGN TO DISK
                      ORGANIZATION IS INDEXED
                      ACCESS MODE IS DYNAMIC
                      RECORD KEY IS CHAPA
                      FILE STATUS IS ST-ERRO.
      *------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
      *---------------------ARQUIVO DO DEPENDENTE------------------   
       FD ARQDEPE
                 LABEL RECORD IS STANDARD
                 VALUE OF FILE-ID IS "ARQDEPE.DAT".
       01 REGDEPE.
           03 DEPEN-KEY.
            05 DEPEN-CHAPA        PIC 9(06).
            05 DEPEN-SEQ          PIC 9(01).
           03 NOME                PIC X(30).
           03 PARENTESCO          PIC 9(02).		   
           03 DEPEN-DATANASC           PIC 9(08).
           03 DTNASCIMENTO REDEFINES DEPEN-DATANASC.
            05 DIA                 PIC 9(02).
            05 MES                 PIC 9(02).
            05 ANO                 PIC 9(04).
           03 SEXO        PIC X(01).
           03 DEPEN-INSS        PIC X(01).
           03 DEPEN-IMPRENDA    PIC X(01).
           03 STATUS1      PIC X(20).
      *---------------------ARQUIVO DO FUNCIONARIO--------------------
       FD ARQFUNC
                 LABEL RECORD IS STANDARD
                 VALUE OF FILE-ID IS "ARQFUNC.DAT".
       01 REGFUNC.
           03 CHAPA             PIC 9(06).

      *---------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-CONT          PIC 9(06) VALUE ZEROS.
       77 W-OPCAO         PIC X(01) VALUE SPACES.
       77 W-ACT           PIC 9(02) VALUE ZEROS.
       77 MENS            PIC X(50) VALUE SPACES.
       77 LIMPA           PIC X(50) VALUE SPACES.
       01 ST-ERRO         PIC X(02) VALUE "00".
       01 W-SEL           PIC 9(01) VALUE ZEROS.
       01 AUX             PIC 9(01) VALUE ZEROS.
       01 TXTTGRAU        PIC X(20) VALUE SPACES.
       01 TEXSEXO         PIC X(15) VALUE SPACES.
       01 TDEPENINSS      PIC X(10) VALUE SPACES.
       01 TDEPENSEXO      PIC X(10) VALUE SPACES.
       01 TABGRAU.
           03 TB         PIC X(20) OCCURS 10 TIMES.
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
      *----------------------------------------------------------------*  
      *                         ROTINA DE TELA                         *
      *----------------------------------------------------------------* 
       SCREEN SECTION.
       01  TELA.
           05  BLANK SCREEN.  	     
           05  LINE 02  COLUMN 01 
               VALUE  "                     CADASTRAD0 DE DE".
           05  LINE 02  COLUMN 41 
               VALUE  "PENDENTE".
           05  LINE 05  COLUMN 01 
               VALUE  " CHAPA:              |".
           05  LINE 06  COLUMN 01 
               VALUE  " SEQUENCIA:          |".
           05  LINE 07  COLUMN 01
               VALUE  " NOME:               |".
           05  LINE 08  COLUMN 01
               VALUE  " PARENTESCO:         |".
           05  LINE 09  COLUMN 01 
               VALUE  " DATA DE NASCIMENTO: |".
           05  LINE 10  COLUMN 01              
               VALUE  " SEXO:               |".
           05  LINE 11  COLUMN 01 
               VALUE  " DEPEND.INSS:        |".       
           05  LINE 12  COLUMN 01 
               VALUE  " DEPEND.IMP.RENDA:   |".  
           05  LINE 13  COLUMN 01 
               VALUE  " STATUS:             |".
           05  LINE 23  COLUMN 01 
               VALUE  " MENSAGEM :". 

      *---------------------[ CHAPA ]-------------------
           05  TCHAPA
               LINE 05  COLUMN 24  PIC 9(06)
               USING  DEPEN-CHAPA.
      *---------------------[ SEQUENCIA ]---------------			   
           05  TSEQUENCIA
               LINE 06  COLUMN 24  PIC 9(01)
               USING  DEPEN-SEQ
               HIGHLIGHT.	
      *---------------------[ NOME]---------------------		   
           05  TNOME
               LINE 07  COLUMN 24  PIC X(30)
               USING  NOME
               HIGHLIGHT.
      *---------------------[ PARENTESCO]---------------
           05  TTPGRAU
               LINE 08  COLUMN 24  PIC 9(02)
               USING  PARENTESCO
               HIGHLIGHT.
           05  TXTGRAU
               LINE 08  COLUMN 26  PIC X(20)
               USING  TXTTGRAU
               HIGHLIGHT.
      *---------------------[ NASCIMENTO ]--------------			   
           05  TDTNASCIMENTO
               LINE 09  COLUMN 24  PIC 99/99/9999
               USING  DTNASCIMENTO
               HIGHLIGHT.
      *---------------------[ SEXO]---------------------			   
           05  TSEXO
               LINE 10  COLUMN 24 PIC X(01)
               USING  SEXO
               HIGHLIGHT.
           05  TTSEXO
               LINE 10  COLUMN 26 PIC X(10)
               USING  TDEPENSEXO
               HIGHLIGHT.
      *---------------------[ INSS]---------------------			   
           05  TDEPEN-INSS
               LINE 11  COLUMN 24 PIC X(01)
               USING  DEPEN-INSS
               HIGHLIGHT.
           05  TTDEPEN-INSS
               LINE 11  COLUMN 26 PIC X(10)
               USING TDEPENINSS
               HIGHLIGHT.
      *---------------------[ IMPOSTO]-------------------			   
           05  TDEPEN-IMPRENDA
               LINE 12  COLUMN 24 PIC X(01)
               USING  DEPEN-IMPRENDA
               HIGHLIGHT.
      *---------------------[ STATUS]-------------------			   
           05  TSTATUS
               LINE 13 COLUMN 24  PIC X(20)
               USING  STATUS1
               HIGHLIGHT.
           05  TSTATUS2
               LINE 13  COLUMN 26  PIC X(12)
               USING TXT2.		   
			   
      *---------------------[ TABELA DE GENERO ]-------------
       01  TELA-PARENTESCO.
           05  LINE 13  COLUMN 50
               VALUE  "0-ESPOSA".
           05  LINE 14  COLUMN 50
               VALUE  "1-FILHO(A)".
           05  LINE 15  COLUMN 50
               VALUE  "2-ENTEADO(A)".
           05  LINE 16  COLUMN 50
               VALUE  "3-PAI". 
           05  LINE 17  COLUMN 50
               VALUE  "4-MAE".
           05  LINE 18  COLUMN 50
               VALUE  "5-AVOS".
           05  LINE 19  COLUMN 50
               VALUE  "6-NETO(A)".
           05  LINE 20  COLUMN 50
               VALUE  "7-AGREGADO". 
           05  LINE 21  COLUMN 50
               VALUE  "8-RESPONSAVEL LEGAL".
           05  LINE 22  COLUMN 50
               VALUE  "OUTROS".   
      *---------------------[ TABELA DE STATUS ]-------------
       01  TELA-STATUS.
           05  LINE 06  COLUMN 50
               VALUE  "A-ATIVA".
           05  LINE 07  COLUMN 50
               VALUE  "S-SUSPENSA".
           05  LINE 08  COLUMN 50
               VALUE  "D-DESATIVADA".			   
      *---------------------[ TABELA DE IMPOSTO ]------------
       01  TELA-IMP.
           05  LINE 05  COLUMN 50
               VALUE  "S-SIM".
           05  LINE 06  COLUMN 50
               VALUE  "N-NAO".
           05  LINE 07  COLUMN 50
               VALUE  "E-ESPECIAL".       	
      *---------------------[ TABELA DE SEXO ]-------------
       01  TELA-SEXO.
           05  LINE 05  COLUMN 50
               VALUE  "F-FEMININO".
           05  LINE 06  COLUMN 50
               VALUE  "M-MASCULINO".   			   
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
           MOVE "1-ESPOSA "           TO TB(01)
           MOVE "2-FILHO(A)"          TO TB(02)
           MOVE "3-ENTEADO(A)"        TO TB(03)
           MOVE "4-PAI"               TO TB(04)
           MOVE "5-MAE"               TO TB(05)
           MOVE "6-AVOS"              TO TB(06)
           MOVE "7-NETO(A)"           TO TB(07)
           MOVE "AGREGADO"            TO TB(08)		   
           MOVE "8-RESPONSAVEL LEGAL" TO TB(09)
           MOVE "9-OUTROS"            TO TB(10).
       CREATE-FILES.
           PERFORM I-O-ARQDEPE.
           PERFORM I-O-ARQFUNC.
      *-----------------------------------------------------------------
      *                   INICIALIZACAO DAS VARIAVEIS                  *
      *-----------------------------------------------------------------
       R0.
           MOVE SPACES TO SEXO DEPEN-INSS  DEPEN-IMPRENDA
           MOVE SPACES TO STATUS1 TXTTGRAU NOME
           MOVE ZEROS TO DEPEN-SEQ PARENTESCO DTNASCIMENTO DEPEN-CHAPA
      *-----------------------------------------------------------------
      *                        ROTINA DE SELECAO                       *
      *-----------------------------------------------------------------
           DISPLAY TELA.
       R2.
           ACCEPT TCHAPA.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO ROT-FIM.
           IF DEPEN-CHAPA = ZEROS
                 MOVE "*** CODIGO CARGO. NAO INFORMADO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R2.
       R3.
           MOVE DEPEN-CHAPA TO CHAPA.
              PERFORM READ-ARQDEPE. 
              PERFORM READ-ARQFUNC.
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "23"
                   MOVE "*** FUNCIONARIO DIGITADO NAO ENCONTRADO ***"
                   TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R2
               ELSE
                  MOVE "ERRO NA LEITURA ARQUIVO CADASTRO FUNCIONARIO"
                  TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM
           ELSE
                DISPLAY TELA.
       ROT-NOME.
       R4.
       LER-ARQDEPE.
           READ ARQDEPE
           IF ST-ERRO NOT = "23"
              IF ST-ERRO = "00"
                DISPLAY TELA
                MOVE "*** DEPENDENTE JA CADASTRAD0 ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ACE-001
      *          GO TO R5
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO ARQDEPE" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                MOVE "*** DEPENDENTE NAO CADASTRADO ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM.
       R5.
           ACCEPT TSEQUENCIA.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R2.
           IF DEPEN-SEQ = 0
              MOVE "ESPACO NAO PODE FICAR EM BRANCO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R5.
       R6.
           ACCEPT TNOME.
           ACCEPT W-ACT FROM ESCAPE KEY.
             IF W-ACT = 01 
                GO TO R5.		   
           IF NOME = SPACES
              MOVE "ESPACO NAO PODE FICAR EM BRANCO" 
			  TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R6.
       R7.
           DISPLAY TELA-PARENTESCO.	   
           ACCEPT TTPGRAU.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                DISPLAY TELA
                GO TO R6.
           IF PARENTESCO < 0 OR > 10
                MOVE "*** DIGITE APENAS DE 0 ATE 9 ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R7.
       R7A.
           ADD PARENTESCO TO AUX
           MOVE TB(AUX) TO TXTTGRAU
           DISPLAY TXTGRAU.
		   DISPLAY TELA.

       R8.
           ACCEPT TDTNASCIMENTO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                GO TO R7.	   
           IF DIA < 0 OR DIA = 0 OR DIA > 31 OR MES < 0 OR MES = 0 OR
              MES > 12 OR ANO < 1900 OR ANO > 2021
              MOVE " DATA DE NASCIMENTO INVALIDO " TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R8.
       R9.
           DISPLAY TELA-SEXO.	   
           ACCEPT TSEXO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R8.
       R9A.
           IF SEXO = "M" OR SEXO = "m"
              MOVE "MASCULINO" TO TDEPENSEXO
           ELSE
              IF SEXO = "F" OR SEXO = "f"
                 MOVE "FEMININO" TO TDEPENSEXO
              ELSE
                 MOVE "*DIGITE M=MASCULINO    F=FEMININO*" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R9.
           DISPLAY TTSEXO.
		   DISPLAY TELA.
       R10. 
           DISPLAY TELA-IMP.	   
           ACCEPT TDEPEN-INSS
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R9.
       R10A.
           IF DEPEN-INSS = "S" OR DEPEN-INSS = "s"
              MOVE "SIM" TO TDEPENINSS
           ELSE
              IF DEPEN-INSS = "N" OR DEPEN-INSS = "n"
                 MOVE "NAO" TO TDEPENINSS
              ELSE
                 IF DEPEN-INSS = "E" OR DEPEN-INSS = 'e'
                     MOVE "ESPECIAL" TO TDEPENINSS
                 ELSE
                     MOVE "*DIGITE M=MASCULINO    F=FEMININO*" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     GO TO R10.
           DISPLAY TDEPENINSS.
           DISPLAY TELA.
       R11.
           ACCEPT TDEPEN-IMPRENDA.
           IF DEPEN-IMPRENDA = SPACES
              MOVE "CAMPO IMP. RENDA VAZIO!" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R11.
       R12.
           DISPLAY TELA-STATUS.	   
           MOVE 1 TO AUX
           ACCEPT TSTATUS.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R11.
           IF STATUS1 = SPACES
              MOVE "ESPACO NAO PODE FICAR EM BRANCO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R12. 			  
       R12A.
           MOVE TBSTATUS(AUX) TO TXT
           IF TXT1 NOT = STATUS1
              ADD 1 TO AUX
              IF AUX < 4
                 GO TO R12A
              ELSE
                 MOVE "*** TIPO STATUS INCORRETA***" TO MENS
                 MOVE 1 TO AUX
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R12
           ELSE

              ADD 1 TO AUX
              DISPLAY TSTATUS2.
              DISPLAY TELA.
      * ------------- VERICAR SE E ALTERACAO ---------------------------
      
      *-----------------------------------------------------------------  
      *             ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO              *
      *-----------------------------------------------------------------      
       SALVAR.
           MOVE "VOCE QUER GRAVAR OS DADOS? (S/N)" TO MENS
           DISPLAY (23, 12) MENS.
           ACCEPT W-OPCAO.
           MOVE SPACES TO MENS.
           DISPLAY (23, 12) MENS.

           IF W-ACT = 01 GO TO R12.

           IF W-OPCAO = "S" or "s"
               PERFORM WRITE-ARQDEPE           
               PERFORM WRITE-ARQFUNC

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

           IF W-ACT = 01 GO TO R9.

           IF W-OPCAO = "S" or "s"
               PERFORM DELETE-ARQDEPE
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
                      GO TO R5
                ELSE
                   IF W-OPCAO = "E"
                      GO TO DELETAR.
      *                 

      *---------------------[ ROTINA DE ABERTURA ]---------------------

       I-O-ARQDEPE.
           OPEN I-O ARQDEPE
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                  OPEN OUTPUT ARQDEPE
                  CLOSE ARQDEPE
                  PERFORM LOG-FILE-CREATED
                  GO TO I-O-ARQDEPE
              ELSE
                 IF ST-ERRO = "95"
                    MOVE "*** ISAM NAO EXECUTADO ***" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO ROT-FIM                 
              ELSE 
                  PERFORM ERROR-FILE-NOT-OPENED
                  GO TO ROT-FIM.

       I-O-ARQFUNC.
           OPEN I-O ARQFUNC
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                  OPEN OUTPUT ARQFUNC
                  CLOSE ARQFUNC
                  PERFORM LOG-FILE-CREATED
                  GO TO I-O-ARQFUNC
              ELSE 
                  PERFORM ERROR-FILE-NOT-OPENED
                  GO TO ROT-FIM.

      *---------------------[ ROTINA DE VERIFICACAO ]---------------------
       CHECK-WRITE.
           IF ST-ERRO = "00" OR "02" 
              PERFORM LOG-DATA-SAVED.
      *     ELSE 
      *       PERFORM ERROR-DATA-NOT-SAVED 
      *       GO TO ROT-FIM.

       CHECK-READ.
           IF ST-ERRO NOT = "23"
               IF ST-ERRO = "00" 
               PERFORM R7A
               PERFORM R9A
			   PERFORM R10A
			   PERFORM R12A
      ****************************************                 
      ****************************************
      * NAO ESQUECER DE BOTAR OS PERFORM AQUI 
      ****************************************    
      ****************************************        
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
 
       WRITE-ARQDEPE.
           WRITE REGDEPE.
           IF ST-ERRO = "00" OR "02" 
              PERFORM LOG-DATA-SAVED
           ELSE IF 
              ST-ERRO = "22" 
              PERFORM REWRITE-ARQDEPE.
      *     ELSE PERFORM ERROR-DATA-NOT-SAVED 
      *        GO TO ROT-FIM.
       
       WRITE-ARQFUNC.
           WRITE REGFUNC.
           IF ST-ERRO = "00" OR "02" 
              PERFORM LOG-DATA-SAVED
           ELSE IF 
              ST-ERRO = "22" 
              PERFORM REWRITE-ARQFUNC.
      *     ELSE PERFORM ERROR-DATA-NOT-SAVED 
      *       GO TO ROT-FIM.

      *---------------------[ ROTINA DE REESCRITA ]---------------------

       REWRITE-ARQDEPE. 
           REWRITE REGDEPE. 
           PERFORM CHECK-WRITE.
           
       REWRITE-ARQFUNC. 
           REWRITE REGFUNC. 
           PERFORM CHECK-WRITE.
      *---------------------[ ROTINA DE LEITURA ]-----------------------

       READ-ARQDEPE. 
           READ ARQDEPE. 
           PERFORM CHECK-READ.
           DISPLAY TELA.   
           
       READ-ARQFUNC. 
           READ ARQFUNC. 
            MOVE DEPEN-CHAPA TO CHAPA
           PERFORM CHECK-READ.
           DISPLAY TELA.  
    
      *--------------------[ ROTINA DE EXCLUSAO ]----------------------
     
       DELETE-ARQDEPE. 
           DELETE ARQDEPE RECORD. 
           PERFORM CHECK-DELETE.           
     
       DELETE-ARQFUNC. 
           DELETE ARQFUNC RECORD. 
           PERFORM CHECK-DELETE. 
       
      *-----------------------[ ROTINA DE FIM ]------------------------
       ROT-FIM.
           CLOSE ARQDEPE       
           CLOSE ARQFUNC
           STOP RUN.
           EXIT PROGRAM.
      *---------------------[ ROTINA DE MENSAGEM ]---------------------

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