       IDENTIFICATION DIVISION.
       PROGRAM-ID. FPP004.
      *AUTOR: DIEGO TOSHIO TAKEDA KOGA              *
      ***********************************************
      *           CADASTRO DE FUNCIONARIO          *
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
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CHAPA
                    ALTERNATE RECORD KEY IS CHAVE2 = NOME DATANASC CHAPA
                                    WITH DUPLICATES
                    FILE STATUS  IS ST-ERRO.		
      *---------------------ARQUIVO DO DEPARTAMENTO--------------------
           SELECT ARQDEP ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODIGO1 
                    FILE STATUS  IS ST-ERRO.	
      *------------------------ARQUIVO DO CARGO------------------------
           SELECT ARQCARG ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODIGO
                    FILE STATUS  IS ST-ERRO.	
      *-------------------------ARQUIVO DO CEP-------------------------
           SELECT CADCEP ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CEP 
                    ALTERNATE RECORD KEY IS CHAVE2 = NUMERO CEP
                    FILE STATUS  IS ST-ERRO.			   
      *-----------------------------------------------------------------
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
      *---------------------ARQUIVO DO DEPARTAMENTO--------------------
       FD ARQDEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQDEP.DAT".
       01 REGDEP.
           03 CODIGO1          PIC 9(03).
      *------------------------ARQUIVO DO CARGO------------------------
       FD ARQCARG
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQCARG.DAT".
       01 REGCARG.
           03 CODIGO          PIC 9(03).
      *-------------------------ARQUIVO DO CEP-------------------------
       FD CADCEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCEP.DAT".
       01 REGCEP.
           03 CEP             PIC 9(08).
		   03 NUMERO          PIC 9(04).
		   03 COMPLEMENTO     PIC X(12).		   

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
       77 TXTSEXO            PIC X(09) VALUE SPACES.
       77 TXTIMPOSTO         PIC X(09) VALUE SPACES.       	   
      *-----------------------------------------------------------------
       01 TIPO-GENERO.
          03 FILLER     PIC X(25) VALUE "HHETEROSEXUAL".
          03 FILLER     PIC X(25) VALUE "MHOMESEXUAL".
          03 FILLER     PIC X(25) VALUE "BBISEXUAL".
          03 FILLER     PIC X(25) VALUE "TTRANSSEXUAL".
          03 FILLER     PIC X(25) VALUE "PPANSEXUAL".
          03 FILLER     PIC X(25) VALUE "OOUTROS OU NAO INFORMADO".		  
      *
       01 TABGENERO REDEFINES TIPO-GENERO.
          03 TBGENERO   PIC X(25) OCCURS 6 TIMES.
      *
       01 TXTGENERO.
          03 TXT3 PIC X(01) VALUE SPACES.
          03 TXT4 PIC X(25) VALUE SPACES.   
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
               VALUE  "                        CADASTRAD0 DE FU".
           05  LINE 02  COLUMN 41 
               VALUE  "NCIONARIO".
           05  LINE 05  COLUMN 01 
               VALUE  "CHAPA:".
           05  LINE 05  COLUMN 20 
               VALUE  "NOME:".
		   05  LINE 07  COLUMN 01
		       VALUE  "DATA DE NASCIMENTO:".
		   05  LINE 09  COLUMN 01
		       VALUE  "SEXO:".
           05  LINE 09  COLUMN 30 
               VALUE  "GENERO:".
           05  LINE 11  COLUMN 01 			   
               VALUE  "CODIGO DEPARTAMENTO:".
           05  LINE 11  COLUMN 35 
               VALUE  "CODIGO CARGO:".		
           05  LINE 13  COLUMN 01 
               VALUE  "NIVEL SALARIAL:".	
           05  LINE 13  COLUMN 35 
               VALUE  "IMPOSTO DE RENDA:".
           05  LINE 15  COLUMN 01 
               VALUE  "QUANTIDADE DEPENDENTES:".	
           05  LINE 17  COLUMN 01 
               VALUE  "CEP FUNCIONARIO:".	
           05  LINE 17  COLUMN 35 
               VALUE  "NUMERO RESIDENCIA:".		
           05  LINE 19  COLUMN 01 
               VALUE  "COMPLEMENTO:".
           05  LINE 19  COLUMN 35
               VALUE  "STATUS:".			   
		   05  LINE 23  COLUMN 01 
               VALUE  " MENSAGEM :".		

      *---------------------[ CHAPA E NOME]----------------------
           05  TCHAPA
               LINE 05  COLUMN 10  PIC 9(06)
               USING  CHAPA.
           05  TNOME
               LINE 05  COLUMN 25  PIC X(30)
               USING  NOME.		   
      *---------------------[ DATA DE NASCIMENTO ]----------------
           05  TDATA
               LINE 07  COLUMN 20  PIC 99/99/9999
               USING  DATANASC.
      *---------------------[ SEXO E GENERO ]---------------------  
           05  TSEXO
               LINE 09  COLUMN 07  PIC X(01)
               USING  SEXO.
           05  TTEXSEXO
               LINE 09  COLUMN 09  PIC X(15)
               USING  TXTSEXO.			   
		   05  TGENERO
		       LINE 09  COLUMN 40  PIC X(01)
			   USING GENERO.
           05  TGENERO2
               LINE 09  COLUMN 43 PIC X(25)
               USING TXT4.
      *---------------------[ COD.DEPARTAMENTO E COD.CARGO ]-----		   
           05  TCODIGO1
               LINE 11  COLUMN 22  PIC 9(03)
               USING  CODPTO.
           05  TCODIGO2
               LINE 11  COLUMN 48  PIC 9(03)
               USING  CODCARG.
      *---------------------[ NVL.SALARIAL E IMP.RENDA ]---------			   
           05  TNIVEL
               LINE 13  COLUMN 17  PIC 9(02)
               USING  NVSALARIAL.   
           05  TIMPOSTO
               LINE 13  COLUMN 54  PIC X(01)
               USING  IMPOSTORENDA.
           05  TTIMPOSTO
               LINE 13  COLUMN 56  PIC X(15)
               USING  TXTIMPOSTO. 			   
      *---------------------[ DEPENDENTES ]----------------------			   
           05  TDEPENDETES
               LINE 15  COLUMN 25  PIC 9(01)
               USING  DEPENDENTES. 
      *---------------------[ CEP E NUMERO ]---------------------		   
		   05  TCEP 
		       LINE 17 COLUMN 20  PIC 99999.999
			   USING CEP-FUNC.
           05  TNUMERO
               LINE 17  COLUMN 54  PIC 9(04)
               USING  NUMERO-FUNC.			   
      *---------------------[ COMPLEMENTO E STATUS ]-------------		   
           05  TCOMPLEMENTO
               LINE 19  COLUMN 15  PIC X(12)
               USING  COMPLEMENTO-FUNC.
           05  TSTATUS1
		       LINE 19  COLUMN 45 PIC X(01)
			   USING STATUS1.
		   05  TSTATUS2
		       LINE 19  COLUMN 47  PIC X(12)
			   USING TXT2.

      *---------------------[ TABELA DE GENERO ]-------------		   
       01  TELA-GENERO.
           05  LINE 05  COLUMN 50
		       VALUE  "H-HETEROSEXUAL".
           05  LINE 06  COLUMN 50
		       VALUE  "M-HOMESEXUAL".
           05  LINE 07  COLUMN 50
		       VALUE  "B-BISEXUAL".
           05  LINE 08  COLUMN 50
		       VALUE  "T-TRANSSEXUAL".
           05  LINE 09  COLUMN 50
		       VALUE  "P-PANSEXUAL".
           05  LINE 10  COLUMN 50
		       VALUE  "O-OUTROS OU NAO INFORMADO".	
			   
      *---------------------[ TABELA DE STATUS ]-------------		   
       01  TELA-STATUS.
           05  LINE 06  COLUMN 50
		       VALUE  "A-ATIVA".
           05  LINE 07  COLUMN 50
		       VALUE  "S-SUSPENSA".
           05  LINE 08  COLUMN 50
		       VALUE  "D-DESATIVADA".

      *---------------------[ TABELA DE IMPOSTO ]-------------		   			   
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

       CREATE-FILES.
	       PERFORM I-O-ARQFUNC.
	       PERFORM I-O-ARQDEP.
	       PERFORM I-O-ARQCARG.
           PERFORM I-O-CADCEP.		   
      *-----------------------------------------------------------------  
      *                   INICIALIZACAO DAS VARIAVEIS                  *
      *----------------------------------------------------------------- 	   
       R0.
            MOVE ZEROS  TO CEP NUMERO CODPTO NUMERO-FUNC.
			MOVE ZEROS  TO CODCARG CODPTO CEP-FUNC.
            MOVE ZEROS  TO CODIGO CODIGO1 DATANASC CODCARG.
            MOVE ZEROS  TO CHAPA W-SEL NVSALARIAL DEPENDENTES.
            MOVE SPACES TO STATUS1 TXT TXTGENERO COMPLEMENTO-FUNC.
            MOVE SPACES TO GENERO IMPOSTORENDA COMPLEMENTO.
            MOVE SPACES TO NOME SEXO TXTSEXO TXTIMPOSTO.
      *-----------------------------------------------------------------  
      *                        ROTINA DE SELECAO                       *
      *----------------------------------------------------------------- 
      *
               DISPLAY TELA.
	  
       R1.
           ACCEPT TCHAPA.
           ACCEPT W-ACT FROM ESCAPE KEY.
           IF W-ACT = 01 
		      GO TO ROT-FIM.
           IF CHAPA = ZEROS
              MOVE "*** DADO NAO PODE FICAR VAZIO **" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
			  GO TO R1.
                           
              PERFORM READ-ARQFUNC.
           IF ST-ERRO = "00" 
		        MOVE "*** CHAPA JA CADASTRAD0 ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ACE-001.

       R2. 
           ACCEPT TNOME
           ACCEPT W-ACT FROM ESCAPE KEY
             IF W-ACT = 01 
                GO TO R1.
             IF NOME  = SPACES
                MOVE " ESPACO NAO PODE FICAR EM BRANCO " TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R2.  	

       R3.
           ACCEPT TDATA
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                GO TO R2.
           IF DIA < 0 OR DIA = 0 OR DIA > 31 OR MES < 0 OR MES = 0 OR 
              MES > 12 OR ANO < 1900 OR ANO > 2021
              MOVE " DATA DE NASCIMENTO INVALIDO " TO MENS			  
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R3.			

       R4.
           DISPLAY TELA-SEXO
           ACCEPT TSEXO
           ACCEPT W-ACT FROM ESCAPE KEY
              IF W-ACT = 01 
			     GO TO R3.
       R4A.				 
           IF SEXO = "F" OR SEXO = "f"
              MOVE "FEMININO" TO TXTSEXO
           ELSE
              IF SEXO = "M" OR SEXO = "m"
                 MOVE "MASCULINO" TO TXTSEXO
              ELSE
                 MOVE "*** DIGITE M=MASCULINO    F=FEMININO ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R4.			
                 DISPLAY TTEXSEXO.				 
                 DISPLAY TELA.  
					   
       R5.
	       DISPLAY TELA-GENERO
           MOVE 1 TO AUX
           ACCEPT TGENERO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R4.
		   IF GENERO = SPACES
	          MOVE "ESPACO NAO PODE FICAR EM BRANCO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R5.
       R5A.
           MOVE TBGENERO(AUX) TO TXTGENERO
           IF TXT3 NOT = GENERO
              ADD 1 TO AUX
              IF AUX < 7
                 GO TO R5A
              ELSE
                 MOVE "*** TIPO INCORRETO***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R5
           ELSE
               DISPLAY TGENERO2.
			   DISPLAY TELA.
	   
       R6.
	       ACCEPT TCODIGO1
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                 GO TO R5.
	       MOVE CODPTO TO CODIGO1
		   PERFORM READ-ARQDEP.
		   IF ST-ERRO = "00"
	          MOVE "DEPARTAMENTO NAO EXISTE" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R6.		   

       R7.
	       ACCEPT TCODIGO2
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                 GO TO R6.
	       MOVE CODCARG TO CODIGO
		   PERFORM READ-ARQCARG.
		   IF ST-ERRO = "00"
	          MOVE "DEPARTAMENTO NAO EXISTE" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R7.	

       R8.
           ACCEPT TNIVEL
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                GO TO R7.
           IF NVSALARIAL < 0 OR NVSALARIAL > 20 
              MOVE " DIGITE APENAS VALORES DE 0 A 20" TO MENS			  
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R8.	
			  
       R9.
           DISPLAY TELA-IMP
           ACCEPT TIMPOSTO
           ACCEPT W-ACT FROM ESCAPE KEY
            IF W-ACT = 01 
			   GO TO R8.
       R9A.		   
            IF IMPOSTORENDA = "S" OR IMPOSTORENDA = "s"
               MOVE "SIM" TO TXTIMPOSTO
            ELSE
               IF IMPOSTORENDA = "N" OR IMPOSTORENDA = "n"
                   MOVE "NAO" TO TXTIMPOSTO
			   ELSE
                   IF IMPOSTORENDA = "E" OR IMPOSTORENDA = "e"
                      MOVE "ESPECIAL"	TO TXTIMPOSTO						
                   ELSE
                      MOVE "*** TIPO  INVALIDO ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO R9.		
                      DISPLAY TTIMPOSTO.				  
                      DISPLAY TELA. 

       R10.
           ACCEPT TDEPENDETES.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R9.
		   IF DEPENDENTES = ZEROS
	          MOVE "ESPACO NAO PODE FICAR EM BRANCO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R10.	   
      
       R11.
           ACCEPT TCEP.
           ACCEPT W-ACT FROM ESCAPE KEY.
           IF W-ACT = 01 
		   GO TO R10.
           IF CEP-FUNC = ZEROS
               PERFORM ERROR-EMPTY
               GO TO R11.
           PERFORM READ-CADCEP.	   
       			  
       R12.
           ACCEPT TNUMERO.
           ACCEPT W-ACT FROM ESCAPE KEY.
           IF W-ACT = 01 
		   GO TO R11.
           IF NUMERO-FUNC = ZEROS
               PERFORM ERROR-EMPTY
               GO TO R12.
			   
       R13.
           ACCEPT TCOMPLEMENTO.
           ACCEPT W-ACT FROM ESCAPE KEY.
           IF W-ACT = 01 
		       GO TO R12.
           IF COMPLEMENTO-FUNC = ZEROS
              PERFORM ERROR-EMPTY
              GO TO R13.			  
			   
       R14.
	       DISPLAY TELA-STATUS
           MOVE 1 TO AUX2
           ACCEPT TSTATUS1.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R13.
		   IF STATUS1 = SPACES
	          MOVE "ESPACO NAO PODE FICAR EM BRANCO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R14.	
       R14A.
           MOVE TBSTATUS(AUX2) TO TXT
           IF TXT1 NOT = STATUS1
              ADD 1 TO AUX2
              IF AUX2 < 4
                 GO TO R14A
              ELSE
                 MOVE "*** TIPO STATUS INCORRETA***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R14
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

           IF W-ACT = 01 GO TO R14.

           IF W-OPCAO = "S" or "s"
		   
               PERFORM WRITE-ARQFUNC
               PERFORM WRITE-ARQDEP
               PERFORM WRITE-ARQCARG
               PERFORM WRITE-CADCEP
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

           IF W-ACT = 01 GO TO R14.

           IF W-OPCAO = "S" or "s"
               PERFORM DELETE-ARQFUNC
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
       I-O-ARQFUNC.
           OPEN I-O ARQFUNC
		   IF ST-ERRO NOT = "00"
		      IF ST-ERRO = "30"
			   OPEN OUTPUT ARQFUNC
			   CLOSE ARQFUNC
			   PERFORM LOG-FILE-CREATED
			   GO TO I-O-ARQFUNC
		      ELSE
			     IF ST-ERRO = "95"
                    MOVE "*** ISAM NAO EXECUTADO ***" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
					GO TO ROT-FIM
			    ELSE 
				    PERFORM ERROR-FILE-NOT-OPENED
					GO TO ROT-FIM.

       I-O-ARQDEP.
	       OPEN I-O ARQDEP
		   IF ST-ERRO NOT = "00"
		      IF ST-ERRO = "30"
			      OPEN OUTPUT ARQDEP
			      CLOSE ARQDEP
			      PERFORM LOG-FILE-CREATED
			      GO TO I-O-ARQDEP
			  ELSE 
				  PERFORM ERROR-FILE-NOT-OPENED
				  GO TO ROT-FIM.

       I-O-ARQCARG.
	       OPEN I-O ARQCARG
		   IF ST-ERRO NOT = "00"
		      IF ST-ERRO = "30"
			      OPEN OUTPUT ARQCARG
			      CLOSE ARQCARG
			      PERFORM LOG-FILE-CREATED
			      GO TO I-O-ARQCARG
			  ELSE 
				  PERFORM ERROR-FILE-NOT-OPENED
				  GO TO ROT-FIM.

       I-O-CADCEP.
           OPEN I-O CADCEP
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADCEP
                 CLOSE CADCEP
                 PERFORM LOG-FILE-CREATED
                 GO TO I-O-CADCEP
              ELSE
                 PERFORM ERROR-FILE-NOT-OPENED
                 GO TO ROT-FIM.
      *---------------------[ ROTINA DE VERIFICACAO ]---------------------
       CHECK-WRITE.
           IF ST-ERRO = "00" OR "02" 
		      PERFORM LOG-DATA-SAVED.
      *     ELSE 
      *       PERFORM ERROR-DATA-NOT-SAVED 
      *	      GO TO ROT-FIM.

       CHECK-READ.
           IF ST-ERRO NOT = "23"
               IF ST-ERRO = "00" 
			   PERFORM R4A
			   PERFORM R5A
			   PERFORM R9A
			   PERFORM R14A
      ****************************************				   
      ****************************************
      *	NAO ESQUECER DE BOTAR OS PERFORM AQUI 
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
       WRITE-ARQFUNC.
           WRITE REGFUNC.
           IF ST-ERRO = "00" OR "02" 
		      PERFORM LOG-DATA-SAVED
           ELSE IF 
		      ST-ERRO = "22" 
			  PERFORM REWRITE-ARQFUNC.
      *     ELSE PERFORM ERROR-DATA-NOT-SAVED 
      *       GO TO ROT-FIM.

       WRITE-ARQDEP.
           WRITE REGDEP.
           IF ST-ERRO = "00" OR "02" 
		      PERFORM LOG-DATA-SAVED
           ELSE IF 
		      ST-ERRO = "22" 
              PERFORM REWRITE-ARQDEP.
      *     ELSE PERFORM ERROR-DATA-NOT-SAVED 
      *        GO TO ROT-FIM.

       WRITE-ARQCARG.
           WRITE REGCARG.
           IF ST-ERRO = "00" OR "02" 
		      PERFORM LOG-DATA-SAVED
           ELSE IF 
		      ST-ERRO = "22" 
              PERFORM REWRITE-ARQCARG.
      *     ELSE PERFORM ERROR-DATA-NOT-SAVED 
      *       GO TO ROT-FIM.

       WRITE-CADCEP.
           WRITE REGCEP.
           IF ST-ERRO = "00" OR "02" 
		      PERFORM LOG-DATA-SAVED
           ELSE IF 
		      ST-ERRO = "22" 
			  PERFORM REWRITE-CADCEP.
      *     ELSE PERFORM ERROR-DATA-NOT-SAVED 
      *        GO TO ROT-FIM.
      *---------------------[ ROTINA DE REESCRITA ]---------------------
       REWRITE-ARQFUNC. 
	       REWRITE REGFUNC. 
		   PERFORM CHECK-WRITE.

       REWRITE-ARQDEP. 
	       REWRITE REGDEP. 
		   PERFORM CHECK-WRITE.

       REWRITE-ARQCARG.
     	   REWRITE REGCARG. 
		   PERFORM CHECK-WRITE.	

       REWRITE-CADCEP. 
	       REWRITE REGCEP. 
		   PERFORM CHECK-WRITE.
      *---------------------[ ROTINA DE LEITURA ]-----------------------
       READ-ARQFUNC. 
           READ ARQFUNC. 
           PERFORM CHECK-READ.
           DISPLAY TELA.  

       READ-ARQDEP. 
           READ ARQDEP. 
           PERFORM CHECK-READ.
           DISPLAY TELA.      

       READ-ARQCARG.
     	   READ ARQCARG. 
           PERFORM CHECK-READ.	   
           DISPLAY TELA.
		   
       READ-CADCEP. 
           READ CADCEP. 
             MOVE CEP-FUNC TO CEP
             MOVE NUMERO-FUNC TO NUMERO
             MOVE COMPLEMENTO-FUNC TO COMPLEMENTO
           PERFORM CHECK-READ. 
           DISPLAY TELA.		
      *--------------------[ ROTINA DE EXCLUSAO ]----------------------
        DELETE-ARQFUNC. 
	       DELETE ARQFUNC RECORD. 
		   PERFORM CHECK-DELETE.      
	   
       DELETE-ARQDEP. 
	       DELETE ARQDEP RECORD. 
		   PERFORM CHECK-DELETE.		   

       DELETE-ARQCARG. 
	       DELETE ARQCARG RECORD. 
		   PERFORM CHECK-DELETE.

       DELETE-CADCEP.
     	   DELETE CADCEP RECORD.
		   PERFORM CHECK-DELETE.		   
      *-----------------------[ ROTINA DE FIM ]------------------------
       ROT-FIM.
	       CLOSE ARQFUNC
		   CLOSE ARQDEP
           CLOSE ARQCARG		   
           CLOSE CADCEP
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
