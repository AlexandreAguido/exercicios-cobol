       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGR1.
      *este programa converte numeros decimais de 0 a 99
      *para numeros binarios utilizando vetores
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SPECIAL-NAMES. DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
           SELECT NUMDEC ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT NUMBIN ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.
           FD NUMDEC
             LABEL RECORD ARE STANDARD 
             VALUE OF FILE-ID IS "NUMDEC.DAT".

             01 REG-NUMDEC.
                02 DEC PIC 9(2).

           FD NUMBIN
             LABEL RECORD ARE STANDARD
             VALUE OF FILE-ID IS "NUMBIN.DAT".

             01 REG-NUMBIN.
                02 BIN PIC 9(8).
                    

           WORKING-STORAGE SECTION.
           01  RESTO PIC 9(1).
           01  CONTADOR PIC 9(1).
           01  BIT-TABLE.
               05 BIT-ITEM PIC 9(1) OCCURS 8 TIMES.
           77  FLAG PIC 9(1) VALUE 0.


       PROCEDURE DIVISION.
       PROG-COBOL.
           PERFORM INICIO.
           PERFORM PRINCIPAL UNTIL FLAG EQUAL 1.         

       INICIO.
           OPEN INPUT NUMDEC OUTPUT NUMBIN.

       LEITURA.
           READ NUMDEC AT END MOVE 1 TO FLAG.

       PRINCIPAL.
           PERFORM LEITURA.
           IF FLAG EQUAL 1 PERFORM FIM.
           MOVE 1 TO CONTADOR.
           PERFORM ZERAVETOR 8 TIMES.
           MOVE 8 TO CONTADOR.
           PERFORM DEC-TO-BIN UNTIL DEC EQUAL 0.
           PERFORM GRAVAR.
       
       ZERAVETOR.
           MOVE 0 TO BIT-ITEM(CONTADOR).
           COMPUTE CONTADOR = CONTADOR + 1.

       DEC-TO-BIN.
           DIVIDE DEC BY 2 GIVING DEC REMAINDER RESTO.
           MOVE RESTO TO BIT-ITEM(CONTADOR).
           COMPUTE CONTADOR = CONTADOR - 1.

       GRAVAR.
           MOVE BIT-TABLE TO BIN.
           WRITE REG-NUMBIN.

       FIM.
           CLOSE NUMDEC NUMBIN.
           STOP RUN.
