       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGR1.
      *este programa lê o arquivo de funcionarios
      *e gera o holerite
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SPECIAL-NAMES. DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
           SELECT CADFUNC ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT HOLERITE ASSIGN TO DISK.

       DATA DIVISION.

       FILE SECTION.
           FD CADFUNC
             LABEL RECORD ARE STANDARD 
             VALUE OF FILE-ID IS "CADFUNC.DAT".

             01 REGFUNC.
                02 CODIGO PIC 9(5).
                02 SALBR PIC 9(8)V99.
                02 DEP PIC 9(2).
           
           FD HOLERITE
             LABEL RECORD IS OMITTED.
           
           01 REGHOLERITE PIC X(80).

           WORKING-STORAGE SECTION.
           77 FLAG PIC 9(1) VALUE 0.
           77 CNT-DT PIC 9(2) VALUE 0.
           77 CNT-PG PIC 9(2) VALUE 1.

           01 CAB-01.
             02 FILLER PIC X(15) VALUE SPACES.
             02 FILLER PIC X(25) VALUE "RELATORIO DE FUNCIONARIOS".
             02 FILLER PIC X(34) VALUE SPACES.
             02 FILLER PIC X(4) VALUE "PAG ".
             02 PG-NUM PIC 9(2).
           
           01 CAB-02.
             02 FILLER PIC X(10) VALUE SPACES.
             02 FILLER PIC X(6) VALUE "CODIGO".
             02 FILLER PIC X(24) VALUE SPACES.
             02 FILLER PIC X(13) VALUE "SALARIO BRUTO".
             02 FILLER PIC X(7) VALUE SPACES.
             02 FILLER PIC X(20) VALUE "N. DEPS".

           01 MODELO-HOLERITE.
             02 FILLER PIC X(10) VALUE SPACES.
             02 FUNC-CODE PIC 9(5).
             02 FILLER PIC X(20) VALUE SPACES.
             02 FUNC-SAL PIC ZZ.ZZZ.ZZZ,99.
             02 FILLER PIC X(9) VALUE SPACES.
             02 FUNC-DEPS PIC 9(2).


       PROCEDURE DIVISION.
       PROG-RELATORIO.
           PERFORM ABRIR.
           PERFORM LEITURA.
           PERFORM IMPCAB.
           PERFORM PRINCIPAL UNTIL FLAG EQUAL 1.
           PERFORM FIM.

       ABRIR.
           OPEN INPUT CADFUNC
                OUTPUT HOLERITE.
       
       LEITURA.
           READ CADFUNC AT END MOVE 1 TO FLAG.
       
       FIM.
           CLOSE CADFUNC HOLERITE.
           STOP RUN.
       
       PRINCIPAL.
           MOVE CODIGO TO FUNC-CODE.
           MOVE SALBR TO FUNC-SAL.
           MOVE DEP TO FUNC-DEPS.
           WRITE REGHOLERITE FROM MODELO-HOLERITE 
             BEFORE ADVANCING 1 LINES.
           COMPUTE CNT-DT = CNT-DT + 1.
           IF CNT-DT EQUAL 29
             PERFORM IMPCAB
             COMPUTE CNT-DT = 0.
           PERFORM LEITURA.
          

       IMPCAB.
           MOVE CNT-PG TO PG-NUM.
           WRITE REGHOLERITE FROM CAB-01 BEFORE ADVANCING 1 LINES.
           WRITE REGHOLERITE FROM CAB-02 BEFORE ADVANCING 2 LINES.
           COMPUTE CNT-PG = CNT-PG + 1.
           