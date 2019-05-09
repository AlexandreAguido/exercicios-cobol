       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGR1.
      *este programa lÊ o arquivo de alunos
      *e gera o arquivo de reprovados e o relatorio de aprovados
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SPECIAL-NAMES. DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
           SELECT ARQALU ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ARQREP ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT RELAPROV ASSIGN TO DISK.

       DATA DIVISION.

       FILE SECTION.
           FD ARQALU
             LABEL RECORD ARE STANDARD 
             VALUE OF FILE-ID IS "ARQALU.DAT".

             01 REGALU.
                02 MAT-ALU PIC 9(6).
                02 NOM-ALU PIC X(20).
                02 NOTA-ALU PIC 9(2).

           FD ARQREP
             LABEL RECORD ARE STANDARD
             VALUE OF FILE-ID IS "ARQREP.DAT".

             01 REGREP.
               02 REP-MAT-ALU PIC 9(6).
               02 REP-NOM-ALU PIC X(20).
           
           FD RELAPROV
             LABEL RECORD IS OMITTED.
           
           01 REGAPROV PIC X(80).

           WORKING-STORAGE SECTION.
           77 FLAG PIC 9(1) VALUE 0.
           77 CNT-DT PIC 9(2) VALUE 0.
           77 CNT-PG PIC 9(2) VALUE 1.

           01 CAB-01.
             02 FILLER PIC X(15) VALUE SPACES.
             02 FILLER PIC X(29) VALUE "RELATORIO DE ALUNOS APROVADOS".
             02 FILLER PIC X(28) VALUE SPACES.
             02 FILLER PIC X(5) VALUE "PAG. ".
             02 PG-NUM PIC 9(2).
           
           01 CAB-02.
             02 FILLER PIC X(17) VALUE SPACES.
             02 FILLER PIC X(9) VALUE "MATRICULA".
             02 FILLER PIC X(20) VALUE SPACES.
             02 FILLER PIC X(4) VALUE "NOME".
             02 FILLER PIC X(20) VALUE SPACES.
           
           01 REG-RELAPROV.
             02 FILLER PIC X(18) VALUE SPACES.
             02 APROV-MAT-ALU PIC X(6).
             02 FILLER PIC X(20) VALUE SPACES.
             02 APROV-NOM-ALU PIC X(30).


       PROCEDURE DIVISION.
       PROG-RELATORIO.
           PERFORM ABRIR.
           PERFORM LEITURA.
           PERFORM IMPCAB.
           PERFORM PRINCIPAL UNTIL FLAG EQUAL 1.
           PERFORM FIM.

       ABRIR.
           OPEN INPUT ARQALU
                OUTPUT ARQREP RELAPROV.
       
       LEITURA.
           READ ARQALU AT END MOVE 1 TO FLAG.
       
       FIM.
           CLOSE ARQALU ARQREP RELAPROV.
           STOP RUN.
       
       PRINCIPAL.

           IF NOTA-ALU LESS THAN 6
             PERFORM WRITE-REP
           ELSE 
             PERFORM WRITE-APROV
             COMPUTE CNT-DT = CNT-DT + 1.
           IF CNT-DT EQUAL 19
             PERFORM IMPCAB
             MOVE 0 TO CNT-DT.
           PERFORM LEITURA.

       IMPCAB.
           MOVE CNT-PG TO PG-NUM.
           MOVE SPACES TO REGAPROV.
           WRITE REGAPROV AFTER ADVANCING PAGE.
           WRITE REGAPROV FROM CAB-01 BEFORE ADVANCING 1 LINES.
           WRITE REGAPROV FROM CAB-02 BEFORE ADVANCING 2 LINES.
           COMPUTE CNT-PG = CNT-PG + 1.

       WRITE-REP.
           MOVE MAT-ALU TO REP-MAT-ALU.
           MOVE NOM-ALU TO REP-NOM-ALU.
           WRITE REGREP.  

       WRITE-APROV.
           MOVE MAT-ALU TO APROV-MAT-ALU.
           MOVE NOM-ALU TO APROV-NOM-ALU.
           WRITE REGAPROV FROM REG-RELAPROV BEFORE ADVANCING 1 LINE.
           