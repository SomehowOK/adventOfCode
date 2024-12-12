       identification division.
       program-id. "02".
       environment division.
       input-output section.
       file-control.
           select InputFile assign to
            ""
           ORGANIZATION IS LINE SEQUENTIAL.

       data division.

       FILE SECTION.
       FD InputFile.
       01 InputRecord  PIC X(23).
                
       working-storage section.
       01 Input-R      PIC X(23).
       01 InputLength  PIC 9(2).
       01 InputTrim   PIC X(24) Value Spaces.
       01 CNTR         PIC 9(2).
       01 CNTR-Table   PIC 9(2).
       01 CNTR-Start   PIC 9(2).
       01 CNTR-EXTRA   PIC 9(2).
       01 CNTR-End     PIC 9(2).
       01 DEBUG1        PIC X(20).

       01 InputFile-Status   PIC XX.
           88 InputFile-OK   VALUE "00".
           88 InputFile-EOF  VALUE "10".
       
       01 TableIndex VALUE ZEROS    PIC 9999. 
    
       01 NumberTable.
           05 Table-Data OCCURS 10 TIMES PIC 9(4).  

       procedure division.
           perform Main
           Stop run.
       
       Main section.
           Display InputTrim
           Open Input InputFile.
           Move Zeros TO InputFile-Status
           Move 1 TO TableIndex

           perform until InputFile-EOF 
               READ InputFile INTO InputRecord
                   AT END 
                       Move "10" TO InputFile-Status
                   NOT AT END 
                       ADD 1 TO TableIndex

                       perform TrimString
                       perform CreateTable
                       perform InitTable
                   
               END-READ
           end-perform

           CLOSE InputFile.
       Exit. 

       TrimString Section. 
           MOVE FUNCTION REVERSE(InputRecord) TO Input-R.
                       
           Inspect Input-R Tallying CNTR FOR leading spaces.

           Compute InputLength = 
                   Length of Input-R -
                   CNTR. 
                       
           MOVE InputRecord(1:InputLength) TO InputTrim
       Exit. 

       CreateTable Section.
           MOVE 01 TO CNTR-START
           Display InputTrim
           PERFORM VARYING CNTR-End
           
                   FROM 1 BY 1 
                   UNTIL CNTR-End >= InputLength

                   MOVE InputTrim(CNTR-END:1) TO DEBUG1
                   IF InputTrim(CNTR-END:1) = " "

                      MOVE InputTrim(CNTR-Start:CNTR-EXTRA)
                      TO Table-Data(TableIndex)

                      Compute CNTR-Start = 
                              CNTR-End + 1

                      MOVE 00 TO CNTR-EXTRA
                      ADD 1 TO TableIndex
                   end-if
                   ADD 1 TO CNTR-EXTRA
           end-perform

           MOVE InputTrim(CNTR-Start:CNTR-EXTRA)
                      TO Table-Data(TableIndex)

           Display NumberTable
       Exit.

       InitTable Section.
           PERFORM VARYING CNTR-Table
                   FROM 1 BY 1 
                   UNTIL CNTR-Table > TableIndex
                   
                   MOVE ZEROS TO Table-Data(CNTR-TABLE)
                
           end-perform
           MOVE ZEROS TO CNTR-TABLE
           MOVE ZEROS TO TABLEINDEX
           Move 01 TO CNTR-Start
           Move ZEROS TO CNTR-END
           Move ZEROS TO CNTR-EXTRA
           MOVE ZEROS TO CNTR

       Exit.
