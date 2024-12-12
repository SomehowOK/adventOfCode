       identification division.
       program-id. "02".
       environment division.
       input-output section.
       file-control.
           select InputFile assign to
            "/Users/jakobweber/adventOfCode/02.12/input.txt"
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
       01 delta         PIC S9(4).
       01 CNTR-INPUTS   PIC 9(2).
       01 CNTR-INPUTS-VAR PIC 9(2).

       01 Number1 Pic 9(2).
       01 Number2 Pic 9(2).

       01 SAFE             PIC 9(4).

       01 deltaBool       PIC X(2).
           88 deltaBool-rising  VALUE "00".
           88 deltaBool-declining  VALUE "10".
           88 deltaBool-first     VALUE "99".

       01 InputFile-Status   PIC XX.
           88 InputFile-OK   VALUE "00".
           88 InputFile-EOF  VALUE "10".
       
       01 TableIndex VALUE 01    PIC 9999. 
    
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
                       Display InputRecord


                       perform TrimString
                       perform CreateTable
                       perform redNoseReports
                       perform InitTable
                       
               END-READ
           end-perform
           Display "Final: " Safe

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
           PERFORM VARYING CNTR-End
           
                   FROM 1 BY 1 
                   UNTIL CNTR-End >= InputLength

                   MOVE InputTrim(CNTR-END:1) TO DEBUG1
                   IF InputTrim(CNTR-END:1) = " "

                      MOVE InputTrim(CNTR-Start:CNTR-EXTRA)
                      TO Table-Data(TableIndex)

                      ADD 1 TO CNTR-INPUTS

                      Compute CNTR-Start = 
                              CNTR-End + 1

                      MOVE 00 TO CNTR-EXTRA
                      ADD 1 TO TableIndex
                   end-if
                   ADD 1 TO CNTR-EXTRA
           end-perform

           MOVE InputTrim(CNTR-Start:CNTR-EXTRA)
                      TO Table-Data(TableIndex)

       Exit.

       InitTable Section.
           PERFORM VARYING CNTR-Table
                   FROM 1 BY 1 
                   UNTIL CNTR-Table > TableIndex
                   
                   MOVE ZEROS TO Table-Data(CNTR-TABLE)
                
           end-perform
           MOVE ZEROS TO CNTR-TABLE
           MOVE 01 TO TABLEINDEX
           Move 01 TO CNTR-Start
           Move ZEROS TO CNTR-END
           Move ZEROS TO CNTR-EXTRA
           MOVE ZEROS TO CNTR
           MOVE ZEROS TO CNTR-INPUTS
           
       Exit.
 
       redNoseReports Section.

           MOVE "99" TO deltaBool
           Move 01 TO TableIndex

           Perform Until TableIndex >= (CNTR-INPUTS + 1)

               COMPUTE delta = Table-Data(TableIndex) - 
                               Table-Data(TableIndex + 1)
               IF delta = 0 
               THEN                                 
                  Display "bc same"
                  exit section
               ENd-IF

               IF deltaBool = "99"                    
                   if delta > 0
                   Then
                       MOVE "10" TO deltaBool 
                   end-if
                   
                   IF delta < 0
                       MOVE "00" TO deltaBool  
                   END-IF
               END-IF
           
               IF deltaBool = "00" AND ( delta < (- 3) OR delta >= 0)
                   Then                  
                   exit section
               End-If

               IF deltaBool = "10" AND (delta > 3 OR delta <= 0)
               Then
                   exit section
               End-If
               Add 1 To TableIndex

           End-perform
           Add 1 To Safe
       Exit.
