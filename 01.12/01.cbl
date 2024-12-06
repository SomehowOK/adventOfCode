       identification division.
       program-id. "01".
       environment division.
       input-output section.
       file-control.
           select InputFile assign to
            "/home/somehowok/adventOfCode/01.12/input.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

       data division.

       FILE SECTION.
       FD InputFile.
       01 InputRecord.
           05 Number1 PIC 99999. 
           05 Useless PIC X(3).
           05 Number2 PIC 99999.

                

       working-storage section.
       01 TotalDistance   PIC 9(32) VALUE ZEROS.
       01 Distance         PIC 9(5).
       
      
       01 InputFile-Status   PIC XX.
           88 InputFile-OK   VALUE "00".
           88 InputFile-EOF  VALUE "10".
       
       01 TableIndex         PIC 9999. 
    
       01 Table1.
           05 Table1-Data OCCURS 1000 TIMES PIC 9(5).  
       01 Table2.
           05 Table2-Data OCCURS 1000 TIMES PIC 9(5).

       procedure division.
           perform Main
           Stop run.
       
       Main section.
           Open Input InputFile.
           Move Zeros TO InputFile-Status
           Move 1 TO TableIndex

           perform until InputFile-EOF 
               READ InputFile INTO InputRecord
                   AT END 
                       Move "10" TO InputFile-Status
                   NOT AT END 
                       Move Number1 TO Table1-Data(TableIndex)
                       Move Number2 To Table2-Data(TableIndex)
                       ADD 1 TO TableIndex  
               END-READ
           end-perform
           CLOSE InputFile.

           Move 1 To TableIndex

           Sort Table1-Data ascending KEY Table1-Data.
           Sort Table2-Data ascending KEY Table2-Data.

           Perform Until TableIndex > 1000
           

           IF Table1-Data(TableIndex) >= Table2-Data(TableIndex) 
           THEN
               COMPUTE Distance = Table1-Data(TableIndex) - 
                              Table2-Data(TableIndex)
           ELSE
               COMPUTE Distance = Table2-Data(TableIndex) - 
                              Table1-Data(TableIndex)
           END-IF
           
           Add Distance TO TotalDistance
           MOVE ZEROS TO Distance 
           ADD 1 TO TableIndex

           END-Perform
           Display "TotalDistance: " TotalDistance
       Exit. 
