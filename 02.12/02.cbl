       identification division.
       program-id. "02".
       environment division.
       input-output section.
       file-control.
           select InputFile assign to
            "/home/somehowok/adventOfCode/02.12/input.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

       data division.

       FILE SECTION.
       FD InputFile.
       01 InputRecord.
                
       working-storage section.

       01 InputFile-Status   PIC XX.
           88 InputFile-OK   VALUE "00".
           88 InputFile-EOF  VALUE "10".
       
       01 TableIndex         PIC 9999. 
    
       01 Table1.
           05 Table1-Data OCCURS 1000 TIMES PIC 9(5).  

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
       Exit. 
