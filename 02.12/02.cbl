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
       01 InputRecord  PIC X(23).
                
       working-storage section.
       01 Input-R      PIC X(23).
       01 InputLength  PIC 9(2).
       01 InputTrim    PIC X(23).
       01 CNTR         PIC 9(2).
       01 CNTR-Table   PIC 9(2).
       01 CNTR-Start   PIC 9(2) Value 01.
       01 CNTR-EXTRA   PIC 9(2).
       01 CNTR-End     PIC 9(2).

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
                       display InputTrim
                       display NumberTable
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
           Display "moin"
           PERFORM VARYING CNTR-End
           
                   FROM 1 BY 1 
                   UNTIL CNTR-End > FUNCTION LENGTH(InputTrim)
                   ADD 1 TO CNTR-EXTRA
             

                   Display InputTrim(CNTR-END:1)

                   IF InputTrim(CNTR-EXTRA:1) = " "
                      ADD 1 TO TableIndex

                      MOVE InputTrim(CNTR-Start:CNTR-EXTRA - 1)
                      TO Table-Data(TableIndex)

                      Display Table-Data(TableIndex)


                      Compute CNTR-Start = 
                              CNTR-End + 1
                      Display "safe"
                      
                      MOVE ZEROS TO CNTR-EXTRA
                   end-if
           end-perform
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
           Display NumberTable
       Exit.
