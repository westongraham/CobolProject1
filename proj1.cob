
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  Project1.
       AUTHOR. Weston Graham.
      *  Project 1.                               
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                                   
            SELECT INPUT-FILE   ASSIGN TO 'COB1-EMPLOYEE'.  
            SELECT PRNT-FILE    ASSIGN TO 'UR-PRNT'.   

     
      *deleted EJECT from here
       DATA DIVISION.  
                                
     
      *deleted SKIP3 from here
       FILE SECTION.                                   
       FD  INPUT-FILE                                
           BLOCK CONTAINS 0 RECORDS                  
           LABEL RECORDS ARE STANDARD.               
       01  INPUT-REC                 PIC X(98).      

      
      *deleted SKIP2 from here
       FD  PRNT-FILE                                 
           LABEL RECORDS ARE OMITTED.                
       01  PRNT-REC                   PIC X(125).    
       WORKING-STORAGE SECTION.                                      
      
      ************************************************************** 
      *           LAYOUT FOR THE INPUT FILE                       *  
      ************************************************************** 
       01  INPUT-DATA.                                               
                03  I-EMPID                PIC X(7).               
                03  I-LASTNAME             PIC X(15).                
                03  I-FIRSTNAME            PIC X(15).
                03  I-EMPTYPE              PIC X(2).
                03  I-TITLE                PIC X(17).
                03  I-SSN                  PIC X(9).           
                03  FILLER                 PIC X(25)      VALUE SPACES.
                03  I-DATE                 PIC X(8).
      ************************************************************** 
      *      LAYOUT FOR THE 1ST  DATA LINE OF REPORT PRNTING       * 
      **************************************************************
       01  PRNT-DATA1.                                               
           	03  FILLER                 PIC X(3)      VALUE SPACES.   
                03  P-SSN                  PIC XXXBXXBXXXX.  
                03  FILLER                 PIC X(3)      VALUE SPACES. 
                03  P-EMPID                PIC X(7).
                03  FILLER                 PIC X(3)      VALUE SPACES.
                03  P-LASTNAME             PIC X(15).
                03  FILLER                 PIC X(3)      VALUE SPACES.
                03  P-FIRSTNAME            PIC X(15).
                03  FILLER                 PIC X(3)      VALUE SPACES.
                03  P-TITLE                PIC X(17).
                03  FILLER                 PIC X(3)      VALUE SPACES.
                03  P-EMPTYPE              PIC X(2).
                03  FILLER                 PIC X(3)      VALUE SPACES.  
                03  P-DATE                 PIC 99/99/9999.
      ************************************************************** 
      *    LAYOUT FOR THE 1ST HEADING LINE OF REPORT PRNTING       * 
      ************************************************************** 
       01  PRNT-HEADING1.                                            
           	03  FILLER                 PIC X(3)      VALUE SPACES.   
                03  FILLER                 PIC X(14)     VALUE 'SSN'.
                03  FILLER                 PIC X(10)     VALUE 'EMP ID'.
                03  FILLER                 PIC X(18)     VALUE 'LAST'.
                03  FILLER                 PIC X(18)     VALUE 'FIRST'.
                03  FILLER                 PIC X(20)     VALUE 'TITLE'.
                03  FILLER                 PIC X(5)      VALUE 'TYPE'.
                03  FILLER                 PIC X(10)     VALUE 'DATE'. 
       01  MISC.                                                      
      **************************************************************  
      *                 END OF FILE (EOF) SWITCHES                  *  
      *            0 = NOT AT EOF          1 = AT EOF              *  
      **************************************************************  
           	03  EOF-I                  PIC 9         VALUE 0.          
      **************************************************************  
      *               START OF PROCEDURE DIVISION                  *  
      **************************************************************  
       PROCEDURE DIVISION.                                            
       000-MAINLINE.                                                  
           	OPEN INPUT INPUT-FILE                    
                OUTPUT PRNT-FILE.                   
               	PERFORM 2000-READ-INPUT.                 
           	PERFORM 1400-PRINT-HEAD.                 
           	PERFORM 1500-LOOP                        
                   UNTIL EOF-I = 1.                 
           	CLOSE INPUT-FILE                         
                 PRNT-FILE.                         
           	STOP RUN.                                
       1400-PRINT-HEAD.                             
           	WRITE PRNT-REC FROM PRNT-HEADING1        
                 AFTER ADVANCING PAGE.              
           	MOVE SPACES TO PRNT-REC.                 
           	WRITE PRNT-REC                           
                AFTER ADVANCING 1 LINE.                             
       
       1500-LOOP.                                                    
                PERFORM 1600-PRINT-NAMES.
           	PERFORM 2000-READ-INPUT.                                  
      
      ************************************************************** 
      
      *   PRINTS THE SCHEDULE INFORMATION                          * 
      
      ************************************************************** 
      
       1600-PRINT-NAMES.                                   
           MOVE I-SSN                      TO  P-SSN.
           INSPECT P-SSN REPLACING ALL ' ' BY '-'.          
           MOVE I-EMPID                    TO  P-EMPID.
           MOVE I-LASTNAME                 TO  P-LASTNAME.
           MOVE I-FIRSTNAME                TO  P-FIRSTNAME.
           MOVE I-TITLE                    TO  P-TITLE.
           MOVE I-EMPTYPE                  TO  P-EMPTYPE.
           MOVE I-DATE                     TO  P-DATE.
          	WRITE PRNT-REC FROM PRNT-DATA1                            
                AFTER ADVANCING 1 LINE.                             
        
     
  
      ************************************************************** 
      
      *                READS THE INPUT FILE                       *  
      
      ************************************************************** 
       
       2000-READ-INPUT.                                              
          	READ INPUT-FILE INTO INPUT-DATA                           
               AT END MOVE 1 TO EOF-I.
