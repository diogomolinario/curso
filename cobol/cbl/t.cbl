      $SET ANS85 MF NOOSVS
      **************************************************************
      * COPYRIGHT MICRO FOCUS INTERNATIONAL LTD. ALL RIGHTS        *
      * RESERVED.                                                  *
      * THIS DEMONSTRATION PROGRAM IS PROVIDED FOR USE BY USERS    *
      * OF MICRO FOCUS PRODUCTS AND MAY BE USED, MODIFIED AND      *
      * DISTRIBUTED AS PART OF YOUR APPLICATION PROVIDED THAT YOU  *
      * PROPERLY ACKNOWLEDGE THE COPYRIGHT OF MICRO FOCUS IN THIS  *
      * MATERIAL.                                                  *
      **************************************************************
      
      **************************************************************
      *    PANDEMO.CBL: SHOWS HOW TO USE THE PANELS FACILITY TO    *
      *                 CREATE, WRITE TO, ENABLE AND DISABLE A     *
      *                 USER PANEL UNDER COBOL.                    *
      *                                                            *
      *       CALLS USED BY THIS PROGRAM:                          *
      *                                                            *
      *       PANELS: SUBROUTINE FOR HANDLING PANEL                *
      *               FUNCTIONS.                                   *
      *                                                            *
      **************************************************************
       WORKING-STORAGE SECTION.
           
      **************************************************************
      *  PANELS PARAMETER BLOCK.
      **************************************************************
     
       01   PANELS-PARAMETER-BLOCK.
           03 PPB-FUNCTION                         PIC 9(2) COMP.
           03 PPB-STATUS                           PIC 9(2) COMP.
           03 PPB-PANEL-ID                         PIC 9(4) COMP.
           03 PPB-PANEL-WIDTH                      PIC 9(4) COMP.
           03 PPB-PANEL-HEIGHT                     PIC 9(4) COMP.
           03 PPB-VISIBLE-WIDTH                    PIC 9(4) COMP.
           03 PPB-VISIBLE-HEIGHT                   PIC 9(4) COMP.
           03 PPB-FIRST-VISIBLE-COL                PIC 9(4) COMP.
           03 PPB-FIRST-VISIBLE-ROW                PIC 9(4) COMP.
           03 PPB-PANEL-START-COLUMN               PIC 9(4) COMP.
           03 PPB-PANEL-START-ROW                  PIC 9(4) COMP.
           03 PPB-BUFFER-OFFSET                    PIC 9(4) COMP.
           03 PPB-VERTICAL-STRIDE                  PIC 9(4) COMP.
           03 PPB-UPDATE-GROUP.
               05 PPB-UPDATE-COUNT                 PIC 9(4) COMP.
               05 PPB-RECTANGLE-OFFSET             PIC 9(4) COMP.
               05 PPB-UPDATE-START-COL             PIC 9(4) COMP.
               05 PPB-UPDATE-START-ROW             PIC 9(4) COMP.
               05 PPB-UPDATE-WIDTH                 PIC 9(4) COMP.
               05 PPB-UPDATE-HEIGHT                PIC 9(4) COMP.
           03 PPB-FILL.
               05 PPB-FILL-CHARACTER               PIC X.
               05 PPB-FILL-ATTRIBUTE               PIC X.
           03 PPB-UPDATE-MASK                      PIC X.
           03 PPB-SCROLL-DIRECTION                 PIC 9(2) COMP.
           03 PPB-SCROLL-COUNT                     PIC 9(4) COMP.
       
       01 WS-SAVE-PANEL-ID                         PIC 9(4) COMP-X.
       01 WS-TEXT-BUFFER.
          05 FILLER PIC X(20) VALUE "                    ".
          05 FILLER PIC X(20) VALUE "                    ".
          05 FILLER PIC X(20) VALUE "********************".
          05 FILLER PIC X(20) VALUE "********************".
          05 FILLER PIC X(20) VALUE "********************".
          05 FILLER PIC X(20) VALUE "********************".
          05 FILLER PIC X(20) VALUE "** HELLO, I AM A  **".
          05 FILLER PIC X(20) VALUE "** PANEL. TO SEE  **".
          05 FILLER PIC X(20) VALUE "** ME DISAPPEAR,  **".
          05 FILLER PIC X(20) VALUE "** PRESS RETURN.  **".
          05 FILLER PIC X(20) VALUE "********************".
          05 FILLER PIC X(20) VALUE "********************".
          05 FILLER PIC X(20) VALUE "********************".
          05 FILLER PIC X(20) VALUE "********************".
          05 FILLER PIC X(20) VALUE "                    ".
          05 FILLER PIC X(20) VALUE "                    ".
       
       01 WS-ATTRIB-BUFFER.
          05 FILLER           PIC X(300) VALUE ALL X"07".
       
       01 WS-ACCEPT           PIC X.
       
       PROCEDURE DIVISION.
       MAIN-LINE.
           PERFORM REDRAW-SCREEN
           PERFORM PROCEED-TO-CREATE-A-PANEL
           PERFORM CREATE-A-PANEL 
           PERFORM WRITE-TO-THE-PANEL
           PERFORM ENABLE-THE-PANEL 
           PERFORM DISABLE-THE-PANEL 
           PERFORM STOP-RUN.
       
       REDRAW-SCREEN.
           MOVE 2 TO PPB-FUNCTION 
           CALL "PANELS" USING PANELS-PARAMETER-BLOCK 
           PERFORM STATUS-CHECK.
       
       PROCEED-TO-CREATE-A-PANEL.
           DISPLAY "PRESS RETURN TO MAKE THE PANEL APPEAR"
           ACCEPT  WS-ACCEPT 
           PERFORM REDRAW-SCREEN.
       
       CREATE-A-PANEL.
           MOVE 20 TO PPB-PANEL-WIDTH
           MOVE 15 TO PPB-PANEL-HEIGHT 
           MOVE  5 TO PPB-PANEL-START-ROW 
           MOVE 20 TO PPB-PANEL-START-COLUMN 
           MOVE 20 TO PPB-VISIBLE-WIDTH 
           MOVE 15 TO PPB-VISIBLE-HEIGHT 
           MOVE  0 TO PPB-FIRST-VISIBLE-ROW 
           MOVE  0 TO PPB-FIRST-VISIBLE-COL
           MOVE 3  TO PPB-FUNCTION
           CALL "PANELS" USING PANELS-PARAMETER-BLOCK 
           PERFORM STATUS-CHECK 
           MOVE PPB-PANEL-ID TO WS-SAVE-PANEL-ID.
       
       WRITE-TO-THE-PANEL.
           MOVE WS-SAVE-PANEL-ID TO PPB-PANEL-ID
           MOVE  15 TO PPB-UPDATE-HEIGHT 
           MOVE  20 TO PPB-UPDATE-WIDTH 
           MOVE   0 TO PPB-UPDATE-START-ROW
           MOVE   0 TO PPB-UPDATE-START-COL 
           MOVE 300 TO PPB-UPDATE-COUNT 
           MOVE   0 TO PPB-RECTANGLE-OFFSET
           MOVE X"03" TO PPB-UPDATE-MASK 
           MOVE   1 TO PPB-BUFFER-OFFSET 
           MOVE  20 TO PPB-VERTICAL-STRIDE 
           MOVE  11 TO PPB-FUNCTION 
           CALL "PANELS" USING PANELS-PARAMETER-BLOCK
                               WS-TEXT-BUFFER 
                               WS-ATTRIB-BUFFER 
           PERFORM STATUS-CHECK.
       
       ENABLE-THE-PANEL.
           MOVE WS-SAVE-PANEL-ID TO PPB-PANEL-ID 
           MOVE 7 TO PPB-FUNCTION 
           CALL "PANELS" USING PANELS-PARAMETER-BLOCK 
           PERFORM STATUS-CHECK.
       
       DISABLE-THE-PANEL.
           ACCEPT WS-ACCEPT
           MOVE WS-SAVE-PANEL-ID TO PPB-PANEL-ID 
           MOVE 8 TO PPB-FUNCTION 
           CALL "PANELS" USING PANELS-PARAMETER-BLOCK 
           PERFORM STATUS-CHECK.
       
       STATUS-CHECK.
           IF PPB-STATUS NOT = 0
              DISPLAY "BAD STATUS" 
              PERFORM STOP-RUN.
       
       STOP-RUN.
         DISPLAY "PRESS RETURN TO STOP THE PANDEMO PROGRAM"
         ACCEPT WS-ACCEPT 
         STOP RUN.