      * 
      * 
      *                             
      *                 
      * 
      * 
      *  


       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANK-APP.
       AUTHOR. Alexus Calhoun.
       DATE-WRITTEN. 11/5/25.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NEWLINE          PIC X VALUE X"0A".
       01 WS-I             PIC 9(1).
       01 WS-NAME          PIC X(10).
       01 WS-MESSAGE       PIC X(50)
            VALUE "Please enter your name: ".
       01 WS-PIN           PIC 9(4).
       01 WS-PIN-MESSAGE   PIC X(22)
            VALUE "Create a 4 Digit Pin: ".
       01 WS-MENU-LAYOUT   PIC X(200)
            VALUE "   ***MENU***" & X"0A" &
                  "1. DEPOSIT" & X"0A" &
                  "2. WITHDRAWAL" & X"0A" &
                  "3. BALANCE INQUIRY" & X"0A" &
                  "4. Transaction History" & X"0A" &
                  "5. EXIT" & X"0A" & X"0A" &
                  "Please Enter Your Selection: ".
       01 WS-MENU-INPUT    PIC 9(1).
       01 WS-EXIT.
          05 WS-GOODBYE    PIC X(27)
                VALUE "Signing you off. Thank you ".
       01 WS-BALANCE       PIC 9(7)V99 VALUE 0.
       01 WS-DEPOSIT       PIC 9(7)V99 VALUE 0.
       01 WS-WITHDRAWAL    PIC 9(7)V99 VALUE 0.
       01 WS-BALANCE-DISPLAY PIC $,$$$,$$9.99.
       01 WS-TRANS-HIS.
          03 WS-TRANS      OCCURS 0 TO 20 TIMES
                           DEPENDING ON WS-IDX
                           INDEXED BY WS-IDX.
             05 WS-ID      PIC 9(3) VALUE 001.
             05 WS-TYPE    PIC X(4) VALUE "NULL".
             05 WS-AMT     PIC Z,ZZZ,ZZ9.99 VALUE 0.
          

       PROCEDURE DIVISION.
           DISPLAY WS-MESSAGE
           ACCEPT WS-NAME
           DISPLAY WS-PIN-MESSAGE
           ACCEPT WS-PIN
           DISPLAY "Account created for: " WS-NAME
           DISPLAY NEWLINE
           SET WS-IDX TO 1
           
        PERFORM UNTIL WS-MENU-INPUT = 5
           DISPLAY WS-MENU-LAYOUT
           ACCEPT WS-MENU-INPUT

           EVALUATE WS-MENU-INPUT
               WHEN 1
                   DISPLAY "Deposit selected."
                   DISPLAY NEWLINE
                   DISPLAY "Current Balance:  " WS-BALANCE-DISPLAY
                   DISPLAY "Enter Deposit Amount: "
                   ACCEPT WS-DEPOSIT
                   ADD WS-DEPOSIT TO WS-BALANCE
                   MOVE WS-BALANCE to WS-BALANCE-DISPLAY
                   DISPLAY "NEW Balance:  " WS-BALANCE-DISPLAY
                   DISPLAY NEWLINE
                   *> Adding to Transaction History
                   MOVE WS-IDX TO WS-ID(WS-IDX)
                   MOVE "DEPO" TO WS-TYPE(WS-IDX)
                   MOVE WS-DEPOSIT TO WS-AMT(WS-IDX)
                   SET WS-IDX UP BY 1
               WHEN 2
                   DISPLAY "Withdrawal selected."
                   DISPLAY NEWLINE
                   MOVE WS-BALANCE TO WS-BALANCE-DISPLAY
                   DISPLAY "Current Balance:  " WS-BALANCE-DISPLAY
                   DISPLAY "Enter Withdrawal Amount: "
                   ACCEPT WS-WITHDRAWAL
                   *> Validate Withdrawal < Account Balance
                   PERFORM UNTIL WS-WITHDRAWAL <= WS-BALANCE
                      IF WS-WITHDRAWAL > WS-BALANCE
                        DISPLAY "Withdrawal Amount Exceeds Balance"
                        DISPLAY "Please Enter Amount Less Than Balance"
                        ACCEPT WS-WITHDRAWAL
                      END-IF
                   END-PERFORM
                   SUBTRACT WS-WITHDRAWAL FROM WS-BALANCE
                   MOVE WS-BALANCE TO WS-BALANCE-DISPLAY
                   DISPLAY "NEW Balance:  " WS-BALANCE-DISPLAY
                   DISPLAY NEWLINE
                   *> Adding to Transaction History
                   MOVE WS-IDX TO WS-ID(WS-IDX)
                   MOVE "WITH" TO WS-TYPE(WS-IDX)
                   MOVE WS-WITHDRAWAL TO WS-AMT(WS-IDX)
                   SET WS-IDX UP BY 1
               WHEN 3
                   DISPLAY "Balance inquiry selected."
                   DISPLAY NEWLINE
                   DISPLAY "***"
                   DISPLAY "ACCOUNT NAME: " WS-NAME
                   DISPLAY "BALANCE: $" WS-BALANCE-DISPLAY
                   DISPLAY "***"
                   DISPLAY NEWLINE
               WHEN 4
                   DISPLAY "Transaction History selected."
                   *> Transaction History
                   DISPLAY WS-TRANS-HIS
                   *> using index to parse thru history
                   PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I = WS-IDX
                       DISPLAY "***"
                       DISPLAY "TRANSACTION #" WS-ID(WS-I) ":"
                       DISPLAY "TYPE: " WS-TYPE(WS-I)
                       DISPLAY "AMOUNT: $" FUNCTION TRIM(WS-AMT(WS-I))
                       DISPLAY "***"
                   END-PERFORM
               WHEN 5
                   DISPLAY WS-GOODBYE  FUNCTION TRIM(WS-NAME)  "!"
                   STOP RUN
               WHEN OTHER
                   DISPLAY "Invalid input, try again."
           END-EVALUATE
        END-PERFORM.

           STOP RUN.
  