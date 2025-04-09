      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 BINARIO-ENTRADA PIC X(10).
       01 NUMERO-BINARIO OCCURS 10 TIMES PIC 9.
       01 EXPONENTE PIC 9(2).
       01 WS-INDICE PIC 9(2).
       01 WS-INDICE-EXPONENTE PIC 9(2).
       01 WS-NUMERO PIC 9.
       01 ES-VALIDO PIC X VALUE 'S'.
       01 CARACTER PIC X.
       01 BINARIO-ENTRADA-SIN-ESPACIOS PIC 9(2).
       01 TOTAL-EXPONENTE PIC 9(6) VALUE 2.
       01 TOTAL-POTENCIA PIC 9(6).
       01 ACUMULADOR PIC 9(6).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY
           "Ingrese un numero binario (solo 0 y 1, max 10 caracteres):".
           ACCEPT BINARIO-ENTRADA.

            COMPUTE BINARIO-ENTRADA-SIN-ESPACIOS =
                 FUNCTION LENGTH (FUNCTION TRIM(BINARIO-ENTRADA))

           IF BINARIO-ENTRADA-SIN-ESPACIOS > 10 THEN
                DISPLAY
                "ERROR, EL BINARIO NO PUEDE SER MAYOR DE 10 CARACTERES"
                STOP RUN
           END-IF

           MOVE  BINARIO-ENTRADA-SIN-ESPACIOS TO EXPONENTE

           PERFORM VARYING WS-INDICE FROM 1 BY 1
                   UNTIL WS-INDICE > BINARIO-ENTRADA-SIN-ESPACIOS
               MOVE BINARIO-ENTRADA(WS-INDICE:1) TO CARACTER
               IF CARACTER NOT = '0' AND CARACTER NOT = '1'
                   MOVE 'N' TO ES-VALIDO
                   EXIT PERFORM
               ELSE IF CARACTER = '1' THEN
                 MOVE CARACTER TO WS-NUMERO

                 DISPLAY "NUMERO: " WS-NUMERO



               PERFORM VARYING WS-INDICE-EXPONENTE FROM 1 BY 1
                      UNTIL WS-INDICE-EXPONENTE > EXPONENTE

                    DISPLAY "EXPONENTE: " EXPONENTE

                    DISPLAY "TOTAL EXPONENTE :" TOTAL-EXPONENTE

                     COMPUTE TOTAL-EXPONENTE =
                      TOTAL-EXPONENTE * TOTAL-EXPONENTE

                 DISPLAY TOTAL-EXPONENTE

               COMPUTE TOTAL-POTENCIA =
                      TOTAL-EXPONENTE * WS-NUMERO

              DISPLAY "TOTAL POTENCIA :" TOTAL-POTENCIA
              ADD TOTAL-POTENCIA TO ACUMULADOR



               COMPUTE EXPONENTE = EXPONENTE - 1
               END-PERFORM
             ELSE IF CARACTER = '0' THEN
                    DISPLAY "EXPONENTE: " EXPONENTE
                    COMPUTE EXPONENTE = EXPONENTE - 1
             END-IF
           END-PERFORM.

           IF ES-VALIDO = 'S'
               INITIALIZE ES-VALIDO
           ELSE
               DISPLAY
               "El número ingresado NO es un número binario válido."
                STOP RUN
           END-IF.


       2000-PRESENTAR-TOTAL.
            DISPLAY "EL NUMERO ES NUMERO BINARIO FORMA DECIMAL : "
            DISPLAY "============================================"
            DISPLAY ACUMULADOR
            DISPLAY "============================================"












           STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
