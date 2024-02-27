       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLIENTES.
      ******************************************************************
      * Author: LEONARDO OLIVEIRA
      * Date:
      * Purpose: MENU PRINCIPAL (SISTEMA DE GESTÃO)
      * Tectonics: cobc
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLIENTE ASSIGN TO "C:\PROJETO_COBOL\CLIENTE.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               FILE STATUS IS CLIENTE-STATUS
               RECORD KEY IS CLIENTE-CHAVE.

           SELECT RELATO ASSIGN TO "C:\PROJETO_COBOL\RELATO.TXT"
               ORGANIZATION IS SEQUENTIAL.





       DATA DIVISION.
       FILE SECTION.
       FD CLIENTE.
       01 CLIENTES-REG.
           05 CLIENTE-CHAVE.
               10 CLIENTES-FONE        PIC 9(09).
           05 CLIENTES-NOME            PIC X(30).
           05 CLIENTES-EMAIL           PIC X(40).

       FD RELATO.
       01 RELATO-REG.
           05 RELATO-DADOS PIC X(79).



       WORKING-STORAGE SECTION.

       77 WS-OPCAO                     PIC X(01).
       77 WS-MODULO                    PIC X(25).
       77 WS-TECLA                     PIC X(01).

       77 CLIENTE-STATUS               PIC 9(02).
       77 WS-MSG-ERRO                  PIC X(30).
       77 WS-CONTADOR                  PIC 9(02) VALUE 0.
       77 WS-QTDREGISTROS              PIC 9(05) VALUE 0.

       SCREEN SECTION.
       01 TELA.
           05 LIMPA-TELA.
               10 BLANK SCREEN.
               10 LINE 01 COLUMN 01 PIC X(20) ERASE EOL
                   BACKGROUND-COLOR 5.
               10 LINE 01 COLUMN 20 PIC X(20)
                  BACKGROUND-COLOR 5 FROM "SISTEMA CLIENTES".
               10 LINE 02 COLUMN 01 PIC X(25) ERASE EOL
                   BACKGROUND-COLOR 1.
               10 LINE 02 COLUMN 20 PIC X(25)
               BACKGROUND-COLOR 1 FROM WS-MODULO.
       01 MENU.
           05 LINE 07 COLUMN 20 VALUE "1 - INCLUIR".
           05 LINE 08 COLUMN 20 VALUE "2 - CONSULTAR".
           05 LINE 09 COLUMN 20 VALUE "3 - ALTERAR".
           05 LINE 10 COLUMN 20 VALUE "4 - EXCLUIR".
           05 LINE 11 COLUMN 20 VALUE "5 - RELATORIO EM TELA".
           05 LINE 12 COLUMN 20 VALUE "6 - RELATORIO EM DISCO".
           05 LINE 13 COLUMN 20 VALUE "X - SAIDA".
           05 LINE 14 COLUMN 20 VALUE "ESCOLHA: ".
           05 LINE 14 COLUMN 28 USING  WS-OPCAO.

       01 TELA-REGISTRO.
           05 CHAVE FOREGROUND-COLOR 2.
               10 LINE 10 COLUMN 10 VALUE "TELEFONE".
               10 COLUMN PLUS 2 PIC 9(09) USING CLIENTES-FONE
                   BLANK WHEN ZEROS.
           05 SS-DADOS.
               10 LINE 11 COLUMN 10 VALUE "NOME:   ".
               10 COLUMN PLUS 2 PIC X(30) USING CLIENTES-FONE.
               10 LINE 12 COLUMN 10 VALUE "EMAIL:   ".
               10 COLUMN PLUS 2 PIC X(40) USING CLIENTES-EMAIL.


       01 MOSTRA-ERRO.
           02 MSG-ERRO.
               10 LINE 16 COLUMN 01 ERASE EOL
                                    BACKGROUND-COLOR 3.
               10 LINE 16 COLUMN 10 PIC X(30)
                                    BACKGROUND-COLOR 3
                                    FROM WS-MSG-ERRO.
               10 COLUMN PLUS 2 PIC X(01)
                                    BACKGROUND-COLOR 3
                                    USING WS-TECLA.

       PROCEDURE DIVISION.
       0001-PRINCIPAL SECTION.
           PERFORM 1000-INICIAR THRU 1100-MONTATELA.
           PERFORM 2000-PROCESSAR UNTIL WS-OPCAO EQUAL "X".
           PERFORM 3000-FINALIZAR.


       1000-INICIAR.
           OPEN I-O CLIENTE
               IF CLIENTE-STATUS = 35 THEN
                   OPEN OUTPUT CLIENTE
                   CLOSE CLIENTE
                   OPEN I-O CLIENTE
               END-IF.
       1100-MONTATELA.
           MOVE 0 TO WS-QTDREGISTROS.
           DISPLAY TELA.
           ACCEPT MENU.

       2000-PROCESSAR.
           MOVE SPACES TO CLIENTES-NOME CLIENTES-EMAIL.
           EVALUATE WS-OPCAO
               WHEN 1
                   PERFORM 5000-INCLUIR
               WHEN 2
                   PERFORM 6000-CONSULTAR
               WHEN 3
               PERFORM 7000-ALTERAR
               WHEN 4
               PERFORM 8000-EXCLUIR
               WHEN 5
               PERFORM 9000-RELATORIOTELA
               WHEN 6
               PERFORM 9100-RELATORIODISCO
               WHEN OTHER
                   IF WS-OPCAO NOT EQUAL 'X'
                       DISPLAY "ENTRE COM UMA OPACAO VALIDA!" AT 1620
                   END-IF
           END-EVALUATE.

               PERFORM 1100-MONTATELA.



       3000-FINALIZAR.
           CLOSE CLIENTE
            STOP RUN.

       5000-INCLUIR.
           MOVE "MODULO - INCLUSAO" TO WS-MODULO.
           DISPLAY TELA.
           ACCEPT TELA-REGISTRO.
               WRITE CLIENTES-REG
                   INVALID KEY
                   MOVE "JA EXISTE" TO WS-MSG-ERRO
                   ACCEPT MOSTRA-ERRO
                   ACCEPT WS-TECLA
                END-WRITE.

           ACCEPT MENU.
           ACCEPT WS-TECLA AT 1620.

       6000-CONSULTAR.
           MOVE "MODULO - CONSULTA" TO WS-MODULO.
           DISPLAY TELA.
           DISPLAY TELA-REGISTRO.
           ACCEPT CHAVE.
               READ CLIENTE
                   INVALID KEY
                       MOVE "NAO ENCONTRADO" TO WS-MSG-ERRO
                   NOT INVALID KEY
                   MOVE "-- ENCONTRADO --" TO WS-MSG-ERRO
                       DISPLAY SS-DADOS
               END-READ.
               ACCEPT MOSTRA-ERRO.
       7000-ALTERAR.
           MOVE "MODULO - ALTERAR" TO WS-MODULO.
           DISPLAY TELA.
           DISPLAY TELA-REGISTRO.
           ACCEPT CHAVE.
               READ CLIENTE
               IF CLIENTE-STATUS = 0
                   ACCEPT SS-DADOS
                   REWRITE CLIENTES-REG
                   IF CLIENTE-STATUS = 0
                       MOVE "REGISTRO ALTERADO" TO WS-MSG-ERRO
                       ACCEPT MOSTRA-ERRO
                   ELSE
                       MOVE "REGISTRO NAO ALTERADO" TO WS-MSG-ERRO
                       ACCEPT MOSTRA-ERRO
                   END-IF
               ELSE
                   MOVE "REGISTRO NAO ENCONTRADO" TO WS-MSG-ERRO
                   ACCEPT MOSTRA-ERRO
               END-IF.





       8000-EXCLUIR.
           MOVE "MODULO - EXCLUSAO" TO WS-MODULO.
           DISPLAY TELA.
           DISPLAY TELA-REGISTRO.
           ACCEPT CHAVE.
               READ CLIENTE
               INVALID KEY
               MOVE "NAO ENCONTRADO" TO WS-MSG-ERRO
               NOT INVALID KEY
               MOVE "ENCONTRADO S/N? " TO WS-MSG-ERRO
               DISPLAY SS-DADOS
               END-READ.
                   ACCEPT MOSTRA-ERRO.
                   IF WS-TECLA EQUAL "S" AND CLIENTE-STATUS = 0
                       DELETE CLIENTE
                       INVALID KEY
                       MOVE "NAO EXCLUIDO" TO WS-MSG-ERRO
                       ACCEPT MOSTRA-ERRO
                       END-DELETE
                   END-IF.
       9000-RELATORIOTELA.
           MOVE "MODULO - RELATORIO" TO WS-MODULO.
           DISPLAY TELA.

           MOVE 12345 TO CLIENTES-FONE.
           START CLIENTE KEY EQUAL CLIENTES-FONE.
           READ CLIENTE
               INVALID KEY
                MOVE "NENHUM REGISTRO ENCONTRADO" TO WS-MSG-ERRO
               NOT INVALID KEY
                DISPLAY "    RELATORIO CLIENTES    "
                DISPLAY "=========================="
                PERFORM UNTIL CLIENTE-STATUS EQUAL 10
                ADD 1 TO WS-QTDREGISTROS
                   DISPLAY CLIENTES-FONE " "
                        CLIENTES-NOME " "
                        CLIENTES-EMAIL
                   READ CLIENTE NEXT

                ADD 1 TO WS-CONTADOR
                IF WS-CONTADOR GREATER 5
                    MOVE "PRESSIONE ALGUMA TECLA" TO WS-MSG-ERRO
                    ACCEPT MOSTRA-ERRO
                    MOVE "MODULO - RELATORIO" TO WS-MODULO
                    DISPLAY TELA
                    DISPLAY "    RELATORIO CLIENTES    "
                    DISPLAY "=========================="
                    MOVE 0 TO WS-CONTADOR
                END-IF
                END-PERFORM
           END-READ.
               MOVE "REGISTROS LIDOS " TO WS-MSG-ERRO.
               MOVE WS-QTDREGISTROS TO WS-MSG-ERRO(16:05).
               ACCEPT MOSTRA-ERRO.



       9100-RELATORIODISCO.


           MOVE "MODULO - RELATORIO" TO WS-MODULO.
           DISPLAY TELA.

           MOVE 12345 TO CLIENTES-FONE.
           START CLIENTE KEY EQUAL CLIENTES-FONE.
           READ CLIENTE
               INVALID KEY
                MOVE "NENHUM REGISTRO ENCONTRADO" TO WS-MSG-ERRO
               NOT INVALID KEY
                OPEN OUTPUT RELATO
                   PERFORM UNTIL CLIENTE-STATUS EQUAL 10
                       ADD 1 TO WS-QTDREGISTROS
                       MOVE CLIENTES-REG TO RELATO-REG
                       WRITE RELATO-REG
                       READ CLIENTE NEXT
                   END-PERFORM
                   MOVE "REGISTROS LIDOS " TO RELATO-REG
                   MOVE WS-QTDREGISTROS TO RELATO-REG(16:05)
                   WRITE RELATO-REG
                CLOSE RELATO
           END-READ.
               MOVE "REGISTROS LIDOS " TO WS-MSG-ERRO.
               MOVE WS-QTDREGISTROS TO WS-MSG-ERRO(16:05).
               ACCEPT MOSTRA-ERRO.

       END PROGRAM CLIENTES.
