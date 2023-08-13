       identification division.
       program-id. data.
       working-storage section.
       01 ws-dtTeste                       pic 9(008) value 20011983.
       01 redefines ws-dtTeste.
          02 ws-ddTeste                    pic 9(002).
          02 ws-mmTeste                    pic 9(002).
          02 ws-aaTeste                    pic 9(004).
       01 ws-dtTeste1                      pic 9(008) value zeros.
       01 redefines ws-dtTeste1.
          02 ws-aaTeste1                   pic 9(004).
          02 ws-mmTeste1                   pic 9(002).
          02 ws-ddTeste1                   pic 9(002).
       77 dt1 pic 9(008) value zeros.
       77 dt2 pic 9(008) value zeros.
       01 tab-ddMes                        pic x(024) values
        "312831303130313130313031".
       *>010203040506070809101112
       01 redefines tab-ddMes.
          02 tb-ddMes                      pic 9(002) occurs 12.
       01 ws-CalcAno.
          02 ws-intAno                     pic 9(004) value zeros.
          02 ws-restoAno                   pic 9(002) value zeros.

       procedure division.
       inicio.
          
           *>compute ws-dtTeste1 = ((ws-aaTeste * 10000) 
           *>                    +  (ws-mmTeste * 100)
           *>                    +  ws-ddTeste)
           *>compute dt1 = function integer-of-date(ws-dtTeste) + 5
           *>compute dt1 = function date-of-integer(dt1) 
           *>
           *>compute dt2 = function integer-of-date(ws-dtTeste1) + 5
           *>compute dt2 = function date-of-integer(dt2) 
           *>
           *>display "ws-dtTeste:" at 0201
           *>display ws-dtTeste    at 0214
           *>
           *>display "ws-dtTeste1:" at 0301
           *>display ws-dtTeste1    at 0314
           *>
           *>display "dt1 erro:"    at 0401
           *>display dt1    at 0414
           *>
           *>display "dt2 ok:"    at 0501
           *>display dt2    at 0514
           *>stop " "
           *>stop run.
           initialize ws-dtTeste1 
           display "DIGITE A DATA:   /  /    " AT 0201
           accept ws-ddTeste1 at 0216 auto-SKIP highlight          
           if ws-ddTeste1 < 1 or
              ws-ddTeste1 > 31
              display "DIA ERRADO - TECLE ALGO" at 0501 blink
			  stop " "
              stop run
           end-if
           accept ws-mmTeste1 at 0219 auto-SKIP highlight          
           if ws-mmTeste1 < 1 or
              ws-mmTeste1 > 12
              display "MES ERRADO - TECLE ALGO" at 0501 blink
			  stop " "
              stop run
           end-if
           accept ws-aaTeste1 at 0222 auto-SKIP highlight
           if ws-aaTeste1 < 1900 
              display "ANO ERRADO - TECLE ALGO" at 0501 blink
			  stop " "
              stop run
           end-if
           
           initialize ws-CalcAno
           divide ws-aaTeste1          by 4
                                       giving ws-intAno
                                    remainder ws-restoAno
           if ws-restoAno = 0
              move 29                      to tb-ddMes(2)
           end-if
           if ws-ddTeste1 > tb-ddMes(ws-mmTeste1)
              display "DIA ERRADO(MES) - TECLE ALGO" at 0501 blink
			  stop " "
              stop run
           end-if
           
           display "DATA DE ACORDO - TECLE ALGO" at 0501 blink
		   stop " "
           STOP RUN.
                                    
           
           

