       IDENTIFICATION DIVISION.
       PROGRAM-ID. inicio.
       AUTHOR. diogo.
       DATE-WRITTEN. 26/07/2023.
      *---------------------------------------------------------------*
       environment division.
       configuration section.
       special-names.
           crt status is crt-status       
           decimal-point is comma.
      *---------------------------------------------------------------*
       input-output section.
       file-control.
      *---------------------------------------------------------------*
           select arquivo       ASSIGN to 
                   "/curso/cobol/dados/arquivo"
                  organization         is indexed
                  access mode          is dynamic
                  lock   mode          is automatic
                  file status          is ws-status
                  record key           is arq-chave.                  
           select arquivos      ASSIGN to 
                   "/curso/cobol/dados/x"
                  organization         is line SEQUENTIAL
                  file status          is ws-status.
      *---------------------------------------------------------------*
       data division.
       file section.
      *---------------------------------------------------------------*
           fd arquivos.
           01 arqs-registro.
              02 arqs-chave.
                 03 arqs-codigo         pic 9(005).
              02 arqs-descricao         pic x(020).
              02 arqs-dtTeste           pic 9(008).
              02 redefines arqs-dtTeste.
                 03 arqs-aaTeste        pic 9(004).
                 03 arqs-mmTeste        pic 9(002).
                 03 arqs-ddTeste        pic 9(002).
           fd arquivo.
           01 arq-registro.
              02 arq-chave.
                 03 arq-codigo         pic 9(005).
              02 arq-descricao         pic x(020).
              02 arq-dtTeste           pic 9(008).
              02 redefines arq-dtTeste.
                 03 arq-aaTeste        pic 9(004).
                 03 arq-mmTeste        pic 9(002).
                 03 arq-ddTeste        pic 9(002).
                 
      *---------------------------------------------------------------*
       working-storage section.
      *---------------------------------------------------------------*
       01 ws-registro.
          03 ws-codigo                 pic 9(005).
          02 ws-descricao              pic x(020).
          02 ws-dtTeste                pic 9(008).
          02 redefines ws-dtTeste.
             03 ws-aaTeste             pic 9(004).
             03 ws-mmTeste             pic 9(002).
             03 ws-ddTeste             pic 9(002).
       01 tb-consulta.
          02 occurs 16.
             03 tb-codigo              pic 9(005).
             03 tb-descricao           pic x(020).
             03 tb-dtTeste             pic x(010).
       01 tab-lin                      pic x(032)  value
           "06070809101112131415161718192021".
       01 redefines tab-lin.
          02 tb-lin                    pic 9(002)  occurs 16. 
          
       01 ws-status                    pic x(002)  value spaces.
       01 ws-mens                      pic x(067)  value spaces.
       01 ws-opc                       pic x(001)  value spaces.
       01 idxm                         pic 9(003)  value zeros.
       01 ind                          pic 9(003)  value zeros.
       01 ind1                         pic 9(003)  value zeros.
       01 ws-line                      pic 9(002)  value zeros.
       01 crt-status.
          02 crt-status-1              pic 9.
             88 terminate-key            value 0.
             88 function-key             value 1.
             88 adis-key                 value 2.
             88 status-1-error           value 9.
          02 crt-status-2              pic 99 comp-x.
             88 esc-key                  value 0.
             88 f1-key                   value 1.
             88 left-key                 value 64.
             88 right-key                value 65.
             88 up-key                   value 31.
             88 down-key                 value 32.
             88 enter-key                value 48.
          02 crt-status-3              pic 99 comp-x.
             88 raw-key-code              values 0 thru 255.
      *---------------------------------------------------------------*
       screen section.
      *---------------------------------------------------------------*
       01 tela1.
          02 LINE 01 COLUMN 01
             " Inclusao  Alteracao  Exclusao  Consulta  Sair "
      -      "".
          02 LINE 02 COLUMN 01
             "                                                         "
      -        "                    ".
          02 line 02 column 02
             pic x(077) from spaces HIGHLIGHT REVERSE-VIDEO.          
          02 LINE 02 COLUMN 35 
             "CURSO COBOL"  HIGHLIGHT REVERSE-VIDEO.
          02 LINE 03 COLUMN 01
             ""
      -        "".
          02 LINE 04 COLUMN 01
             " Codigo...:                                              "
      -        "                    ".
          02 LINE 05 COLUMN 01
             " Descricao:                                              "
      -        "                    ".
          02 LINE 06 COLUMN 01
             " Data.....:   /  /                                       "
      -        "                    ".
          02 LINE 07 COLUMN 01
             "                                                         "
      -        "                    ".
          02 LINE 08 COLUMN 01
             "                                                         "
      -        "                    ".
          02 LINE 09 COLUMN 01
             "                                                         "
      -        "                    ".
          02 LINE 10 COLUMN 01
             "                                                         "
      -        "                    ".
          02 LINE 11 COLUMN 01
             "                                                         "
      -        "                    ".
          02 LINE 12 COLUMN 01
             "                                                         "
      -        "                    ".
          02 LINE 13 COLUMN 01
             "                                                         "
      -        "                    ".
          02 LINE 14 COLUMN 01
             "                                                         "
      -        "                    ".
          02 LINE 15 COLUMN 01
             "                                                         "
      -        "                    ".
          02 LINE 16 COLUMN 01
             "                                                         "
      -        "                    ".
          02 LINE 17 COLUMN 01
             "                                                         "
      -        "                    ".
          02 LINE 18 COLUMN 01
             "                                                         "
      -        "                    ".
          02 LINE 19 COLUMN 01
             "                                                         "
      -        "                    ".
          02 LINE 20 COLUMN 01
             "                                                         "
      -        "                    ".
          02 LINE 21 COLUMN 01
             "                                                         "
      -        "                    ".
          02 LINE 22 COLUMN 01
             ""
      -        "".
          02 LINE 23 COLUMN 01
             "Mensagem:                                                "
      -        "                    ".
          02 LINE 24 COLUMN 01
             ""
      -        "".
       01 tela1-lin23.
          02 LINE 23 COLUMN 02 
             "Mensagem:"  HIGHLIGHT REVERSE-VIDEO.
          02 LINE 23 COLUMN 12 from WS-MENS HIGHLIGHT.
       01 tela1-inc. 
          02 LINE 01 COLUMN 03
             "Inclusao"   HIGHLIGHT REVERSE-VIDEO.
       01 tela1-alt. 
          02 LINE 01 COLUMN 14
             "Alteracao"  HIGHLIGHT REVERSE-VIDEO.
       01 tela1-exc. 
          02 LINE 01 COLUMN 26
             "Exclusao"  HIGHLIGHT REVERSE-VIDEO.
       01 tela1-con. 
          02 LINE 01 COLUMN 37
             "Consulta"  HIGHLIGHT REVERSE-VIDEO.
       01 tela1-sai. 
          02 LINE 01 COLUMN 48
             "Sair"  HIGHLIGHT REVERSE-VIDEO.
       01 acc-codigo.
          02 line 04 col 14 BACKGROUND-COLOR 1 HIGHLIGHT 
          pic z(005) using ws-codigo.
       01 acc-descricao.
          02 line 05 col 14 BACKGROUND-COLOR 1 HIGHLIGHT 
          pic x(020) using ws-descricao UPPER. 

       01 acc-ddTeste.
          02 line 06 col 14 BACKGROUND-COLOR 1 HIGHLIGHT 
          pic 9(002) using ws-ddTeste blank zeros.
       01 acc-mmTeste.
          02 line 06 col 17 BACKGROUND-COLOR 1 HIGHLIGHT 
          pic 9(002) using ws-mmTeste blank zeros.
       01 acc-aaTeste.
          02 line 06 col 20 BACKGROUND-COLOR 1 HIGHLIGHT 
          pic 9(004) using ws-aaTeste blank zeros.
       01 tela1-conf.
          02 LINE 23 COLUMN 02 
             "Mensagem:"  HIGHLIGHT REVERSE-VIDEO.
          02 LINE 23 COLUMN 12 
             pic x(060) from WS-MENS HIGHLIGHT.
          02 line 23 column 76 
             "[ ]" HIGHLIGHT REVERSE-VIDEO.
          02 line 23 column 77  BACKGROUND-COLOR 1 HIGHLIGHT 
             pic x using ws-opc UPPER.
             
      *              +-Inclusao-Alteracao-Exclusao-Consulta-Sair-----------------------------------+
      *              |                                 CURSO COBOL                                 |
      *              |-----------------------------------------------------------------------------|
      *              | Codigo|Descricao          |Data    |________________________________________|
      *              |-----------------------------------------------------------------------------|
      *              |                                                                             |61
      *              |                                                                             |72
      *              |                                                                             |83
      *              |                                                                             |94
      *              |                                                                             |05
      *              |                                                                             |16
      *              |                                                                             |27
      *              |                                                                             |38
      *              |                                                                             |49
      *              |                                                                             |50
      *              |                                                                             |61
      *              |                                                                             |72
      *              |                                                                             |83
      *              |                                                                             |94
      *              |                                                                             |05
      *              |                                                                             |16
      *              |-----------------------------------------------------------------------------|
      *              |Mensagem:                                                                    |
      *              +-----------------------------------------------------------------------------+       
       01 tela2 blank screen.
          02 LINE 01 COLUMN 01
             " Inclusao  Alteracao  Exclusao  Consulta  Sair "
      -      "".
          02 LINE 02 COLUMN 01
             "                                                         "
      -        "                    ".
          02 line 02 column 02
             pic x(077) from spaces HIGHLIGHT REVERSE-VIDEO.          
          02 LINE 02 COLUMN 35 
             "CURSO COBOL"  HIGHLIGHT REVERSE-VIDEO.
          02 LINE 03 COL 1 VALUE ""
      -"".
          02 LINE 04 COL 1 VALUE " Codigo Descricao              Data  "
      -"                                       ".
          02 LINE 05 COL 1 VALUE ""
      -"".
          02 LINE 06 COLUMN 01
             "                                                         "
      -        "                    ".
          02 LINE 07 COLUMN 01
             "                                                         "
      -        "                    ".
          02 LINE 08 COLUMN 01
             "                                                         "
      -        "                    ".
          02 LINE 09 COLUMN 01
             "                                                         "
      -        "                    ".
          02 LINE 10 COLUMN 01
             "                                                         "
      -        "                    ".
          02 LINE 11 COLUMN 01
             "                                                         "
      -        "                    ".
          02 LINE 12 COLUMN 01
             "                                                         "
      -        "                    ".
          02 LINE 13 COLUMN 01
             "                                                         "
      -        "                    ".
          02 LINE 14 COLUMN 01
             "                                                         "
      -        "                    ".
          02 LINE 15 COLUMN 01
             "                                                         "
      -        "                    ".
          02 LINE 16 COLUMN 01
             "                                                         "
      -        "                    ".
          02 LINE 17 COLUMN 01
             "                                                         "
      -        "                    ".
          02 LINE 18 COLUMN 01
             "                                                         "
      -        "                    ".
          02 LINE 19 COLUMN 01
             "                                                         "
      -        "                    ".
          02 LINE 20 COLUMN 01
             "                                                         "
      -        "                    ".
          02 LINE 21 COLUMN 01
             "                                                         "
      -        "                    ".
          02 LINE 22 COLUMN 01
             ""
      -        "".
          02 LINE 23 COLUMN 01
             "Mensagem:                                                "
      -        "                    ".
          02 LINE 24 COLUMN 01
             ""
      -        "".
*>├───────┬─────────────────────┬──────────┬─────────────────────────────────────┤
*>│ Codigo│ Descricao           │   Data   │                                     │
*>├───────┴─────────────────────┴──────────┴─────────────────────────────────────┤
*>   12345 12345678901234567890  12/45/789012345678901234567890123456789012345678     
*>12345678901234567890123456789012345678901234567890   
   
       01 tl2-linConsulta.
          02 line ws-line column 04 
               pic z(005) using tb-codigo(ind). 
          02 line ws-line column 10 
               pic x(020) using tb-descricao(ind). 
          02 line ws-line column 32 
               pic x(010) using tb-dtTeste(ind). 
          02 line ws-line column 02
               pic x(002) from spaces.
          02 line ws-line column 09
               pic x(001) from spaces.
          02 line ws-line column 30
               pic x(002) from spaces.
          02 line ws-line column 42 
               pic x(037) from spaces. 
               
       01 tl2-linConsulta2  HIGHLIGHT REVERSE-VIDEO.
          02 line ws-line column 04 
               pic z(005) using tb-codigo(ind). 
          02 line ws-line column 10 
               pic x(020) using tb-descricao(ind). 
          02 line ws-line column 32 
               pic x(010) using tb-dtTeste(ind). 
          02 line ws-line column 02
               pic x(002) from spaces.
          02 line ws-line column 09
               pic x(001) from spaces.
          02 line ws-line column 30
               pic x(002) from spaces.
          02 line ws-line column 42 
               pic x(037) from spaces. 
      *---------------------------------------------------------------*
       PROCEDURE DIVISION.
      *---------------------------------------------------------------*
       001-INICIO.
      *---------------------------------------------------------------*
           DISPLAY tela1 
           INITIALIZE ws-mens
           display tela1-lin23
           *>open output arquivo
           *>close arquivo
           OPEN I-O arquivo
           if ws-status <> "00" and "05"
              initialize ws-mens
              string "Erro de abertura no arq {ARQUIVO}, FS: ("
                     ws-status ") - tecle ENTER!"
                                         into ws-mens
              end-string
              display tela1-lin23
              
              accept ws-opc at 2378
              go 999-fim
           end-if
           *>open input arquivos 
           *>if ws-status <> "00" and "05"
           *>   initialize ws-mens
           *>   string "Erro de abertura no arq {ARQUIVOS}, FS: ("
           *>          ws-status ") - tecle ENTER!"
           *>                              into ws-mens
           *>   end-string
           *>   display tela1-lin23
           *>   
           *>   accept ws-opc at 2378
           *>   go 999-fim
           *>end-if
           *>perform until exit
           *>   read arquivos
           *>   if ws-status <> "00"
           *>      exit perform
           *>   end-if
           *>   write arq-registro from arqs-registro
           *>move arq-codigo                 to ws-codigo     
           *>move arq-descricao              to ws-descricao
           *>move arq-dtTeste                to ws-dtTeste
           *>display acc-Codigo
           *>        acc-descricao
           *>        acc-ddTeste
           *>        acc-mmTeste
           *>        acc-aaTeste
           *>          stop " "
           *>end-perform
           *>stop run
           
           move 1                          to idxm
           display tela1-inc
           perform 050-recebeMenu
              thru 050-recebeMenu-fim
           go 999-fim.
           
      *---------------------------------------------------------------*
       050-recebeMenu.
      *---------------------------------------------------------------*
           initialize ws-mens
           string "Utilize setas " 
		   	    x"e28690"
                " "  				
		   	    x"e28692"
		        " do teclado e Enter p/ opcao, "
                "ESC=Encerra"
                                         into ws-mens          
           end-string
           display tela1-lin23
           accept ws-opc at 2378 
           evaluate true
           when esc-key
              display tela1-sai
              go 050-recebeMenu-fim
           when left-key
              subtract 1                 from idxm
              if idxm = 0
                 move 5                    to idxm            
              end-if
           when right-key  
              add 1                        to idxm
              if idxm = 6
                 move 1                    to idxm
              end-if
           when up-key     
              continue
           when down-key   
              continue
           when enter-key  
               evaluate idxm
               when 1
                  display tela1-inc
                  perform 100-inclusao
                     thru 100-inclusao-fim
               when 2
                  display tela1-alt
                  perform 200-alteracao
                     thru 200-alteracao-fim
               when 3
                  display tela1-exc
                  perform 300-exclusao
                     thru 300-exclusao-fim
               when 4
                  display tela2
                          tela1-con
                  *>display tela1-conf
                  display tela1-lin23
                  perform 400-consulta
                     thru 400-consulta-fim
                  accept tela1-conf
               when 5
                  display tela1-sai
                  go 050-recebeMenu-fim
               end-evaluate
           end-evaluate

           display tela1           
           evaluate idxm
           when 1
              display tela1-inc
           when 2
              display tela1-alt
           when 3
              display tela1-exc
           when 4
              display tela2
              display tela1-con
           when 5
              display tela1-sai
           end-evaluate.
           
           go 050-recebeMenu.
           
           
           *>move crt-status-2             to key-code-1-display
           *>display "You pressed function key" at 0705
           *>display key-code-1-display         at 0730
           *>display crt-status-1               at 0750
           *>stop " ".
       050-recebeMenu-fim.
           exit.       
      *---------------------------------------------------------------*
       100-inclusao.
      *---------------------------------------------------------------*
           
       100-inclusaoCodigo.
           initialize ws-registro
           display acc-codigo
                   acc-descricao
                   acc-ddTeste
                   acc-mmTeste
                   acc-aaTeste
           perform recebeCodigo
              thru recebeCodigo-fim
           if esc-key
              go 100-inclusao-fim
           end-if
           initialize arq-registro
           move ws-codigo                  to arq-codigo
           read arquivo with ignore lock
           if ws-status = "00" or "02"
              move arq-descricao           to ws-descricao
              move arq-dtTeste             to ws-dtTeste
              display acc-descricao
                      acc-ddTeste
                      acc-mmTeste
                      acc-aaTeste
              initialize ws-mens
              move "Codigo ja cadastrado, tecle ENTER"
                                           to ws-mens
              display tela1-lin23
              
              accept ws-opc at 2378
              go 100-inclusao
           end-if
           .
       100-inclusaoDescricao.
           perform recebeDescricao
              thru recebeDescricao-Fim
           if esc-key
              go 100-inclusaoCodigo
           end-if
           .
       100-inclusaoData.
           perform recebeData
              thru recebeDataFim
           if esc-key
              go 100-inclusaoDescricao
           end-if
                       
           .
       100-inclusao-grava.
           
           initialize ws-opc
           move "Confirma a gravacao? <S/N>"      
                                           to ws-mens
           display tela1-conf
           perform until ws-opc = "S" or "N"
              accept tela1-conf
           end-perform
           initialize ws-mens
           display tela1-conf
           if ws-opc = "N"
              go 100-inclusaoData
           end-if
           initialize ws-opc
           initialize arq-registro
           move ws-codigo                  to arq-codigo
           move ws-descricao               to arq-descricao
           move ws-dtTeste                 to arq-dtTeste
           write arq-registro
           if ws-status <> "00" and "02"
              initialize ws-mens
              string "Erro de gravacao no arq {ARQUIVO}, FS: ("
                     ws-status ") - tecle ENTER!"
                                         into ws-mens
              end-string
              display tela1-lin23
              
              accept ws-opc at 2378
           end-if
           .
       100-inclusao-fim.
           exit.       
      *---------------------------------------------------------------*
       200-alteracao.
      *---------------------------------------------------------------*
           
       200-alteracaoCodigo.
           initialize ws-registro
           display acc-codigo
                   acc-descricao
                   acc-ddTeste
                   acc-mmTeste
                   acc-aaTeste
           perform recebeCodigo
              thru recebeCodigo-fim
           if esc-key
              go 200-alteracao-fim
           end-if
           initialize arq-registro
           move ws-codigo                  to arq-codigo
           read arquivo with ignore lock 
           if ws-status <> "00" and "02"
              initialize ws-mens
              move "Codigo nao cadastrado, tecle ENTER"
                                           to ws-mens
              display tela1-lin23
              
              accept ws-opc at 2378
              go 200-alteracaoCodigo
           end-if
           move arq-descricao              to ws-descricao
           move arq-dtTeste                to ws-dtTeste
           display acc-descricao
                   acc-ddTeste
                   acc-mmTeste
                   acc-aaTeste
           .
       200-alteracaoDescricao.
           perform recebeDescricao
              thru recebeDescricao-Fim
           if esc-key
              go 200-alteracaoCodigo
           end-if
           .
       200-alteracaoData.
           perform recebeData
              thru recebeDataFim
           if esc-key
              go 200-alteracaoDescricao
           end-if
                       
           .
       200-alteracao-grava.
           initialize ws-opc
           move "Confirma a gravacao? <S/N>"      
                                           to ws-mens
           display tela1-conf
           perform until ws-opc = "S" or "N"
              accept tela1-conf
           end-perform
           initialize ws-mens
           display tela1-conf
           if ws-opc = "N"
              go 200-alteracaoData
           end-if
           initialize ws-opc
           initialize arq-registro
           move ws-codigo                  to arq-codigo
           move ws-descricao               to arq-descricao
           move ws-dtTeste                 to arq-dtTeste
           rewrite arq-registro
           if ws-status <> "00" and "02"
              initialize ws-mens
              string "Erro de alteracao no arq {ARQUIVO}, FS: ("
                     ws-status ") - tecle ENTER!"
                                         into ws-mens
              end-string
              display tela1-lin23
              
              accept ws-opc at 2378
           end-if
           .
       200-alteracao-fim.
           exit.       
      *---------------------------------------------------------------*
       300-exclusao.
      *---------------------------------------------------------------*
       300-exclusaoCodigo.
           initialize ws-registro
           display acc-codigo
                   acc-descricao
                   acc-ddTeste
                   acc-mmTeste
                   acc-aaTeste
           perform recebeCodigo
              thru recebeCodigo-fim
           if esc-key
              go 300-exclusao-fim
           end-if
           initialize arq-registro
           move ws-codigo                  to arq-codigo
           read arquivo with ignore lock 
           if ws-status <> "00" and "02"
              initialize ws-mens
              move "Codigo nao cadastrado, tecle ENTER"
                                           to ws-mens
              display tela1-lin23
              
              accept ws-opc at 2378
              go 300-exclusaoCodigo
           end-if
           move arq-descricao              to ws-descricao
           move arq-dtTeste                to ws-dtTeste
           display acc-descricao
                   acc-ddTeste
                   acc-mmTeste
                   acc-aaTeste
                   
                   .
                   
       300-exclusao-grava.
           initialize ws-opc
           move "Confirma a exclusao? <S/N>"      
                                           to ws-mens
           display tela1-conf
           perform until ws-opc = "S" or "N"
              accept tela1-conf
           end-perform
           initialize ws-mens
           display tela1-conf
           if ws-opc = "N"
              go 300-exclusaoCodigo
           end-if
           initialize ws-opc
           initialize arq-registro
           move ws-codigo                  to arq-codigo
           delete arquivo
           if ws-status <> "00" and "02"
              initialize ws-mens
              string "Erro de exclusao no arq {ARQUIVO}, FS: ("
                     ws-status ") - tecle ENTER!"
                                         into ws-mens
              end-string
              display tela1-lin23
              
              accept ws-opc at 2378
           end-if
           .
           
       300-exclusao-fim.       
           exit.       
      *---------------------------------------------------------------*
       400-consulta.
      *---------------------------------------------------------------*
      
           perform 400-consulta-limpa
      
           initialize arq-registro
                      ind
                      tb-consulta
           perform 400-consulta-next              
           perform 400-consulta-exibe
           move 1                          to ind
           if ind > 0
              perform 400-consulta-tela
              go 400-consulta-fim
           end-if
           
           .
           exit.
       400-consulta-next.      
       
           initialize ind
           start arquivo key not < arq-chave
           perform until exit
               read arquivo next with ignore lock
               if ws-status <> "00" and "02"
                  exit perform
               end-if
               add 1                       to ind
               if ind > 16 
                  read arquivo previous with ignore lock
                  exit perform
               end-if
               perform 400-consulta-carrega-tab
           end-perform
           unlock arquivo
           
           .
           exit.
       400-consulta-previous.      
           
           initialize ind
           start arquivo key not > arq-chave
           perform until exit
               read arquivo previous with ignore lock
               if ws-status <> "00" and "02"
                  exit perform
               end-if
               add 1                       to ind
               if ind > 16 
                  read arquivo next with ignore lock
                  exit perform
               end-if
               perform 400-consulta-carrega-tab
           end-perform
           unlock arquivo
           
           .
           exit.
       400-consulta-exibe.     
       
           perform varying ind from 1 by 1 until ind > 16 or
              tb-codigo(ind) = 0
              move tb-lin(ind)             to ws-line
              display tl2-linConsulta
           end-perform
           
           .
           exit.
       400-consulta-carrega-tab.
       
           move arq-codigo                 to tb-codigo(ind)
           move arq-descricao              to tb-descricao(ind)
           string arq-ddTeste "/"
                  arq-mmTeste "/"
                  arq-aaTeste            into tb-dtTeste(ind)
           end-string
          
           .
           exit.
       400-consulta-tela.
       
           move tb-lin(ind)                to ws-line
           display tl2-linConsulta2
           initialize ws-mens
           string "Utilize setas " 
				  x"e28691"  
				  " " 
				  x"e28693"  
                  " do teclado, ESC=Encerra"
                                         into ws-mens          
           end-string
		   *>← → 
           display tela1-lin23
           perform until exit
               accept ws-opc at 2378 
               evaluate true
               when up-key
                 display tl2-linConsulta
                 subtract 1              from ind
                 if ind = 0
                    move tb-codigo(1)      to ws-codigo
                    perform 400-consulta-limpa
                    initialize arq-registro
                    move ws-codigo         to arq-codigo
                    initialize ws-codigo
                    perform 400-consulta-previous             
                    move tb-codigo(16)     to ws-codigo
                    perform 400-consulta-limpa
                    initialize arq-registro
                    move ws-codigo         to arq-codigo
                    initialize ws-codigo
                    perform 400-consulta-next             
                    perform 400-consulta-exibe
                    move 1                 to ind
                 end-if
                 move tb-lin(ind)          to ws-line
                 display tl2-linConsulta2
               when down-key
                 display tl2-linConsulta
                 add 1                     to ind
                 if ind = 17
                    move tb-codigo(16)     to ws-codigo
                    perform 400-consulta-limpa
                    initialize arq-registro
                    move ws-codigo         to arq-codigo
                    initialize ws-codigo
                    perform 400-consulta-next             
                    perform 400-consulta-exibe
                    move 16                to ind
                 end-if
                 perform varying ind1 from 1 by 1 until ind1 > 16 or
                        tb-codigo(ind1) = 0
                 end-perform
                 if ind1 < 16
                    if tb-codigo(ind1) = 0
                       subtract 1        from ind1
                       move ind1           to ind
                    end-if
                 end-if
                 move tb-lin(ind)          to ws-line
                 display tl2-linConsulta2
               when esc-key
                 perform 400-consulta-limpa
                 perform 400-consulta-exibe
                 exit perform
               end-evaluate
           end-perform
  
           .
           exit.
           
       400-consulta-limpa.
           
           initialize tb-consulta
           perform varying ind from 1 by 1 until ind > 16 
              move tb-lin(ind)             to ws-line
              display tl2-linConsulta
           end-perform
  
           .
           exit.
      *---------------------------------------------------------------*
       400-consulta-fim.
           exit.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       recebeCodigo.
      *---------------------------------------------------------------*
      
           initialize ws-mens
           move "Digite o codigo, ESC=Sair"
                                           to ws-mens
           display tela1-lin23
           accept acc-codigo
           if esc-key
              go recebeCodigo-FIM
           end-if
           if ws-codigo = 0
              move "Codigo nao informado"  to ws-mens
              display tela1-lin23
              
              accept ws-opc at 2378
              go recebeCodigo
           end-if
              
           .
       recebeCodigo-FIM.
           exit.       
      *---------------------------------------------------------------*
       recebeDescricao.
      *---------------------------------------------------------------*
      
           initialize ws-mens
           move "Digite a descricao, ESC=Voltar"
                                           to ws-mens
           display tela1-lin23
           accept acc-descricao
           if esc-key
              go recebeDescricao-Fim
           end-if
           if ws-descricao = spaces
              move "Descricao nao informada"  to ws-mens
              display tela1-lin23
              
              accept ws-opc at 2378
              go recebeDescricao
           end-if
              
           .
      
       recebeDescricao-Fim.
           exit.       
      *---------------------------------------------------------------*
       recebeData.
      *---------------------------------------------------------------*
       recebeDataDia.
       
           initialize ws-mens
           move "Digite o dia, ESC=Voltar"
                                           to ws-mens
           display tela1-lin23
           accept acc-ddTeste
           if esc-key
              go recebeDataFim
           end-if
           if ws-ddTeste = 0 
              move "Dia nao informado"  to ws-mens
              display tela1-lin23
              
              accept ws-opc at 2378
              go recebeDataDia
           end-if
              
           .
       recebeDataMes.
       
           initialize ws-mens
           move "Digite o mes, ESC=Voltar"
                                           to ws-mens
           display tela1-lin23
           accept acc-mmTeste
           if esc-key
              go recebeDataDia
           end-if
           if ws-mmTeste = 0 
              move "Mes nao informado"  to ws-mens
              display tela1-lin23
              
              accept ws-opc at 2378
              go recebeDataMes
           end-if
              
           .
       recebeDataAno.
       
           initialize ws-mens
           move "Digite o ano, ESC=Voltar"
                                           to ws-mens
           display tela1-lin23
           accept acc-aaTeste
           if esc-key
              go recebeDataMes
           end-if
           if ws-aaTeste = 0 
              move "Ano nao informado"  to ws-mens
              display tela1-lin23
              
              accept ws-opc at 2378
              go recebeDataAno
           end-if
              
           .
       recebeDataFim.
           exit.       
      *---------------------------------------------------------------*
       999-fim.
      *---------------------------------------------------------------*
           close arquivo
           stop run.
