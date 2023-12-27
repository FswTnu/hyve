#INCLUDE 'PROTHEUS.CH'
#INCLUDE 'RWMAKE.CH'
#include 'TOPCONN.CH'
#include 'parmtype.ch'

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Funcao    ³ FIN06R01 ³ Autor ³ Sigatec.herminio    ³ Data ³ 04/06/2020 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descriao  ³                                                            ³±±
±±           ³                                                            ³±±
±±           ³                                                            ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³ Uso      ³ Generico                                                   ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*/


User Function FIN06R01()

	Local   _aPerg      := {}

	Private _aFiltro := {"1=Data do PC","2=Data da Baixa","3=Ambas"}
	Private _dDtIni  := dDatabase 
	Private _dDtFim  := dDatabase 
	Private _dBxIni  := dDatabase 
	Private _dBxFim  := dDatabase 
	Private _aOpcoes := {"1=Pago/Parcial","2=Em Aberto","3=Todos"}

	Private _cPerg      := PadR("FIN06R01",Len(SX1->X1_GRUPO))

	// monta a lista de perguntas
	aAdd(_aPerg,{"Data p/ filtro ?" ,"C",1	     	  			,0,"C",_aFiltro          ,""  	   ,"" })	//mv_par01
	aAdd(_aPerg,{"Data PC de     ?" ,"D",10     				,0,"G",                  ,""       ,"" })	//mv_par02
	aAdd(_aPerg,{"Data PC ate    ?" ,"D",10     				,0,"G",                  ,""       ,"" })	//mv_par03
	aAdd(_aPerg,{"Data Bx de     ?" ,"D",10     				,0,"G",                  ,""       ,"" })	//mv_par04
	aAdd(_aPerg,{"Data Bx ate    ?" ,"D",10     				,0,"G",                  ,""       ,"" })	//mv_par05
	aAdd(_aPerg,{"Filtrar Pgto   ?" ,"C",1	     	  			,0,"C",_aOpcoes          ,""  	   ,"" })	//mv_par06

	// cria o grupo de perguntas
	AjustaSX1(_cPerg,_aPerg)

	Pergunte(_cPerg,.F.)

	oReport := ReportDef()
	oReport:PrintDialog()

Return()


Static Function ReportDef()
	Private oReport

	//Montando o objeto oReport
	oReport := TReport():NEW("FIN06R01", "Pedidos de Compras x Pagamentos", _cPerg, {|oReport|PrintReport(oReport)}, "", , , , , .T. )

	oSecSC7 := TRSection():New( oReport , "Pedidos de Compras x Pagamentos"  , {"SC7"}  )

	//oSecSC7:SetLineStyle() //Define a impressao da secao em linha
	oSecSC7:SetReadOnly()
	
	TRCell():New(oSecSC7  ,"C7_EMISSAO" ,"","Emissao PC"    ,PesqPict('SC7','C7_EMISSAO') 	,10    						,,,"LEFT"  	,,"LEFT" ,,,,,,.T.)
	TRCell():New(oSecSC7  ,"C7_FORNECE" ,"","Fornecedor"    ,PesqPict('SC7','C7_FORNECE') 	,TamSx3("C7_FORNECE")[1]    ,,,"LEFT"  	,,"LEFT" ,,,,,,.T.)
	TRCell():New(oSecSC7  ,"C7_LOJA"  	,"","Loja"       	,PesqPict('SC7','C7_LOJA')    	,TamSx3("C7_LOJA")[1]    	,,,"LEFT"  	,,"LEFT" ,,,,,,.T.)
	TRCell():New(oSecSC7  ,"A2_NOME"    ,"","Razao Social"  ,PesqPict('SA2','A2_NOME')    	,TamSx3("A2_NOME")[1]       ,,,"LEFT"  	,,"LEFT" ,,,,,,.T.)
	TRCell():New(oSecSC7  ,"C7_UM"  	,"","Un."         	,PesqPict('SC7','C7_UM')    	,TamSx3("C7_UM")[1]    		,,,"LEFT"  	,,"LEFT" ,,,,,,.T.)
	TRCell():New(oSecSC7  ,"C7_QUANT"   ,"","Quant."       	,PesqPict('SC7','C7_QUANT')     ,TamSx3("C7_QUANT")[1]     	,,,"RIGHT"  ,,"LEFT" ,,,,,,.T.)
	TRCell():New(oSecSC7  ,"C7_CC"    	,"","CC"        	,PesqPict('SC7','C7_CC')      	,TamSx3("C7_CC")[1]         ,,,"LEFT"  	,,"LEFT" ,,,,,,.T.)
	TRCell():New(oSecSC7  ,"C7_ITEM"    ,"","Item PC"		,PesqPict('SC7','C7_ITEM')      ,TamSx3("C7_ITEM")[1]     	,,,"LEFT"  	,,"LEFT" ,,,,,,.T.)
	TRCell():New(oSecSC7  ,"C7_PRODUTO" ,"","Produto"       ,PesqPict('SC7','C7_PRODUTO')   ,TamSx3("C7_PRODUTO")[1]  	,,,"LEFT"  	,,"LEFT" ,,,,,,.T.)
	TRCell():New(oSecSC7  ,"C7_DESCRI"  ,"","Descricao"     ,PesqPict('SC7','C7_DESCRI')    ,TamSx3("C7_DESCRI")[1]     ,,,"LEFT" 	,,"LEFT" ,,,,,,.T.)
	TRCell():New(oSecSC7  ,"C7_ITEMCTA" ,"","It. Cont."     ,PesqPict('SC7','C7_ITEMCTA')   ,TamSx3("C7_ITEMCTA")[1]    ,,,"LEFT" 	,,"LEFT" ,,,,,,.T.)
	TRCell():New(oSecSC7  ,"C7_TES"  	,"","TES"        	,PesqPict('SC7','C7_TES')     	,TamSx3("C7_TES")[1]     	,,,"LEFT" 	,,"LEFT" ,,,,,,.T.)
	TRCell():New(oSecSC7  ,"C7_CLVL"  	,"","Cl. Valor"     ,PesqPict('SC7','C7_CLVL')     	,TamSx3("C7_CLVL")[1]     	,,,"LEFT" 	,,"LEFT" ,,,,,,.T.)
	TRCell():New(oSecSC7  ,"C7_TOTAL"  	,"","Total"        	,PesqPict('SC7','C7_TOTAL')     ,TamSx3("C7_TOTAL")[1]     	,,,"RIGHT" 	,,"LEFT" ,,,,,,.T.)
	TRCell():New(oSecSC7  ,"C7_OBS"  	,"","Observacao"    ,PesqPict('SC7','C7_OBS')     	,TamSx3("C7_OBS")[1]     	,,,"LEFT" 	,,"LEFT" ,,,,,,.T.)
//	TRCell():New(oSecSC7  ,"D1_DOC"  	,"","NF"        	,PesqPict('SD1','D1_DOC')     	,TamSx3("D1_DOC")[1]     	,,,"LEFT" 	,,"LEFT" ,,,,,,.T.)
//	TRCell():New(oSecSC7  ,"D1_SERIE"  	,"","Serie"        	,PesqPict('SD1','D1_SERIE')     ,TamSx3("D1_SERIE")[1]     	,,,"LEFT" 	,,"LEFT" ,,,,,,.T.)
//	TRCell():New(oSecSC7  ,"D1_EMISSAO" ,"","Data NF"       ,PesqPict('SD1','D1_EMISSAO')   ,10						    ,,,"LEFT" 	,,"LEFT" ,,,,,,.T.)
//	TRCell():New(oSecSC7  ,"D1_DTDIGIT" ,"","Dig. NF"       ,PesqPict('SD1','D1_DTDIGIT')   ,10    						,,,"LEFT" 	,,"LEFT" ,,,,,,.T.)
	TRCell():New(oSecSC7  ,"E2_NUM"  	,"","Titulo"        ,PesqPict('SE2','E2_NUM')     	,TamSx3("E2_NUM")[1]     	,,,"LEFT" 	,,"LEFT" ,,,,,,.T.)
//	TRCell():New(oSecSC7  ,"E2_PREFIXO" ,"","Prefixo"       ,PesqPict('SE2','E2_PREFIXO')   ,TamSx3("E2_PREFIXO")[1]    ,,,"LEFT" 	,,"LEFT" ,,,,,,.T.)
//	TRCell():New(oSecSC7  ,"E2_PARCELA" ,"","Parcela"       ,PesqPict('SE2','E2_PARCELA')   ,TamSx3("E2_PARCELA")[1]    ,,,"LEFT" 	,,"LEFT" ,,,,,,.T.)
	TRCell():New(oSecSC7  ,"E2_NATUREZ" ,"","Natureza"      ,PesqPict('SE2','E2_NATUREZ')   ,TamSx3("E2_NATUREZ")[1]    ,,,"LEFT" 	,,"LEFT" ,,,,,,.T.)
	TRCell():New(oSecSC7  ,"E2_VENCTO"  ,"","Vencto"        ,PesqPict('SE2','E2_VENCTO')    ,10     					,,,"LEFT" 	,,"LEFT" ,,,,,,.T.)
//	TRCell():New(oSecSC7  ,"E2_VENCREA" ,"","Vencto Real"   ,PesqPict('SE2','E2_VENCREA')   ,10    						,,,"LEFT" 	,,"LEFT" ,,,,,,.T.)
	TRCell():New(oSecSC7  ,"E2_VALOR"  	,"","Vlr Titulo"    ,PesqPict('SE2','E2_VALOR')     ,TamSx3("E2_VALOR")[1]     	,,,"RIGHT" 	,,"LEFT" ,,,,,,.T.)
	TRCell():New(oSecSC7  ,"E2_BAIXA"  	,"","Data Bx."      ,PesqPict('SE2','E2_BAIXA')     ,10     					,,,"LEFT" 	,,"LEFT" ,,,,,,.T.)
	TRCell():New(oSecSC7  ,"E2_VALLIQ"  ,"","Valor Bx."     ,PesqPict('SE2','E2_VALLIQ')    ,TamSx3("E2_VALLIQ")[1]     ,,,"RIGHT" 	,,"LEFT" ,,,,,,.T.)
	TRCell():New(oSecSC7  ,"E2_ADTO"    ,"","Vlr Adto."     ,PesqPict('SE2','E2_VALLIQ')    ,TamSx3("E2_VALLIQ")[1]     ,,,"RIGHT" 	,,"LEFT" ,,,,,,.T.)
	TRCell():New(oSecSC7  ,"E2_SALDO"  	,"","Saldo"        	,PesqPict('SE2','E2_SALDO')     ,TamSx3("E2_SALDO")[1]     	,,,"RIGHT" 	,,"LEFT" ,,,,,,.T.)

	oReport:SetLandScape(.F.)
	//oReport:SetPortrait(.T.)

Return(oReport)

/*
AND  BETWEEN '20200501' AND '20200531'
*/

Static Function PrintReport(oReport)
	Local _cQuery	 := ""
	Local _xy        := 0
	Local oSecSC7 	 := oReport:Section(1)
	Local _aRetSQL

	_cFiltr		:= MV_PAR01
	_dDtIni 	:= MV_PAR02
	_dDtFim 	:= MV_PAR03 
	_dBxIni 	:= MV_PAR04
	_dBxFim 	:= MV_PAR05 
	_cOpcao		:= MV_PAR06

	_cQuery := " SELECT C7_EMISSAO,C7_FORNECE,C7_LOJA,A2_NOME,C7_UM,C7_QUANT,C7_CC,C7_ITEM,C7_PRODUTO,C7_DESCRI,C7_ITEMCTA,C7_TES,C7_CLVL,C7_TOTAL " + CRLF
//	_cQuery += " ,C7_OBS,D1_DOC,D1_SERIE,D1_EMISSAO,D1_DTDIGIT,E2_NUM,E2_PREFIXO,E2_PARCELA,E2_NATUREZ,E2_VENCTO,E2_VENCREA,E2_VALOR,E2_BAIXA,E2_VALLIQ,E2_SALDO " + CRLF
	_cQuery += " ,C7_OBS,E2_NUM,E2_PREFIXO,E2_PARCELA,E2_NATUREZ,E2_VENCTO,E2_VALOR,E2_BAIXA,E2_VALLIQ,E2_SALDO " + CRLF
	_cQuery += " ,C7_NUM,D1_TOTAL " + CRLF
	_cQuery += " ,CASE WHEN F1_VALMERC<>0 THEN D1_TOTAL/F1_VALMERC*E2_VALLIQ ELSE 0 END LIQUID " + CRLF
	_cQuery += " ,CASE WHEN F1_VALMERC<>0 THEN D1_TOTAL/F1_VALMERC*E2_SALDO ELSE 0 END SALDO " + CRLF
	_cQuery += " ,F1_VALMERC " + CRLF

	_cQuery += " ,(SELECT SUM(E5_VALOR) FROM " + RetSqlName("SE5") + " SE5 WHERE " + RetSqlCond("SE5") + " AND E5_NUMERO=E2_NUM AND E5_PREFIXO=E2_PREFIXO AND E5_PARCELA=E2_PARCELA AND E5_FORNECE=E2_FORNECE AND E5_LOJA=E2_LOJA AND E5_TIPODOC IN ('VL','BA') AND E5_DTCANBX='') TTL_PGTO " + CRLF
	_cQuery += " ,CASE WHEN F1_VALMERC<>0 THEN D1_TOTAL/F1_VALMERC*(SELECT SUM(E5_VALOR) FROM " + RetSqlName("SE5") + " SE5 WHERE " + RetSqlCond("SE5") + " AND E5_NUMERO=E2_NUM AND E5_PREFIXO=E2_PREFIXO AND E5_PARCELA=E2_PARCELA AND E5_FORNECE=E2_FORNECE AND E5_LOJA=E2_LOJA AND E5_TIPODOC IN ('VL','BA') AND E5_DTCANBX='') ELSE 0 END PGTO " + CRLF

	_cQuery += " ,(SELECT SUM(E5_VALOR) FROM " + RetSqlName("SE5") + " SE5 WHERE " + RetSqlCond("SE5") + " AND E5_NUMERO=E2_NUM AND E5_PREFIXO=E2_PREFIXO AND E5_PARCELA=E2_PARCELA AND E5_FORNECE=E2_FORNECE AND E5_LOJA=E2_LOJA AND E5_TIPODOC='CP' AND E5_DTCANBX='') TTL_ADTO " + CRLF
	_cQuery += " ,CASE WHEN F1_VALMERC<>0 THEN D1_TOTAL/F1_VALMERC*(SELECT SUM(E5_VALOR) FROM " + RetSqlName("SE5") + " SE5 WHERE " + RetSqlCond("SE5") + " AND E5_NUMERO=E2_NUM AND E5_PREFIXO=E2_PREFIXO AND E5_PARCELA=E2_PARCELA AND E5_FORNECE=E2_FORNECE AND E5_LOJA=E2_LOJA AND E5_TIPODOC='CP' AND E5_DTCANBX='') ELSE 0 END ADTO " + CRLF
	
	_cQuery += " FROM " + RetSqlName("SC7") + " SC7 " + CRLF
	_cQuery += " INNER JOIN " + RetSqlName("SD1") + " SD1 " + CRLF 
	_cQuery += " ON D1_FILIAL+D1_PEDIDO+D1_ITEMPC=C7_FILIAL+C7_NUM+C7_ITEM " + CRLF
	_cQuery += " AND " + RetSqlCond("SD1") + CRLF

	_cQuery += " INNER JOIN " + RetSqlName("SF1") + " SF1 " + CRLF 
	_cQuery += " ON F1_FILIAL+F1_DOC+F1_SERIE+F1_FORNECE+F1_LOJA=D1_FILIAL+D1_DOC+D1_SERIE+D1_FORNECE+D1_LOJA " + CRLF
	_cQuery += " AND " + RetSqlCond("SF1") + CRLF

	_cQuery += " INNER JOIN " + RetSqlName("SE2") + " SE2 " + CRLF 
	_cQuery += " ON E2_FILIAL+E2_NUM+E2_PREFIXO+E2_FORNECE+E2_LOJA=D1_FILIAL+D1_DOC+D1_SERIE+D1_FORNECE+D1_LOJA " + CRLF
	_cQuery += " AND " + RetSqlCond("SE2") + CRLF
	_cQuery += " LEFT JOIN " + RetSqlName("SA2") + " SA2 " + CRLF 
	_cQuery += " ON A2_COD+A2_LOJA=C7_FORNECE+C7_LOJA " + CRLF
	_cQuery += " AND " + RetSqlCond("SA2") + CRLF
	_cQuery += " WHERE " + RetSqlCond("SC7") + CRLF
	
	If _cFiltr == 1 .or. _cFiltr == 3 //{"1=Data do PC","2=Data da Baixa","3=Ambas"}
		_cQuery += " AND C7_EMISSAO BETWEEN '" + DtoS(_dDtIni) + "' AND '" + DtoS(_dDtFim) + "'" + CRLF
	EndIf
	
	IF 	_cOpcao == 1  //{"1=Pago/Parcial","2=Em Aberto","3=Todos"}
	
		If _cFiltr == 1  //{"1=Data do PC","2=Data da Baixa","3=Ambas"}
			_cQuery += " AND E2_BAIXA <> ''" + CRLF
		Else
			_cQuery += " AND E2_BAIXA BETWEEN '" + DtoS(_dBxIni) + "' AND '" + DtoS(_dBxFim) + "'" + CRLF
		EndIf
		
	ELSEIF _cOpcao == 2 //{"1=Pago/Parcial","2=Em Aberto","3=Todos"}
	
		_cQuery += " AND E2_BAIXA = ''" + CRLF
		
	ELSE

		If _cFiltr <> 1  //{"1=Data do PC","2=Data da Baixa","3=Ambas"}
			_cQuery += " AND ( E2_BAIXA = '' OR E2_BAIXA BETWEEN '" + DtoS(_dBxIni) + "' AND '" + DtoS(_dBxFim) + "' )" + CRLF
		EndIf

	ENDIF
	_cQuery += " ORDER BY C7_EMISSAO,C7_FORNECE,C7_NUM,C7_ITEM" + CRLF
	//memowrit("d:\FIN06R01.txt",_cQuery)

	_aRetSQL := CriaDados(_cQuery)

	oSecSC7:SetMeter(Len(_aRetSQL)) //numero de registros a processar

	oSecSC7:Init()

	For _xy:= 1 To Len(_aRetSQL)

		oSecSC7:Cell("C7_EMISSAO") 	:SetValue(   Stod(  _aRetSQL[_xy][01]))
		oSecSC7:Cell("C7_FORNECE") 	:SetValue(Alltrim(  _aRetSQL[_xy][02]))
		oSecSC7:Cell("C7_LOJA") 	:SetValue(Alltrim(  _aRetSQL[_xy][03]))
		oSecSC7:Cell("A2_NOME")    	:SetValue(Alltrim(  _aRetSQL[_xy][04]))
		oSecSC7:Cell("C7_UM") 		:SetValue(Alltrim(  _aRetSQL[_xy][05]))
		oSecSC7:Cell("C7_QUANT")  	:SetValue(          _aRetSQL[_xy][06] )
		oSecSC7:Cell("C7_CC")   	:SetValue(Alltrim(  _aRetSQL[_xy][07]))
		oSecSC7:Cell("C7_ITEM")   	:SetValue(Alltrim(  _aRetSQL[_xy][08]))
		oSecSC7:Cell("C7_PRODUTO")  :SetValue(Alltrim(  _aRetSQL[_xy][09]))
		oSecSC7:Cell("C7_DESCRI") 	:SetValue(Alltrim(  _aRetSQL[_xy][10]))
		oSecSC7:Cell("C7_ITEMCTA") 	:SetValue(Alltrim(  _aRetSQL[_xy][11]))
		oSecSC7:Cell("C7_TES") 		:SetValue(Alltrim(  _aRetSQL[_xy][12]))
		oSecSC7:Cell("C7_CLVL") 	:SetValue(Alltrim(  _aRetSQL[_xy][13]))
		oSecSC7:Cell("C7_TOTAL") 	:SetValue(          _aRetSQL[_xy][14] )
		oSecSC7:Cell("C7_OBS") 		:SetValue(Alltrim(  _aRetSQL[_xy][15]))
//		oSecSC7:Cell("D1_DOC") 		:SetValue(Alltrim(  _aRetSQL[_xy][16]))
//		oSecSC7:Cell("D1_SERIE") 	:SetValue(Alltrim(  _aRetSQL[_xy][17]))
//		oSecSC7:Cell("D1_EMISSAO") 	:SetValue(   Stod(  _aRetSQL[_xy][18]))
//		oSecSC7:Cell("D1_DTDIGIT") 	:SetValue(   Stod(  _aRetSQL[_xy][19]))
		oSecSC7:Cell("E2_NUM") 		:SetValue(Alltrim(  _aRetSQL[_xy][16]))
//		oSecSC7:Cell("E2_PREFIXO") 	:SetValue(Alltrim(  _aRetSQL[_xy][17]))
//		oSecSC7:Cell("E2_PARCELA") 	:SetValue(Alltrim(  _aRetSQL[_xy][18]))
		oSecSC7:Cell("E2_NATUREZ") 	:SetValue(Alltrim(  _aRetSQL[_xy][19]))
		oSecSC7:Cell("E2_VENCTO") 	:SetValue(   Stod(  _aRetSQL[_xy][20]))
//		oSecSC7:Cell("E2_VENCREA") 	:SetValue(   Stod(  _aRetSQL[_xy][25]))
		oSecSC7:Cell("E2_VALOR") 	:SetValue(          _aRetSQL[_xy][26] )   //_aRetSQL[_xy][21]
		oSecSC7:Cell("E2_BAIXA") 	:SetValue(   Stod(  _aRetSQL[_xy][22]))
		oSecSC7:Cell("E2_VALLIQ") 	:SetValue(          _aRetSQL[_xy][31] )   //_aRetSQL[_xy][23]
		oSecSC7:Cell("E2_ADTO") 	:SetValue(          _aRetSQL[_xy][33] )   // ADIANTAMENTO POR COMPENSACAO
		oSecSC7:Cell("E2_SALDO") 	:SetValue(          _aRetSQL[_xy][28] )   //_aRetSQL[_xy][24]
		
		oSecSC7:PrintLine()

		oReport:IncMeter()//incrementa regua

	Next _xy

	oSecSC7:Finish()

Return(oReport)


// Função para retornar um array a partir da query
Static Function CriaDados(cQuery)
	Local aRet    := {}
	Local aRet1   := {}
	Local nRegAtu := 0
	Local x       := 0
	Local _cAlias := GetNextAlias()

	cQuery := ChangeQuery(cQuery)

	dbUseArea(.T.,"TOPCONN",TCGenQry(,,cQuery),_cAlias,.T.,.T.)

	(_cAlias)->(dbgotop())

	aRet1   := Array(Fcount())
	nRegAtu := 1

	While !(_cAlias)->(Eof())

		For x:=1 To Fcount()
			aRet1[x] := FieldGet(x)
		Next
		Aadd(aRet,aclone(aRet1))

		(_cAlias)->(dbSkip())
		nRegAtu += 1
	Enddo

	If Select(_cAlias) <> 0
		(_cAlias)->(dbCloseArea())
	EndIf

Return(aRet)


Static Function AjustaSX1(mvPerg,vList,lDel)
	Local aArea := GetArea()
	Local cSeq := "1"
	Local _Lin := 0
	Local nAdic := 0
	Local nTamOrd := Len(SX1->X1_ORDEM)
	Local cOrdPerg := ""
	Local cCpoTmp := ""
	Local _LisOpc

	// abre arquivo de perguntas
	DBSelectArea("SX1")
	SX1->( DBSetOrder(1) )

	// padroniza tamanho do cPerg
	mvPerg := PadR(mvPerg,Len(SX1->X1_GRUPO))

	//verifica se deve recriar as perguntas
	If lDel
		SX1->( DBSeek(mvPerg) )
		//Apaga todo o grupo de Perguntas
		While SX1->( !Eof() ) .and. SX1->X1_GRUPO == mvPerg
			RecLock("SX1",.F.)
			SX1->( DbDelete() )
			MsUnLock("SX1")
			SX1->(DbSkip())
		EndDo
	EndIf

	// verifica se todas os parametros existem
	For _Lin := 1 to Len(vList)
		// cria a variavel Ordem
		cOrdPerg := StrZero(_Lin,nTamOrd)

		// pesquisa pelo parametro
		SX1->( DBSeek(mvPerg+cOrdPerg) )

		// operacao (alteracao ou inclusao)
		RecLock("SX1",SX1->(Eof()))
		SX1->X1_GRUPO	:= mvPerg
		SX1->X1_ORDEM	:= cOrdPerg
		SX1->X1_PERGUNT	:= vList[_Lin,1]
		SX1->X1_PERSPA	:= vList[_Lin,1]
		SX1->X1_PERENG	:= vList[_Lin,1]
		SX1->X1_VARIAVL	:= "mv_ch" + cSeq
		SX1->X1_TIPO	:= vList[_Lin,2]
		SX1->X1_TAMANHO	:= vList[_Lin,3]
		SX1->X1_DECIMAL	:= vList[_Lin,4]
		SX1->X1_GSC		:= vList[_Lin,5]
		//Lista de Opções
		If vList[_Lin,5] = "C"
			For _LisOpc := 1 to Len(vList[_Lin,6])
				cCpoTmp := "X1_DEF" + StrZero(_LisOpc,2)
				SX1->&cCpoTmp := vList[_Lin,6,_LisOpc]
			Next _LisOpc
		Else
			SX1->X1_F3 := vList[_Lin,7]
		EndIf
		SX1->X1_PICTURE	:= vList[_Lin,8]
		SX1->X1_VAR01   := "mv_par" + StrZero(_Lin,2)

		// verifica se tem informacoes de campos adicionais
		If (Len(vList[_Lin])==8).and.(ValType(vList[_Lin,8])=="A")
			// grava informacoes adicionais
			For nAdic := 1 to Len(vList[_Lin,8])
				// grava campo
				SX1->&(vList[_Lin,8][nAdic,1]) := vList[_Lin,8][nAdic,2]
			Next nAdic
		EndIf

		SX1->(MsUnlock())

		//Atualiza Seq
		cSeq := Soma1(cSeq)
	Next _Lin

	// restaura area inicial
	RestArea(aArea)

Return

