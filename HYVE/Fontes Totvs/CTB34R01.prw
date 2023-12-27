#INCLUDE 'PROTHEUS.CH'
#INCLUDE 'RWMAKE.CH'
#include 'TOPCONN.CH'
#include 'parmtype.ch'

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Funcao    ³ CTB34R01 ³ Autor ³ Sigatec.herminio    ³ Data ³ 27/02/2020 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descriao  ³ Impressao de Lancamentos Contabeis conf. layout solicitado ³±±
±±           ³                                                            ³±±
±±           ³                                                            ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³ Uso      ³ Generico                                                   ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*/


User Function CTB34R01()

	Local   _aPerg      := {}

	Private _cCodDe	:= TamSx3("CT1_CONTA")[1]
	Private _cCodAt	:= TamSx3("CT1_CONTA")[1]
	Private _dDtIni := dDatabase 
	Private _dDtFim := dDatabase 

	Private _cPerg      := PadR("CTB34R01",Len(SX1->X1_GRUPO))

	// monta a lista de perguntas
	aAdd(_aPerg,{"Conta de  ?" ,"C",TamSx3("CT1_CONTA")[1]   ,0,"G",                  ,"CT1"   ,"" })	//mv_par01
	aAdd(_aPerg,{"Conta ate ?" ,"C",TamSx3("CT1_CONTA")[1]   ,0,"G",                  ,"CT1"   ,"" })	//mv_par02
	aAdd(_aPerg,{"Data de   ?" ,"D",10     					 ,0,"G",                  ,""      ,"" })	//mv_par03
	aAdd(_aPerg,{"Data ate  ?" ,"D",10     					 ,0,"G",                  ,""      ,"" })	//mv_par04

	// cria o grupo de perguntas
	AjustaSX1(_cPerg,_aPerg)

	Pergunte(_cPerg,.F.)

	oReport := ReportDef()
	oReport:PrintDialog()

Return()


Static Function ReportDef()
	Private oReport

	//Montando o objeto oReport
	oReport := TReport():NEW("CTB34R01", "Impressao de Lancamentos Contabeis", _cPerg, {|oReport|PrintReport(oReport)}, "", , , , , .T. )

	oSecCT2 := TRSection():New( oReport , "Lancamentos Contabeis"  , {"CT2"}  )

	//oSecCT2:SetLineStyle() //Define a impressao da secao em linha
	oSecCT2:SetReadOnly()
	
	TRCell():New(oSecCT2  ,"CT2_DEBITO"  ,"","Conta"        	 ,PesqPict('CT2','CT2_DEBITO')    ,TamSx3("CT2_DEBITO")[1]    ,,,"LEFT"  ,,"LEFT" ,,,,,,.T.)
	TRCell():New(oSecCT2  ,"CT1_DESC01"  ,"","Descricao"         ,PesqPict('CT1','CT1_DESC01')    ,TamSx3("CT1_DESC01")[1]    ,,,"LEFT"  ,,"LEFT" ,,,,,,.T.)
	TRCell():New(oSecCT2  ,"CT2_CLVLDB"  ,"","Cod.Cl.Val."       ,PesqPict('CT2','CT2_CLVLDB')    ,TamSx3("CT2_CLVLDB")[1]    ,,,"LEFT"  ,,"LEFT" ,,,,,,.T.)
	TRCell():New(oSecCT2  ,"CT2_CCD"     ,"","C.Custo"        	 ,PesqPict('CT2','CT2_CCD')       ,TamSx3("CT2_CCD")[1]       ,,,"LEFT"  ,,"LEFT" ,,,,,,.T.)
	TRCell():New(oSecCT2  ,"CT2_CREDIT"  ,"","C.Partida"         ,PesqPict('CT2','CT2_CREDIT')    ,TamSx3("CT2_CREDIT")[1]    ,,,"LEFT"  ,,"LEFT" ,,,,,,.T.)
	TRCell():New(oSecCT2  ,"CT2_ITEMD"   ,"","It.Contabil"       ,PesqPict('CT2','CT2_ITEMD')     ,TamSx3("CT2_ITEMD")[1]     ,,,"LEFT"  ,,"LEFT" ,,,,,,.T.)
	TRCell():New(oSecCT2  ,"CT2_DATA"    ,"","Data"        	     ,PesqPict('CT2','CT2_DATA')      ,10                         ,,,"LEFT"  ,,"LEFT" ,,,,,,.T.)
	TRCell():New(oSecCT2  ,"CT2_LOTE"    ,"","Lote/Sub/Doc/Linha",                                ,18                         ,,,"LEFT"  ,,"LEFT" ,,,,,,.T.)
	TRCell():New(oSecCT2  ,"CT2_HIST"    ,"","Historico"         ,PesqPict('CT2','CT2_HIST')      ,TamSx3("CT2_HIST")[1] * 3  ,,,"LEFT"  ,,"LEFT" ,,,,,,.T.)
	TRCell():New(oSecCT2  ,"CT2_VALORD"  ,"","Debito"        	 ,PesqPict('CT2','CT2_VALOR')     ,TamSx3("CT2_VALOR")[1]     ,,,"RIGHT" ,,"RIGHT",,,,,,.T.)
	TRCell():New(oSecCT2  ,"CT2_VALORC"  ,"","Credito"        	 ,PesqPict('CT2','CT2_VALOR')     ,TamSx3("CT2_VALOR")[1]     ,,,"RIGHT" ,,"RIGHT",,,,,,.T.)

	oReport:SetLandScape(.F.)
	//oReport:SetPortrait(.T.)

Return(oReport)


Static Function PrintReport(oReport)
	Local _cQuery	 := ""
	Local _xy        := 0
	Local oSecCT2 	 := oReport:Section(1)
	Local _aRetSQL

	_cCodDe		:= MV_PAR01
	_cCodAt		:= MV_PAR02
	_dDtIni 	:= MV_PAR03 
	_dDtFim 	:= MV_PAR04 

	_cQuery := " SELECT CT2_DEBITO Conta,CT1_DESC01 Descricao,CT2_CLVLDB Cod_Cl_Val,CT2_CCD C_Custo,CT2_CREDIT C_Partida,CT2_ITEMD Item_Contabil,CT2_DATA Data,CT2_LOTE+CT2_SBLOTE+CT2_DOC+CT2_LINHA Lote_Sub_Doc_Linha " + CRLF
	_cQuery += " ,CT2_HIST Historico " + CRLF
	_cQuery += " ,(SELECT ISNULL(CT2_HIST,'A') FROM CT2010 WHERE CT2_LOTE+CT2_SBLOTE+CT2_DOC+CT2_SEQLAN=CT2.CT2_LOTE+CT2.CT2_SBLOTE+CT2.CT2_DOC+CT2.CT2_SEQLAN AND CT2_DATA=CT2.CT2_DATA AND CT2_SEQHIS='002' AND D_E_L_E_T_='') Hist_Lin1 " + CRLF
	_cQuery += " ,(SELECT ISNULL(CT2_HIST,'A') FROM CT2010 WHERE CT2_LOTE+CT2_SBLOTE+CT2_DOC+CT2_SEQLAN=CT2.CT2_LOTE+CT2.CT2_SBLOTE+CT2.CT2_DOC+CT2.CT2_SEQLAN AND CT2_DATA=CT2.CT2_DATA AND CT2_SEQHIS='003' AND D_E_L_E_T_='') Hist_Lin2 " + CRLF
	_cQuery += " ,CT2_VALOR Debito,0 Credito " + CRLF
	_cQuery += " FROM " + RetSqlName("CT2") + " CT2 " + CRLF
	_cQuery += " LEFT JOIN " + RetSqlName("CT1") + " CT1 " + CRLF 
	_cQuery += " ON CT1_CONTA=CT2_DEBITO " + CRLF
	_cQuery += " AND " + RetSqlCond("CT1") + CRLF
	_cQuery += " WHERE " + RetSqlCond("CT2") + CRLF
	_cQuery += " AND CT2_DEBITO BETWEEN '" + _cCodDe + "' AND '" + _cCodAt + "'" + CRLF
	_cQuery += " AND CT2_DEBITO <> '' " + CRLF
	_cQuery += " AND CT2_DATA BETWEEN '" + DtoS(_dDtIni) + "' AND '" + DtoS(_dDtFim) + "'" + CRLF

	_cQuery += " UNION " + CRLF

	_cQuery += " SELECT CT2_CREDIT Conta,CT1_DESC01 Descricao,CT2_CLVLCR Cod_Cl_Val,CT2_CCC C_Custo,CT2_DEBITO C_Partida,CT2_ITEMC Item_Contabil,CT2_DATA Data,CT2_LOTE+CT2_SBLOTE+CT2_DOC+CT2_LINHA Lote_Sub_Doc_Linha " + CRLF
	_cQuery += " ,CT2_HIST Historico " + CRLF
	_cQuery += " ,(SELECT ISNULL(CT2_HIST,'A') FROM CT2010 WHERE CT2_LOTE+CT2_SBLOTE+CT2_DOC+CT2_SEQLAN=CT2.CT2_LOTE+CT2.CT2_SBLOTE+CT2.CT2_DOC+CT2.CT2_SEQLAN AND CT2_DATA=CT2.CT2_DATA AND CT2_SEQHIS='002' AND D_E_L_E_T_='') Hist_Lin1 " + CRLF
	_cQuery += " ,(SELECT ISNULL(CT2_HIST,'A') FROM CT2010 WHERE CT2_LOTE+CT2_SBLOTE+CT2_DOC+CT2_SEQLAN=CT2.CT2_LOTE+CT2.CT2_SBLOTE+CT2.CT2_DOC+CT2.CT2_SEQLAN AND CT2_DATA=CT2.CT2_DATA AND CT2_SEQHIS='003' AND D_E_L_E_T_='') Hist_Lin2 " + CRLF
	_cQuery += " ,0 Debito,CT2_VALOR Credito " + CRLF
	_cQuery += " FROM " + RetSqlName("CT2") + " CT2 " + CRLF
	_cQuery += " LEFT JOIN " + RetSqlName("CT1") + " CT1 " + CRLF 
	_cQuery += " ON CT1_CONTA=CT2_CREDIT " + CRLF
	_cQuery += " AND " + RetSqlCond("CT1") + CRLF
	_cQuery += " WHERE " + RetSqlCond("CT2") + CRLF
	_cQuery += " AND CT2_CREDIT BETWEEN '" + _cCodDe + "' AND '" + _cCodAt + "'" + CRLF
	_cQuery += " AND CT2_CREDIT <> '' " + CRLF
	_cQuery += " AND CT2_DATA BETWEEN '" + DtoS(_dDtIni) + "' AND '" + DtoS(_dDtFim) + "'" + CRLF
	
	_cQuery += " ORDER BY CT2_DATA,1 " + CRLF
	
	//memowrit("d:\CTB34R01.txt",_cQuery)

	_aRetSQL := CriaDados(_cQuery)

	oSecCT2:SetMeter(Len(_aRetSQL)) //numero de registros a processar

	oSecCT2:Init()

	For _xy:= 1 To Len(_aRetSQL)

		oSecCT2:Cell("CT2_DEBITO") :SetValue(Alltrim(  _aRetSQL[_xy][01]))
		oSecCT2:Cell("CT1_DESC01") :SetValue(Alltrim(  _aRetSQL[_xy][02]))
		oSecCT2:Cell("CT2_CLVLDB") :SetValue(Alltrim(  _aRetSQL[_xy][03]))
		oSecCT2:Cell("CT2_CCD")    :SetValue(Alltrim(  _aRetSQL[_xy][04]))
		oSecCT2:Cell("CT2_CREDIT") :SetValue(Alltrim(  _aRetSQL[_xy][05]))
		oSecCT2:Cell("CT2_ITEMD")  :SetValue(Alltrim(  _aRetSQL[_xy][06]))
		oSecCT2:Cell("CT2_DATA")   :SetValue(   Stod(  _aRetSQL[_xy][07]))
		oSecCT2:Cell("CT2_LOTE")   :SetValue(Alltrim(  _aRetSQL[_xy][08]))
		oSecCT2:Cell("CT2_HIST")   :SetValue( _aRetSQL[_xy][09] + _aRetSQL[_xy][10] + _aRetSQL[_xy][11] )
		oSecCT2:Cell("CT2_VALORD") :SetValue(		   _aRetSQL[_xy][12] )
		oSecCT2:Cell("CT2_VALORC") :SetValue(  		   _aRetSQL[_xy][13] )
		
		oSecCT2:PrintLine()

		oReport:IncMeter()//incrementa regua

	Next _xy

	oSecCT2:Finish()

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

