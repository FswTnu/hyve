#INCLUDE 'PROTHEUS.CH'
#INCLUDE 'RWMAKE.CH'
#include 'TOPCONN.CH'
#include 'parmtype.ch'

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Funcao    ³ FIN06R02 ³ Autor ³ Sigatec.herminio    ³ Data ³ 15/08/2020 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descriao  ³                                                            ³±±
±±           ³                                                            ³±±
±±           ³                                                            ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³ Uso      ³ Generico                                                   ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*/


User Function FIN06R02()

	Local   _aPerg      := {}

	Private _aFiltro := {"1=Data do RPS","2=Data da Baixa","3=Ambas"}
	Private _dDtIni  := dDatabase 
	Private _dDtFim  := dDatabase 
	Private _dBxIni  := dDatabase 
	Private _dBxFim  := dDatabase 
	Private _aOpcoes := {"1=Receb./Parcial","2=Em Aberto","3=Todos"}

	Private _cPerg      := PadR("FIN06R02",Len(SX1->X1_GRUPO))

	// monta a lista de perguntas
	aAdd(_aPerg,{"Data p/ filtro ?" ,"C",1	     	  			,0,"C",_aFiltro          ,""  	   ,"" })	//mv_par01
	aAdd(_aPerg,{"Data RPS de    ?" ,"D",10     				,0,"G",                  ,""       ,"" })	//mv_par02
	aAdd(_aPerg,{"Data RPS ate   ?" ,"D",10     				,0,"G",                  ,""       ,"" })	//mv_par03
	aAdd(_aPerg,{"Data Bx de     ?" ,"D",10     				,0,"G",                  ,""       ,"" })	//mv_par04
	aAdd(_aPerg,{"Data Bx ate    ?" ,"D",10     				,0,"G",                  ,""       ,"" })	//mv_par05
	aAdd(_aPerg,{"Filtrar Receb. ?" ,"C",1	     	  			,0,"C",_aOpcoes          ,""  	   ,"" })	//mv_par06

	// cria o grupo de perguntas
	AjustaSX1(_cPerg,_aPerg)

	Pergunte(_cPerg,.F.)

	oReport := ReportDef()
	oReport:PrintDialog()

Return()


Static Function ReportDef()
	Private oReport

	//Montando o objeto oReport
	oReport := TReport():NEW("FIN06R02", "Contas a Receber - RPS", _cPerg, {|oReport|PrintReport(oReport)}, "", , , , , .T. )

	oSecSF2 := TRSection():New( oReport , "Contas a Receber x RPS"  , {"SF2"}  )

	oSecSF2:SetReadOnly()
	
	TRCell():New(oSecSF2  ,"F2_EMISSAO" ,"","Emissao RPS"   ,PesqPict('SF2','F2_EMISSAO') 	,10    					,,,"LEFT"  	,,"LEFT" ,,,,,,.F.)
	TRCell():New(oSecSF2  ,"F2_DOC"     ,"","RPS"           ,PesqPict('SF2','F2_DOC') 	    ,TamSx3("F2_DOC")[1]    ,,,"LEFT"  	,,"LEFT" ,,,,,,.F.)
	TRCell():New(oSecSF2  ,"F2_CLIENTE" ,"","C. Cliente"    ,PesqPict('SF2','F2_CLIENTE')   ,TamSx3("F2_CLIENTE")[1],,,"LEFT"  	,,"LEFT" ,,,,,,.F.)
	TRCell():New(oSecSF2  ,"A1_NOME"  	,"","Razao Social"  ,PesqPict('SA1','A1_NOME')    	,TamSx3("A1_NOME")[1]   ,,,"LEFT"  	,,"LEFT" ,,,,,,.F.)
	TRCell():New(oSecSF2  ,"A1_NREDUZ"  ,"","Nome Fantasia" ,PesqPict('SA1','A1_NREDUZ')    ,TamSx3("A1_NREDUZ")[1] ,,,"LEFT"  	,,"LEFT" ,,,,,,.F.)
	TRCell():New(oSecSF2  ,"A1_CGC"     ,"","CNPJ"       	,PesqPict('SA1','A1_CGC')       ,18					    ,,,"LEFT"   ,,"LEFT" ,,,,,,.F.)
	TRCell():New(oSecSF2  ,"D2_PEDIDO"  ,"","Pedido"        ,PesqPict('SD2','D2_PEDIDO')    ,TamSx3("D2_PEDIDO")[1] ,,,"LEFT" 	,,"LEFT" ,,,,,,.F.)
	TRCell():New(oSecSF2  ,"E1_NUMBOR"  ,"","Bordero"       ,PesqPict('SE1','E1_NUMBOR')    ,TamSx3("E1_NUMBOR")[1] ,,,"LEFT" 	,,"LEFT" ,,,,,,.F.)
	TRCell():New(oSecSF2  ,"E1_NATUREZ" ,"","Natureza"      ,PesqPict('SE1','E1_NATUREZ')   ,TamSx3("E1_NATUREZ")[1] ,,,"LEFT" 	,,"LEFT" ,,,,,,.F.)
	TRCell():New(oSecSF2  ,"D2_COD"     ,"","Produto"       ,PesqPict('SD2','D2_COD')       ,TamSx3("D2_COD")[1]    ,,,"LEFT" 	,,"LEFT" ,,,,,,.F.)
	TRCell():New(oSecSF2  ,"B1_DESC"    ,"","N. Conta"      ,PesqPict('SB1','B1_DESC')      ,TamSx3("B1_DESC")[1]   ,,,"LEFT" 	,,"LEFT" ,,,,,,.F.)
	TRCell():New(oSecSF2  ,"D2_ITEMCC"  ,"","Item Conta"    ,PesqPict('SD2','D2_ITEMCC')    ,TamSx3("D2_ITEMCC")[1] ,,,"LEFT" 	,,"LEFT" ,,,,,,.F.)
	TRCell():New(oSecSF2  ,"D2_CCUSTO"  ,"","Centro Custo"  ,PesqPict('SD2','D2_CCUSTO')    ,TamSx3("D2_CCUSTO")[1] ,,,"LEFT" 	,,"LEFT" ,,,,,,.F.)
	TRCell():New(oSecSF2  ,"D2_CLVL"    ,"","Cl. Valor"     ,PesqPict('SD2','D2_CLVL')      ,TamSx3("D2_CLVL")[1]   ,,,"LEFT" 	,,"LEFT" ,,,,,,.F.)
	TRCell():New(oSecSF2  ,"E5_MOTBX"   ,"","Mot. BX."      ,PesqPict('SE5','E5_MOTBX')     ,TamSx3("E5_MOTBX")[1]  ,,,"LEFT" 	,,"LEFT" ,,,,,,.F.)
	TRCell():New(oSecSF2  ,"F2_VALBRUT" ,"","Valor"         ,PesqPict('SF2','F2_VALBRUT')   ,TamSx3("F2_VALBRUT")[1],,,"RIGHT" 	,,"LEFT" ,,,,,,.F.)
	TRCell():New(oSecSF2  ,"E1_PARCELA" ,"","Parc."         ,PesqPict('SE1','E1_PARCELA')   ,TamSx3("E1_PARCELA")[1],,,"LEFT" 	,,"LEFT" ,,,,,,.F.)
	TRCell():New(oSecSF2  ,"E1_VENCTO"  ,"","Vencimento"    ,PesqPict('SE1','E1_VENCTO')    ,TamSx3("E1_VENCTO")[1] ,,,"LEFT" 	,,"LEFT" ,,,,,,.F.)
	TRCell():New(oSecSF2  ,"E1_BAIXA"  	,"","Data Baixa"    ,PesqPict('SE1','E1_BAIXA')     ,TamSx3("E1_BAIXA")[1]  ,,,"LEFT" 	,,"LEFT" ,,,,,,.F.)
	TRCell():New(oSecSF2  ,"E1_VALLIQ"  ,"","Valor Baixa"   ,PesqPict('SE1','E1_VALLIQ')    ,TamSx3("E1_VALLIQ")[1] ,,,"RIGHT" 	,,"LEFT" ,,,,,,.F.)
	TRCell():New(oSecSF2  ,"ADTO"  	    ,"","Valor Adto"    ,PesqPict('SE1','E1_VALLIQ')    ,TamSx3("E1_VALLIQ")[1] ,,,"RIGHT" 	,,"LEFT" ,,,,,,.F.)
	TRCell():New(oSecSF2  ,"E1_JUROS"  	,"","Juros"         ,PesqPict('SE1','E1_JUROS')     ,TamSx3("E1_JUROS")[1]  ,,,"RIGHT" 	,,"LEFT" ,,,,,,.F.)
	TRCell():New(oSecSF2  ,"E1_DESCONT" ,"","Desconto"      ,PesqPict('SE1','E1_DESCONT')   ,TamSx3("E1_DESCONT")[1],,,"RIGHT" 	,,"LEFT" ,,,,,,.F.)
	//TRCell():New(oSecSF2  ,"E1_IRRF"  	,"","IRPF"          ,PesqPict('SE1','E1_IRRF')      ,TamSx3("E1_IRRF")[1]   ,,,"RIGHT" 	,,"LEFT" ,,,,,,.F.)
	//TRCell():New(oSecSF2  ,"E1_PIS"  	,"","PIS"           ,PesqPict('SE1','E1_PIS')       ,TamSx3("E1_PIS")[1]    ,,,"RIGHT" 	,,"LEFT" ,,,,,,.F.)
	//TRCell():New(oSecSF2  ,"E1_COFINS"  ,"","COFINS"        ,PesqPict('SE1','E1_COFINS')    ,TamSx3("E1_COFINS")[1] ,,,"RIGHT" 	,,"LEFT" ,,,,,,.F.)
	//TRCell():New(oSecSF2  ,"E1_CSLL"  	,"","CSLL"          ,PesqPict('SE1','E1_CSLL')      ,TamSx3("E1_CSLL")[1]   ,,,"RIGHT" 	,,"LEFT" ,,,,,,.F.)
	TRCell():New(oSecSF2  ,"E1_SALDO"  	,"","Saldo"         ,PesqPict('SE1','E1_SALDO')     ,TamSx3("E1_SALDO")[1]  ,,,"RIGHT" 	,,"LEFT" ,,,,,,.F.)
	//TRCell():New(oSecSF2  ,"E5_HISTOR"  ,"","Observacoes"   ,PesqPict('SE5','E5_HISTOR')    ,TamSx3("E5_HISTOR")[1] ,,,"LEFT" 	,,"LEFT" ,,,,,,.F.)

	TRFunction():New(oSecSF2:Cell("F2_VALBRUT") ,"F2_VALBRUT","SUM",,,,,.F.,.T.)
	TRFunction():New(oSecSF2:Cell("E1_VALLIQ") ,"E1_VALLIQ","SUM",,,,,.F.,.T.)
	TRFunction():New(oSecSF2:Cell("ADTO") ,"ADTO","SUM",,,,,.F.,.T.)
	TRFunction():New(oSecSF2:Cell("E1_SALDO") ,"E1_SALDO","SUM",,,,,.F.,.T.)

	//Define a impressão dos totalizadores em colunas
	oReport:SetTotalInLine(.F.)

	oReport:SetLandScape(.F.)

Return(oReport)


Static Function PrintReport(oReport)
	Local _cQuery	 := ""
	Local _xy        := 0
	Local oSecSF2 	 := oReport:Section(1)
	Local _aRetSQL

	_cFiltr		:= MV_PAR01
	_dDtIni 	:= MV_PAR02
	_dDtFim 	:= MV_PAR03 
	_dBxIni 	:= MV_PAR04
	_dBxFim 	:= MV_PAR05 
	_cOpcao		:= MV_PAR06

	_cQuery := " SELECT F2_EMISSAO,F2_DOC,A1_NOME,A1_NREDUZ,F2_CLIENTE,A1_CGC " + CRLF

	_cQuery += " ,ISNULL(SD2A.D2_COD,'X') AS PROD_4  " + CRLF
	_cQuery += " ,ISNULL(SB1A.B1_DESC,'X') AS COD_4 " + CRLF
	_cQuery += " ,ISNULL(SD2A.D2_PEDIDO,'X') AS PED_4 " + CRLF
	_cQuery += " ,ISNULL(SD2A.D2_ITEMCC,'X') AS ITCC_4 " + CRLF
	_cQuery += " ,ISNULL(SD2A.D2_CCUSTO,'X') AS CC_4 " + CRLF
	_cQuery += " ,ISNULL(SD2A.D2_CLVL,'X') AS CLVL_4 " + CRLF

	_cQuery += " ,ISNULL(SD2B.D2_COD,'X') AS PROD_5  " + CRLF
	_cQuery += " ,ISNULL(SB1B.B1_DESC,'X') AS COD_5 " + CRLF
	_cQuery += " ,ISNULL(SD2B.D2_PEDIDO,'X') AS PED_5 " + CRLF
	_cQuery += " ,ISNULL(SD2B.D2_ITEMCC,'X') AS ITCC_5 " + CRLF
	_cQuery += " ,ISNULL(SD2B.D2_CCUSTO,'X') AS CC_5 " + CRLF
	_cQuery += " ,ISNULL(SD2B.D2_CLVL,'X') AS CLVL_5 " + CRLF

	_cQuery += " ,ISNULL(SD2C.D2_COD,'X') AS PROD_12  " + CRLF
	_cQuery += " ,ISNULL(SB1C.B1_DESC,'X') AS COD_12 " + CRLF
	_cQuery += " ,ISNULL(SD2C.D2_PEDIDO,'X') AS PED_12 " + CRLF
	_cQuery += " ,ISNULL(SD2C.D2_ITEMCC,'X') AS ITCC_12 " + CRLF
	_cQuery += " ,ISNULL(SD2C.D2_CCUSTO,'X') AS CC_12 " + CRLF
	_cQuery += " ,ISNULL(SD2C.D2_CLVL,'X') AS CLVL_12 " + CRLF

	_cQuery += " ,ISNULL((SELECT TOP 1 D2_COD FROM " + RetSqlName("SD2") + " SD2  " + CRLF
	_cQuery += " WHERE D2_FILIAL=F2_FILIAL AND D2_DOC=F2_DOC AND D2_SERIE=F2_SERIE AND D2_COD NOT IN ('000000004','000000005','000000012') AND " + RetSqlCond("SD2") + "),'X') AS PROD " + CRLF

	_cQuery += " ,ISNULL((SELECT TOP 1 B1_DESC FROM " + RetSqlName("SD2") + " SD2  " + CRLF
	_cQuery += " LEFT JOIN " + RetSqlName("SB1") + " SB1 ON B1_COD=D2_COD AND " + RetSqlCond("SB1") + " " + CRLF
	_cQuery += " WHERE D2_FILIAL=F2_FILIAL AND D2_DOC=F2_DOC AND D2_SERIE=F2_SERIE AND D2_COD NOT IN ('000000004','000000005','000000012') AND " + RetSqlCond("SD2") + "),'X') AS COD " + CRLF

	_cQuery += " ,ISNULL((SELECT TOP 1 D2_PEDIDO FROM " + RetSqlName("SD2") + " SD2  " + CRLF
	_cQuery += " WHERE D2_FILIAL=F2_FILIAL AND D2_DOC=F2_DOC AND D2_SERIE=F2_SERIE AND D2_COD NOT IN ('000000004','000000005','000000012') AND " + RetSqlCond("SD2") + "),'X') AS PED " + CRLF

	_cQuery += " ,ISNULL((SELECT TOP 1 D2_ITEMCC FROM " + RetSqlName("SD2") + " SD2  " + CRLF
	_cQuery += " WHERE D2_FILIAL=F2_FILIAL AND D2_DOC=F2_DOC AND D2_SERIE=F2_SERIE AND D2_COD NOT IN ('000000004','000000005','000000012') AND " + RetSqlCond("SD2") + "),'X') AS ITCC " + CRLF

	_cQuery += " ,ISNULL((SELECT TOP 1 D2_CCUSTO FROM " + RetSqlName("SD2") + " SD2  " + CRLF
	_cQuery += " WHERE D2_FILIAL=F2_FILIAL AND D2_DOC=F2_DOC AND D2_SERIE=F2_SERIE AND D2_COD NOT IN ('000000004','000000005','000000012') AND " + RetSqlCond("SD2") + "),'X') AS CC " + CRLF

	_cQuery += " ,ISNULL((SELECT TOP 1 D2_CLVL FROM " + RetSqlName("SD2") + " SD2  " + CRLF
	_cQuery += " WHERE D2_FILIAL=F2_FILIAL AND D2_DOC=F2_DOC AND D2_SERIE=F2_SERIE AND D2_COD NOT IN ('000000004','000000005','000000012') AND " + RetSqlCond("SD2") + "),'X') AS CLVL " + CRLF

	_cQuery += " ,E5_MOTBX " + CRLF

	_cQuery += " ,F2_VALBRUT,E1_VENCTO,E1_BAIXA,E1_VALLIQ,E1_JUROS,E1_DESCONT,E1_IRRF,E1_PIS,E1_COFINS,E1_CSLL,E1_SALDO,E1_NUMBOR,E1_NATUREZ " + CRLF

	_cQuery += " ,E5_HISTOR,E5_DATA,E5_VALOR,E5_VLJUROS,E5_VLMULTA,E5_VLDESCO,E1_PARCELA,E1_VALOR " + CRLF

	_cQuery += " FROM " + RetSqlName("SF2") + " SF2 " + CRLF

	_cQuery += " LEFT JOIN " + RetSqlName("SA1") + " SA1 " + CRLF 
	_cQuery += " ON A1_COD=F2_CLIENTE AND A1_LOJA=F2_LOJA " + CRLF
	_cQuery += " AND " + RetSqlCond("SA1") + CRLF

	_cQuery += " LEFT JOIN " + RetSqlName("SE1") + " SE1 " + CRLF 
	_cQuery += " ON E1_FILIAL=F2_FILIAL AND E1_NUM=F2_DOC AND E1_PREFIXO=F2_SERIE AND E1_CLIENTE=F2_CLIENTE AND E1_LOJA=F2_LOJA AND E1_TIPO='NF' " + CRLF
	_cQuery += " AND " + RetSqlCond("SE1") + CRLF

	_cQuery += " LEFT JOIN " + RetSqlName("SE5") + " SE5 " + CRLF 
	_cQuery += " ON E5_FILIAL=F2_FILIAL AND E5_NUMERO=F2_DOC AND E5_PREFIXO=F2_SERIE AND E5_CLIFOR=F2_CLIENTE AND E5_LOJA=F2_LOJA AND E5_PARCELA=E1_PARCELA " + CRLF
	_cQuery += " AND " + RetSqlCond("SE5") + CRLF
   	// Controla o tipo de movimento bancario para sair no relatorio
	_cQuery += " AND E5_TIPODOC NOT IN ('DC','JR','MT') " + CRLF
   	// Filtrar os movimentos bancarios que foram cancelados
	_cQuery += " AND E5_DTCANBX='' " + CRLF

	_cQuery += " LEFT JOIN " + RetSqlName("SD2") + " SD2A  " + CRLF
	_cQuery += " ON SD2A.D2_FILIAL=F2_FILIAL AND SD2A.D2_DOC=F2_DOC AND SD2A.D2_SERIE=F2_SERIE AND SD2A.D2_COD='000000004' AND SD2A.D_E_L_E_T_='' " + CRLF
	_cQuery += " LEFT JOIN " + RetSqlName("SB1") + " SB1A ON SB1A.B1_COD=SD2A.D2_COD AND SB1A.D_E_L_E_T_='' " + CRLF

	_cQuery += " LEFT JOIN " + RetSqlName("SD2") + " SD2B  " + CRLF
	_cQuery += " ON SD2B.D2_FILIAL=F2_FILIAL AND SD2B.D2_DOC=F2_DOC AND SD2B.D2_SERIE=F2_SERIE AND SD2B.D2_COD='000000005' AND SD2B.D_E_L_E_T_='' " + CRLF
	_cQuery += " LEFT JOIN " + RetSqlName("SB1") + " SB1B ON SB1B.B1_COD=SD2B.D2_COD AND SB1B.D_E_L_E_T_='' " + CRLF

	_cQuery += " LEFT JOIN " + RetSqlName("SD2") + " SD2C  " + CRLF
	_cQuery += " ON SD2C.D2_FILIAL=F2_FILIAL AND SD2C.D2_DOC=F2_DOC AND SD2C.D2_SERIE=F2_SERIE AND SD2C.D2_COD='000000012' AND SD2C.D_E_L_E_T_='' " + CRLF
	_cQuery += " LEFT JOIN " + RetSqlName("SB1") + " SB1C ON SB1C.B1_COD=SD2C.D2_COD AND SB1C.D_E_L_E_T_='' " + CRLF

	_cQuery += " WHERE " + RetSqlCond("SF2") + CRLF
	_cQuery += " AND F2_ESPECIE = 'RPS' " + CRLF
	//_cQuery += " AND F2_EMISSAO>'2019' " + CRLF
	//_cQuery += " AND F2_SERIE='1' " + CRLF

	// filtros
	//_cQuery += " AND F2_EMISSAO BETWEEN '" + DtoS(_dDtIni) + "' AND '" + DtoS(_dDtFim) + "'" + CRLF
	//_cQuery += " AND ( E1_BAIXA = '' OR E1_BAIXA BETWEEN '" + DtoS(_dBxIni) + "' AND '" + DtoS(_dBxFim) + "' )" + CRLF

	If _cFiltr == 1 .or. _cFiltr == 3 //{"1=Data do RPS","2=Data da Baixa","3=Ambas"}
		_cQuery += " AND F2_EMISSAO BETWEEN '" + DtoS(_dDtIni) + "' AND '" + DtoS(_dDtFim) + "'" + CRLF
	EndIf

	IF 	_cOpcao == 1  //{"1=Receb/Parcial","2=Em Aberto","3=Todos"}
	
		If _cFiltr == 1  //{"1=Data do PC","2=Data da Baixa","3=Ambas"}
			_cQuery += " AND E1_BAIXA <> ''" + CRLF
		Else
			//_cQuery += " AND E1_BAIXA BETWEEN '" + DtoS(_dBxIni) + "' AND '" + DtoS(_dBxFim) + "'" + CRLF
			_cQuery += " AND E5_DATA BETWEEN '" + DtoS(_dBxIni) + "' AND '" + DtoS(_dBxFim) + "'" + CRLF
		EndIf
		
	ELSEIF _cOpcao == 2 //{"1=Receb./Parcial","2=Em Aberto","3=Todos"}
	
		_cQuery += " AND E1_BAIXA = ''" + CRLF
		
	ELSE

		If _cFiltr <> 1  //{"1=Data do RPS","2=Data da Baixa","3=Ambas"}
			//_cQuery += " AND ( E1_BAIXA = '' OR E1_BAIXA BETWEEN '" + DtoS(_dBxIni) + "' AND '" + DtoS(_dBxFim) + "' )" + CRLF
			_cQuery += " AND ( E1_BAIXA = '' OR E5_DATA BETWEEN '" + DtoS(_dBxIni) + "' AND '" + DtoS(_dBxFim) + "' )" + CRLF
		EndIf

	ENDIF

   	_cQuery += " ORDER BY 1,2,50" + CRLF

	memowrit("d:\FIN06R02.txt",_cQuery)

	_aRetSQL := CriaDados(_cQuery)

	oSecSF2:SetMeter(Len(_aRetSQL)) //numero de registros a processar

	oSecSF2:Init()

	For _xy:= 1 To Len(_aRetSQL)

		oSecSF2:Cell("F2_EMISSAO") 	:SetValue(      Stod(  _aRetSQL[_xy][01]))
		oSecSF2:Cell("F2_DOC") 	    :SetValue(   Alltrim(  _aRetSQL[_xy][02]))
		oSecSF2:Cell("A1_NOME") 	:SetValue(   Alltrim(  _aRetSQL[_xy][03]))
		oSecSF2:Cell("A1_NREDUZ") 	:SetValue(   Alltrim(  _aRetSQL[_xy][04]))
		oSecSF2:Cell("F2_CLIENTE") 	:SetValue(   Alltrim(  _aRetSQL[_xy][05]))
		oSecSF2:Cell("A1_CGC") 	    :SetValue(   Alltrim(  _aRetSQL[_xy][06]))
		
		If Alltrim(_aRetSQL[_xy][07]) <> "X"
			oSecSF2:Cell("D2_COD") 		:SetValue(   Alltrim(  _aRetSQL[_xy][07]))
			oSecSF2:Cell("B1_DESC") 	:SetValue(   Alltrim(  _aRetSQL[_xy][08]))
			oSecSF2:Cell("D2_PEDIDO") 	:SetValue(   Alltrim(  _aRetSQL[_xy][09]))
			oSecSF2:Cell("D2_ITEMCC") 	:SetValue(   Alltrim(  _aRetSQL[_xy][10]))
			oSecSF2:Cell("D2_CCUSTO") 	:SetValue(   Alltrim(  _aRetSQL[_xy][11]))
			oSecSF2:Cell("D2_CLVL") 	:SetValue(   Alltrim(  _aRetSQL[_xy][12]))
		ElseIf Alltrim(_aRetSQL[_xy][13]) <> "X"
			oSecSF2:Cell("D2_COD") 		:SetValue(   Alltrim(  _aRetSQL[_xy][13]))
			oSecSF2:Cell("B1_DESC") 	:SetValue(   Alltrim(  _aRetSQL[_xy][14]))
			oSecSF2:Cell("D2_PEDIDO") 	:SetValue(   Alltrim(  _aRetSQL[_xy][15]))
			oSecSF2:Cell("D2_ITEMCC") 	:SetValue(   Alltrim(  _aRetSQL[_xy][16]))
			oSecSF2:Cell("D2_CCUSTO") 	:SetValue(   Alltrim(  _aRetSQL[_xy][17]))
			oSecSF2:Cell("D2_CLVL") 	:SetValue(   Alltrim(  _aRetSQL[_xy][18]))
		ElseIf Alltrim(_aRetSQL[_xy][19]) <> "X"
			oSecSF2:Cell("D2_COD") 		:SetValue(   Alltrim(  _aRetSQL[_xy][19]))
			oSecSF2:Cell("B1_DESC") 	:SetValue(   Alltrim(  _aRetSQL[_xy][20]))
			oSecSF2:Cell("D2_PEDIDO") 	:SetValue(   Alltrim(  _aRetSQL[_xy][21]))
			oSecSF2:Cell("D2_ITEMCC") 	:SetValue(   Alltrim(  _aRetSQL[_xy][22]))
			oSecSF2:Cell("D2_CCUSTO") 	:SetValue(   Alltrim(  _aRetSQL[_xy][23]))
			oSecSF2:Cell("D2_CLVL") 	:SetValue(   Alltrim(  _aRetSQL[_xy][24]))
		Else
			oSecSF2:Cell("D2_COD") 		:SetValue(   Alltrim(  _aRetSQL[_xy][25]))
			oSecSF2:Cell("B1_DESC") 	:SetValue(   Alltrim(  _aRetSQL[_xy][26]))
			oSecSF2:Cell("D2_PEDIDO") 	:SetValue(   Alltrim(  _aRetSQL[_xy][27]))
			oSecSF2:Cell("D2_ITEMCC") 	:SetValue(   Alltrim(  _aRetSQL[_xy][28]))
			oSecSF2:Cell("D2_CCUSTO") 	:SetValue(   Alltrim(  _aRetSQL[_xy][29]))
			oSecSF2:Cell("D2_CLVL") 	:SetValue(   Alltrim(  _aRetSQL[_xy][30]))
		EndIf

		oSecSF2:Cell("E5_MOTBX") 	:SetValue(             _aRetSQL[_xy][31] )
		oSecSF2:Cell("F2_VALBRUT") 	:SetValue(             _aRetSQL[_xy][52] ) 	// _aRetSQL[_xy][32]
		oSecSF2:Cell("E1_PARCELA") 	:SetValue(   Alltrim(  _aRetSQL[_xy][51]))
		oSecSF2:Cell("E1_VENCTO") 	:SetValue(      Stod(  _aRetSQL[_xy][33]))
		oSecSF2:Cell("E1_BAIXA") 	:SetValue(      Stod(  _aRetSQL[_xy][46]))	// _aRetSQL[_xy][34]
		
		If _aRetSQL[_xy][31]=='CMP'
			oSecSF2:Cell("E1_VALLIQ") 	:SetValue(             0 )	
			oSecSF2:Cell("ADTO") 		:SetValue(             _aRetSQL[_xy][47] )	// _aRetSQL[_xy][35]
		Else
			oSecSF2:Cell("E1_VALLIQ") 	:SetValue(             _aRetSQL[_xy][47] ) 	// _aRetSQL[_xy][35]
			oSecSF2:Cell("ADTO") 		:SetValue(             0 )	
		EndIf

		oSecSF2:Cell("E1_JUROS") 	:SetValue(             _aRetSQL[_xy][48] + _aRetSQL[_xy][49] )	  // _aRetSQL[_xy][36]
		oSecSF2:Cell("E1_DESCONT") 	:SetValue(             _aRetSQL[_xy][50] ) 	// _aRetSQL[_xy][37]
		//oSecSF2:Cell("E1_IRRF") 	:SetValue(             _aRetSQL[_xy][38] )
		//oSecSF2:Cell("E1_PIS") 	    :SetValue(             _aRetSQL[_xy][39] )
		//oSecSF2:Cell("E1_COFINS") 	:SetValue(             _aRetSQL[_xy][40] )
		//oSecSF2:Cell("E1_CSLL") 	:SetValue(             _aRetSQL[_xy][41] )
		oSecSF2:Cell("E1_SALDO") 	:SetValue(             _aRetSQL[_xy][42] )
		oSecSF2:Cell("E1_NUMBOR") 	:SetValue(   Alltrim(  _aRetSQL[_xy][43]))
		oSecSF2:Cell("E1_NATUREZ") 	:SetValue(   Alltrim(  _aRetSQL[_xy][44]))
		//oSecSF2:Cell("E5_HISTOR") 	:SetValue(   Alltrim(  _aRetSQL[_xy][45]))

		oSecSF2:PrintLine()

		oReport:IncMeter() //incrementa regua

	Next _xy

	oSecSF2:Finish()

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

