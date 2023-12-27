#INCLUDE "PROTHEUS.CH"
#INCLUDE "TOPCONN.CH"

/*ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
ฑฑบPrograma  ณ LISTSE1  บAutor  ณEduardo Augusto     บ Data ณ  26/09/2014 บฑฑ
ฑฑบPrograma  ณ Alterado บAutor  ณRafael Domingues    บ Data ณ  29/09/2014 บฑฑ
ฑฑฬออออออออออุออออออออออสอออออออฯออออออออออออออออออออสออออออฯอออออออออออออนฑฑ
ฑฑบDesc.     ณ Fonte para Tratamento de Pagamento de Veiculos conforme    บฑฑ
ฑฑบ          ณ Layout PAGFROTAS Bradesco para Gerar um Arquivo .CSV e     บฑฑ
ฑฑบ          ณ Importa-lo no Aplicativo do Banco.                         บฑฑ
ฑฑฬออออออออออุออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบUso       ณ I2I Eventos						                          บฑฑ
฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿*/

User Function TELABOL()
Private cTitulo  := "SELEวรO DE BOLETOS"
Private oOk := LoadBitmap(GetResources(),"LBOK")
Private oNo := LoadBitmap(GetResources(),"LBNO")
Private cVar
Private oDlg
Private oChk
Private oLbx
Private lChk := .F.
Private lMark := .F.
Private aVetor := {}
Private cPerg := "TELABOL"
Private cSerie := ""	// TNU - Release 25
// ValidPerg()			// TNU - Release 25
If !Pergunte(cPerg,.T.)	// SELECIONE O BANCO
	Return
EndIf
If SM0->M0_CODIGO == "01"
//	Private cSerie 		:= Iif(Mv_Par06 == 1,"1","ND")	// TNU - Release 25
	cSerie 		:= Iif(Mv_Par06 == 1,"1","ND")			// TNU - Release 25
ElseIf SM0->M0_CODIGO == "02"
//	Private cSerie 		:= Iif(Mv_Par06 == 1,"1","ND")	// TNU - Release 25
	cSerie 		:= Iif(Mv_Par06 == 1,"1","ND")			// TNU - Release 25
EndIf
Private _cBanco		:= Mv_Par01
Private _cAgencia	:= Mv_Par02
Private _cConta		:= Mv_Par03
Private _cSubcta	:= Mv_Par04
Private _Tipo		:= Mv_Par05
Private _Relat		:= Mv_Par06
Private _EmisIni	:= Mv_Par07
Private _EmisFim	:= Mv_Par08
Private cQuery := ""
If Select("TMP") > 0
	TMP->(DbCloseArea())
EndIf
cQuery := " SELECT E1_PREFIXO, E1_NUM, E1_PARCELA, E1_CLIENTE, E1_LOJA, E1_NOMCLI, E1_EMISSAO, E1_VALOR, E1_VENCTO, E1_VENCREA, E1_TIPO, E1_PORTADO, E1_NUMBOR, E1_NUMBCO, E1_XNUMBCO FROM "
cQuery += RetSqlName("SE1")
cQuery += " WHERE D_E_L_E_T_ = '' "
If Mv_Par05 == 1
	cQuery += " AND E1_SALDO <> 0 "
	cQuery += " AND E1_NUMBCO = '' "
	cQuery += " AND E1_XNUMBCO = '' "
	cQuery += " AND E1_TIPO IN ('NF','BOL','FT','DP','ND') "
	cQuery += " AND E1_PREFIXO = '"+cSerie+"' "
	cQuery += " AND E1_EMISSAO BETWEEN  '"+DtoS(_EmisIni)+"' AND '"+DtoS(_EmisFim)+"' "
ElseIf Mv_Par05 == 2
	cQuery += " AND E1_SALDO <> 0 "
	cQuery += " AND E1_XNUMBCO <> '' "
	cQuery += " AND E1_TIPO IN ('NF','BOL','FT','DP','ND') "
	cQuery += " AND E1_PREFIXO = '"+cSerie+"' "
	cQuery += " AND E1_EMISSAO BETWEEN  '"+DtoS(_EmisIni)+"' AND '"+DtoS(_EmisFim)+"' "
EndIf
cQuery := ChangeQuery(cQuery)
DbUseArea(.T., "TOPCONN", TCGenQry(,,cQuery), 'TMP', .F., .T.)
TcSetField("TMP","E1_EMISSAO","D")
TcSetField("TMP","E1_VENCTO" ,"D")
TcSetField("TMP","E1_VENCREA","D")
TcSetField("TMP","E1_VALOR"  ,"N",12,2)
DbSelectArea("TMP")
DbGoTop()
While !Eof()
aAdd(aVetor, { lMark, TMP->E1_PREFIXO, TMP->E1_NUM, TMP->E1_PARCELA, TMP->E1_CLIENTE, TMP->E1_LOJA, TMP->E1_NOMCLI, TMP->E1_EMISSAO, AllTrim(Transform(TMP->E1_VALOR,"@E 999,999,999.99")), TMP->E1_VENCTO, TMP->E1_VENCREA, TMP->E1_TIPO, TMP->E1_PORTADO, TMP->E1_NUMBOR, TMP->E1_NUMBCO, TMP->E1_XNUMBCO })
DbSelectArea("TMP")
DbSkip()
End
DbSelectArea("TMP")
DbCloseArea()
If Len(aVetor) == 0
	MsgAlert("Nใo foi Selecionado nenhum Titulo para Impressใo de Boleto",cTitulo)
	Return
EndIf
DEFINE MSDIALOG oDlg TITLE cTitulo FROM 0,0 To 511,1292 PIXEL
@010,010 LISTBOX oLbx VAR cVar FIELDS Header " ", "Prefixo", "Nฐ Titulo", "Parcela", "Cod. Cliente", "Loja", "Nome Cliente", "Data Emissใo", "Valor R$", "Vencimento", "Vencimento Real", "Tipo", "Portador", "Bordero", "Nosso Nฐ Sistema", "Nosso Nฐ Backup" SIZE 630,230 Of oDlg PIXEL ON dblClick(aVetor[oLbx:nAt,1] := !aVetor[oLbx:nAt,1],oLbx:Refresh())
oLbx:SetArray(aVetor)
oLbx:bLine := {|| { Iif(aVetor[oLbx:nAt,1],oOk,oNo), aVetor[oLbx:nAt,2], aVetor[oLbx:nAt,3], aVetor[oLbx:nAt,4], aVetor[oLbx:nAt,5], aVetor[oLbx:nAt,6], aVetor[oLbx:nAt,7], aVetor[oLbx:nAt,8], aVetor[oLbx:nAt,9], aVetor[oLbx:nAt,10], aVetor[oLbx:nAt,11], aVetor[oLbx:nAt,12], aVetor[oLbx:nAt,13], aVetor[oLbx:nAt,14], aVetor[oLbx:nAt,15], aVetor[oLbx:nAt,16] }}
If oChk <> Nil
	@245,010 CHECKBOX oChk VAR lChk Prompt "Marca/Desmarca" Size 60,007 PIXEL Of oDlg On Click(Iif(lChk,Marca(lChk),Marca(lChk)))
EndIf
@245,010 CHECKBOX oChk VAR lChk Prompt "Marca/Desmarca" SIZE 60,007 PIXEL Of oDlg On Click(aEval(aVetor,{|x| x[1] := lChk}),oLbx:Refresh())
@243,130 BUTTON "Cancelar Boletos Total" SIZE 100, 011 Font oDlg:oFont ACTION CanceTot(aVetor) OF oDlg PIXEL
@243,305 BUTTON "Cancelar Boletos p/ 2ฐ Via" SIZE 100, 011 Font oDlg:oFont ACTION CanceBol() OF oDlg PIXEL
@243,480 BUTTON "Confirmar" SIZE 050, 011 Font oDlg:oFont ACTION u_BOLETOS(aVetor) Of oDlg PIXEL
@243,535 BUTTON "Consulta"  SIZE 050, 011 Font oDlg:oFont ACTION VisuSE1() OF oDlg PIXEL
@243,590 BUTTON "Cancela"   SIZE 050, 011 Font oDlg:oFont ACTION oDlg:End() OF oDlg PIXEL
ACTIVATE MSDIALOG oDlg CENTER
Return

/*ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
ฑฑบPrograma  ณ VisuSE1  บAutor  ณEduardo Augusto     บ Data ณ  22/10/2013 บฑฑ
ฑฑฬออออออออออุออออออออออสอออออออฯออออออออออออออออออออสออออออฯอออออออออออออนฑฑ
ฑฑบDesc.     ณ Funcao para Chamada do mBrowse da Tela de Inlcusao do      บฑฑ
ฑฑบ          ณ Contas a Receber (Somente Consulta)             			  บฑฑ
ฑฑฬออออออออออุออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบUso       ณ I2I Eventos							                      บฑฑ
฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿*/

Static Function VisuSE1()
Private cCadastro := "Tela do Contas a Receber"
Private aRotina := { {"Pesquisar","AxPesqui",0,1}, {"Visualizar","AxVisual",0,2} }
Private cDelFunc := ".T."
Private cString := "SE1"
DbSelectArea("SE1")
SE1->(DbSetOrder(1)) // E1_FILIAL + E1_PREFIXO + E1_NUM + E1_PARCELA + E1_TIPO
DbSelectArea(cString)
mBrowse(6,1,22,75,cString)
Return

/*ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
ฑฑบPrograma  ณMarca     บAutor  ณEduardo Augusto     บ Data ณ  22/10/2013 บฑฑ
ฑฑฬออออออออออุออออออออออสอออออออฯออออออออออออออออออออสออออออฯอออออออออออออนฑฑ
ฑฑบDesc.     ณ Funcao que Marca ou Desmarca todos os Objetos.             บฑฑ
ฑฑฬออออออออออุออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบUso       ณ I2I Eventos						                          บฑฑ
฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿*/

Static Function Marca(lMarca)
Local i := 0 	// TNU - Release 25
For i := 1 To Len(aVetor)
	aVetor[i][1] := lMarca
Next
oLbx:Refresh()
Return

/*ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
ฑฑบPrograma  ณCANCEBOL  บAutor  ณEduardo Augusto     บ Data ณ  22/10/2013 บฑฑ
ฑฑฬออออออออออุออออออออออสอออออออฯออออออออออออออออออออสออออออฯอออออออออออออนฑฑ
ฑฑบDesc.     ณ Funcao para Limpar os campos da Tabela SE1 quando o Boleto บฑฑ
ฑฑบ		     ณ sofrer Altera็ใo p/ gerar novamente 2ฐ Via...			  บฑฑ
ฑฑฬออออออออออุออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบUso       ณ Mirai							                          บฑฑ
฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿*/

Static Function CanceBol() 
Local m := 0	// TNU - Release 25
For m := 1 To Len(aVetor)
	If aVetor [m][1] == .T.
		DbSelectArea("SE1")
		DbSetOrder(1)
		If DbSeek(xFilial("SE1") + aVetor[m][2] + aVetor[m][3] + aVetor[m][4] + aVetor[m][12])
			RecLock("SE1",.F.)
			SE1->E1_NUMBCO := ""
			SE1->E1_CODBAR := ""
			SE1->E1_CODDIG := ""
			MsUnLock()
		EndIf
	EndIf
Next
MsgInfo("Cancelamento de 2ฐ via Finalizado com Sucesso")
Return

/*ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
ฑฑบPrograma  ณCANCETOT  บAutor  ณEduardo Augusto     บ Data ณ  22/10/2013 บฑฑ
ฑฑฬออออออออออุออออออออออสอออออออฯออออออออออออออออออออสออออออฯอออออออออออออนฑฑ
ฑฑบDesc.     ณ Funcao para Limpar os campos da Tabela SE1 quando o Boleto บฑฑ
ฑฑบ		     ณ sofrer cancelamento total das informa็๕es...				  บฑฑ
ฑฑฬออออออออออุออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบUso       ณ Mirai							                          บฑฑ
฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿*/

Static Function CanceTot(aVetor)
Local j := 0	// TNU - Release 25
For j := 1 To Len(aVetor)
	If aVetor [j][1] == .T.
		DbSelectArea("SE1")                      
		DbSetOrder(1)
		//If DbSeek(xFilial("SE1") + aVetor[j][2] + aVetor[j][3] + aVetor[j][4])
		If DbSeek(xFilial("SE1") + aVetor[j][2] + aVetor[j][3] + aVetor[j][4] + aVetor[j][12])
			RecLock("SE1",.F.)
			SE1->E1_NUMBCO := ""
			SE1->E1_XNUMBCO := ""
			SE1->E1_CODBAR := ""
			SE1->E1_CODDIG := ""
			MsUnLock()
		EndIf
	EndIf
Next
MsgInfo("Cancelamento de Boleto Total Finalizado com Sucesso")
Return

/*ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
ฑฑบPrograma  ณMarca     บAutor  ณEduardo Augusto     บ Data ณ  22/10/2013 บฑฑ
ฑฑฬออออออออออุออออออออออสอออออออฯออออออออออออออออออออสออออออฯอออออออออออออนฑฑ
ฑฑบDesc.     ณ Funcao que Perguntas do SX1.					              บฑฑ
ฑฑฬออออออออออุออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบUso       ณ I2I Eventos						                          บฑฑ
฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿*/
/*			// TNU - Release 25
Static Function ValidPerg()
_sAlias := Alias()
DBSelectArea("SX1")
DBSetOrder(1)
cPerg := PADR(cPerg,10)
aRegs:={}
// Grupo/Ordem/Pergunta/Variavel/Tipo/Tamanho/Decimal/Presel/GSC/Valid/Var01/Def01/Cnt01/Var02/Def02/Cnt02/Var03/Def03/Cnt03/Var04/Def04/Cnt04/Var05/Def05/Cnt05
aAdd(aRegs,{cPerg,"01","Banco              :","","","mv_chB","C",03,0,0,"G","","Mv_Par01",""    ,"","",""      ,"","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"02","Agencia            :","","","mv_chC","C",05,0,0,"G","","Mv_Par02",""    ,"","",""      ,"","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"03","Conta              :","","","mv_chD","C",10,0,0,"G","","Mv_Par03",""    ,"","",""      ,"","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"04","SubCta             :","","","mv_chE","C",03,0,0,"G","","Mv_Par04",""    ,"","",""      ,"","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"05","Tipo de Impressao  :","","","mv_chF","N",01,0,0,"C","","Mv_Par05","1ฐ Via","1ฐ Via","1ฐ Via","","","2ฐ Via","2ฐ Via","2ฐ Via","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"06","Tipo de Relat๓rio :","","","mv_chG","C",01,0,0,"C","","Mv_Par06","1","1","1","","","ND","ND","ND","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"07","Emissao de         :","","","mv_chH","D",08,0,0,"G","","Mv_Par07",""    ,"","",""      ,"","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"08","Emissao ate        :","","","mv_chI","D",08,0,0,"G","","Mv_Par08",""    ,"","",""      ,"","","","","","","","","","",""})
For i:=1 to Len(aRegs)
	If ! DBSeek(cPerg+aRegs[i,2])
		RecLock("SX1",.T.)
		For j:=1 to Len(aRegs[i])
			FieldPut(j,aRegs[i,j])
		Next
		MsUnlock()
	EndIf
Next
DBSkip()
DBSelectArea(_sAlias)
Return
*/