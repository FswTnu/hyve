#INCLUDE "PROTHEUS.CH"

/*ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
ฑฑบPrograma  ณ TELEMAI  บAutor  ณEduardo Augusto     บ Data ณ  04/12/2014 บฑฑ
ฑฑฬออออออออออุออออออออออสอออออออฯออออออออออออออออออออสออออออฯอออออออออออออนฑฑ
ฑฑบDesc.     ณ Tela para Sele็ใo de NF de Servi็o para Envio ao E-mail do บฑฑ
ฑฑบ          ณ Cliente Automatico juntamente com os Boletos em Anexos.    บฑฑ
ฑฑฬออออออออออุออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบUso       ณ I2I Eventos						                          บฑฑ
฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿*/

User Function TELEMAI()
Private cTitulo:= "SELEวรO DE NOTAS FISCAIS DE SERVIวO E NOTAS DE DEBITOS"
Private oOk    := LoadBitmap(GetResources(),"LBOK")
Private oNo    := LoadBitmap(GetResources(),"LBNO")
Private oStNo  := LoadBitmap( GetResources(), "BR_VERDE" )
Private oStYes := LoadBitmap( GetResources(), "BR_VERMELHO" )
Private cVar
Private oDlg
Private oChk
Private oLbx
Private lChk := .F.
Private lMark := .F.
Private aVetor := {}
Private cPerg := "INFORME AS NFs"
Private _aCores:={}
Private cTpImp	:=""
Private cAnexImp:=""

//Valperg()				// TNU - Release 25

If !Pergunte(cPerg,.T.)
	Return
EndIf  

cSerie  := Iif(Mv_Par01== 1,"1","ND") 
cTpImp	:=  Iif(Mv_Par01== 1,"1","ND") 
cAnexImp:= Iif(Mv_Par02== 1,"SIM","NAO")

AADD(_aCores,{"SF2->F2_XOK = 'S'","BR_VERMELHO"})
AADD(_aCores,{"SF2->F2_XOK = ' '","BR_VERDE"})

DbSelectArea("SF2")
SF2->(DbSetOrder(1)) // F2_FILIAL + F2_DOC + F2	_SERIE + F2_CLIENTE + F2_LOJA + F2_FORMUL + F2_TIPO
SF2->(DbClearFilter())
SF2->(DbGoTop())
While SF2->(!EOF())
	If SF2->F2_FILIAL == xFilial("SF2") .And. Alltrim(SF2->F2_SERIE) == Alltrim(cSerie)                                                                                                            
		aAdd(aVetor, { lMark,SF2->F2_XOK,SF2->F2_DOC, SF2->F2_SERIE, SF2->F2_CLIENTE, Posicione("SA1",1,xFilial("SA1") + SF2->F2_CLIENTE + SF2->F2_LOJA,"A1_NOME"), SF2->F2_LOJA, cTpImp,cAnexImp })
		SF2->(DbSkip())
	Else	          		
		SF2->(DbSkip())
	EndIf
End
If Len(aVetor) == 0
	MsgAlert("Nใo foi Selecionado nenhuma Nota Fiscal de Servi็o ",cTitulo)
	Return
EndIf
DEFINE MSDIALOG oDlg TITLE cTitulo FROM 0,0 To 511,0900 PIXEL // 511,1292    // 630,230

@010,010 LISTBOX oLbx VAR cVar FIELDS Header " ","","Documento", "Serie", "Cliente", "Nome do Cliente", "Loja" SIZE 430,230 Of oDlg PIXEL ON dblClick(aVetor[oLbx:nAt,1] := !aVetor[oLbx:nAt,1],oLbx:Refresh())

oLbx:SetArray(aVetor)
oLbx:bLine := {|| { Iif(aVetor[oLbx:nAt,1],oOk,oNo),Iif(Alltrim(aVetor[oLbx:nAt,2])=="S",oStYes,oStNo), aVetor[oLbx:nAt,3], aVetor[oLbx:nAt,4], aVetor[oLbx:nAt,5], aVetor[oLbx:nAt,6], aVetor[oLbx:nAt,7] }}

If oChk <> Nil
	@245,010 CHECKBOX oChk VAR lChk Prompt "Marca/Desmarca" Size 60,007 PIXEL Of oDlg On Click(Iif(lChk,Marca(lChk),Marca(lChk)))
EndIf

@245,010 CHECKBOX oChk VAR lChk Prompt "Marca/Desmarca" SIZE 60,007 PIXEL Of oDlg On Click(aEval(aVetor,{|x| x[1] := lChk}),oLbx:Refresh())
@243,225 BUTTON "Legenda"   SIZE 050, 011 Font oDlg:oFont ACTION Legenda() OF oDlg PIXEL
@243,280 BUTTON "Confirmar" SIZE 050, 011 Font oDlg:oFont ACTION Iif(cSerie == "1",(U_RECIRPS(aVetor),oDlg:End()),(U_NFDEBITO(aVetor),oDlg:End())) Of oDlg PIXEL
@243,335 BUTTON "Consulta"  SIZE 050, 011 Font oDlg:oFont ACTION VisuSF2() OF oDlg PIXEL
@243,390 BUTTON "Cancela"   SIZE 050, 011 Font oDlg:oFont ACTION oDlg:End() OF oDlg PIXEL

ACTIVATE MSDIALOG oDlg CENTER

Return

/*ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
ฑฑบPrograma  ณ VisuSF2  บAutor  ณEduardo Augusto     บ Data ณ  22/10/2013 บฑฑ
ฑฑฬออออออออออุออออออออออสอออออออฯออออออออออออออออออออสออออออฯอออออออออออออนฑฑ
ฑฑบDesc.     ณ Funcao para Chamada do mBrowse da Tela de Inlcusao do      บฑฑ
ฑฑบ          ณ Contas a Receber (Somente Consulta)             			  บฑฑ
ฑฑฬออออออออออุออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบUso       ณ I2I Eventos							                      บฑฑ
฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿*/

Static Function VisuSF2()
Private cCadastro := "Tela do Cabe็alho da Nota Fiscal"
Private aRotina := { {"Pesquisar","AxPesqui",0,1}, {"Visualizar","AxVisual",0,2} }
Private cDelFunc := ".T."
Private cString := "SF2"
DbSelectArea("SF2")
SF2->(DbSetOrder(1)) // F2_FILIAL + F2_DOC + F2_SERIE + F2_CLIENTE + F2_LOJA + F2_FORMUL + F2_TIPO
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

Local i := 0

For i := 1 To Len(aVetor)
	aVetor[i][1] := lMarca
Next
oLbx:Refresh()
Return

/*ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
ฑฑบPrograma  ณMarca     บAutor  ณEduardo Augusto     บ Data ณ  22/10/2013 บฑฑ
ฑฑฬออออออออออุออออออออออสอออออออฯออออออออออออออออออออสออออออฯอออออออออออออนฑฑ
ฑฑบDesc.     ณ Funcao que Perguntas do SX1.					              บฑฑ
ฑฑฬออออออออออุออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบUso       ณ I2I Eventos						                          บฑฑ
฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿*/
/*			// TNU - Release 25
Static Function ValPerg()
_sAlias := Alias()
DBSelectArea("SX1")
DBSetOrder(1)
cPerg := PADR(cPerg,10)
aRegs:={}
// Grupo/Ordem/Pergunta/Variavel/Tipo/Tamanho/Decimal/Presel/GSC/Valid/Var01/Def01/Cnt01/Var02/Def02/Cnt02/Var03/Def03/Cnt03/Var04/Def04/Cnt04/Var05/Def05/Cnt05
aAdd(aRegs,{cPerg,"01","Tipo de Relat๓rio :","","","mv_chA","C",01,0,0,"C","","Mv_Par01","RPS","RPS","RPS","","","ND","ND"  ,"ND","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"02","Anexar Boleto ?   :","","","mv_chB","C",01,0,0,"C","","Mv_Par02","SIM","SIM","SIM","","","NAO","NAO","NAO","","","","","","","","","","","","","","","","","","","","",""})
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
*/			// TNU - Release 25

Static Function Legenda()

_aCores2:={}
AADD(_aCores2,{"BR_VERMELHO","E-mail Enviado"})   
AADD(_aCores2,{"BR_VERDE"   ,"Pendente de envio"})   


BrwLegenda(cCadastro,"Envio de e-mails",_aCores2)

Return .t.