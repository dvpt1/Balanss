*----------------------------------------äÆ≠ÓÂÆ¢ Ñ.ã.-----*
*                     Å Ä ã Ä H ë                         *
*---------------------------------------------------------*
*=========================================================*
*                     CLIPPER  2.0                        *
*=========================================================*
external mdict,strnum,nulvar,strroun,strline,translate
external snum,dnum,nsub,formula
external varstr,sstr,ssubstr,isubstr,psubstr
external padc,vall
*---------------------------------------------------------*
#include "balans.ch"
*---------------------------------------------------------*
PROCEDURE BALANS
parameters param1,param2,param3
if type("param1") <> "C"
  param1 = ""
endif
param1 = upper(param1)
if type("param2") <> "C"
  param2 = ""
endif
param2 = upper(param2)
if type("param3") <> "C"
  param3 = ""
endif
param3 = upper(param3)
if param1 = "/M" .OR. param2 = "/M"
 is_mono = "/M"
else
 is_mono = ""
endif
InitSetj()
if param1 = "/S" .OR. param2 = "/S"
 is_setj = .T.
else
 is_setj = .F.
endif
do case
case at("\",param1) != 0
 cur_path = param1
case at("\",param2) != 0
 cur_path = param2
case at("\",param3) != 0
 cur_path = param3
othe
 cur_path=getdir()
endcase
if is_setj
 if cur_path==getdir()
  is_key_setj=.F.
 else
  is_key_setj=.T.
 endif
else
 is_key_setj=.F.
endif
************************
***InitTest()
************************
*OpenMouse()
InitSets()
InitVars()
InitPmsg()
InitColor(is_mono)
InitFormul()
InitCalc()
InitMenu()
InitDict()
InitConfig()
InitParol()
InitProtoc()
InitHelp()
*******************************************************
*name_firm = __pinit(getdir()+"\"+'BALANS.',__panti())
*******************************************************
bal_path=cur_path+"\"+str(year(I->T_DATE),4)+"."+strtran(str(month(I->T_DATE),2)," ","0")
use_path=bal_path+"\"
set path to &bal_path
mkdir(bal_path)
CreatDBF()
OpenDBF(.T.)
InitOborot()
is_arhiv=IsArhiv()
is_protoc=IsProtoc()
redraw=.T.
is_f9=.F.
PrgExit=.T.
do while PrgExit
 if redraw
  if is_f9
   is_f9=.F.
  else
   LoadParol()
  endif
  DrawBalans()
  redraw=.F.
 endif
 setcolor(c)
 select 1
 dbedit(Y_min+1,X_min+1,Y_max-5,X_max-1,jour_jou,"JourFunc","",jour_nam)
enddo
CloseDBF()
set color to w/n
clear
set cursor On
*CloseMouse()
RETURN

*---------------------------------------------------------*
function JourFunc
parameters mode,fld_ptr
cur_row=row()
RETR=1
Last_User=0
Retr_User=.T.
do while Retr_User
Retr_User=.F.
do case
case mode = 0
  @ Y_max-1, 4 say I->SALBEG
  @ Y_max-1,24 say I->RECSUM
  @ Y_max-1,44 say I->EXPSUM
  @ Y_max-1,63 say I->SALEND
  @ Y_max-4, 2 say '€€€€€€€€€€€€€€'
  setcolor(cl)
  if mark != 0
   @ Y_max-4, 2 say "èÆ¨•Á•≠Æ "+ltrim(str(Mark))
  endif
  @ Y_min, 2 say "[     ]"
  @ Y_min,10 say "[     ]"
  @ Y_min, 3 say str(reccount(),5)
  @ Y_min,11 say str(recno(),5)
  setcolor(c)
case mode = 3
case mode = 4
do case
case (Last_User = 32).or.(lastkey() = 32)       && SPACE
  if .not.Pravo0()
   return 1
  endif
  if IsSetj()
   return 1
  endif
  if is_adm
   if delete()
    recall
    Mark=Mark-1
   else
    delete
    Mark=Mark+1
   endif
   return 2
  else
   WarnWrite(PMSG1)
  endif
case (Last_User = 22).or.(lastkey() = 22)       && INS
  if NewBalans()
   InpBalans()
  endif
case (Last_User = 13).or.(lastkey() = 13)       && Enter
  InpBalans(.T.)
case (Last_User =  7).or.(lastkey()=   7)       && DEL
  DelBalans()
case (Last_User = -1).or.(lastkey() = -1)       && F2
  Futur()
case (Last_User = -2).or.(lastkey() = -2)       && F3
  Kopij()
case (Last_User = -3).or.(lastkey() = -3)       && F4
  Orfograf()
case (Last_User = -4).or.(lastkey() = -4)       && F5
  FindBalans(fld_ptr,Y_srm-2,X_srm-35,Y_srm+2,X_srm+35)
case (Last_User = -5).or.(lastkey() = -5)       && F6
  InpFilter()
case (Last_User = -6).or.(lastkey() = -6)       && F7
  txtedit("*.txt")
case (Last_User = -7).or.(lastkey() = -7)       && F8
  InpMonth()
case (Last_User = -8).or.(lastkey() = -8)       && F9
  TuneRegim()
case (Last_User = -9).or.(lastkey() = -9)       && F10
  MenuMain()
case (Last_User = -40).or.(lastkey() = -40)     && F11
  Sablon()
case (Last_User = -41).or.(lastkey() = -41)     && F12
  Docum()
case (Last_User = -45).or.(lastkey() = -45)     && Ctrl-F12
  Sum51()
  Retr_User=.T.
  mode = 0
case (Last_User = 45).or.(lastkey() = 45)       && -
  SeleRec(.F.,fld_ptr,Y_srm-3,X_srm-30,Y_srm+3,X_srm+30)
case (Last_User = 43).or.(lastkey() = 43)       && +
  SeleRec(.T.,fld_ptr,Y_srm-3,X_srm-30,Y_srm+3,X_srm+30)
case (Last_User = 27).or.(lastkey() = 27)       && ESC
  if Fragen(' Ç õ ï é Ñ ','á†¢•‡Ë®‚Ï ‡†°Æ‚„ · Ø‡Æ£‡†¨¨Æ©?')
   PrgExit=.F.
   RETR=0
  endif
endcase
endcase
enddo
if old_balans != reccount()
 old_balans = reccount()
 RETR=2
endif
return RETR
*---------------------------------------------------------*
procedure CreatDBF
for i=1 to SIZE_LIST
 name_dbf=use_path+strline(list_dbf[i])+".DIV"
 if .not.file(name_dbf)
  select 1
  name_str=DivisionSTR
  creat &name_dbf from &name_str
  use
 endif
next
for i=1 to SIZE_PLAN
 name_dbf=use_path+plan_dbf[i]
 if .not.file(name_dbf)
  select 1
  name_str=plan_str[i]
  creat &name_dbf from &name_str
  use
 endif
next
for i=1 to SIZE_BASE
 name_dbf=use_path+base_dbf[i]
 if .not.file(name_dbf)
  select 1
  name_str=base_str[i]
  creat &name_dbf from &name_str
  if .not.empty(base_ntx[i])
   name_ntx=use_path+base_ntx[i]
   name_key=base_key[i]
   index on &name_key to &name_ntx
  endif
  use
 endif
next
public old_balans
return
*---------------------------------------------------------*
procedure OpenDBF
parameters new_ntx
if pcount()=0
 new_ntx=.F.
endif
if new_ntx
 MsgWrite(PMSGNDBF)
 for i=1 to SIZE_BASE
  select 1
  name_dbf=use_path+base_dbf[i]
  use &name_dbf
  if .not.neterr()                      && •·´® Æ§®≠ ¢ ·®·‚•¨•
   MsgWrite(PMSGNNTX)
   if .not.empty(base_ntx[i])
    name_ntx=use_path+base_ntx[i]
    name_key=base_key[i]
    index on &name_key to &name_ntx
   endif
   use
  else
   exit
  endif
 next
endif
select 1
use_dbf=use_path+MainDBF
if is_setj
 use &use_dbf SHARED
else
 use &use_dbf
endif
if neterr()
 WarnWrite("à H î é ê å Ä ñ à ü   å é H é è é ã à á à ê é Ç Ä H Ä")
 quit
endif
InitIndex()
old_balans = reccount()
return
*---------------------------------------------------------*
procedure InitIndex
use_ntx=use_path+MainNTX
set index to &use_ntx
return
*---------------------------------------------------------*
procedure DrawBalans
InitSum51()
InitFilter()
setcolor(c)
DrawBox(Y_min,X_min,Y_max,X_max,ibox)
@ Y_max-4,X_min,Y_max-4,X_max box ibox
@ Y_max- 3,X_min+1 say " ë†´Ï§Æ ≠† ≠†Á†´Æ  ≥     Ñ Ö Å Ö í     ≥    ä P Ö Ñ à í    ≥ ë†´Ï§Æ ≠† ™Æ≠•Ê  "
@ Y_max- 2,X_min+1 say "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ"
@ Y_max- 1,X_min+1 say "                   ≥                   ≥                   ≥                  "
setcolor(cl)
MSG=' Ç Ö Ñ é å é ë í ú '
@ Y_min,int(X_srm-len(MSG)/2)+1 say MSG
if I->ISDEB
 MSG=" ëóÖí:"+alltrim(mDict("alltrim(strroun(D1))+' '+D2",I->DEB51,"D1",use_path+MainDIC,""))+' '
 if I->ISSUB
  var_file=use_path+strline(I->DEB51)+".DIC"
  if file(var_file)
   MSG=MSG+"/ ëìÅëóÖí:"+alltrim(mDict("alltrim(strroun(D1))+' '+D2",I->SUB51,"D1",var_file,""))+' '
  endif
 endif
 @ Y_max-4,int(X_srm-len(MSG)/2) say MSG
else
 setcolor(c)
 @ Y_max-4,X_min say replicate("€",X_max)
endif
setcolor(cl)
@ Y_max,X_max-16 say '∫ F10-å Ö ç û ∫'
DrawDate()
DrawFilter()
return
*---------------------------------------------------------*
procedure InitSum51
if I->ISDEB.and.debpravo(I->DEB51).and.krepravo(I->DEB51)
name_sub=use_path+strline(I->DEB51)+".DIC"
if file(name_sub)
 select 8
 use &name_sub
 if I->ISSUB
  sum H->SALBED,H->SALBEK to sald,salk for D1=I->SUB51
 else
  sum H->SALBED,H->SALBEK to sald,salk
 endif
 repl I->SALBEG with sald-salk
 use
 select 1
 set filter to
 go top
 if I->ISSUB
  sum A->SUMMA to summ for DEBET=I->DEB51.and.SUBDEB=I->SUB51.and.!FUTURE
 else
  sum A->SUMMA to summ for DEBET=I->DEB51.and.!FUTURE
 endif
 repl I->RECSUM with summ
 if I->ISSUB
  sum A->SUMMA to summ for KREDIT=I->DEB51.and.SUBKRE=I->SUB51.and.!FUTURE
 else
  sum A->SUMMA to summ for KREDIT=I->DEB51.and.!FUTURE
 endif
 repl I->EXPSUM with summ
 repl I->SALEND with I->SALBEG+I->RECSUM-I->EXPSUM
 InitFilter()
else
 repl I->SALBEG with 0
 repl I->RECSUM with 0
 repl I->EXPSUM with 0
 repl I->SALEND with 0
endif
else
 repl I->SALBEG with 0
 repl I->RECSUM with 0
 repl I->EXPSUM with 0
 sum A->SUMMA to summ for !FUTURE
 repl I->SALEND with summ
endif
return
*---------------------------------------------------------*
procedure Sum51
private old_nz
save scre
MsgWrite("è Ö ê Ö ê Ä ë ó Ö í")
old_nz=recno()
InitSum51()
go old_nz
rest scre
return
*---------------------------------------------------------*
procedure DrawDate
nm=month(I->T_DATE)
setcolor(c)
@ Y_min,X_max-15 say replicate("€",15)
setcolor(cl)
MSG=alltrim(Month[month(I->T_DATE)])+" "+str(year(I->T_DATE),4)+"£."
@ Y_min,X_max-15 say MSG
return
*---------------------------------------------------------*
function DrawFilter
private d_filter,k_filter
d_filter=''
do case
case I->L_DEBET.and.(.not.I->L_SUBDEB)
  d_filter=d_filter+"ÑÖÅÖí:"+mDict("alltrim(strroun(D1))+' '+D2",I->F_DEBET,"D1",use_path+MainDIC,"")
case I->L_DEBET.and.I->L_SUBDEB
  d_filter=d_filter+"ÑÖÅÖí:"+mDict("alltrim(strroun(D1))+' '+D2",I->F_DEBET,"D1",use_path+MainDIC,"")+"/"+alltrim(strroun(I->F_SUBDEB))
endcase
k_filter=''
do case
case I->L_KREDIT.and.(.not.I->L_SUBKRE)
  k_filter=k_filter+"äêÖÑàí:"+mDict("alltrim(strroun(D1))+' '+D2",I->F_KREDIT,"D1",use_path+MainDIC,"")
case I->L_KREDIT.and.I->L_SUBKRE
  k_filter=k_filter+"äêÖÑàí:"+mDict("alltrim(strroun(D1))+' '+D2",I->F_KREDIT,"D1",use_path+MainDIC,"")+"/"+alltrim(strroun(I->F_SUBKRE))
endcase
if (.not.empty(d_filter)).or.(.not.empty(k_filter))
 set color to &cl
 repl I->N_FILTER with d_filter+iif(empty(d_filter).and.empty(k_filter),"","≥")+k_filter
 @ Y_max,X_min+2 say I->N_FILTER
else
 set color to &c
 @ Y_max,X_min+2 say replicate("€",len(I->N_FILTER))
endif
return " "
*---------------------------------------------------------*
procedure InitFilter
private ch_filter
ch_filter=''
do case
case I->L_DEBET.and.(.not.I->L_SUBDEB)
  ch_filter=ch_filter+"DEBET=I->F_DEBET"
case I->L_DEBET.and.I->L_SUBDEB
  ch_filter=ch_filter+"(DEBET=I->F_DEBET.and.SUBDEB=I->F_SUBDEB)"
endcase
do case
case I->L_KREDIT.and.(.not.I->L_SUBKRE)
  if .not.empty(ch_filter)
   ch_filter=ch_filter+".and."
  endif
  ch_filter=ch_filter+"KREDIT=I->F_KREDIT"
case I->L_KREDIT.and.I->L_SUBKRE
  if .not.empty(ch_filter)
   ch_filter=ch_filter+".and."
  endif
  ch_filter=ch_filter+"(KREDIT=I->F_KREDIT.and.SUBKRE=I->F_SUBKRE)"
endcase
if .not.empty(ch_filter)
  set filter to &ch_filter
else
  set filter to
endif
go top
return
*---------------------------------------------------------*
function CloseDBF
parameters is_recall
if pcount()<1
 is_recall=.T.
endif
if .not.is_setj
 if Mark # 0
  save screen
  MsgWrite('ì§†´ÔÓ ¨•‚™®...')
  recall for delete()
  Mark = 0
  restore screen
 endif
endif
if is_recall
 select 1
 use
endif
return " "
