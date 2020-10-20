#include "balans.ch"

*---------------------------------------------------------*
procedure SaveArhiv
private disk,old_Nz
if is_setj
 WarnWrite(PMSG6)
 return
endif
if is_key_setj
 disk=diskABC()
else
 disk=diskAB()
endif
if empty(disk)
  return
endif
frg=FrgArhiv(.T.)
if frg=0
  return
endif
old_Nz=recno()
CloseDBF()
save_path=disk+"\"+str(year(I->T_DATE),4)+"."+strtran(str(month(I->T_DATE),2)," ","0")
mkdir(save_path)
do case
case frg=1
 deles("*.*",save_path)
 copys("*.*",bal_path,save_path)
case frg=2
 deles("*.DIC",save_path)
 copys("*.DIC",bal_path,save_path)
 deles("*.DIV",save_path)
 copys("*.DIV",bal_path,save_path)
case frg=3
 deles("*.DBF",save_path)
 copys("*.DBF",bal_path,save_path)
endcase
OpenDBF()
go old_Nz
redraw=.T.
RETR=0
return
*---------------------------------------------------------*
procedure LoadArhiv
private disk,new_date
if is_setj
 WarnWrite(PMSG6)
 return
endif
if is_key_setj
 disk=diskABC()
else
 disk=diskAB()
endif
if empty(disk)
  return
endif
frg=FrgArhiv(.F.)
if frg=0
  return
endif
new_date=DateArh(disk)
if .not.empty(new_date)
if isoccu(cur_path+"\"+str(year(new_date),4)+"."+strtran(str(month(new_date),2)," ","0")+"\"+MainDBF).or.new_date=I->T_DATE
 CloseDBF()
 repl I->T_DATE with new_date
 load_path=disk+"\"+str(year(I->T_DATE),4)+"."+strtran(str(month(I->T_DATE),2)," ","0")
 bal_path=cur_path+"\"+str(year(I->T_DATE),4)+"."+strtran(str(month(I->T_DATE),2)," ","0")
 use_path=bal_path+"\"
 set path to &bal_path
 mkdir(bal_path)
 do case
 case frg=1
  deles("*.*",bal_path)
  copys("*.*",load_path,bal_path)
 case frg=2
  deles("*.DIC",bal_path)
  copys("*.DIC",load_path,bal_path)
  deles("*.DIV",bal_path)
  copys("*.DIV",load_path,bal_path)
 case frg=3
  deles("*.DBF",bal_path)
  copys("*.DBF",load_path,bal_path)
 endcase
 OpenDBF()
 is_arhiv=IsArhiv()
 redraw=.T.
 RETR=0
else
 WarnWrite(PMSG2_2)
endif
endif
return
*---------------------------------------------------------*
procedure FrgArhiv
parameters is_frg
aOptions={" В С Е "," П Л А Н "," ПРОВОДКИ "}
return frags(" Д А Т А ",iif(is_frg,"С О Х Р А H И Т Ь ?","В О С С Т А Н О В И Т Ь ?"),aOptions)
*---------------------------------------------------------*
procedure FragenDK
aOptions={" ДЕБ/КPЕ "," Д Е Б Е Т "," К Р Е Д И Т "}
return frags(" Д А Т А ","Д О П О Л Н И Т Ь ?",aOptions)
*---------------------------------------------------------*
procedure AppArhiv
private disk,Nz,src_path,dst_path
if is_setj
 WarnWrite(PMSG6)
 return
endif
if is_key_setj
 disk=diskABC()
else
 disk=diskAB()
endif
if empty(disk)
  return
endif
private new_date
new_date=DateArh(disk)
if .not.empty(new_date)
 load_path=disk+"\"+str(year(I->T_DATE),4)+"."+strtran(str(month(I->T_DATE),2)," ","0")
 bal_path=cur_path+"\"+str(year(I->T_DATE),4)+"."+strtran(str(month(I->T_DATE),2)," ","0")
 name_dbf=load_path+"\"+MainDIC
 if file(name_dbf)
  Nz=recno()
  setcolor(c)
  BoxTenj(Y_min+1,X_min+20,Y_max-1,X_max-20,ibox)
  AppMenu()
  setcolor(c)
  select 2
  use expenses.str SHARED
  j=lastrec()
  declare dict_fld[j]
  declare dict_nam[j]
  for i=1 to j
   dict_fld[i]=alltrim(FIELD_NAME)
   dict_nam[i]=alltrim(NAME_FIELD)
   skip
  next
  use &name_dbf SHARED
  dbedit(Y_min+2,X_min+21,Y_max-2,X_max-21,dict_fld,"AppFunc","",dict_nam,.T.,.T.,"─┴─")
  use
  select 1
  go Nz
  redraw=.T.
  RETR=0
 endif
endif
return
*---------------------------------------------------------*
function AppFunc
parameters mode,fld_ptr
do case
case mode <= 3
 return 1
case LASTKEY() = 27
 return 0
case LASTKEY() = 13
 otwet=FragenDK()
 if otwet!=0
  MsgWrite("Копиpую... Ждите...")
  select 1
  src_path=load_path+"\"+MainDBF
  do case
  case otwet=1
   append from &src_path for DEBET=B->D1.or.KREDIT=B->D1
  case otwet=2
   append from &src_path for DEBET=B->D1
  case otwet=3
   append from &src_path for KREDIT=B->D1
  endcase
  select 8
  use &MainDIC SHARED
  locate for B->D1=H->D1
  if .not.found()
   append blank
   repl H->D1 with B->D1,H->D2 with B->D2
  endif
  use
  name_sub=use_path+strline(B->D1)+".DIC"
  copys(name_sub,load_path,cur_path)
  select 2
  return 2
 endif
endcase
return 1
*---------------------------------------------------------*
function AppMenu
set color to &cl
@ Y_max-1,X_min+26 say ' МЕНЮ:'
set color to &cmenu
MSG='Enter'
@ row(),col() get MSG
@ row(),col() say 'Копиpовать'
MSG='ESC'
@ row(),col() get MSG
@ row(),col() say 'Выход'
clear gets
return " "
*---------------------------------------------------------*
procedure OldArhiv
private new_date
new_date=DateArh(cur_path)
if new_date!=I->T_DATE
if .not.empty(new_date)
if isoccu(cur_path+"\"+str(year(new_date),4)+"."+strtran(str(month(new_date),2)," ","0")+"\"+MainDBF)
 CloseDBF()
 repl I->T_DATE with new_date
 bal_path=cur_path+"\"+str(year(I->T_DATE),4)+"."+strtran(str(month(I->T_DATE),2)," ","0")
 use_path=bal_path+"\"
 set path to &bal_path
 OpenDBF()
 is_arhiv=IsArhiv()
 redraw=.T.
 RETR=0
else
 WarnWrite(PMSG2_2)
endif
endif
endif
return
*---------------------------------------------------------*
procedure DelArhiv
private new_date
if nparol != 1
 WarnWrite(PMSG1)
 return
endif
do while .T.
new_date=DateArh(cur_path)
if .not.empty(new_date)
if new_date != I->T_DATE
if isoccu(cur_path+"\"+str(year(new_date),4)+"."+strtran(str(month(new_date),2)," ","0")+"\"+MainDBF)
save scre
if Yes_Or_No('','У д а л и т ь   а p х и в ?',2)
 del_path=cur_path+"\"+str(year(new_date),4)+"."+strtran(str(month(new_date),2)," ","0")
 deles("*.*",del_path)
 rmdir(del_path)
endif
rest scre
else
 WarnWrite(PMSG2_2)
endif
else
 WarnWrite("Текущий аpхив удалять  H Е Л Ь З Я !!!")
endif
else
 exit
endif
enddo
return
*---------------------------------------------------------*
function DateArh
parameters disk_path
private old_color
old_color = setcolor()
save scre
MsgWrite("Читаю список архивов...")
private nNames := {}, aNames := {}
aFiles := DIRECTORY(disk_path+"\*.*", "D")
AEVAL(aFiles,{ | file | AADD(nNames, file[F_NAME]), AADD(aNames, file[F_ATT]) })
fcount=len(nNames)
j=0
for i=1 to fcount
 if aNames[i] == "D".AND.nNames[i] != "..".AND.nNames[i] != ".".AND.isdigit(nNames[i])
  j++
 endif
next
ncount=j
private iArhiv[ncount]
j=1
for i=1 to fcount
 if aNames[i] == "D".AND.nNames[i] != "..".AND.nNames[i] != ".".AND.isdigit(nNames[i])
  iArhiv[j] = ctod("01."+substr(nNames[i],6,2)+"."+substr(nNames[i],1,4))
  j++
 endif
next
asort(iArhiv)
private aArhiv[ncount]
for i=1 to ncount
 aArhiv[i] = " "+Month[month(iArhiv[i])]+" "+str(year(iArhiv[i]),4)+" "
next
rest scre
save scre
setcolor(cdict)
BoxTenj(Y_srm-10,X_srm-8,Y_srm+10,X_srm+8,ibox)
setcolor(cl)
MSG=' А Р Х И В '
@ Y_srm-10,int(X_srm-len(MSG)/2)+1 say MSG
setcolor(cdict)
ptr=1
new_ptr=achoice(Y_srm-9,X_srm-7,Y_srm+9,X_srm+7,aArhiv,"","",ptr)
rest scre
setcolor(old_color)
return iif(new_ptr=0,ctod(""),iif(empty(iArhiv[new_ptr]),ctod(""),iArhiv[new_ptr]))
