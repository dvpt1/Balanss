*---------------------------------------------------------*
procedure InitProtoc
public OLDDEBET,OLDSUBDEB,OLDKREDIT,OLDSUBKRE,OLDSUMMA,OLDFUTURE
return
*---------------------------------------------------------*
procedure OldProtoc
OLDDEBET  = A->DEBET
OLDSUBDEB = A->SUBDEB
OLDKREDIT = A->KREDIT
OLDSUBKRE = A->SUBKRE
OLDSUMMA  = A->SUMMA
OLDFUTURE = A->FUTURE
return
*---------------------------------------------------------*
procedure DldProtoc
OLDDEBET  = dDEBET
OLDSUBDEB = dSUBDEB
OLDKREDIT = dKREDIT
OLDSUBKRE = dSUBKRE
OLDSUMMA  = dSUMMA
OLDFUTURE = dFUTURE
return
*---------------------------------------------------------*
procedure InpProtoc
private old_select
if .not.is_protoc
 return
endif
if M->OLDDEBET != A->DEBET.or.M->OLDSUBDEB != A->SUBDEB.or.;
   M->OLDKREDIT != A->KREDIT.or.M->OLDSUBKRE != A->SUBKRE.or.;
   M->OLDSUMMA  != A->SUMMA.or.M->OLDFUTURE != A->FUTURE
old_select=select()
select 8
use_dbf=use_path+ProtocDBF
if is_setj
 use &use_dbf SHARED
else
 use &use_dbf
endif
append blank
repl H->OLDDEBET  with M->OLDDEBET
repl H->OLDSUBDEB with M->OLDSUBDEB
repl H->OLDKREDIT with M->OLDKREDIT
repl H->OLDSUBKRE with M->OLDSUBKRE
repl H->OLDSUMMA  with M->OLDSUMMA
repl H->OLDFUTURE with M->OLDFUTURE
repl H->NEWDEBET  with A->DEBET
repl H->NEWSUBDEB with A->SUBDEB
repl H->NEWKREDIT with A->KREDIT
repl H->NEWSUBKRE with A->SUBKRE
repl H->NEWSUMMA  with A->SUMMA
repl H->NEWFUTURE with A->FUTURE
repl H->PNAME     with M->PNAME
use
select(old_select)
endif
return
*---------------------------------------------------------*
procedure ComProtoc
private old_select
if .not.is_protoc
 return
endif
old_select=select()
select 8
use_dbf=use_path+ProtocDBF
if is_setj
 use &use_dbf SHARED
else
 use &use_dbf
endif
append blank
repl H->NEWDEBET  with A->DEBET
repl H->NEWSUBDEB with A->SUBDEB
repl H->NEWKREDIT with A->KREDIT
repl H->NEWSUBKRE with A->SUBKRE
repl H->NEWSUMMA  with A->SUMMA
repl H->PNAME     with M->PNAME
use
select(old_select)
return
*---------------------------------------------------------*
procedure ShaProtoc
private old_select
if .not.is_protoc
 return
endif
old_select=select()
select 8
use_dbf=use_path+ProtocDBF
if is_setj
 use &use_dbf SHARED
else
 use &use_dbf
endif
append blank
repl H->NEWDEBET  with A->DEBET
repl H->NEWSUBDEB with A->SUBDEB
repl H->NEWKREDIT with A->KREDIT
repl H->NEWSUBKRE with A->SUBKRE
repl H->NEWSUMMA  with A->SUMMA
repl H->PNAME     with M->PNAME
use
select(old_select)
return
*---------------------------------------------------------*
procedure KopProtoc
private old_select
if .not.is_protoc
 return
endif
old_select=select()
select 8
use_dbf=use_path+ProtocDBF
if is_setj
 use &use_dbf SHARED
else
 use &use_dbf
endif
append blank
repl H->NEWDEBET  with A->DEBET
repl H->NEWSUBDEB with A->SUBDEB
repl H->NEWKREDIT with A->KREDIT
repl H->NEWSUBKRE with A->SUBKRE
repl H->NEWSUMMA  with A->SUMMA
repl H->PNAME     with M->PNAME
use
select(old_select)
return
*---------------------------------------------------------*
procedure FutProtoc
private old_select
if .not.is_protoc
 return
endif
old_select=select()
select 8
use_dbf=use_path+ProtocDBF
if is_setj
 use &use_dbf SHARED
else
 use &use_dbf
endif
append blank
repl H->OLDDEBET  with A->DEBET
repl H->OLDSUBDEB with A->SUBDEB
repl H->OLDKREDIT with A->KREDIT
repl H->OLDSUBKRE with A->SUBKRE
repl H->OLDSUMMA  with A->SUMMA
repl H->OLDFUTURE with .not.(A->FUTURE)
repl H->NEWDEBET  with A->DEBET
repl H->NEWSUBDEB with A->SUBDEB
repl H->NEWKREDIT with A->KREDIT
repl H->NEWSUBKRE with A->SUBKRE
repl H->NEWSUMMA  with A->SUMMA
repl H->NEWFUTURE with A->FUTURE
repl H->PNAME     with M->PNAME
use
select(old_select)
return
*---------------------------------------------------------*
procedure Protocol
private old_select
old_select=select()
old_color = setcolor(cinp)
save screen to InpPage
setcolor(cinp)
DrawBox(Y_min,X_min,Y_max,X_max,ibox)
setcolor(cl)
TITL1=" П Р О Т О К О Л  Р А Б О Т Ы "
@ Y_min,int((X_min+X_max)/2-len(TITL1)/2) say TITL1
ProtMenu()
select 8
use_dbf=use_path+ProtocDBF
if is_setj
 use &use_dbf SHARED
else
 use &use_dbf
endif
setcolor(cinp)
old_dict=reccount()
dbedit(Y_min+1,X_min+1,Y_max-1,X_max-1,prot_jou,"ProtFunc","",prot_nam)
use
select(old_select)
restore screen from InpPage
setcolor(old_color)
return
*---------------------------------------------------------*
function ProtMenu
setcolor(cl)
@ Y_max,X_min+20 say ' МЕНЮ:'
setcolor(cmenu)
MSG='DEL'
@ row(),col() get MSG
@ row(),col() say 'Удал.'
MSG='F2'
@ row(),col() get MSG
@ row(),col() say 'Вкл/Выкл'
MSG='F5'
@ row(),col() get MSG
@ row(),col() say 'Печать'
MSG='F6'
@ row(),col() get MSG
@ row(),col() say 'Искать'
MSG='ESC'
@ row(),col() get MSG
@ row(),col() say 'Выход'
clear gets
return " "
*---------------------------------------------------------*
function ProtFunc
parameters mode,fld_ptr
private cur_field,key_last
key_last=LASTKEY()
do case
case mode <= 3
case LASTKEY() = 27
  return 0
case lastkey() = -1         && F2
  save screen
  clos=use_path+'protoc.is'
  if is_protoc
   if Yes_Or_No(' ПРОТОКОЛ ','В ы к л ю ч и т ь ?',2)
    dele file &clos
    is_protoc=.F.
   endif
  else
   if Yes_Or_No(' ПРОТОКОЛ ','В к л ю ч и т ь ?',2)
    memowrit(clos,"")
    is_protoc=.T.
   endif
  endif
  restore screen
case lastkey() = -4        && F5
  dblpt1(prot_jou,prot_nam,TITL1)
case lastkey() = -5        && F6
  FindProtoc(fld_ptr,Y_srm-2,X_srm-30,Y_srm+2,X_srm+30)
case lastkey() = 7         && Del
  if .not.Pravo0()
   return 1
  endif
  if IsSetj()
   return 1
  endif
  save screen
  if Yes_Or_No(' У Д А Л И Т Ь ','У д а л и т ь  В С Е ?',2)
   MsgWrite("Ждите... Удаляю...")
   zap
   restore screen
   return 2
  endif
  restore screen
endcase
if old_dict!=reccount()
 old_dict=reccount()
 return 2
endif
return 1
*---------------------------------------------------------*
function FindProtoc
parameters fld_ptr,Y1,X1,Y2,X2
if type("ol_ptr")="U"
 public ol_ptr,ol_find
 ol_ptr=0
 ol_find=""
endif
private var_find,var_fld,otwet,Nz
private old_color,FindPage
old_color = setcolor()
save screen to FindPage
set color to &cfr
BoxTenj(Y1,X1,Y2,X2,ibox)
set color to &cl
MSG='  И С К А Т Ь '
@ Y1,int((X1+X2)/2-len(MSG)/2) say MSG
set color to &cfr
do case
case .not.empty(prot_iif[fld_ptr])
 var_fld=prot_fld[fld_ptr]
othe
 var_fld=prot_fld[fld_ptr]
endcase
if fld_ptr = ol_ptr
 var_find=ol_find
else
 var_find=nulvar(&var_fld)
endif
@ Y1+2,X1+2 say prot_nam[fld_ptr]
do case
case .not.empty(prot_iif[fld_ptr])
 @ row(),col()+1 prompt " ДА "
 @ row(),col()+1 prompt " НЕТ "
 menu to otwetf
 do case
 case otwetf=1
  var_find=.T.
 case otwetf=2
  var_find=.F.
 endcase
othe
 set cursor on
 @ row(),col()+2 get var_find
 read
 set cursor off
endcase
if type("var_find")="C"
 var_fnd=alltrim(var_find)
else
 var_fnd=var_find
endif
if lastkey() != 27
  MsgWrite("Сейчас поищем...")
  Nz=recno()
  skip
  if type("var_find")="C"
   locate rest for at(var_fnd,&var_fld.)!=0
  else
   locate rest for &var_fld.=var_fnd
  endif
  if .not.found()
    ??chr(7)
    WarnWrite("Поиск не удачен...")
    go Nz
  endif
endif
ol_ptr=fld_ptr
ol_find=var_find
restore screen from FindPage
set color to &old_color
return " "
*---------------------------------------------------------*
function IsProtoc
return file(use_path+'protoc.is')
