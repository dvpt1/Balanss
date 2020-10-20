#include "balans.ch"

*---------------------------------------------------------*
procedure InpFilter
private otwet,old_color,var,is_var
old_color = setcolor()
save screen to InpPage
old_color=setcolor(cmenu)
BoxTenj(Y_srm-4,X_srm-25,Y_srm+4,X_srm+26,ibox)
is_var=.F.
otwet=1
do while otwet!=0
  setcolor(cmenu)
  @ Y_srm-3,X_srm-23 prompt "‘ç¥â ¤¥¡¥â    : "+mDict("str(D1)+' '+D2",I->F_DEBET,"D1",use_path+MainDIC,"")
  if I->L_DEBET
   var_file=use_path+strline(I->F_DEBET)+".DIC"
   if file(var_file)
    var_prm=mDict("str(D1)+' '+D2",I->F_SUBDEB,"D1",var_file,"")
   else
    var_prm=space(21)
   endif
  else
   var_prm=space(21)
  endif
  @ Y_srm-2,X_srm-23 prompt "‘ã¡áç¥â ¤¥¡¥â : "+var_prm
  @ Y_srm-1,X_srm-23 prompt "‘ç¥â ªp¥¤¨â   : "+mDict("str(D1)+' '+D2",I->F_KREDIT,"D1",use_path+MainDIC,"")
  if I->L_KREDIT
   var_file=use_path+strline(I->F_KREDIT)+".DIC"
   if file(var_file)
    var_prm=mDict("str(D1)+' '+D2",I->F_SUBKRE,"D1",var_file,"")
   else
    var_prm=space(21)
   endif
  else
   var_prm=space(21)
  endif
  @ Y_srm  ,X_srm-23 prompt "‘ã¡áç¥â ªp¥¤¨â: "+var_prm
  @ Y_srm+1,X_srm-24 say replicate("Ä",50)
  @ Y_srm+2,X_srm-18 prompt ' “ ‘ ’ € H  ‚ ˆ ’ œ  ‚ ›    Š “ '
  @ Y_srm+3,X_srm-18 prompt '    “ „ € ‹ ˆ ’ œ  ‚ ›    Š “    '
  menu to otwet
  if otwet != 0
    do case
    case otwet=1
      var=DictFind(I->F_DEBET,"D1","strroun(D1)+' '+D2","‚ë¡¥p¨â¥ áç¥â",use_path+MainDIC,"")
      if lastkey() != 7
       repl I->F_DEBET with var
       repl I->L_DEBET with .T.
       is_var=.T.
      else
       repl I->F_DEBET with 0
       repl I->L_DEBET with .F.
       repl I->F_SUBDEB with 0
       repl I->L_SUBDEB with .F.
       is_var=.T.
      endif
    case otwet=2
      if I->L_DEBET
       var_file=use_path+strline(I->F_DEBET)+".DIC"
       if file(var_file)
        var=DictFind(I->F_SUBDEB,"D1","strroun(D1)+' '+D2","‚ë¡¥p¨â¥ áã¡áç¥â",var_file,"")
        if lastkey() != 7
         repl I->F_SUBDEB with var
         repl I->L_SUBDEB with .T.
         is_var=.T.
        else
         repl I->F_SUBDEB with 0
         repl I->L_SUBDEB with .F.
         is_var=.T.
        endif
       endif
      endif
    case otwet=3
      var=DictFind(I->F_KREDIT,"D1","strroun(D1)+' '+D2","‚ë¡¥p¨â¥ áç¥â",use_path+MainDIC,"")
      if lastkey() != 7
       repl I->F_KREDIT with var
       repl I->L_KREDIT with .T.
       is_var=.T.
      else
       repl I->F_KREDIT with 0
       repl I->L_KREDIT with .F.
       repl I->F_SUBKRE with 0
       repl I->L_SUBKRE with .F.
       is_var=.T.
      endif
    case otwet=4
      if I->L_KREDIT
       var_file=use_path+strline(I->F_KREDIT)+".DIC"
       if file(var_file)
        var=DictFind(I->F_SUBKRE,"D1","strroun(D1)+' '+D2","‚ë¡¥p¨â¥ áã¡áç¥â",var_file,"")
        if lastkey() != 7
         repl I->F_SUBKRE with var
         repl I->L_SUBKRE with .T.
         is_var=.T.
        else
         repl I->F_SUBKRE with 0
         repl I->L_SUBKRE with .F.
         is_var=.T.
        endif
       endif
      endif
    case otwet=5
      exit
    case otwet=6
      repl I->F_DEBET with 0
      repl I->F_SUBDEB with 0
      repl I->F_KREDIT with 0
      repl I->F_SUBKRE with 0
      repl I->L_DEBET with .F.
      repl I->L_SUBDEB with .F.
      repl I->L_KREDIT with .F.
      repl I->L_SUBKRE with .F.
      exit
    endcase
  endif
enddo
if (otwet=0.and.is_var).or.otwet=5.or.otwet=6
  CloseDBF(.F.)
endif
restore screen from InpPage
if (otwet=0.and.is_var).or.otwet=5.or.otwet=6
  InitFilter()
  DrawFilter()
  RETR=0
endif
set color to &old_color
return
*---------------------------------------------------------*
procedure DelFilter
del_filter=.T.
keyboard chr(27)
return
*---------------------------------------------------------*
function FindBalans
parameters fld_ptr,Y1,X1,Y2,X2
if type("old_ptr")="U"
 public old_ptr,old_find
 old_ptr=0
 old_find=""
endif
private var_find,var_fld,otwet
private old_color
old_color = setcolor()
save screen to InpPage
set color to &cfr
BoxTenj(Y1,X1,Y2,X2,ibox)
set color to &cl
MSG='  ˆ ‘ Š € ’ œ '
@ Y1,int((X1+X2)/2-len(MSG)/2) say MSG
set color to &cfr
var_fld=jour_fld[fld_ptr]
if fld_ptr = old_ptr
 var_find=old_find
else
 var_find=nulvar(&var_fld)
endif
do case
case .not.empty(jour_iif[fld_ptr])
 @ Y1+2,X_srm-5 prompt " „€  "
 @ Y1+2,X_srm+3 prompt " …’ "
 menu to otwetf
 do case
 case otwetf=1
  var_find=.T.
 case otwetf=2
  var_find=.F.
 endcase
othe
 set cursor on
 @ Y1+2,(X_srm-int(lenvar(var_find)/2)) get var_find
 read
 set cursor off
endcase
if type("var_find")="C"
 var_fnd=alltrim(var_find)
else
 var_fnd=var_find
endif
if lastkey() != 27
  MsgWrite("‘¥©ç á ¯®¨é¥¬...")
  Nz=recno()
  skip
  if type("var_find")="C"
   locate rest for at(var_fnd,&var_fld.)!=0
  else
   locate rest for &var_fld.=var_fnd
  endif
  if found()
    RETR=2
  else
    ??chr(7)
    WarnWrite("®¨áª ­¥ ã¤ ç¥­...")
    go Nz
  endif
endif
old_ptr=fld_ptr
old_find=var_find
restore screen from InpPage
set color to &old_color
return " "
*---------------------------------------------------------*
procedure SeleRec
parameters log,fld_ptr,Y1,X1,Y2,X2
if fld_ptr = 1
 WarnWrite("® ¯®«î ¬¥âª¨ á¥«¥ªâ¨à®¢ ­¨¥ ­¥ ®áãé¥áâ¢«ï¥âáï...")
 RETR=1
 return
endif
if is_setj
 WarnWrite("’…“…’‘Ÿ ŒH‹œH… ˆ‘‹œ‡‚€Hˆ…")
 RETR=1
 return
endif
private var_sele1,var_sele2,var_fld,var_num,otwet
private old_color
old_color = setcolor()
save screen to InpPage
set color to &cfr
BoxTenj(Y1,X1,Y2,X2,ibox)
set color to &cl
if log
  MSG=' ‡ € ‘ … ‹ … Š ’ ˆ   ‚ € ’ œ '
else
  MSG='  € ‡ ‘ … ‹ … Š ’ ˆ   ‚ € ’ œ '
endif
@ Y1,int((X1+X2)/2-len(MSG)/2) say MSG
set color to &cfr
var_fld=jour_jou[fld_ptr]
var_sele1=nulvar(&var_fld)
var_sele2=var_sele1
set cursor on
@ Y1+2,X1+2 say " á "
@ row(),col() get var_sele1
@ Y1+4,X1+2 say "¯® "
@ row(),col() get var_sele2
read
set cursor off
if lastkey() != 27
  if log
    MsgWrite("‘¥©ç á § á¥«¥ªâ¨àã¥¬...")
  else
    MsgWrite("‘¥©ç á à §á¥«¥ªâ¨àã¥¬...")
  endif
  Nz=recno()
  go top
  var_fld=jour_jou[fld_ptr]
  if empty(var_sele2)
    var_sele2=var_sele1
  endif
  if log
    if type("var_sele1")="L"
      do while .not.eof()
        if chetpravo()
         if iif(var_sele1,&var_fld.,.not.&var_fld)
          if .not.delete()
            delete
            Mark=Mark+1
          endif
         endif
        endif
        skip
      enddo
    else
      do while .not.eof()
        if chetpravo()
         if &var_fld.>=var_sele1.and.&var_fld.<=var_sele2
          if .not.delete()
            delete
            Mark=Mark+1
          endif
         endif
        endif
        skip
      enddo
    endif
  else
    if type("var_sele1")="L"
      do while .not.eof()
        if chetpravo()
         if iif(var_sele1,&var_fld.,.not.&var_fld)
          if delete()
            recall
            Mark=Mark-1
          endif
         endif
        endif
        skip
      enddo
    else
      do while .not.eof()
        if chetpravo()
         if &var_fld.>=var_sele1.and.&var_fld.<=var_sele2
          if delete()
            recall
            Mark=Mark-1
          endif
         endif
        endif
        skip
      enddo
    endif
  endif
  go Nz
  RETR=2
endif
restore screen from InpPage
set color to &old_color
return
*---------------------------------------------------------*
procedure Orfograf
private Nz,i,var,fld,is_debet,is_subdeb,is_kredit,is_subkre
private old_color
if eof()
 return
endif
old_color = setcolor(cw)
save screen
MsgWrite("¥àã ¯« ­ áç¥â®¢ ¤«ï ¯à®¢¥àª¨...")
select 8
use_dic=use_path+MainDIC
use &use_dic
if reccount()!=0
decl expenses[reccount()]
i=1
do while .not.eof()
 expenses[i]=D1
 name_sub=strline(expenses[i])+".DIC"
 if file(name_sub)
  select 5
  use &name_sub
  ii=reccount()
  if ii!=0
   name_dim="d_"+strline(expenses[i])
   decl &name_dim.[ii]
   ii=1
   do while .not.eof()
    &name_dim.[ii]=D1
    ii++
    skip
   enddo
  endif
  use
 endif
 select 8
 i++
 skip
enddo
use
is_debet=.F.
is_subdeb=.F.
is_kredit=.F.
is_subkre=.F.
select 1
NN=0
do while .not.eof()
  MsgWrite("p®¢¥pïî... "+alltrim(str(NN))+" ESC - ’Œ…H€..."+alltrim(str(memory(0))))
  NN++
  if ascan(expenses,A->DEBET)=0
   is_debet=.T.
  endif
  name_sub=strline(A->DEBET)+".DIC"
  if file(name_sub)
   name_dim="d_"+strline(A->DEBET)
   if ascan(&name_dim,A->SUBDEB)=0
    is_subdeb=.T.
   endif
  else
   is_subdeb=.T.
  endif
  if ascan(expenses,A->KREDIT)=0
   is_kredit=.T.
  endif
  name_sub=strline(A->KREDIT)+".DIC"
  if file(name_sub)
   name_dim="d_"+strline(A->KREDIT)
   if ascan(&name_dim,A->SUBKRE)=0
    is_subkre=.T.
   endif
  else
   is_subkre=.T.
  endif
  if is_debet.OR.is_subdeb.OR.is_kredit.OR.is_subkre
    exit
  endif
  if inkey()=27
    exit
  endif
  skip
enddo
select 1
RETR=2
if is_debet.OR.is_subdeb.OR.is_kredit.OR.is_subkre
 BoxTenj(Y_srm-5,X_srm-25,Y_srm+5,X_srm+25,ibox)
 if is_debet
  @ Y_srm-3,X_srm-23 say "H…€‚ˆ‹œH ‚‚…„…H HŒ… ‘—…’€  „……’“"
 endif
 if is_subdeb
  @ Y_srm-1,X_srm-23 say "H…€‚ˆ‹œH ‚‚…„…H HŒ… ‘“‘—…’€  „……’“"
 endif
 if is_kredit
  @ Y_srm+1,X_srm-23 say "H…€‚ˆ‹œH ‚‚…„…H HŒ… ‘—…’€  Š…„ˆ’“"
 endif
 if is_subkre
  @ Y_srm+3,X_srm-23 say "H…€‚ˆ‹œH ‚‚…„…H HŒ… ‘“‘—…’€  Š…„ˆ’“"
 endif
 inkey(0)
else
 if lastkey()!=27
  MsgWrite("‚…Š‰ H…‚›Ÿ‚‹…H ˜ˆŠ ‚ ‚„Š€•")
  inkey(0)
 endif
endif
else
 use
 select 1
endif
restore screen
setcolor(old_color)
return
*---------------------------------------------------------*
function IsArhiv
disk_path=cur_path
old_color = setcolor()
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
is_close=file(use_path+'close.dbf')
return iif(iArhiv[ncount]=I->T_DATE,.F.,.T.)
