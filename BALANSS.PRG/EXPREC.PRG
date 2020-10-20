*---------------------------------------------------------*
function RecExp
parameters Y1,X1,Y2,X2
private old_color,old_select,Inp1Page,otwet,is_done,Nz,is_dk,i
is_data=1
if SIZE_LIST=0
 return is_data
endif
old_select=select()
old_color = setcolor(cinp)
save screen to Inp1Page
otwet=0
is_dk=.F.
for i=1 to SIZE_LIST
 if list_dbf[i]=A->DEBET.or.list_dbf[i]=A->KREDIT
  if otwet != 0
   is_dk=.T.
  endif
  if .not.is_dk
   otwet=i
  endif
 endif
next
if otwet=0.or.is_dk
 setcolor(cmenu)
 Y2=Y2+SIZE_LIST
 BoxTenj(Y1,X1,Y2,X2,tbox)
 otwet=LevExpRec
 @ Y1,X1+1 say ''
 for i=1 to SIZE_LIST
  @ row()+1,X1+1 prompt mDict("D2",list_dbf[i],"D1",use_path+MainDIC,"")
 next
 menu to otwet
endif
if otwet!=0
LevExpRec=otwet
lst_dbf=list_dbf[otwet]
lst_ext=list_ext[otwet]
lst_frm=list_frm[otwet]
MsgWrite('   ฃ เ ใ ฆ   ๎  ช   เ โ ฎ ็ ช ใ ...')
select 7
use_dbf=use_path+OborotDBF
if is_setj
 use &use_dbf SHARED
else
 use &use_dbf
endif
use_ntx=use_path+OborotNTX
set index to &use_ntx
seek str(A->NUMBER)+str(lst_dbf)
if found()
 is_found=.T.
 Nz7=recno()
 dDEBKRE=DEBKRE
 dNUMB=NUMB
 dNAME=NAME
 dMEAS=MEAS
 dQUAN=QUAN
 dEXPENSES=EXPENSES
 dRECEIPTS=RECEIPTS
 dDATBEG=DATBEG
 dDATEKS=DATEKS
 dDATPER=DATPER
 dDATEND=DATEND
 dPROBEG=PROBEG
 dPROTEK=PROTEK
 dASHIFR=ASHIFR
 dANORMA=ANORMA
 dKOEF=KOEF
 dNOTEP=NOTES
else
 is_found=.F.
 dDEBKRE=lst_dbf
 dNUMB=nulvar(NUMB)
 dNAME=nulvar(NAME)
 dMEAS=nulvar(MEAS)
 dQUAN=nulvar(QUAN)
 dEXPENSES=nulvar(EXPENSES)
 dRECEIPTS=nulvar(RECEIPTS)
 dDATBEG=nulvar(DATBEG)
 dDATEKS=nulvar(DATEKS)
 dDATPER=nulvar(DATPER)
 dDATEND=nulvar(DATEND)
 dPROBEG=nulvar(PROBEG)
 dPROTEK=nulvar(PROTEK)
 dASHIFR=nulvar(ASHIFR)
 dANORMA=nulvar(ANORMA)
 dKOEF=nulvar(KOEF)
 dNOTEP=nulvar(NOTES)
endif
setcolor(cinp)
for i=1 to SIZE_EXPREC
 if .not.empty(exre_dbf[i])
  exre_dbf[i]=strline(lst_dbf)+".DIV"
 endif
next
is_data=dbinput(SIZE_EXPREC,exre_fld,exre_dbf,exre_iif,exre_row,exre_col,;
                       exre_scr,exre_cod,exre_nam,exre_ntx,"d",".DIC",;
                       "DrawList()","ListUser()")
if is_data=0
 if Fragen('•/€‘•','๋ฏฎซญจโ์ ฎฏฅเ ๆจ๎?')
  MsgWrite("           …  € –   ...")
  is_divis = .T.
  iSUMMA = 0
  iQUAN = 0
  QuanSumma()
  is_done=.F.
  do case
  case dRECEIPTS = 0.or.iQUAN >= dQUAN
   is_done=.T.
  othe
   if .not.Fragen('  ’  …   ’     …  € –   ? ','ฎซจ็ฅแโขฎ แฏจแ๋ข ฅฌฎฃฎ กฎซ์่ฅ ็ฅฌ ฅแโ์ ข ญ ซจ็จจ !')
    is_done=.T.
   endif
  endcase
  if is_done
   MsgWrite("           …  € –   ...")
   if A->SUMMA = 0 .and. iQUAN != 0
    select 1
    repl SUMMA with dQUAN*iSUMMA/iQUAN
   endif
   select 7
   set filter to
   go top
   if is_found
    go Nz7
   else
    append blank
    repl NUMBER with A->NUMBER
    commit
   endif
   OnSetj()
   repl DEBKRE with dDEBKRE
   repl NUMB with dNUMB
   repl NAME with dNAME
   repl MEAS with dMEAS
   repl QUAN with dQUAN
   repl EXPENSES with dEXPENSES
   repl RECEIPTS with dRECEIPTS
   repl DATBEG with dDATBEG
   repl DATEKS with dDATEKS
   repl DATEND with dDATEND
   repl ASHIFR with dASHIFR
   repl ANORMA with dANORMA
   repl KOEF with dKOEF
   repl NOTES with dNOTEP
   UnSetj()
   name_dbf=use_path+strline(dEXPENSES)+"."+lst_ext        && ฅแซจ ช เโฎ็ชจ ญฅโ
   if file(name_dbf)
    select 8
    use &name_dbf SHARED
    locate for NUMB=dNUMB.and.NAME=dNAME.and.MEAS=dMEAS
    if .not.found()
     append blank
     repl NUMB with dNUMB
     repl NAME with dNAME
     repl MEAS with dMEAS
     repl EXPENS with A->DEBET
     repl SUBEXP with A->SUBDEB
     repl DATBEG with dDATBEG
     repl DATEKS with dDATEKS
     repl DATEND with dDATEND
     repl ASHIFR with dASHIFR
     repl ANORMA with dANORMA
     repl KOEF with dKOEF
    endif
    use
   endif
  endif
 endif
endif
select 7
use
endif
restore screen from Inp1Page
setcolor(old_color)
select(old_select)
return is_data
*---------------------------------------------------------*
procedure QuanSumma
private Nz1
if dRECEIPTS = 0 .or. dQUAN = 0
 return
endif
name_dbf=use_path+strline(dRECEIPTS)+"."+lst_ext
if file(name_dbf)
 select 8
 use &name_dbf SHARED
 locate for NUMB=dNUMB.and.NAME=dNAME.and.MEAS=dMEAS
 if found()
  iSUMMA = iSUMMA + DEBET
  iQUAN = iQUAN + QUAN
 endif
 use
endif
select 7
set filter to (dRECEIPTS=EXPENSES.or.dRECEIPTS=RECEIPTS).and.;
              dNUMB=NUMB.and.dNAME=NAME.and.dMEAS=MEAS
go top
select 1
Nz1=recno()
set filter to
go top
select 7
do while .not.eof()
 select 1
 seek G->NUMBER
 if found()
  select 7
  if is_found
   if Nz7!=recno()
    do case
    case dRECEIPTS=G->EXPENSES
     iSUMMA = iSUMMA + A->SUMMA
     iQUAN = iQUAN + G->QUAN
    case dRECEIPTS=G->RECEIPTS
     iSUMMA = iSUMMA - A->SUMMA
     iQUAN = iQUAN - G->QUAN
    endcase
   endif
  else
    do case
    case dRECEIPTS=G->EXPENSES
     iSUMMA = iSUMMA + A->SUMMA
     iQUAN = iQUAN + G->QUAN
    case dRECEIPTS=G->RECEIPTS
     iSUMMA = iSUMMA - A->SUMMA
     iQUAN = iQUAN - G->QUAN
    endcase
  endif
 endif
 select 7
 skip
enddo
select 1
InitFilter()
go Nz1
return
*---------------------------------------------------------*
procedure DrawList
setcolor(cinp)
BoxTenj(Y_min+2,X_min+3,Y_max-5,X_max-2,ibox)
setcolor(cl)
MSG='  €  ’  —  €      …    '
@ Y_min+2,int((X_min+X_max)/2-len(MSG)/2) say MSG
setcolor(cinp)
@ Y_min+ 3,X_min+4 say "อออญข.Nออออออออออออออออ €   …    €   …ออออออออออออออออ…..อออ"
@ Y_min+ 4,X_min+4 say "                                                                         "
@ Y_min+ 5,X_min+4 say "ออออออออออออออออออออออออออ    — … ‘ ’  ออออออออออออออออออออออออออออ"
@ Y_min+ 6,X_min+4 say "                                                                         "
@ Y_min+ 7,X_min+4 say "ออออออ   •  ออออออออออออออออออออออออออออออออออออออออออออออออออออออออ"
@ Y_min+ 8,X_min+4 say "                                                                         "
@ Y_min+ 9,X_min+4 say "ออออออ € ‘ •  ออออออออออออออออออออออออออออออออออออออออออออออออออออออออ"
@ Y_min+10,X_min+4 say "                                                                         "
@ Y_min+11,X_min+4 say "ออออ€’€ €—€€อออออ€’€ ‘“€’€–อออออ€’€ ……€—อออออ€’€ –€อออ"
@ Y_min+12,X_min+4 say "                                                                         "
@ Y_min+13,X_min+4 say "ออออ …อออออออออออออออออออออออออออ’…“ …ออออออออออออออออ"
@ Y_min+14,X_min+4 say "                                                                         "
@ Y_min+15,X_min+4 say "ออออ” €’€–อออออ€ €’€–ออออ””–…’ ……–…อออออ"
@ Y_min+16,X_min+4 say "                                                                         "
@ Y_min+17,X_min+4 say "ออออออออออออออออออออออออออ    … — € H  …ออออออออออออออออออออออออออออ"
@ Y_min+18,X_min+4 say "                                                                         "
setcolor(cl)
@ Y_min+19,X_min+20 say '…:'
setcolor(cmenu)
MSG='Enter'
@ row(),col() get MSG
@ row(),col() say 'ขฎค'
MSG='F2'
@ row(),col() get MSG
@ row(),col() say '‘ฏจแฎช'
MSG='ESC'
@ row(),col() get MSG
@ row(),col() say '๋ๅฎค'
clear gets
setcolor(cinp)
return
*---------------------------------------------------------*
function ListUser
private do_retr
do_retr=1
do case
case key_last =  -1
 if (.not.is_arhiv)
  if FondMater(strline(lst_dbf)+".DIV","."+lst_ext)
   do_retr=2
  endif
 endif
endcase
return do_retr
*---------------------------------------------------------*
function FondMater
parameters dic_dbf,dic_ext
private old_color,old_select,Inp2Page,otwet,Nz,var_find
var_find=space(50)
var_exp=DictFind(0,"D1","str(D1)+' '+D2","๋กฅpจโฅ ฏฎคเ งคฅซฅญจฅ",use_path+dic_dbf,"")
if lastkey()!=13
  return .F.
endif
old_select=select()
old_color = setcolor(cdict)
save screen to Inp2Page
declare dict_fld[1]
declare dict_nam[1]
dict_fld[1]="str(NUMB)+' '+NAME+' '+MEAS"
dict_nam[1]="๋กฅเจโฅ ญ จฌฅญฎข ญจฅ"
select 3
name_dbf=use_path+strline(var_exp)+dic_ext
use &name_dbf SHARED
DrawBox(Y_srm-11,X_srm-33,Y_srm+11,X_srm+33,ibox)
dbedit(Y_srm-10,X_srm-32,Y_srm+10,X_srm+32,dict_fld,"dvfind","",dict_nam)
use
select(old_select)
restore screen from Inp2Page
setcolor(old_color)
return iif(lastkey()=13,.T.,.F.)
*---------------------------------------------------------*
function dvFind
parameters mode,ptr
private old_color,fld_fld
do case
case mode <= 3
 return 1
case lastkey()=27
 return 0
case lastkey()=13
 dNUMB=NUMB
 dNAME=NAME
 dMEAS=MEAS
 return 0
case lastkey()=-5
 old_color = setcolor()
 save screen
 setcolor(cfr)
 BoxTenj(Y_srm-1,X_srm-26,Y_srm+1,X_srm+26,ibox)
 setcolor(cl)
 MSG='  ‘  € ’  '
 @ Y_srm-1,X_srm-len(MSG)/2 say MSG
 setcolor(cfr)
 set cursor on
 @ Y_srm,X_srm-25 get var_find
 read
 set cursor off
 if lastkey() != 27
  MsgWrite(PMSGFIND1)
  Nz=recno()
  skip
  if .not.empty(var_find)
   var_fnd=alltrim(var_find)
  else
   var_fnd=var_find
  endif
  fld_fld=dict_fld[1]
  locate rest for at(var_fnd,&fld_fld.)!=0
  if found()
   restore screen
   setcolor(old_color)
   return 2
  else
   ??chr(7)
   restore screen
   WarnWrite(PMSGFIND2)
   go Nz
  endif
 endif
 restore screen
 setcolor(old_color)
endcase
return 1
