#include "balans.ch"

#define LEN_PAROL 500
#define CNT_CHET 30
#define LEN_CHET 6
*---------------------------------------------------------*
procedure InitParol
public pparol
public cPAROL
cPAROL=space(LEN_PAROL)
public nparol
public pname
public pravo
pravo=0
public d_chet[CNT_CHET]
afill(d_chet,I->FORMATC)
public k_chet[CNT_CHET]
afill(k_chet,I->FORMATC)
public is_adm
is_adm=.F.
return
*---------------------------------------------------------*
procedure LoadParol
private dparol
if is_adm
 return
endif
select 8
parol_path=use_path+"parol.bln"
use &parol_path SHARED
if neterr()
 WarnWrite("€„ŒˆHˆ‘’€’ ‡€HŸ’ „†„ˆ’…")
 quit
endif
if reccount()=0
 WarnWrite("   “ ˜ … H  ” € ‰ ‹  „  ‘ ’ “  €  ! ! !")
 quit
* ParolNew()
endif
setcolor(c)
@ Y_min,X_min,Y_max,X_max box '±±±±±±±±±'
setcolor(cmenu)
BoxTenj(Y_srm-1,X_srm-11,Y_srm+1,X_srm+10,ibox)
setcolor(cl)
MSG=' …„‘’€‚’…‘œ '
@ Y_srm-1,X_srm-len(MSG)/2 say MSG
setcolor(cmenu)
pname=space(20)
@ Y_srm,X_srm-10 get pname
read
locate for pname == pread(1,20)
if .not.found()
 clear screen
 WarnWrite(" Ÿ  ‚ € ‘  H … ‡ H €  . . . ")
 quit
endif
setcolor(cmenu)
BoxTenj(Y_srm-1,X_srm-11,Y_srm+1,X_srm+10,ibox)
setcolor(cl)
MSG='  €   ‹ œ '
@ Y_srm-1,X_srm-len(MSG)/2 say MSG
setcolor("n/n,n/n,,,n/n")
pparol=space(20)
@ Y_srm,X_srm-10 get pparol
read
if .not.(pparol == pread(31,20))
 clear screen
 WarnWrite("€ ‚’ ˆ H…€‚ˆ‹œH...")
 quit
endif
pravo=val(pread(51,1))
for i=1 to CNT_CHET
 d_chet[i]=val(pread(60+(i-1)*6,LEN_CHET))
next
for i=1 to CNT_CHET
 k_chet[i]=val(pread(300+(i-1)*6,LEN_CHET))
next
nparol=recno()
if nparol=1
 is_adm=.T.
endif
use
@ Y_min,X_min,Y_max,X_max box '±±±±±±±±±'
return
*---------------------------------------------------------*
procedure TuneParol
private old_color,ThrPage,old_select
private dparol,iparol,dparol1,dparol2
old_select=select()
save screen to ThrPage
parol_path=use_path+"parol.bln"
old_color = setcolor(cmenu)
BoxTenj(Y_srm-1,X_srm-11,Y_srm+1,X_srm+10,ibox)
if .not.empty(pparol)
 setcolor(cl)
 MSG='‚€˜ €‹œ'
 @ Y_srm-1,X_srm-len(MSG)/2 say MSG
 setcolor("n/n,n/n,,,n/n")
 iparol=space(20)
 @ Y_srm,X_srm-10 get iparol
 read
 if .not.(pparol == iparol)
  clear screen
  quit
 endif
endif
if lastkey() != 27
setcolor(cl)
MSG='H‚›‰ €‹œ'
@ Y_srm-1,X_srm-len(MSG)/2 say MSG
setcolor("n/n,n/n,,,n/n")
dparol1=space(20)
@ Y_srm,X_srm-10 get dparol1
read
if lastkey() != 27
setcolor(cl)
MSG='‚’ˆ’… …™…'
@ Y_srm-1,X_srm-len(MSG)/2 say MSG
setcolor("n/n,n/n,,,n/n")
dparol2=space(20)
@ Y_srm,X_srm-10 get dparol2
read
if lastkey() != 27
if dparol1 == dparol2
 select 8
 use &parol_path
 go nparol
 pwrite(dparol1,31,20)
 pparol=dparol1
 use
endif
endif
endif
endif
setcolor(old_color)
restore screen from ThrPage
select(old_select)
return
*---------------------------------------------------------*
procedure TuneAdm
private old_color,ThrPage,old_select
save screen to ThrPage
old_select=select()
select 2
parol_path=use_path+"parol.bln"
use &parol_path
if neterr()
 WarnWrite("€„ŒˆHˆ‘’€’ ‡€HŸ’ „†„ˆ’…")
 select(old_select)
 restore screen from ThrPage
 return
endif
old_color = setcolor(c)
DrawBox(Y_min,X_min,Y_max,X_max,ibox)
setcolor(cl)
TITL1=" € „ Œ ˆ H ˆ ‘ ’  € ’   "
@ Y_min,int((X_min+X_max)/2-len(TITL1)/2) say TITL1
MenuAdm()
declare dict_fld[5]
declare dict_nam[5]
dict_fld[1]="pread(1,20)"
dict_nam[1]="ˆŒŸ;‹œ‡‚€’…‹Ÿ"
dict_fld[2]="pread(31,20)"
dict_nam[2]="€‹œ;;"
dict_fld[3]="iif(recno()=1,' ‹H… ',iif(val(pread(51,1))=0,'‚‚„ˆ’œ ','‘Œ’…’œ'))"
dict_nam[3]="€‚;„‘’“€"
dict_fld[4]="iif(LDEBET(),'‚‘…','H…’')"
dict_nam[4]="‘—…’€;„……’"
dict_fld[5]="iif(LKREDIT(),'‚‘…','H…’')"
dict_nam[5]="„‘’“€;Š…„ˆ’"
setcolor(c)
dbedit(Y_min+1,X_min+1,Y_max-1,X_max-1,dict_fld,"AdmFunc","",dict_nam)
use
setcolor(old_color)
restore screen from ThrPage
select(old_select)
return
*---------------------------------------------------------*
function AdmFunc
parameters mode,fld_ptr
private cur_field,key_last
private p_chet[CNT_CHET]
key_last=LASTKEY()
do case
case mode <= 3
  return 1
case LASTKEY() = 27
  return 0
case LASTKEY() = 13.or.isdata(key_last)
  if key_last <> 13
     keyboard chr(key_last)
  endif
  set cursor on
  do case
  case fld_ptr=1
   cur_field=pread(1,20)
   @ row(),col() get cur_field
   read
   if lastkey() != 27
    pwrite(cur_field,1,20)
   endif
  case fld_ptr=2
   cur_field=pread(31,20)
   @ row(),col() get cur_field
   read
   if lastkey() != 27
    pwrite(cur_field,31,20)
   endif
  case fld_ptr=3
   if recno()!=1
   if val(pread(51,1))=0
    pwrite("1",51,1)
   else
    pwrite("0",51,1)
   endif
   endif
  case fld_ptr=4
   if recno()!=1
   save scre
   setcolor(cmenu)
   BoxTenj(Y_srm-8,X_srm-8,Y_srm+8,X_srm+8,ibox)
   setcolor(cl)
   MSG='„……’'
   @ Y_srm-8,X_srm-len(MSG)/2 say MSG
   for i=1 to CNT_CHET/2
    p_chet[i]=val(pread(60+(i-1)*6,LEN_CHET))
    @ row()+1,X_srm-7 get p_chet[i]
    @ row(),col() say ' ³ '
    j=i+CNT_CHET/2
    p_chet[j]=val(pread(60+(j-1)*6,LEN_CHET))
    @ row(),col() get p_chet[j]
   next
   read
   if updated()
    for i=1 to CNT_CHET
     pwrite(str(p_chet[i]),60+(i-1)*6,LEN_CHET)
    next
   endif
   setcolor(c)
   rest scre
   endif
  case fld_ptr=5
   if recno()!=1
   save scre
   setcolor(cmenu)
   BoxTenj(Y_srm-8,X_srm-8,Y_srm+8,X_srm+8,ibox)
   setcolor(cl)
   MSG='Š…„ˆ’'
   @ Y_srm-8,X_srm-len(MSG)/2 say MSG
   for i=1 to CNT_CHET/2
    p_chet[i]=val(pread(300+(i-1)*6,LEN_CHET))
    @ row()+1,X_srm-7 get p_chet[i]
    @ row(),col() say ' ³ '
    j=i+CNT_CHET/2
    p_chet[j]=val(pread(300+(j-1)*6,LEN_CHET))
    @ row(),col() get p_chet[j]
   next
   read
   if updated()
    for i=1 to CNT_CHET
     pwrite(str(p_chet[i]),300+(i-1)*6,LEN_CHET)
    next
   endif
   setcolor(c)
   rest scre
   endif
  endcase
  set cursor off
case lastkey() = 22        && Ins
  if fragen('‚‚…‘’ˆ','H‚ƒ ‹œ‡‚€’…‹Ÿ')
   ParolNew()
   return 2
  endif
case lastkey() = 7         && Del
  if recno() != 1
   if fragen('“„€‹ˆ’œ','‹œ‡‚€’…‹Ÿ')
    delete
    pack
   endif
   return 2
  else
   WarnWrite(" € „ Œ ˆ H ˆ ‘ ’  € ’   €  “ „ € ‹ Ÿ ’ œ  H … ‹ œ ‡ Ÿ ")
  endif
case lastkey() = -3        && F4
  OpClDate(cur_path)
case lastkey() = -4        && F5
  dblpt1(dict_fld,dict_nam,TITL1)
endcase
return 1
*---------------------------------------------------------*
function MenuAdm
setcolor(cl)
@ Y_max,X_min+10 say ' Œ…:'
setcolor(cmenu)
MSG='INS'
@ row(),col() get MSG
@ row(),col() say '‘®§¤ âì'
MSG='Enter'
@ row(),col() get MSG
@ row(),col() say '‚¢®¤'
MSG='DEL'
@ row(),col() get MSG
@ row(),col() say '“¤ «¨âì'
MSG='F4'
@ row(),col() get MSG
@ row(),col() say '„ â '
MSG='F5'
@ row(),col() get MSG
@ row(),col() say '¥ç âì'
MSG='ESC'
@ row(),col() get MSG
@ row(),col() say '‚ëå®¤'
clear gets
return " "
*---------------------------------------------------------*
function LDebet
for i=1 to CNT_CHET
 if .not.empty(val(pread(60+(i-1)*6,LEN_CHET)))
  return .F.
 endif
next
return .T.
*---------------------------------------------------------*
function LKredit
for i=1 to CNT_CHET
 if .not.empty(val(pread(300+(i-1)*6,LEN_CHET)))
  return .F.
 endif
next
return .T.
*---------------------------------------------------------*
function ischetp
for i=1 to CNT_CHET
 if .not.empty(d_chet[i])
  return .T.
 endif
next
for i=1 to CNT_CHET
 if .not.empty(k_chet[i])
  return .T.
 endif
next
return .F.
*---------------------------------------------------------*
function debpravo
parameters n_chet
if is_adm
 return .T.
endif
if debempty()
 return .T.
endif
for i=1 to CNT_CHET
 if d_chet[i]=n_chet
  return .T.
 endif
next
return .F.
*---------------------------------------------------------*
function debempty
for i=1 to CNT_CHET
 if .not.empty(d_chet[i])
  return .F.
 endif
next
return .T.
*---------------------------------------------------------*
function krepravo
parameters n_chet
if is_adm
 return .T.
endif
if kreempty()
 return .T.
endif
for i=1 to CNT_CHET
 if k_chet[i]=n_chet
  return .T.
 endif
next
return .F.
*---------------------------------------------------------*
function kreempty
for i=1 to CNT_CHET
 if .not.empty(k_chet[i])
  return .F.
 endif
next
return .T.
*---------------------------------------------------------*
function chetpravo      && debpravo.and.krepravo
if A->DEBET=0.or.A->KREDIT=0
 return .T.
endif
if debpravo(A->DEBET).and.krepravo(A->KREDIT)
 return .T.
endif
WarnWrite(PMSG8)
return .F.
*---------------------------------------------------------*
function pread
parameters pnt,cnt
strcpyn(PAROL,cPAROL,len(PAROL))
strcoder(.F.,cPAROL,len(cPAROL))
return substr(cPAROL,pnt,cnt)
*---------------------------------------------------------*
procedure pwrite
parameters varv,pnt,cnt
strcpyn(PAROL,cPAROL,len(PAROL))
strcoder(.F.,cPAROL,len(cPAROL))
cPAROL=stuff(cPAROL,pnt,cnt,varv)
strcoder(.T.,cPAROL,len(cPAROL))
repl PAROL with cPAROL
return
*---------------------------------------------------------*
function ParolNew
parameters p_pparol,p_pname,p_ppravo,d_pschet1,d_pschet2,;
           d_pschet3,d_pschet4,d_pschet5,d_pschet6,k_pschet1,;
           k_pschet2,k_pschet3,k_pschet4,k_pschet5,k_pschet6
private dparol,ch
append blank
InitRandom()
dparol=''
for i=1 to LEN_PAROL
 ch=chr(1)
 do while .not.isdata(asc(ch))
  ch=random()
 enddo
 dparol=dparol+ch
next
dparol=stuff(dparol,1,20,space(20))
dparol=stuff(dparol,31,20,space(20))
dparol=stuff(dparol,51,1,"0")
repl I->FORMATC with 0
for i=1 to CNT_CHET
 dparol=stuff(dparol,60+(i-1)*6,LEN_CHET,str(I->FORMATC))
next
for i=1 to CNT_CHET
 dparol=stuff(dparol,300+(i-1)*6,LEN_CHET,str(I->FORMATC))
next
strcpyn(dparol,cPAROL,len(PAROL))
strcoder(.T.,cPAROL,len(cPAROL))
repl PAROL with cPAROL
commit
return .T.
*---------------------------------------------------------*
procedure OpClDate
parameters disk_path
private old_color
old_color = setcolor()
save scre
MsgWrite("—¨â î á¯¨á®ª  àå¨¢®¢...")
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
 aArhiv[i] = " "+Month[month(iArhiv[i])]+" "+str(year(iArhiv[i]),4)+" "+;
             iif(file(cur_path+"\"+str(year(iArhiv[i]),4)+"."+;
             strtran(str(month(iArhiv[i]),2)," ","0")+'\close.dbf'),;
             '‡€Š›’ ','’Š›’ ')
next
rest scre
save scre
setcolor(cdict)
BoxTenj(Y_srm-10,X_srm-12,Y_srm+10,X_srm+12,ibox)
setcolor(cl)
MSG=' €  • ˆ ‚ '
@ Y_srm-10,int(X_srm-len(MSG)/2)+1 say MSG
setcolor(cdict)
ptr=1
new_ptr=achoice(Y_srm-9,X_srm-11,Y_srm+9,X_srm+11,aArhiv,"","OpenClose",ptr)
is_close=file(use_path+'close.dbf')
rest scre
setcolor(old_color)
return
*---------------------------------------------------------*
function OpenClose
parameters mode,ptr
do case
case mode=0
 return 2
case lastkey()=27
 return 0
case lastkey()=13
 clos=cur_path+"\"+str(year(iArhiv[ptr]),4)+"."+strtran(str(month(iArhiv[ptr]),2)," ","0")+'\close.dbf'
 if file(clos)
  dele file &clos
 else
  memowrit(clos,"")
 endif
 aArhiv[ptr] = " "+Month[month(iArhiv[ptr])]+" "+str(year(iArhiv[ptr]),4)+" "+;
             iif(file(cur_path+"\"+str(year(iArhiv[ptr]),4)+"."+;
             strtran(str(month(iArhiv[ptr]),2)," ","0")+'\close.dbf'),;
             '‡€Š›’ ','’Š›’ ')
endcase
return 2
*---------------------------------------------------------*
function Pravo0
if pravo=1
 WarnWrite(PMSG5)
 return .F.
endif
if is_arhiv
 WarnWrite(PMSG4)
 return .F.
endif
if .not.is_adm
 WarnWrite(PMSG1)
 return .F.
endif
return .T.
*---------------------------------------------------------*
function Pravo1
if pravo=1
 WarnWrite(PMSG5)
 return .F.
endif
if is_arhiv
 WarnWrite(PMSG4)
 return .F.
endif
return .T.

