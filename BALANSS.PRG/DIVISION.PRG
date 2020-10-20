*---------------------------------------------------------*
function Division
parameters lst_dbf,lst_ext,lst_frm
private old_select,Nz1
if (.not.debpravo(lst_dbf)).and.(.not.krepravo(lst_dbf))
 WarnWrite(PMSG8)
 return 1
endif
old_select=select()
setcolor(c)
DrawBox(Y_min,X_min,Y_max,X_max,ibox)
setcolor(cl)
TITL1=" ëèàëéä èéÑêÄáÑÖãÖçàâ "
@ Y_min,int((X_min+X_max)/2-len(TITL1)/2) say TITL1
MenuDivis()
setcolor(c)
select 1
Nz1=recno()
set filter to
go top
select 7
use_dbf=use_path+OborotDBF
if is_setj
 use &use_dbf SHARED
else
 use &use_dbf
endif
use_ntx=use_path+OborotNTX
set index to &use_ntx
set relation to NUMBER into Balans
select 2
name_str=DivisionSTR
use &name_str SHARED
j=lastrec()
declare dict_fld[j]
declare dict_nam[j]
for i=1 to j
 dict_fld[i]=alltrim(FIELD_NAME)
 dict_nam[i]=alltrim(NAME_FIELD)
 skip
next
use
name_dbf=use_path+strline(lst_dbf)+".DIV"
if .not.file(name_dbf)
 select 2
 create &name_dbf from &name_str
 use
endif
select 2
if is_setj
 use &name_dbf SHARED
else
 use &name_dbf
endif
old_dict=reccount()
dbedit(Y_min+1,X_min+1,Y_max-1,X_max-1,dict_fld,"DivisFunc","",dict_nam)
rele dict_fld
rele dict_nam
use
select 7
use
CloseDBF()
OpenDBF()
InitFilter()
go Nz1
return " "
*---------------------------------------------------------*
function DivisFunc
parameters mode,fld_ptr
private cur_field,key_last
key_last=LASTKEY()
do case
case mode <= 3
case LASTKEY() = 27
  return 0
case LASTKEY() = 13.or.isdata(key_last)
 if .not.OnSetj()
  return 1
 endif
 if key_last <> 13
  keyboard chr(key_last)
 endif
 cur_field=dict_fld[fld_ptr]
 do case
 case fld_ptr=1
  new_D1=&cur_field
  set cursor on
  @ row(),col() get new_D1
  read
  set cursor off
  if new_D1!=&cur_field
   Nz=recno()
   locate for new_D1=&cur_field
   if .not.found()
    go Nz
    name_dbf1=use_path+strline(&cur_field)+"."+lst_ext
    name_dbf2=use_path+strline(new_D1)+"."+lst_ext
    rename &name_dbf1 to &name_dbf2
    repl &cur_field with new_D1
   else
    go Nz
    WarnWrite(PMSG3)
   endif
  endif
 othe
  set cursor on
  @ row(),col() get &cur_field
  read
  set cursor off
 endcase
 UnSetj()
 return 2
case lastkey() = 22        && Ins
 if InsGloss("."+lst_ext,QuanSTR,.F.)
  return 2
 endif
case lastkey() = -1        && F2
 Oborotd()
case lastkey() = -4        && F5
 DivisPrint(Y_srm-5,X_srm-18,Y_srm+5,X_srm+18)
case lastkey() = -5        && F6
 if FindGloss(dict_fld[fld_ptr],dict_nam[fld_ptr],Y_srm-2,X_srm-30,Y_srm+2,X_srm+30)
  return 2
 endif
case lastkey() = -6        && F7
 if B->D1 != 0
  DivDict()
 else
  WarnWrite("çéåÖê ØÆ§‡†ß§•´•≠®Ô ≠•¨Æ¶•‚ °Î‚Ï ‡†¢•≠ ≠„´Ó...")
 endif
case lastkey() = -7        && F8
 if SortGloss(dict_fld[fld_ptr],dict_nam[fld_ptr],strline(lst_dbf)+".DIV",2)
  return 2
 endif
case lastkey() = 7         && Del
 if DelGloss("."+lst_ext)
  return 2
 endif
endcase
if old_dict!=reccount()
 old_dict=reccount()
 return 2
endif
return 1
*---------------------------------------------------------*
function MenuDivis
PMenuOpen()
PMenuIns()
PMenuEnter()
PMenuDel()
PMenuObor()
PMenuLpt1()
PMenuFind()
PMenuList()
PMenuSort()
PMenuESC()
PMenuClose()
return " "
*---------------------------------------------------------*
procedure DivisPrint
parameters Y1,X1,Y2,X2
private FivePage,FouPage,otwet,old_color,old_select,old_Nz
old_select=select()
old_Nz=recno()
old_color=setcolor()
save scre to FivePage
setcolor(cmenu)
BoxTenj(Y1,X1,Y2,X2,tbox)
otwet=1
do while (otwet != 0)
  setcolor(cmenu)
  @ Y1+1,   X1+1 prompt " ëØ®·Æ™ ØÆ§‡†ß§•´•≠®©              "
  @ row()+1,X1+1 prompt " Ç•§Æ¨Æ·‚Ï-Æ‡§•‡ ØÆ §•°•‚„         "
  @ row()+1,X1+1 prompt " Ç•§Æ¨Æ·‚Ï-Æ‡§•‡ ØÆ ™‡•§®‚„        "
  @ row()+1,X1+1 prompt " é°Æ‡Æ‚≠†Ô ¢•§Æ¨Æ·‚Ï ØÆ ¨†‚•‡®†´†¨ "
  @ row()+1,X1+1 prompt " éØ®·Ï ¨†‚•‡®†´Æ¢                  "
  @ row()+1,X1+1 prompt " Ä¨Æ‡‚®ß†Ê®Æ≠≠†Ô ¢•§Æ¨Æ·‚Ï         "
  @ row()+1,X1+1 prompt " Ç•§Æ¨Æ·‚Ï ØÆ °†´†≠·Æ¢Æ© ·‚Æ®¨Æ·‚® "
  @ row()+1,X1+1 prompt " Ç•§Æ¨Æ·‚Ï ØÆ †¨Æ‡‚®ß†Ê®®          "
  @ row()+1,X1+1 prompt " éØ®·Ï Æ·≠Æ¢≠ÎÂ ·‡•§·‚¢            "
  menu to otwet
  otwep=1
  if otwet!=0
  endif
  if otwep!=0.or.otwep!=3
   Save Screen to FouPage
   do case
   case otwet=1
    dblpt1(dict_fld,dict_nam,TITL1)
   case otwet=2
    VedomLPT1(.T.)
   case otwet=3
    VedomLPT1(.F.)
   case otwet=4
    Oborot1PRN() 
   case otwet=5
    Oborot2PRN() 
   case otwet=6
    Oborot3PRN()
   case otwet=7
    Oborot4PRN() 
   case otwet=8
    Oborot5PRN()
   case otwet=9
    Oborot6PRN()
   endcase
   Restore Screen from FouPage
  endif
enddo
setcolor(old_color)
Restore Screen from FivePage
select(old_select)
go old_Nz
return
*---------------------------------------------------------*
function DivDict
private old_select,name_str,name_dbf,name_ntx
gloss=gloss+1
Save Screen to FouPage
old_select=select()
set color to &c
DrawBox(Y_min,X_min,Y_max,X_max,ibox)
set color to &cl
TITL2=" ë è à ë é ä  "+'"'+alltrim(B->D2)+'"'
@ Y_min,int((X_min+X_max)/2-len(TITL2)/2) say TITL2
MenuDiv()
set color to &c
select 3
name_str=QuanSTR
use &name_str SHARED
j=lastrec()
declare dict_fld[j]
declare dict_nam[j]
for i=1 to j
 dict_fld[i]=alltrim(FIELD_NAME)
 dict_nam[i]=alltrim(NAME_FIELD)
 skip
next
use
name_dbf3=use_path+strline(B->D1)+"."+lst_ext
if .not.file(name_dbf3)
 select 3
 create &name_dbf3 from &name_str
 use
endif
select 3
if is_setj
 use &name_dbf3 SHARED
else
 use &name_dbf3
endif
old_sict=reccount()
dbedit(Y_min+1,X_min+1,Y_max-1,X_max-1,dict_fld,"DivFunc","",dict_nam)
use
select(old_select)
Restore Screen from FouPage
gloss=gloss-1
return " "
*---------------------------------------------------------*
function DivFunc
parameters mode,fld_ptr
private key_last
key_last=LASTKEY()
do case
case mode <= 3
case LASTKEY() = 27
  return 0
case LASTKEY() = 13.or.isdata(key_last)
 if .not.Pravo1()
  return 1
 endif
 if key_last <> 13
  keyboard chr(key_last)
 endif
 DivInp()
 return 2
case lastkey() = 22        && Ins
 if .not.Pravo1()
  return 1
 endif
 if insert()
  DivInp()
  return 2
 endif
case lastkey() = -4        && F5
 SpravPrint(Y_srm-3,X_srm-14,Y_srm+3,X_srm+14)
case lastkey() = -5        && F6
 if FindGloss(dict_fld[fld_ptr],dict_nam[fld_ptr],Y_srm-2,X_srm-35,Y_srm+2,X_srm+35)
  return 2
 endif
case lastkey() = -6        && F7
 Oborota()
case lastkey() = -7        && F8
 if SortGloss(dict_fld[fld_ptr],dict_nam[fld_ptr],strline(B->D1)+"."+lst_ext,3)
  return 2
 endif
case lastkey() = -8        && F9
 ComDivis()
case lastkey() = 7         && Del
 if DelGloss1()
  return 2
 endif
endcase
if old_sict!=reccount()
 old_sict=reccount()
 return 2
endif
return 1
*---------------------------------------------------------*
function MenuDiv
PMenuOpen()
PMenuIns()
PMenuEnter()
PMenuDel()
PMenuLpt1()
PMenuFind()
PMenuList()
PMenuSort()
PMenuCalc()
PMenuESC()
PMenuClose()
return " "
*---------------------------------------------------------*
procedure DivInp
private old_color,InpPage
if .not.OnSetj()
 return
endif
old_color = setcolor(cinp)
save screen to InpPage
do case
case lst_frm=1
 Ymax=Y_max-6
case lst_frm=2
 Ymax=Y_max-8
case lst_frm=3
 Ymax=Y_max-10
case lst_frm=4
 Ymax=Y_max-8
othe
 Ymax=Y_max-6
endcase
BoxTenj(Y_min+1,X_min+3,Ymax,X_max-2,ibox)
setcolor(cl)
MSG=' ä Ä ê í é ó ä Ä  ì ó Ö í Ä '
@ Y_min+1,int((X_min+X_max)/2-len(MSG)/2) say MSG
setcolor(cinp)
@ Y_min+ 2,X_min+4 say "ÕÕÕà≠¢.NÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕç Ä à å Ö ç é Ç Ä ç à ÖÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÖÑ.àáå.ÕÕÕ"
@ row()+ 1,X_min+4 say "                                                                         "
@ row(),X_min+5 get NUMB
@ row(),col()+2 get NAME
@ row(),col()+2 get MEAS
@ row()+ 1,X_min+4 say "ÕÕÕÕÕÕäéãàóÖëíÇéÕÕÕÕÕÕÕÕÕÕÕÕÑ Ö Å Ö íÕÕÕÕÕÕÕÕÕÕÕÕä ê Ö Ñ à íÕÕÕÕÕÕÕÕÕÕÕÕÕ"
@ row()+ 1,X_min+4 say "                                                                         "
@ row(),X_min+7 get QUAN
@ row(),col()+8 get DEBET
@ row(),col()+8 get KREDIT
@ row()+ 1,X_min+4 say "ÕÕÕÕÕÕè ê à ï é Ñ ç õ â  ë ó Ö í / ë ì Å ë ó Ö íÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ"
@ row()+ 1,X_min+4 say "                                                                         "
@ row(),X_min+ 7 get EXPENS
@ row(),col()+1 say "/"
@ row(),col()+2 get SUBEXP
@ row()+ 1,X_min+4 say "ÕÕÕÕÕÕê Ä ë ï é Ñ ç õ â  ë ó Ö í / ë ì Å ë ó Ö íÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ"
@ row()+ 1,X_min+4 say "                                                                         "
@ row(),X_min+ 7 get RECEIP
@ row(),col()+1 say "/"
@ row(),col()+2 get SUBREC
@ row()+ 1,X_min+4 say "ÕÕÕÕÑÄíÄ çÄóÄãÄÕÕÕÕÕÑÄíÄ ùäëèãìÄíÄñààÕÕÕÕÕÑÄíÄ èÖêÖÑÄóàÕÕÕÕÕÑÄíÄ äéçñÄÕÕÕ"
@ row()+ 1,X_min+4 say "                                                                         "
@ row(),X_min+ 7 get DATBEG
@ row(),col()+12 get DATEKS
@ row(),col()+12 get DATPER
@ row(),col()+10 get DATEND
if lst_frm=1
@ row()+ 1,X_min+4 say "ÕÕÕÕéÅôàâ èêéÅÖÉÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕíÖäìôàâ èêéÅÖÉÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ"
@ row()+ 1,X_min+4 say "                                                                         "
@ row(),X_min+ 7 get PROBEG
@ row(),col()+32 get PROTEK
endif
if lst_frm=1.or.lst_frm=2.or.lst_frm=4
@ row()+ 1,X_min+4 say "ÕÕÕÕòàîê ÄåéêíàáÄñààÕÕÕÕÕçéêåÄ ÄåéêíàáÄñààÕÕÕÕäéùîîàñàÖçí èÖêÖéñÖçäàÕÕÕÕÕ"
@ row()+ 1,X_min+4 say "                                                                         "
@ row(),X_min+7 get ASHIFR
@ row(),col()+16 get ANORMA
@ row(),col()+16 get KOEF
endif
@ row()+ 1,X_min+4 say "ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕè ê à å Ö ó Ä H à ÖÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ"
@ row()+ 1,X_min+4 say "                                                                         "
@ row(),X_min+5 get NOTES
set cursor on
read
set cursor off
restore screen from InpPage
setcolor(old_color)
UnSetj()
return
*---------------------------------------------------------*
procedure ComDivis
private old_Nz3,otwet,old_com
aOptions={" Ç ë Ö ï "," íÖäìôÖÉé "," éíåÖçÄ "}
otwet=frags(" à á ç é ë ","ç Ä ó à ë ã à í ú ?",aOptions)
if otwet=0.or.otwet=3
 return
endif
save scre
MsgWrite('ç†Á®·´ÔÓ †¨Æ‡‚®ß†Ê®Ó...')
select 3
old_Nz3=recno()
if otwet = 1
 go top
endif
select 4
use_dic=use_path+RaschetDIC
use &use_dic SHARED
select 3
do while .not.eof()
 MsgWrite('ç†Á®·´ÔÓ †¨Æ‡‚®ß†Ê®Ó...'+'"'+alltrim(NAME)+'"')
 select 4
 locate for C->EXPENS=D->EXPENS.and.C->SUBEXP=D->SUBEXP
 do while found()
  select 3
  dSUMMA=formula(translate(D->SUMMA))
  if dSUMMA != 0
   old_com=.F.
   select 7
   locate for G->DEBKRE=lst_dbf.and.G->NUMB=C->NUMB.and.;
              G->NAME==C->NAME.and.G->MEAS==C->MEAS
   do while found()
    select 1
    if A->DEBET=D->DEBET.and.A->SUBDEB=D->SUBDEB.and.;
       A->KREDIT=D->KREDIT.and.A->SUBKRE=D->SUBKRE
     old_com=.T.
     exit
    endif
    select 7
    continue
   enddo
   select 1
   if .not.old_com
    go bottom
    n=NUMBER
    append blank
    repl A->NUMBER with n+1
    commit
   endif
   rlock()
   repl A->DEBET  with D->DEBET
   repl A->SUBDEB with D->SUBDEB
   repl A->KREDIT with D->KREDIT
   repl A->SUBKRE with D->SUBKRE
   repl A->SUMMA with dSUMMA
   unlock
   ComProtoc()
   select 7
   if .not.old_com
    append blank
    repl G->NUMBER with A->NUMBER
    commit
   endif
   rlock()
   repl G->DEBKRE with lst_dbf
   repl G->NUMB with C->NUMB
   repl G->NAME with C->NAME
   repl G->MEAS with C->MEAS
   do case
   case C->EXPENS=D->DEBET
    repl G->EXPENSES with B->D1
   case C->RECEIP=D->KREDIT
    repl G->RECEIPTS with B->D1
   endcase
   unlock
  endif
  select 4
  continue
 enddo
 select 3
 if otwet=1
  skip
 else
  exit
 endif
enddo
select 4
use
select 3
go old_Nz3
rest scre
return
*---------------------------------------------------------*
procedure Oborota
private old_color,FivePage,otwet
old_color = setcolor()
Save Screen to FivePage
setcolor(c)
DrawBox(Y_min,X_min,Y_max,X_max,ibox)
setcolor(cl)
TITL3=" é Å é ê é í ç Ä ü  Ç Ö Ñ é å é ë í ú "
@ Y_min,int((X_min+X_max)/2-len(TITL3)/2) say TITL3
MenuOboro()
setcolor(c)
select 7
set filter to lst_dbf=G->DEBKRE.and.(B->D1=EXPENSES.or.B->D1=RECEIPTS).and.;
              C->NUMB=G->NUMB.and.C->NAME=G->NAME.and.C->MEAS=G->MEAS
go top
declare obor_fld[5]
declare obor_nam[5]
obor_fld[1]="substr(G->NAME,1,25)"
obor_nam[1]="ç†®¨•≠Æ¢†≠®•"
obor_fld[2]="G->MEAS"
obor_nam[2]="Ö§.®ß¨."
obor_fld[3]="iif(G->EXPENSES=B->D1,'+','-')"
obor_nam[3]=""
obor_fld[4]="G->QUAN"
obor_nam[4]="äÆ´®Á•·‚¢Æ"
obor_fld[5]="A->SUMMA"
obor_nam[5]="ë„¨¨†"
old_obor=reccount()
dbedit(Y_min+1,X_min+1,Y_max-1,X_max-1,obor_fld,"OborFunc","",obor_nam)
select 7
set filter to
go top
select 3
setcolor(old_color)
Restore Screen from FivePage
return
*---------------------------------------------------------*
function OborFunc
parameters mode,fld_ptr
do case
case mode <= 3
case LASTKEY() = -4
 dblpt1(obor_fld,obor_nam,TITL3)
case lastkey() = 22        && Ins
 Quan(Y_srm-3,X_srm-13,Y_srm-2,X_srm+13,.F.)
case LASTKEY() = 13
 Quan(Y_srm-3,X_srm-13,Y_srm-2,X_srm+13,.T.)
case lastkey() =  7        && Del
 DelOborot()
 return 2
case LASTKEY() = 27
 return 0
endcase
if old_obor!=reccount()
 old_obor=reccount()
 return 2
endif
return 1
*---------------------------------------------------------*
function MenuOboro
PMenuOpen()
PMenuIns()
PMenuEnter()
PMenuDel()
PMenuLpt1()
PMenuESC()
PMenuClose()
return " "
*---------------------------------------------------------*
procedure Oborotd
private old_color,FivePage,otwet
old_color = setcolor()
Save Screen to FivePage
setcolor(c)
DrawBox(Y_min,X_min,Y_max,X_max,ibox)
setcolor(cl)
TITL3=" é Å é ê é í ç Ä ü  Ç Ö Ñ é å é ë í ú "
@ Y_min,int((X_min+X_max)/2-len(TITL3)/2) say TITL3
MenuObord()
setcolor(c)
select 7
set filter to lst_dbf=G->DEBKRE
go top
declare obor_fld[9]
declare obor_nam[9]
obor_fld[1]="G->NUMB"
obor_nam[1]="çÆ¨•‡;;"
obor_fld[2]="G->NAME"
obor_nam[2]="ç†®¨•≠Æ¢†≠®•;;"
obor_fld[3]="G->MEAS"
obor_nam[3]="Ö§.;®ß¨.;"
obor_fld[4]="G->QUAN"
obor_nam[4]="äÆ´®Á•·‚¢Æ;;"
obor_fld[5]="A->SUMMA"
obor_nam[5]="ë„¨¨†;;"
obor_fld[6]="G->EXPENSES"
obor_nam[6]="è‡®ÂÆ§;;"
obor_fld[7]="G->RECEIPTS"
obor_nam[7]="ê†·ÂÆ§;;"
obor_fld[8]="A->DEBET"
obor_nam[8]="Ñ•°•‚;;"
obor_fld[9]="A->KREDIT"
obor_nam[9]="äp•§®‚;;"
old_obor=reccount()
dbedit(Y_min+1,X_min+1,Y_max-1,X_max-1,obor_fld,"OborFund","",obor_nam)
select 7
set filter to
go top
select 2
setcolor(old_color)
Restore Screen from FivePage
return
*---------------------------------------------------------*
function OborFund
parameters mode,fld_ptr
do case
case mode <= 3
case LASTKEY() = -4
 dblpt1(obor_fld,obor_nam,TITL3)
case (lastkey() = -3)       && F4
 return Orfograd()
case (lastkey() = -5)       && F6
 FindObord(fld_ptr,Y_srm-2,X_srm-35,Y_srm+2,X_srm+35)
case LASTKEY() = 13
 Quad(Y_srm-3,X_srm-13,Y_srm-2,X_srm+13)
case lastkey() =  7        && Del
 DelOborot()
 return 2
case LASTKEY() = 27
 return 0
endcase
if old_obor!=reccount()
 old_obor=reccount()
 return 2
endif
return 1
*---------------------------------------------------------*
function FindObord
parameters fld_ptr,Y1,X1,Y2,X2
if type("old_ptd")="U"
 public old_ptd,old_fino
 old_ptd=0
 old_fino=""
endif
private var_find,var_fld,otwet
private old_color
old_color = setcolor()
save screen to InpPage
set color to &cfr
BoxTenj(Y1,X1,Y2,X2,ibox)
set color to &cl
MSG='  à ë ä Ä í ú '
@ Y1,int((X1+X2)/2-len(MSG)/2) say MSG
set color to &cfr
var_fld=obor_fld[fld_ptr]
if fld_ptr = old_ptd
 var_find=old_fino
else
 var_find=nulvar(&var_fld)
endif
set cursor on
@ Y1+2,(X_srm-int(lenvar(var_find)/2)) get var_find
read
set cursor off
if type("var_find")="C"
 var_fnd=alltrim(var_find)
else
 var_fnd=var_find
endif
if lastkey() != 27
  MsgWrite("ë•©Á†· ØÆ®È•¨...")
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
    WarnWrite("èÆ®·™ ≠• „§†Á•≠...")
    go Nz
  endif
endif
old_ptd=fld_ptr
old_fino=var_find
restore screen from InpPage
set color to &old_color
return " "
*---------------------------------------------------------*
function MenuObord
PMenuOpen()
PMenuEnter()
PMenuDel()
PMenuOrfo()
PMenuLpt1()
PMenuFind()
PMenuESC()
PMenuClose()
return " "
*---------------------------------------------------------*
procedure Orfograd
private Nz,Nz7,i,var,fld,is_debet,is_subdeb,is_kredit,is_subkre
private old_color
if eof()
 return
endif
select 2
if reccount()!=0
 save screen
 select 7
 Nz7=recno()
 select 2
 decl expenses[reccount()]
 Nz=recno()
 go top
 do while .not.eof()
  expenses[recno()]=D1			&& °•p„ ·Ø®·Æ™ ØÆ§p†ß§•´•≠®©
  MsgWrite("èpÆ¢•pÔÓ ØÆ§p†ß§•´•≠®• - "+B->D2)
  name_dbf=use_path+strline(B->D1)+"."+lst_ext
  select 3
  use &name_dbf SHARED
  select 7
  go top
  do while .not.eof()
   if G->EXPENSES=B->D1.or.G->RECEIPTS=B->D1
    select 3
    locate for G->NUMB=C->NUMB.and.G->NAME==C->NAME.and.G->MEAS==C->MEAS
    if .not.found()
     append blank
     repl C->NUMB with G->NUMB
     repl C->NAME with G->NAME
     repl C->MEAS with G->MEAS
    endif
    select 7
   endif
   skip
  enddo
  select 3
  use
  select 2
  skip
 enddo
 go Nz
 is_debet=.F.
 is_kredit=.F.
 is_quan=.F.
 is_chet=.F.
 select 7
 go Nz7
 NN=1
 do while .not.eof()
   MsgWrite("èpÆ¢•pÔÓ Æ°ÆpÆ‚ NN "+alltrim(str(NN))+" ESC - éíåÖHÄ...")
   NN++
   if A->DEBET=lst_dbf
    if ascan(M->expenses,G->EXPENSES)=0
     is_debet=.T.
    endif
   endif
   if A->KREDIT=lst_dbf
    if ascan(M->expenses,G->RECEIPTS)=0
     is_kredit=.T.
    endif
   endif
   if A->DEBET=lst_dbf.OR.A->KREDIT=lst_dbf
    if QUAN=0
     is_quan=.T.
    endif
   else
    if QUAN!=0
     is_chet=.T.
    endif
   endif
   if G->EXPENSES=0.AND.G->RECEIPTS=0
    is_debet=.T.
    is_kredit=.T.
   endif
   if G->EXPENSES!=0.AND.G->RECEIPTS!=0
    if QUAN!=0
     if A->DEBET!=lst_dbf.OR.A->KREDIT!=lst_dbf
      is_chet=.T.
     endif
    endif
   endif
   if is_debet.OR.is_kredit.OR.is_quan.OR.is_chet
    exit
   endif
   if inkey()=27
    exit
   endif
   skip
 enddo
 if is_debet.OR.is_kredit.OR.is_quan.OR.is_chet
  old_color = setcolor(cw)
  BoxTenj(Y_srm-5,X_srm-25,Y_srm+5,X_srm+25,ibox)
  if is_debet
   @ Y_srm-3,X_srm-23 say "HÖÇÇÖÑÖH HéåÖê èéÑêÄáÑÖãÖHàü èé ÑÖÅÖíì"
  endif
  if is_kredit
   @ Y_srm-1,X_srm-23 say "HÖÇÇÖÑÖH HéåÖê èéÑêÄáÑÖãÖHàü èé äêÖÑàíì"
  endif
  if is_quan
   @ Y_srm+1,X_srm-23 say "HÖÇÇÖÑÖHé äéãàóÖëíÇé"
  endif
  if is_chet
   @ Y_srm+3,X_srm-23 say "HÖÇÖêçé ëÑÖãÄçÄ èêéÇéÑäÄ"
  endif
  setcolor(old_color)
  inkey(0)
 else
  if lastkey()!=27
   MsgWrite("èêéÇÖêäéâ HÖÇõüÇãÖHé éòàÅéä Ç éÅéêéíÄï")
   inkey(0)
  endif
 endif
 restore screen
else
 select 7
 return 1
endif
select 7
return 2
*---------------------------------------------------------*
procedure DelOborot
private old_select
save screen
if Yes_Or_No(PMSGDEL1,'é ° Æ ‡ Æ ‚ ?',2)
 MsgWrite(PMSGDEL3)
 old_select=select()
 if OnSetj()
 select 1
 if OnSetj()
 if !FUTURE
  if I->ISDEB
   if DEBET=I->DEB51
    if I->ISSUB
     if SUBDEB=I->SUB51
      repl I->RECSUM with I->RECSUM-SUMMA
      repl I->SALEND with I->SALBEG+I->RECSUM-I->EXPSUM
     endif
    else
     repl I->RECSUM with I->RECSUM-SUMMA
     repl I->SALEND with I->SALBEG+I->RECSUM-I->EXPSUM
    endif
   endif
   if KREDIT=I->DEB51
    if I->ISSUB
     if SUBKRE=I->SUB51
      repl I->EXPSUM with I->EXPSUM-SUMMA
      repl I->SALEND with I->SALBEG+I->RECSUM-I->EXPSUM
     endif
    else
     repl I->EXPSUM with I->EXPSUM-SUMMA
     repl I->SALEND with I->SALBEG+I->RECSUM-I->EXPSUM
    endif
   endif
  else
    repl I->SALEND with I->SALEND-SUMMA
  endif
 endif
 repl SUMMA with 0
 select(old_select)
 repl NUMBER with 0
 repl DEBKRE with 0
 repl NUMB with 0
 repl NAME with ""
 repl MEAS with ""
 repl QUAN with 0
 repl EXPENSES with 0
 repl RECEIPTS with 0
 select 1
 UnSetj()
 endif
 select(old_select)
 UnSetj()
 go top
 endif
endif
restore screen
return
