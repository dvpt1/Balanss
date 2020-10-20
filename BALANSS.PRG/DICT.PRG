*---------------------------------------------------------*
function GlossDict
private old_select
old_select=select()
setcolor(c)
DrawBox(Y_min,X_min,Y_max,X_max,ibox)
setcolor(cl)
TITL1=" è ã Ä ç  ë ó Ö í é Ç "
@ Y_min,int((X_min+X_max)/2-len(TITL1)/2) say TITL1
MenuGloss()
setcolor(c)
select 2
name_str=MainDIS
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
name_dbf=use_path+MainDIC
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
dbedit(Y_min+1,X_min+1,Y_max-1,X_max-1,dict_fld,"GlossFunc","",dict_nam)
rele dict_fld
rele dict_nam
use
select(old_select)
return " "
*---------------------------------------------------------*
function GlossFunc
parameters mode,fld_ptr
private cur_field,key_last
key_last=LASTKEY()
do case
case mode <= 3
case LASTKEY() = 27
  return 0
case LASTKEY() = 13.or.isdata(key_last)
 if .not.Pravo0()
  return 1
 endif
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
    name_dbf1=use_path+strline(&cur_field)+".DIC"
    name_dbf2=use_path+strline(new_D1)+".DIC"
    rename &name_dbf1 to &name_dbf2
    repl &cur_field with new_D1
   else
    go Nz
    WarnWrite(PMSG3)
   endif
  endif
 case fld_ptr=2
  set cursor on
  @ row(),col() get &cur_field
  read
  set cursor off
 case fld_ptr=3.or.fld_ptr=4
  set cursor on
  @ row(),col() get &cur_field
  read
  set cursor off
  repl &cur_field with strtran(&cur_field," ","0")
  if &cur_field.=="000"
   repl &cur_field with "   "
  endif
 case fld_ptr=5
  repl D_DOC with DictFind(D_DOC,'DOC_DOC','DOC_NAM','ÑéäìåÖçí',DocumDIC,'')
 case fld_ptr=6
  repl K_DOC with DictFind(K_DOC,'DOC_DOC','DOC_NAM','ÑéäìåÖçí',DocumDIC,'')
 endcase
 UnSetj()
 return 2
case lastkey() = 22        && Ins
 if InsGloss(".DIC",SubDIS)
  return 2
 endif
case lastkey() = -4        && F5
 dblpt1(dict_fld,dict_nam,TITL1)
case lastkey() = -5        && F6
 if FindGloss(dict_fld[fld_ptr],dict_nam[fld_ptr],Y_srm-2,X_srm-30,Y_srm+2,X_srm+30)
  return 2
 endif
case lastkey() = -6        && F7
 if debpravo(D1).or.krepravo(D1)
  SubDict()
 else
  WarnWrite(PMSG8)
 endif
case lastkey() = -7        && F8
 if SortGloss(dict_fld[fld_ptr],dict_nam[fld_ptr],MainDIC,2)
  return 2
 endif
case lastkey() = 7         && Del
 if DelGloss(".DIC")
  return 2
 endif
endcase
if old_dict!=reccount()
 old_dict=reccount()
 return 2
endif
return 1
*---------------------------------------------------------*
function MenuGloss
PMenuOpen()
PMenuIns()
PMenuEnter()
PMenuDel()
PMenuLpt1()
PMenuFind()
PMenuList()
PMenuSort()
PMenuESC()
PMenuClose()
return " "
*---------------------------------------------------------*
function InsGloss
parameters var_ext,file_str,not_pra
if pcount() < 3
 if .not.Pravo0()
  return .F.
 endif
endif
Nz=recno()
locate for D1=0
if .not.found()
 go Nz
 if insert()
  name_dbf=use_path+strline(B->D1)+var_ext
  if .not.file(name_dbf)
   select 3
   creat &name_dbf from &file_str
   append blank
   use
   select 2
  endif
  return .T.
 endif
else
 go Nz
 WarnWrite(PMSG7)
endif
return .F.
*---------------------------------------------------------*
function InsGloss1
if .not.Pravo1()
 return .F.
endif
if type("D1")!="U"
 Nz=recno()
 locate for D1=0
 if .not.found()
  go Nz
  if insert()
   return .T.
  endif
 else
  go Nz
  WarnWrite(PMSG7)
 endif
else
 insert()
 return .T.
endif
return .F.
*---------------------------------------------------------*
function DelGloss
parameters var_ext
if .not.Pravo0()
 return .F.
endif
if IsSetj()
 return .F.
endif
save screen
if Yes_Or_No(PMSGDEL1,PMSGDEL2,2)
 MsgWrite(PMSGDEL3)
 name_dbf=use_path+strline(B->D1)+var_ext
 dele file &name_dbf
 delete
 pack
 go top
 restore screen
 return .T.
endif
restore screen
return .F.
*---------------------------------------------------------*
function DelGloss1
if .not.Pravo0()
 return .F.
endif
if IsSetj()
 return .F.
endif
save screen
if Yes_Or_No(PMSGDEL1,PMSGDEL2,2)
 MsgWrite(PMSGDEL3)
 delete
 pack
 go top
 restore screen
 return .T.
endif
restore screen
return .F.
*---------------------------------------------------------*
function FindGloss
parameters fld_fld,fld_nam,Y1,X1,Y2,X2
private old_color,var_find,otwet,Nz
old_color = setcolor()
save screen
setcolor(cfr)
BoxTenj(Y1,X1,Y2,X2,ibox)
setcolor(cl)
MSG=' à ë ä Ä í ú '
@ Y1,int((X1+X2)/2-len(MSG)/2) say MSG
setcolor(cfr)
var_find=&fld_fld
@ Y1+2,X1+2 say fld_nam
set cursor on
@ row(),col()+2 get var_find
read
set cursor off
if lastkey() != 27
 MsgWrite(PMSGFIND1)
 Nz=recno()
 skip
 do case
 case type("var_find")="L"
  if var_find
   locate rest for &fld_fld
  else
   locate rest for .not.&fld_fld
  endif
 case type("var_find")="C"
  if .not.empty(var_find)
   var_find=alltrim(var_find)
  endif
  locate rest for at(var_find,&fld_fld.)!=0
 othe
  locate rest for var_find=&fld_fld
 endcase
 if found()
  restore screen
  setcolor(old_color)
  return .T.
 else
  ??chr(7)
  restore screen
  WarnWrite(PMSGFIND2)
  go Nz
 endif
endif
restore screen
setcolor(old_color)
return .F.
*---------------------------------------------------------*
function SubDict
private old_select,name_str,name_dbf,name_ntx
gloss=gloss+1
Save Screen to FouPage
old_select=select()
set color to &c
DrawBox(Y_min,X_min,Y_max,X_max,ibox)
set color to &cl
TITL2=" è ã Ä ç  ë ì Å ë ó Ö í Ä  "+'"'+ltrim(str(B->D1))+'"'
@ Y_min,int((X_min+X_max)/2-len(TITL2)/2) say TITL2
MenuSict()
set color to &c
select 3
name_str=SubDIS
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
name_dbf=use_path+strline(B->D1)+".DIC"
if .not.file(name_dbf)
 select 3
 create &name_dbf from &name_str
 append blank
 use
endif
select 3
if is_setj
 use &name_dbf SHARED
else
 use &name_dbf
endif
old_sict=reccount()
dbedit(Y_min+1,X_min+1,Y_max-1,X_max-1,dict_fld,"SictFunc","",dict_nam)
rele dict_fld
rele dict_nam
if lastrec()=0
 use
 dele file &name_dbf
else
 use
endif
select(old_select)
Restore Screen from FouPage
gloss=gloss-1
return " "
*---------------------------------------------------------*
function SictFunc
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
 if InsGloss1()
  return 2
 endif
case lastkey() = -4        && F5
 dblpt1(dict_fld,dict_nam,TITL2)
case lastkey() = -5        && F6
 if FindGloss(dict_fld[fld_ptr],dict_nam[fld_ptr],Y_srm-2,X_srm-30,Y_srm+2,X_srm+30)
  return 2
 endif
case lastkey() = -7        && F8
 if SortGloss(dict_fld[fld_ptr],dict_nam[fld_ptr],strline(B->D1)+".DIC",3)
  return 2
 endif
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
function MenuSict
PMenuOpen()
PMenuIns()
PMenuEnter()
PMenuDel()
PMenuLpt1()
PMenuFind()
PMenuSort()
PMenuESC()
PMenuClose()
return " "
*---------------------------------------------------------*
function SortGloss
parameters fld_fld,fld_nam,file_nam,sel
save screen
if Yes_Or_No(PMSGSORT1,PMSGSORT2+'"'+alltrim(fld_nam)+'"',2)
 MsgWrite(PMSGSORT3)
 sort on &fld_fld to TEMP.DBF
 use
 name_dbf=use_path+file_nam
 dele file &name_dbf
 copy file TEMP.DBF to &name_dbf
 dele file TEMP.DBF
 select &sel
 if is_setj
  use &name_dbf SHARED
 else
  use &name_dbf
 endif
 rest screen
 return .T.
endif
rest screen
return .F.
