*----------------------------------------------*
procedure Docum
private old_color,InpPage,old_Nz
old_color = setcolor(cinp)
save screen to InpPage
select 5
use_dbf=use_path+DocumDIC
if is_setj
 use &use_dbf SHARED
else
 use &use_dbf
endif
setcolor(cmenu)
BoxTenj(Y_srm-5,X_srm-15,Y_srm+5,X_srm+15,ibox)
setcolor(cl)
MSG=' Д О К У М Е Н Т '
@ Y_srm-5,X_srm-len(MSG)/2 say MSG
setcolor(cmenu)
decl dbmenu[1]
dbmenu[1]="' '+DOC_NAM+' '"
dbedit(Y_srm-4,X_srm-14,Y_srm+4,X_srm+14,dbmenu,"dbmfunc","","","","","")
rele dbmenu
file_name=alltrim(E->DOC_DOC)
doc_name=alltrim(E->DOC_NAM)
select 5
use
if lastkey()=13
 MsgWrite("П О Д Г О Т А В Л И В А Ю  Д О К У М Е Н Т")
 Dark = 0
 name_str=use_path+file_name+'.STR'
 name_dbf=use_path+file_name+'.DBF'
 name_pic=memoread(use_path+file_name+'.IN')
 DrawDoc()
 MenuDoc()
 OpenDDBF()
 cnt_line=MLCOUNT(name_pic,len_edil)
 decl line_pic[cnt_line]
 for i=1 to cnt_line
  line_pic[i] = memoline(name_pic,len_edil,i)
 next
 setcolor(c)
 cnt_fields=fcount()
 decl jour_doc[cnt_fields]
 afields(jour_doc)
 for i=cnt_fields to 3 step -1
  jour_doc[i]=jour_doc[i-2]
 next
 jour_doc[1]="iif(delete(),chr(16),' ')"
 jour_doc[2]="DOCUMER"
 dbedit(Y_min+1,X_min+1,Y_max-1,X_max-1,jour_doc,"JourDocum","","",""," ")
 rele jour_doc
 rele line_pic
 CloseDDBF()
endif
restore screen from InpPage
setcolor(old_color)
select 1
return
*-------------------------------*
function dbmfunc
parameters mode,ptr
do case
case mode=0
case lastkey()=13.or.lastkey()=27
 return 0
endcase
return 1
*---------------------------------------------------------*
function JourDocum
parameters mode,fld_ptr
Last_User = lastkey()
do case
case mode = 0
  @ Y_min, 2 say '██████████████'
  if Dark != 0
    setcolor(cl)
    @ Y_min, 2 say "Помечено "+ltrim(str(Dark))
    setcolor(c)
  endif
case mode = 3
case mode = 4
do case
case (Last_User = 32).or.(lastkey() = 32)       && SPACE
  if IsSetj()
   return 1
  endif
  if is_adm
   if delete()
    recall
    Dark=Dark-1
   else
    delete
    Dark=Dark+1
   endif
   return 2
  else
   WarnWrite(PMSG1)
  endif
case (Last_User = 22).or.(lastkey() = 22)       && INS
  if Fragen(' С О З Д А Т Ь ','Hовый документ?')
   go bottom
   n=DOCUMER
   append blank
   rlock()
   repl DOCUMER with n+1
   edil(line_pic,cnt_line,iif(I->CNT_COPY>0,I->CNT_COPY,1))
   unlock
   return 2
  endif
case (Last_User = 13).or.(lastkey() = 13)       && Enter
  if .not.OnSetj()
   return 1
  endif
  edil(line_pic,cnt_line,iif(I->CNT_COPY>0,I->CNT_COPY,1))
  UnSetj()
  return 2
case (Last_User =  7).or.(lastkey()=   7)       && DEL
 if DelGloss1()
  Dark = 0
  return 2
 endif
case (Last_User = -1).or.(lastkey() = -1)       && F2
 asablon()
case (Last_User = -2).or.(lastkey() = -2)       && F3
 if Fragen('','Сделать копию документа?')
   MsgWrite('Делаю копию документа...')
   decl afield[cnt_fields]
   for i=2 to cnt_fields
    fld=jour_doc[i]
    afield[i]=&fld
   next
   go bottom
   n=DOCUMER
   append blank
   rlock()
   for i=2 to cnt_fields
    fld=jour_doc[i]
    repl &fld with afield[i]
   next
   repl DOCUMER with n+1
   unlock
   rele afield
   return 2
 endif
case (Last_User = -4).or.(lastkey() = -4)       && F5
 KnigaLPT1()
case (Last_User = -5).or.(lastkey() = -5)       && F6
 if FindGloss(jour_doc[fld_ptr],"",Y_srm-2,X_srm-30,Y_srm+2,X_srm+30)
  return 2
 endif
case (Last_User = -7).or.(lastkey() = -7)       && F8
 if fld_ptr != 1
   if Fragen('','Соpтиpовать?')
    SortDocum(fld_ptr)
    return 2
   endif
 endif
case (Last_User = 27).or.(lastkey() = 27)       && ESC
 return 0
endcase
endcase
return 1
*---------------------------------------------------------*
procedure DrawDoc
setcolor(c)
DrawBox(Y_min,X_min,Y_max,X_max,ibox)
setcolor(cl)
MSG=alltrim(doc_name)
@ Y_min,int(X_srm-len(MSG)/2) say MSG
return
*---------------------------------------------------------*
procedure MenuDoc
PMenuOpen()
PMenuIns()
PMenuEnter()
PMenuDel()
PMenuSabl()
PMenuCopy()
PMenuLpt1()
PMenuFind()
PMenuSort()
PMenuESC()
PMenuClose()
return
*---------------------------------------------------------*
procedure OpenDDBF
private alia
select 1
old_Nz=recno()
set filter to
go top
if .not.file(name_dbf)
 select 4
 creat &name_dbf from &name_str
 use
endif
select 4
if is_setj
 use &name_dbf SHARED
else
 use &name_dbf
endif
select 1
alia=alias(4)
set relation to NUMBER into &alia
go top
select 4
return
*---------------------------------------------------------*
procedure CloseDDBF
select 4
DarkDecall()
use
select 1
set relation to
InitFilter()
go old_Nz
return
*---------------------------------------------------------*
procedure DarkDecall
if Dark # 0
 save screen
 MsgWrite('Удаляю метки...')
 recall for delete()
 Dark = 0
 restore screen
endif
return
*---------------------------------------------------------*
procedure KnigaLPT1
private str_file,i,j,i_min,i_max,Nz,NN,var,fld,str_prn
if fcount()-2 <= 0
  return
endif
nam_prn=use_path+file_name+'.PRN'
str_file=memoread(nam_prn)
if empty(str_file)
  WarnWrite("Файл отчета не заполнен...")
  return
endif
if .not.QuesPrint()
  return
endif
save screen to LPT1Page
MsgWrite(msgprn)
declare format_str[3]   && 1-заголовок; 2-текст; 3-окончание
for i=1 to 3
  i_min=at(chr(i),str_file)
  i_max=rat(chr(i),str_file)
  if i_min != i_max.and.i_min != (i_max - 1)
    format_str[i]=substr(str_file,i_min+1,i_max-i_min-1)
  else
    format_str[i]=""
  endif
next
for j=1 to fcount()-2
 ch=alltrim(str(j))
 IDC&ch.=0
next
ISUM=0
Nz=recno()
r_cnt=reccount()
go top
N=1
is_prn=.T.
i=1
do while i <= r_cnt.and.is_prn
 is_prn=mprints(strfrml(format_str[1]))
 do while i <= r_cnt.and.N <= I->STROK
  go i
  is_prn=mprints(strfrml(format_str[2]))
  for j=1 to fcount()-2
   ch=alltrim(str(j))
   IDC&ch.=IDC&ch.+val(strtran(DC&ch.,"-","."))
  next
  ISUM=ISUM+A->SUMMA
  N++
  i++
 enddo
 if is_prn
  is_prn=mprints(strfrml(format_str[3]))
 endif
 N=1
enddo
go Nz
rest screen from LPT1Page
return
*---------------------------------------------------------*
procedure SortDocum
parameters fld_ptr
save screen
MsgWrite("Сортирую...")
var_fld=jour_doc[fld_ptr]
name_tmp=use_path+'TEMP.DBF'
sort on &var_fld to &name_tmp
CloseDDBF()
dele file &name_dbf
rename &name_tmp to &name_dbf
OpenDDBF()
rest screen
return
