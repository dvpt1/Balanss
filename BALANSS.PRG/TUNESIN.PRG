*---------------------------------------------------------*
procedure TuneIn
select 5
use_dbf=use_path+DocumDIC
if is_setj
 use &use_dbf SHARED
else
 use &use_dbf
endif
setcolor(cinp)
DrawBox(Y_min,X_min,Y_max,X_max,ibox)
setcolor(cl)
MSG=' Н А С Т Р О Й К А  Д О К У М Е H Т О В '
@ Y_min,int((X_min+X_max)/2-len(MSG)/2) say MSG
MenuTuneIn()
setcolor(cinp)
decl tune_fld[2],tune_nam[2]
tune_nam[1]="Hаименование"
tune_fld[1]="DOC_NAM"
tune_nam[2]="Имя файла"
tune_fld[2]="DOC_DOC"
dbedit(Y_min+1,X_min+1,Y_max-1,X_max-1,tune_fld,"TuneInFunc","",tune_nam,"───"," ")
use
select 1
return
*-------------------------------*
function TuneInFunc
parameters mode,ptr
private RETT
RETT=1
do case
case mode=0
case lastkey() = 22       && INS
 if CreaDoc()
  if .not.OnSetj()
   return 1
  endif
  EdilTune(use_path+rtrim(DOC_DOC)+'.IN',;
           use_path+rtrim(DOC_DOC)+'.STR',;
           use_path+rtrim(DOC_DOC)+'.DBF')
  UnSetj()
 endif
case lastkey() = -1       && F2
  if .not.OnSetj()
   return 1
  endif
  EdilTune(use_path+rtrim(DOC_DOC)+'.IN',;
           use_path+rtrim(DOC_DOC)+'.STR',;
           use_path+rtrim(DOC_DOC)+'.DBF')
  UnSetj()
case lastkey() = -2       && F3
  if .not.OnSetj()
   return 1
  endif
  TuneOtchet()
  UnSetj()
case lastkey() = 13       && Enter
 if ptr=1
  if .not.OnSetj()
   return 1
  endif
  set cursor on
  @ row(),col() get DOC_NAM
  read
  set cursor off
  UnSetj()
 endif
case lastkey()=   7       && DEL
 DeleDoc()
case lastkey()=27
 return 0
endcase
return RETT
*---------------------------------------------------------*
procedure MenuTuneIn
set color to &cl
@ Y_max,X_min+8 say ' М Е Н Ю: '
set color to &cmenu
MSG='INS'
@ row(),col() get MSG
@ row(),col() say 'Создать'
MSG='F2'
@ row(),col() get MSG
@ row(),col() say 'Форма'
MSG='F3'
@ row(),col() get MSG
@ row(),col() say 'Отчет'
MSG='Enter'
@ row(),col() get MSG
@ row(),col() say 'Hаименование'
MSG='DEL'
@ row(),col() get MSG
@ row(),col() say 'Удалить'
MSG='ESC'
@ row(),col() get MSG
@ row(),col() say 'Выход'
clear gets
return
*---------------------------------------------------------*
function CreaDoc
if Fragen('','Создать новый документ?')
 save scre
 name_doc=nulvar(DOC_NAM)
 BoxTenj(Y_srm-3,X_srm-25,Y_srm+3,X_srm+25,ibox)
 @ Y_srm-1,X_srm-22 say "Hаименование:" get name_doc
 file_doc=nulvar(DOC_DOC)
 @ Y_srm+1,X_srm-22 say "Имя файла   :" get file_doc
 set cursor on
 read
 set cursor off
 rest scre
 if lastkey()!=27.and.(.not.empty(name_doc)).and.(.not.empty(file_doc))
  append blank
  repl DOC_NAM with name_doc
  repl DOC_DOC with strtran(file_doc," ")
  RETT=2
  return .T.
 endif
endif
return .F.
*---------------------------------------------------------*
procedure DeleDoc
if .not.Pravo0()
 return .F.
endif
if IsSetj()
 return .F.
endif
if Fragen(PMSGDEL1,PMSGDEL2)
 save scre
 MsgWrite(PMSGDEL3)
 file_nam=alltrim(DOC_DOC)
 file_dc=use_path+file_nam+'.IN'
 nam_str=use_path+file_nam+'.STR'
 nam_dbf=use_path+file_nam+'.DBF'
 nam_prn=use_path+file_nam+'.PRN'
 delete file &file_dc
 delete file &nam_str
 delete file &nam_dbf
 delete file &nam_prn
 delete
 pack
 rest scre
 RETT=2
endif
return
*---------------------------------------------------------*
procedure TuneOtchet
private otwet,old_color
old_color = setcolor()
otwet=1
do while (otwet != 0)
  save scre
  setcolor(cmenu)
  BoxTenj(Y_srm-2,X_srm-6,Y_srm+2,X_srm+6,ibox)
  setcolor(cl)
  MSG=' О Т Ч Е Т '
  @ Y_srm-2,X_srm-len(MSG)/2+1 say MSG
  setcolor(cmenu)
  @ Y_srm-1,X_srm-5 prompt ' Заголовок '
  @ Y_srm+0,X_srm-5 prompt ' Таблица   '
  @ Y_srm+1,X_srm-5 prompt ' Окончание '
  menu to otwet
  rest scre
  if otwet!=0
   save scre
   MsgWrite('Считываю отчет...')
   file_nam=alltrim(DOC_DOC)
   nam_prn=use_path+file_nam+'.PRN'
   str_file=memoread(nam_prn)
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
   rest scre
   format_str[otwet]=ed(Y_min,X_min,Y_max,X_max,format_str[otwet])
   if Fragen('',PMSGSAVE2)
    save scre
    MsgWrite(PMSGSAVE3)
    nam_pic=chr(1)+format_str[1]+chr(1)+chr(2)+format_str[2]+chr(2)+chr(3)+format_str[3]+chr(3)
    memowrit(nam_prn,nam_pic)
    rest scre
   endif
  endif
enddo
set color to (old_color)
return
