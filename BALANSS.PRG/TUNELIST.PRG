*---------------------------------------------------------*
procedure TuneList
select 2
use_dbf="list.bln"
use &use_dbf
setcolor(cinp)
DrawBox(Y_min,X_min,Y_max,X_max,ibox)
setcolor(cl)
MSG='                      '
@ Y_min,int((X_min+X_max)/2-len(MSG)/2) say MSG
MenuLstTune()
setcolor(cinp)
decl tune_fld[3],tune_nam[3]
tune_nam[1]="ç¥â"
tune_fld[1]="LIST_DBF"
tune_nam[2]=" áè¨à¥­¨¥"
tune_fld[2]="LIST_EXT"
tune_nam[3]="®à¬ "
tune_fld[3]="LIST_FRM"
dbedit(Y_min+1,X_min+1,Y_max-1,X_max-1,tune_fld,"TuneLstFunc","",tune_nam,"ÄÄÄ"," ")
use
select 1
return
*-------------------------------*
function TuneLstFunc
parameters mode,ptr
do case
case mode<3
case lastkey() = 22       && INS
 if insert()
  return 2
 endif
case lastkey() = 13       && Enter
 set cursor on
 cur_field=tune_fld[ptr]
 @ row(),col() get &cur_field
 read
 set cursor off
case lastkey()=   7       && DEL
 save screen
 if Yes_Or_No(PMSGDEL1,PMSGDEL2,2)
  MsgWrite(PMSGDEL3)
  name_dbf=use_path+strline(LIST_DBF)+".DIV"
  dele file &name_dbf
  deles("*."+LIST_EXT,use_path)
  delete
  pack
  go top
 endif
 restore screen
 return 2
case lastkey()=27
 return 0
endcase
return 1
*---------------------------------------------------------*
procedure MenuLstTune
set color to &cl
@ Y_max,X_min+8 say '    : '
set color to &cmenu
MSG='INS'
@ row(),col() get MSG
@ row(),col() say '®§¤ âì'
MSG='Enter'
@ row(),col() get MSG
@ row(),col() say 'H ¨¬¥­®¢ ­¨¥'
MSG='DEL'
@ row(),col() get MSG
@ row(),col() say '¤ «¨âì'
MSG='ESC'
@ row(),col() get MSG
@ row(),col() say 'ëå®¤'
clear gets
return
