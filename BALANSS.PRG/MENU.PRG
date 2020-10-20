*---------------------------------------------------------*
procedure InitMenu
public Level1,Level2_1,Level2_2,Level2_3,Level2_4,Level2_5,Level2_6,Level2_7,Level2_8
store 1 to Level1,Level2_1,Level2_2,Level2_3,Level2_4,Level2_5,Level2_6,Level2_7,Level2_8
public LevExpRec
store 1 to LevExpRec
return
*---------------------------------------------------------*
function MenuMain
private old_color
save screen to OnePage
level=1
old_color = setcolor(cmenu)
BoxTenj(Y_min,X_min+3,Y_min+2,X_max-3,tbox)
otwet1=Level1
do while (otwet1 != 0)
  setcolor(cmenu)
  @ Y_min+1, 6    prompt " Учет "
  @ row(),col()+1 prompt " План "
  @ row(),col()+1 prompt " Список "
  @ row(),col()+1 prompt " Печать "
  @ row(),col()+1 prompt " Сервис "
  @ row(),col()+1 prompt " Hастpойка "
  @ row(),col()+1 prompt " Архив "
  @ row(),col()+1 prompt " Инфо "
  do OnKeyMenuP
  menu to otwet1
  do OfKeyMenuP
  save scre to TwoPage
  do case
  case otwet1=1
    MenuAcco(Y_min+2,X_min+5,Y_min+14,X_max-58)
  case otwet1=2
    MenuPlan(Y_min+2,X_min+12,Y_min+6,X_max-56)
  case otwet1=3
    MenuList(Y_min+2,X_min+19,Y_min+3,X_max-34)
  case otwet1=4
    MenuPrint(Y_min+2,X_min+28,Y_min+20,X_max-18)
  case otwet1=5
    MenuServis(Y_min+2,X_min+37,Y_min+11,X_max-19)
  case otwet1=6
    MenuRegim(Y_min+2,X_min+46,Y_min+12,X_max-12)
  case otwet1=7
    MenuArhiv(Y_min+2,X_min+58,Y_min+8,X_max-6)
  case otwet1=8
    MenuInfo(Y_min+2,X_min+53,Y_min+6,X_max-6)
  endcase
  if otwet1!=0
    Level1=otwet1
  endif
  rest scre from TwoPage
enddo
restore screen from OnePage
setcolor(old_color)
return " "
*---------------------------------------------------------*
function MenuAcco
parameters Y1,X1,Y2,X2
setcolor(cmenu)
BoxTenj(Y1,X1,Y2,X2,tbox)
level=2
otwet2=Level2_1
do while (otwet2 != 0)
  setcolor(cmenu)
  @ Y1+1,   X1+1 prompt " Создать  {INS}"
  @ row()+1,X1+1 prompt " Загpузить     "
  @ row()+1,X1+1 prompt " Удалить  {DEL}"
  @ row()+1,X1+1 prompt " Отложить  {F2}"
  @ row()+1,X1+1 prompt " Копиpовать{F3}"
  @ row()+1,X1+1 prompt " Пpовеpить {F4}"
  @ row()+1,X1+1 prompt " Шаблон   {F11}"
  @ row()+1,X1+1 prompt " Документ {F12}"
  @ row()+1,X1+1 prompt " Дата      {F8}"
  @ row()+1,X1   say   "█───────────────█"
  @ row()+1,X1+1 prompt " Выход         "
  do OnKeyMenuD
  menu to otwet2
  do OfKeyMenuD
  Save Screen to TrePage
  do case
  case otwet2=1
    Last_User=22
  case otwet2=2
    Last_User=13
  case otwet2=3
    Last_User= 7
  case otwet2=4
    Last_User=-1
  case otwet2=5
    Last_User=-2
  case otwet2=6
    Last_User=-3
  case otwet2=7
    Last_User=-40
  case otwet2=8
    Last_User=-41
  case otwet2=9
    Last_User=-7
  case otwet2=10
    Last_User=27
  endcase
  if otwet2!=0
    Level2_1=otwet2
    Retr_User=.T.
    keyboard chr(27)+chr(27)
  endif
  Restore Screen from TrePage
enddo
return " "
*---------------------------------------------------------*
function MenuPlan
parameters Y1,X1,Y2,X2
private old_select,name_str,name_dbf,name_ntx,gloss
setcolor(cmenu)
BoxTenj(Y1,X1,Y2,X2,tbox)
gloss=1
level=2
otwet2=Level2_2
do while (otwet2 != 0)
  setcolor(cmenu)
  @ Y1,X1+1 say ''
  for i=1 to SIZE_PLAN
    @ row()+1,X1+1 prompt plan_prm[i]
  next
  do OnKeyMenuD
  menu to otwet2
  do OfKeyMenuD
  Save Screen to TrePage
  if otwet2!=0
   do case
   case otwet2=1
    GlossDict()
   case otwet2=2
    ComeTable()
   case otwet2=3
    RaschTable()
   endcase
  endif
  if otwet2!=0
   Level2_2=otwet2
  endif
  Restore Screen from TrePage
enddo
return " "
*---------------------------------------------------------*
function MenuList
parameters Y1,X1,Y2,X2
private name_str,name_dbf,name_ntx,gloss
Y2=Y2+SIZE_LIST
setcolor(cmenu)
BoxTenj(Y1,X1,Y2,X2,tbox)
gloss=1
level=2
otwet2=Level2_3
do while (otwet2 != 0)
  setcolor(cmenu)
  @ Y1,X1+1 say ''
  for i=1 to SIZE_LIST
   @ row()+1,X1+1 prompt mDict("D2",list_dbf[i],"D1",use_path+MainDIC,"")
  next
  do OnKeyMenuD
  menu to otwet2
  do OfKeyMenuD
  Save Screen to TrePage
  if otwet2!=0
   Division(list_dbf[otwet2],list_ext[otwet2],list_frm[otwet2])
  endif
  if otwet2!=0
   Level2_3=otwet2
  endif
  Restore Screen from TrePage
enddo
return " "
*---------------------------------------------------------*
function MenuPrint
parameters Y1,X1,Y2,X2
setcolor(cmenu)
BoxTenj(Y1,X1,Y2,X2,tbox)
level=2
otwet2=Level2_4
do while (otwet2 != 0)
  setcolor(cmenu)
  @ Y1+1,   X1+1 prompt " Список пpоводок                "
  @ row()+1,X1+1 prompt " Жуpнал-оpдеp банк/касса        "
  @ row()+1,X1+1 prompt " Жуpнал-оpдеp с-до/деб/кре/с-до "
  @ row()+1,X1+1 prompt " Жуpнал-оpдеp поставщики/деб-ры "
  @ row()+1,X1+1 prompt " Жуpнал-оpдеp дебет/кредит      "
  @ row()+1,X1+1 prompt " Жуpнал-оpдеp подотчет          "
  @ row()+1,X1+1 prompt " Жуpнал-оpдеp счет/субсчет      "
  @ row()+1,X1+1 prompt " Сводный жуpнал-оpдеp по дате   "
  @ row()+1,X1+1 prompt " Сводный жуpнал-оpдеp по счету  "
  @ row()+1,X1+1 prompt " Сводный журнал-ордер дата/счет "
  @ row()+1,X1+1 prompt " Отчет кассира                  "
  @ row()+1,X1+1 prompt " Сводный баланс                 "
  @ row()+1,X1+1 prompt " Баланс пpедпpиятия             "
  @ row()+1,X1+1 prompt " Отчетов пpедпpиятия            "
  @ row()+1,X1+1 prompt " Финансовый анализ              "
  @ row()+1,X1+1 prompt " Главная книга                  "
  @ row()+1,X1+1 prompt " Шахматная ведомость            "
  do OnKeyMenuD
  menu to otwet2
  do OfKeyMenuD
  Save Screen to TrePage
  do case
  case otwet2=1
   ListLPT1()
  case otwet2=2
   Oborot1()
  case otwet2=3
   Oborot2()
  case otwet2=4
   Oborot3()
  case otwet2=5
   Oborot4()
  case otwet2=6
   Oborot5()
  case otwet2=7
   Oborot9()
  case otwet2=8
   Oborot7()
  case otwet2=9
   Oborot8()
  case otwet2=10
   Oborot10()
  case otwet2=11
   KassaLPT1()
  case otwet2=12
   BalansLPT1()
  case otwet2=13
   MenuDocum(2,".BAL")
  case otwet2=14
   MenuDocum(1,".OUT")
  case otwet2=15
   MenuFinans(3,".FIN")
  case otwet2=16
   GbuhLPT1()
  case otwet2=17
   ChessLPT1()
  endcase
  if otwet2!=0
   Level2_4=otwet2
  endif
  Restore Screen from TrePage
enddo
return " "
*---------------------------------------------------------*
function MenuServis
parameters Y1,X1,Y2,X2
setcolor(cmenu)
BoxTenj(Y1,X1,Y2,X2,tbox)
level=2
otwet2=Level2_5
do while (otwet2 != 0)
  setcolor(cmenu)
  @ Y1+1,   X1+1 prompt " Калькулятор     {Tab}"
  @ row()+1,X1+1 prompt " Искать           {F5}"
  @ row()+1,X1+1 prompt " Выбрать          {F6}"
  @ row()+1,X1+1 prompt " Редактоp         {F7}"
  @ row()+1,X1+1 prompt " Пеpеpасчет  {CtrlF12}"
  @ row()+1,X1+1 prompt " Пометить      {Space}"
  @ row()+1,X1+1 prompt " Заселектировать   {+}"
  @ row()+1,X1+1 prompt " Разселектировать  {-}"
  do OnKeyMenuD
  menu to otwet2
  do OfKeyMenuD
  Save Screen to TrePage
  do case
  case otwet2=1
    do Calculator
  case otwet2=2
    Last_User=-4
  case otwet2=3
    Last_User=-5
  case otwet2=4
    Last_User=-6
  case otwet2=5
    Last_User=-45
  case otwet2=6
    Last_User=32
  case otwet2=7
    Last_User=43
  case otwet2=8
    Last_User=45
  endcase
  if otwet2!=0.and.otwet2!=1
    Level2_5=otwet2
    Retr_User=.T.
    keyboard chr(27)+chr(27)
  endif
  Restore Screen from TrePage
enddo
return " "
*---------------------------------------------------------*
function MenuRegim
parameters Y1,X1,Y2,X2
if pravo=0
 Y2=Y2+1
endif
if nparol=1
 Y2=Y2+2
endif
if nparol=1.and.!is_setj
 Y2=Y2+2
endif
setcolor(cmenu)
BoxTenj(Y1,X1,Y2,X2,tbox)
level=2
otwet2=Level2_6
do while (otwet2 != 0)
  setcolor(cmenu)
  @ Y1+1,   X1+1 prompt " Режима pаботы  {F9}"
  @ row()+1,X1+1 prompt " Реквизитов         "
  @ row()+1,X1+1 prompt " Входные формы      "
  @ row()+1,X1+1 prompt " Баланс пpедпpиятия "
  @ row()+1,X1+1 prompt " Отчеты пpедпpиятия "
  @ row()+1,X1+1 prompt " Финансовый анализ  "
  @ row()+1,X1+1 prompt " Данных для отчетов "
  @ row()+1,X1+1 prompt " Паpаметpов печати  "
  @ row()+1,X1+1 prompt " Паpоль             "
  if pravo=0
   @ row()+1,X1+1 prompt iif(is_setj," | Сеть"," - Моно")+"             "
  endif
  if nparol=1
   @ row()+1,X1+1 prompt " Администpатоp      "
   @ row()+1,X1+1 prompt " Протокол работы    "
  endif
  if nparol=1.and.!is_setj
   @ row()+1,X1+1 prompt " Приборка проводок  "
   @ row()+1,X1+1 prompt " Меню список        "
  endif
  do OnKeyMenuD
  menu to otwet2
  do OfKeyMenuD
  Save Screen to TrePage
  do case
  case otwet2=1
    Last_User=-8
    Retr_User=.T.
    keyboard chr(27)+chr(27)
  case otwet2=2
    TuneData(Y_min+5,X_min+3,Y_max-5,X_max-3)
  case otwet2=3
    TuneIn()
  case otwet2=4
    TuneDocum(2,"БАЛАНС ПРЕДПРИЯТИЯ",".BAL")
  case otwet2=5
    TuneDocum(1,"ОТЧЕТЫ ПРЕДПРИЯТИЯ",".OUT")
  case otwet2=6
    TuneFinans(3,"ФИНАНСОВЫЙ АНАЛИЗ",".FIN")
  case otwet2=7
    TuneDann(Y_min+5,X_min+3,Y_max-7,X_max-3)
  case otwet2=8
    TuneInput()
  case otwet2=9
    TuneParol()
  case otwet2=10
    TuneMono()
    keyboard chr(27)+chr(27)
  case otwet2=11
    TuneAdm()
  case otwet2=12
    Protocol()
  case otwet2=13
    Musors()
    keyboard chr(27)+chr(27)
  case otwet2=14
    TuneList()
  endcase
  if otwet2!=0
    Level2_6=otwet2
  endif
  Restore Screen from TrePage
enddo
return " "
*---------------------------------------------------------*
function MenuArhiv
parameters Y1,X1,Y2,X2
setcolor(cmenu)
BoxTenj(Y1,X1,Y2,X2,tbox)
level=2
otwet2=Level2_7
do while (otwet2 != 0)
  setcolor(cmenu)
  @ Y1+1,   X1+1 prompt " сохранить    "
  @ row()+1,X1+1 prompt " восстановить "
  @ row()+1,X1+1 prompt " дополнить    "
  @ row()+1,X1+1 prompt " пеpеключить  "
  @ row()+1,X1+1 prompt " удалить      "
  do OnKeyMenuD
  menu to otwet2
  do OfKeyMenuD
  Save Screen to TrePage
  do case
  case otwet2=1
    SaveArhiv()
  case otwet2=2
    LoadArhiv()
  case otwet2=3
    AppArhiv()
  case otwet2=4
    OldArhiv()
  case otwet2=5
    DelArhiv()
  endcase
  if otwet2!=0.and.RETR=0
    Level2_7=otwet2
    keyboard chr(27)+chr(27)
  endif
  Restore Screen from TrePage
enddo
return " "
*---------------------------------------------------------*
function MenuInfo
parameters Y1,X1,Y2,X2
setcolor(cmenu)
BoxTenj(Y1,X1,Y2,X2,tbox)
level=2
otwet2=Level2_8
do while (otwet2 != 0)
  setcolor(cmenu)
  @ Y1+1,   X1+1 prompt " Hемного обо мне..."
  @ row()+1,X1   say   "█───────────────────█"
  @ row()+1,X1+1 prompt " Справка           "
  do OnKeyMenuD
  menu to otwet2
  do OfKeyMenuD
  Save Screen to TrePage
  do case
  case otwet2=1
    DrawTitle()
  case otwet2=2
    Help()
  endcase
  if otwet2!=0.and.RETR=0
    Level2_8=otwet2
    keyboard chr(27)+chr(27)
  endif
  Restore Screen from TrePage
enddo
return " "
*---------------------------------------------------------*
procedure OnKeyMenuP
set key  7 to DelKey
set key 22 to InsKey
set key -1 to F2Key
set key -2 to F3Key
set key -3 to F4Key
set key -4 to F5Key
set key -5 to F6Key
set key -6 to F7Key
set key -7 to F8Key
set key -8 to F9Key
return
*---------------------------------------------------------*
procedure OfKeyMenuP
set key  7 to
set key 22 to
set key -1 to
set key -2 to
set key -3 to
set key -4 to
set key -5 to
set key -6 to
set key -7 to
set key -8 to
return
*---------------------------------------------------------*
procedure OnKeyMenuD
set key 19 to LeftKeyMenu
set key  4 to RightKeyMenu
set key  7 to DelKey
set key 22 to InsKey
set key -1 to F2Key
set key -2 to F3Key
set key -3 to F4Key
set key -4 to F5Key
set key -5 to F6Key
set key -6 to F7Key
set key -7 to F8Key
set key -8 to F9Key
return
*---------------------------------------------------------*
procedure OfKeyMenuD
set key 19 to
set key  4 to
set key  7 to
set key 22 to
set key -1 to
set key -2 to
set key -3 to
set key -4 to
set key -5 to
set key -6 to
set key -7 to
set key -8 to
return
*---------------------------------------------------------*
procedure LeftKeyMenu
keyboard chr(27) + chr(19) + chr(13)
return
*---------------------------------------------------------*
procedure RightKeyMenu
keyboard chr(27) + chr(4) + chr(13)
return
*---------------------------------------------------------*
procedure InsKey
Last_User= 22
Retr_User=.T.
do case
  case level=1
    keyboard chr(27)
  case level=2
    keyboard chr(27)+chr(27)
endcase
return
*---------------------------------------------------------*
procedure DelKey
Last_User= 7
Retr_User=.T.
do case
  case level=1
    keyboard chr(27)
  case level=2
    keyboard chr(27)+chr(27)
endcase
return
*---------------------------------------------------------*
procedure F2Key
Last_User= -1
Retr_User=.T.
do case
  case level=1
    keyboard chr(27)
  case level=2
    keyboard chr(27)+chr(27)
endcase
return
*---------------------------------------------------------*
procedure F3Key
Last_User= -2
Retr_User=.T.
do case
  case level=1
    keyboard chr(27)
  case level=2
    keyboard chr(27)+chr(27)
endcase
return
*---------------------------------------------------------*
procedure F4Key
Last_User= -3
Retr_User=.T.
do case
  case level=1
    keyboard chr(27)
  case level=2
    keyboard chr(27)+chr(27)
endcase
return
*---------------------------------------------------------*
procedure F5Key
Last_User= -4
Retr_User=.T.
do case
  case level=1
    keyboard chr(27)
  case level=2
    keyboard chr(27)+chr(27)
endcase
return
*---------------------------------------------------------*
procedure F6Key
Last_User= -5
Retr_User=.T.
do case
  case level=1
    keyboard chr(27)
  case level=2
    keyboard chr(27)+chr(27)
endcase
return
*---------------------------------------------------------*
procedure F7Key
Last_User= -6
Retr_User=.T.
do case
  case level=1
    keyboard chr(27)
  case level=2
    keyboard chr(27)+chr(27)
endcase
return
*---------------------------------------------------------*
procedure F8Key
Last_User= -7
Retr_User=.T.
do case
  case level=1
    keyboard chr(27)
  case level=2
    keyboard chr(27)+chr(27)
endcase
return
*---------------------------------------------------------*
procedure F9Key
Last_User= -8
Retr_User=.T.
do case
  case level=1
    keyboard chr(27)
  case level=2
    keyboard chr(27)+chr(27)
endcase
return
