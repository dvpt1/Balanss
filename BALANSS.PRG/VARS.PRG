*-------------Инициализация массива лицевого счета--------*
procedure InitConfig
public MainDBF
store 'balans.dbf' to MainDBF
public MainSTR
store 'balans.str' to MainSTR
public MainNTX
store 'balans.ntx' to MainNTX

public MainDIC
store 'expenses.dic' to MainDIC
public MainDIS
store 'expenses.str' to MainDIS

public SubDIS
store 'sub.str' to SubDIS

public ComeDIC
store 'come.dic' to ComeDIC
public ComeSTR
store 'come.str' to ComeSTR

public RaschetDIC
store 'raschet.dic' to RaschetDIC
public RaschetSTR
store 'raschet.STR' to RaschetSTR

public OtchetDIC
store 'otchet.dg' to OtchetDIC
public OtchetSTR
store 'otchet.str' to OtchetSTR

public FinansDIC
store 'finans.dg' to FinansDIC
public FinansSTR
store 'finans.str' to FinansSTR

public BalanpDIC
store 'balanp.dg' to BalanpDIC
public BalanpSTR
store 'balanp.str' to BalanpSTR

public DocumDIC
store 'docum.dg' to DocumDIC
public DocumSTR
store 'docum.str' to DocumSTR

public SablonDBF
store 'sablon.dbf' to SablonDBF
public SablonSTR
store 'sablon.str' to SablonSTR

public ProtocDBF
store 'protoc.dbf' to ProtocDBF
public ProtocSTR
store 'protoc.str' to ProtocSTR

public DivisionSTR
store 'division.str' to DivisionSTR

public QuanSTR
store 'quan.str' to QuanSTR

public OborotDBF
store 'oborot.dbf' to OborotDBF
public OborotSTR
store 'oborot.str' to OborotSTR
public OborotNTX
store 'oborot.ntx' to OborotNTX

select 9
use config.bln
public pic_sum,len_sum,num_sum,pic_tmz,pic_1000
pic_tmz="999999999.99"
if I->CNT_ZAPJ>0.and.(I->CNT_ZAPJ+1)<I->CNT_ZNAK
 pic_1000=replicate("9",I->CNT_ZNAK-I->CNT_ZAPJ-1)+"."+replicate("9",I->CNT_ZAPJ)
else
 pic_1000=replicate("9",iif(I->CNT_ZNAK>0,I->CNT_ZNAK,10))
endif
pic_sum=pictn(I->FORMAT)
len_sum=lenvar(I->FORMAT)
repl I->FORMAT with 0
num_sum=I->FORMAT
public num_1
num_1=val(transform(-1,pictn(I->DEB51)))
public num_0
num_0=val(transform(0,pictn(I->DEB51)))
public nul_date
nul_date=val(transform(0,'99'))
public nul_shet
nul_shet=nulvar(I->DEB51)
public nul_sshet
nul_sshet=nulvar(I->SUB51)
public len_edil
len_edil=120
public nm
nm=month(I->T_DATE)
public is_arhiv
public fl_debet
public fl_kredit
public is_close
is_close=.F.
public is_f9
is_f9=.F.
select 8
name_str="base.bln"
use &name_str SHARED               && список баз
public SIZE_BASE
SIZE_BASE = lastrec()
if SIZE_BASE != 0
  public base_dbf[SIZE_BASE]
  public base_str[SIZE_BASE]
  public base_ntx[SIZE_BASE]
  public base_key[SIZE_BASE]
  i=0
  do while .not.eof()
    i=i+1
    base_dbf[i] = alltrim(H->DICT_DBF)
    base_str[i] = alltrim(H->DICT_STR)
    base_ntx[i] = alltrim(H->DICT_NTX)
    base_key[i] = alltrim(H->DICT_KEY)
    skip
  enddo
endif
name_str="journal.bln"
use &name_str SHARED         && Журнал
public SIZE_JOUR
SIZE_JOUR = lastrec()+1
public jour_jou[SIZE_JOUR]   && Макро поле журнала
public jour_fld[SIZE_JOUR]   && Имя поля
public jour_nam[SIZE_JOUR]   && Название поля
public jour_iif[SIZE_JOUR]   && Название iif
i=1
jour_jou[i]="iif(delete(),chr(16),' ')"
jour_fld[i]="delete()"
jour_nam[i]=""
jour_iif[i]="iif(delete(),chr(16),' ')"
do while .not.eof()
 i=i+1
 do case
 case .not.empty(H->NAME_IIF)
  jour_jou[i]=alltrim(H->NAME_IIF)
 othe
  jour_jou[i]=alltrim(H->FIELD_NAME)
 endcase
 jour_fld[i]=alltrim(H->FIELD_NAME)
 jour_nam[i]=rtrim(H->NAME_FIELD)
 jour_iif[i]=alltrim(H->NAME_IIF)
 skip
enddo
name_str="account.bln"
use &name_str SHARED           && Лицевой счет
public SIZE_ACCO
SIZE_ACCO = lastrec()
if SIZE_ACCO != 0
  public acco_fld[SIZE_ACCO]   && Имя  поля
  public acco_nam[SIZE_ACCO]   && Название поля
  public acco_iif[SIZE_ACCO]   && Название логического поля
  public acco_row[SIZE_ACCO]   && Курсор Y
  public acco_col[SIZE_ACCO]   && Курсор X
  public acco_dbf[SIZE_ACCO]   && Название файла словаря
  public acco_ntx[SIZE_ACCO]   && и его файла индекса
  public acco_cod[SIZE_ACCO]   && Код из словаря
  public acco_scr[SIZE_ACCO]   && Листинг словаря
  public acco_ge1[SIZE_ACCO]   && GET1
  public acco_ge2[SIZE_ACCO]   && GET2
  i=0
  do while .not.eof()
    i=i+1
    acco_fld[i]=alltrim(H->FIELD_NAME)
    acco_nam[i]=alltrim(H->NAME_FIELD)
    acco_iif[i]=alltrim(H->IIF_STR)
    acco_row[i]=H->CUR_ROW
    acco_col[i]=H->CUR_COL
    acco_dbf[i]=alltrim(H->DICT_DBF)
    acco_ntx[i]=alltrim(H->DICT_NTX)
    acco_cod[i]=alltrim(H->FIELD_COD)
    acco_scr[i]=alltrim(H->FIELD_SCR)
    acco_ge1[i]=alltrim(H->FIELD_GET1)
    acco_ge2[i]=alltrim(H->FIELD_GET2)
    skip
  enddo
endif
name_str="filter.bln"
use &name_str SHARED               && Фильтр
public SIZE_FILTER
SIZE_FILTER = lastrec()
if SIZE_FILTER != 0
  public filter_fld[SIZE_FILTER]   && Имя  поля
  public filter_nam[SIZE_FILTER]   && Название поля
  public filter_iif[SIZE_FILTER]   && Название логического поля
  public filter_dbf[SIZE_FILTER]   && Название файла словаря
  public filter_ntx[SIZE_FILTER]   && и его файла индекса
  public filter_cod[SIZE_FILTER]   && Код из словаря
  public filter_scr[SIZE_FILTER]   && Листинг словаря
  i=0
  do while .not.eof()
    i=i+1
    filter_fld[i]=alltrim(H->FIELD_NAME)
    filter_nam[i]=alltrim(H->NAME_FIELD)
    filter_iif[i]=alltrim(H->IIF_STR)
    filter_dbf[i]=alltrim(H->DICT_DBF)
    filter_ntx[i]=alltrim(H->DICT_NTX)
    filter_cod[i]=alltrim(H->FIELD_COD)
    filter_scr[i]=alltrim(H->FIELD_SCR)
    skip
  enddo
endif
name_str="plan.bln"
use &name_str SHARED               && план
public SIZE_PLAN
SIZE_PLAN = lastrec()
if SIZE_PLAN != 0
  public plan_prm[SIZE_PLAN]
  public plan_dbf[SIZE_PLAN]
  public plan_str[SIZE_PLAN]
  i=0
  do while .not.eof()
    i=i+1
    plan_prm[i] = H->DICT_PRM
    plan_dbf[i] = alltrim(H->DICT_DBF)
    plan_str[i] = alltrim(H->DICT_STR)
    skip
  enddo
endif
name_str="list.bln"
use &name_str SHARED               && список
public SIZE_LIST
SIZE_LIST = lastrec()
if SIZE_LIST != 0
  public list_dbf[SIZE_LIST]
  public list_ext[SIZE_LIST]
  public list_frm[SIZE_LIST]
  i=0
  do while .not.eof()
    i=i+1
    list_dbf[i] = H->LIST_DBF
    list_ext[i] = alltrim(H->LIST_EXT)
    list_frm[i] = H->LIST_FRM
    skip
  enddo
endif
name_str="protoc.bln"
use &name_str SHARED         && Протокол
public SIZE_PROT
SIZE_PROT = lastrec()
public prot_jou[SIZE_PROT]   && Макро поле
public prot_fld[SIZE_PROT]   && Имя поля
public prot_nam[SIZE_PROT]   && Название поля
public prot_iif[SIZE_PROT]   && Название iif
i=1
do while .not.eof()
 do case
 case .not.empty(H->NAME_IIF)
  prot_jou[i]=alltrim(H->NAME_IIF)
 othe
  prot_jou[i]=alltrim(H->FIELD_NAME)
 endcase
 prot_fld[i]=alltrim(H->FIELD_NAME)
 prot_nam[i]=rtrim(H->NAME_FIELD)
 prot_iif[i]=alltrim(H->NAME_IIF)
 i=i+1
 skip
enddo
name_str="quan.bln"
use &name_str SHARED           && Лицевой счет
public SIZE_QUAN
SIZE_QUAN = lastrec()
if SIZE_QUAN != 0
  public quan_fld[SIZE_QUAN]   && Имя  поля
  public quan_nam[SIZE_QUAN]   && Название поля
  public quan_iif[SIZE_QUAN]   && Название логического поля
  public quan_row[SIZE_QUAN]   && Курсор Y
  public quan_col[SIZE_QUAN]   && Курсор X
  public quan_dbf[SIZE_QUAN]   && Название файла словаря
  public quan_ntx[SIZE_QUAN]   && и его файла индекса
  public quan_cod[SIZE_QUAN]   && Код из словаря
  public quan_scr[SIZE_QUAN]   && Листинг словаря
  i=0
  do while .not.eof()
    i=i+1
    quan_fld[i]=alltrim(H->FIELD_NAME)
    quan_nam[i]=alltrim(H->NAME_FIELD)
    quan_iif[i]=alltrim(H->IIF_STR)
    quan_row[i]=H->CUR_ROW
    quan_col[i]=H->CUR_COL
    quan_dbf[i]=alltrim(H->DICT_DBF)
    quan_ntx[i]=alltrim(H->DICT_NTX)
    quan_cod[i]=alltrim(H->FIELD_COD)
    quan_scr[i]=alltrim(H->FIELD_SCR)
    skip
  enddo
endif
name_str="exprec.bln"
use &name_str SHARED           && Лицевой счет
public SIZE_EXPREC
SIZE_EXPREC = lastrec()
if SIZE_EXPREC != 0
  public exre_fld[SIZE_EXPREC]   && Имя  поля
  public exre_nam[SIZE_EXPREC]   && Название поля
  public exre_iif[SIZE_EXPREC]   && Название логического поля
  public exre_row[SIZE_EXPREC]   && Курсор Y
  public exre_col[SIZE_EXPREC]   && Курсор X
  public exre_dbf[SIZE_EXPREC]   && Название файла словаря
  public exre_ntx[SIZE_EXPREC]   && и его файла индекса
  public exre_cod[SIZE_EXPREC]   && Код из словаря
  public exre_scr[SIZE_EXPREC]   && Листинг словаря
  i=0
  do while .not.eof()
    i=i+1
    exre_fld[i]=alltrim(H->FIELD_NAME)
    exre_nam[i]=alltrim(H->NAME_FIELD)
    exre_iif[i]=alltrim(H->IIF_STR)
    exre_row[i]=H->CUR_ROW
    exre_col[i]=H->CUR_COL
    exre_dbf[i]=alltrim(H->DICT_DBF)
    exre_ntx[i]=alltrim(H->DICT_NTX)
    exre_cod[i]=alltrim(H->FIELD_COD)
    exre_scr[i]=alltrim(H->FIELD_SCR)
    skip
  enddo
endif
use
return
*---------------------------------------------------------*
procedure InitOborot
public is_divis
public dDEBKRE
public dNUMB
public dNAME
public dMEAS
public dQUAN
public dEXPENSES
public dRECEIPTS
public dDATBEG
public dDATEKS
public dDATPER
public dDATEND
public dPROBEG
public dPROTEK
public dASHIFR
public dANORMA
public dKOEF
public dNOTEP
select 7
use_dbf=use_path+OborotDBF
if is_setj
 use &use_dbf SHARED
else
 use &use_dbf
endif
dDEBKRE=nulvar(DEBKRE)
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
use
is_divis=.F.
select 1
public dDEBET
public dSUBDEB
public dKREDIT
public dSUBKRE
public dSUMMA
public dNOTES
dDEBET=nulvar(A->DEBET)
dSUBDEB=nulvar(A->SUBDEB)
dKREDIT=nulvar(A->KREDIT)
dSUBKRE=nulvar(A->SUBKRE)
dSUMMA=nulvar(A->SUMMA)
dNOTES=nulvar(A->NOTES)
return
