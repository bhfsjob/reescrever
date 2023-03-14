/******************************************************************************
 nfs/v/nfsvpaco.p
 Colhe dados para o emissao do formulario e ou processar o faturamento.
******************************************************************************
Alterado por..: Bruno Hallais - BHFS.001 
Data alteração: 09/06/2022
Motivo........: Validações e atualizações referentes à itens do pacote
                Usado com controle &scoped-define GLPI29549
******************************************************************************/
&scoped-define pp com parametro

&scoped-define GLPI29549 Validações e atualizações
//&scoped-define GLPI20339 Brindes e doacoes

//&scoped-define GLPI28245 Nota fiscal refaturada NAO SUBIR PARA PRODUCAO

&scoped-define GLPI27820 Requisicoes consumo Interno Pacote/Retira
&scoped-define GLPI21345 Pedencia de preco em pedido de hora
&scoped-define GLPI5457  GLPI5457
&scoped-define GLPI440   GLPI440
&scoped-define GLPI643   GLPI643 Brinde
&scoped-define GLPI19319 Numero de notas por pedido
&scoped-define GLPI14387 Pedido orgao publico Municipal
&scoped-define GLPI17955 VENDA PARA CONSUMO - MG (AJUSTE REGIME ESPECIAL)
&scoped-define GLPI21191 Bloquear faturamento na falta do convenio MG

&scoped-define auto trim(string(flg_aut,"auto;/"))
&scoped-define FAT_PARCIAL PARCIAL
&global-define fmt  format ">,>>>,>>9.99"
&global-define fmta format "z,zzz,zz9.99"
&scoped-define fim  no-label at 35 when avail duplicata
&scoped-define fd   then "" else "*" format "X" to 75 skip
&global-define le   last-event:function
&global-define VLD  validate
&scoped-define hm   hide message no-pause
&scoped-define pdt  w_trn.tipo:private-data in frame f_1
&scoped-define te   input frame f_1 w_trn.tipo
&scoped-define tru  then true else false
&SCOPED-DEFINE mppg tambasa.mp_movimento
&scoped-define strf tambasa.sapreqtrf
&scoped-define itrf tambasa.sapitereqtrf
&scoped-define liv  tambasa.livro

&scoped-define err_msg  error-status:get-message(error-status:num-messages) 


def stream s_msg.
def var v_msg as cha no-undo.
def var e_msg as cha no-undo.
def var c_aux as cha no-undo.
def var e_det as cha no-undo.
def var qtd_inf as int no-undo init 0.

&IF DEFINED(GLPI21191) &THEN
function v_prcmedmes returns logical( output txt as character ).
   def var dat_aux as dat no-undo init today.
   assign dat_aux = dat_aux - day(dat_aux)
          dat_aux = dat_aux - day(dat_aux).
   
   for each  prcmedmes no-lock
       where prcmedmes.ano    = year(dat_aux)
       and   prcmedmes.mes    = month(dat_aux)
       and   prcmedmes.recnotfis = 0:
      return true.
   end.

   run rt/mmalfa.p ( month(dat_aux) ).
   assign txt = caps(return-value) + string(year(dat_aux)) + " NAO GERADO".
   return false.           
end function.                                                   /*v_prcmedmes*/
&ENDIF

function codpac_separa returns int64 ( input asd_ped as int64 ).
   def var asd_int as int64 no-undo.
   for first itesep no-lock where itesep.asdped = asd_ped: end.
   if avail itesep then do:
      assign asd_int = itesep.codpac.
      return asd_int.
   end.
   if asd_ped > 999999999 then do:
      assign asd_int = int( substr(string(asd_ped,"9999999999"),1,5)  +
                            substr(string(asd_ped,"9999999999"),7)
                          ).
      if can-find(first separa no-lock
                  where separa.codpac = asd_int) then return asd_int.
   end.
   assign asd_int = int(substr(string(asd_ped,"9999999999"),2)).
   if can-find(first separa no-lock
               where separa.codpac = asd_int ) then return asd_int.
   for first _file no-lock
       where _file._file-name = "separa"
      ,first _field of _file no-lock
       where _field._field-name = "codpac"
       and   (if _field._Data-Type = "integer" then true else false):
      assign asd_ped = asd_int.
   end.
   return asd_ped.
end function.                                                 /*codpac_separa*/

function f_log returns logical ( input var_msg      as character,
                                 input-output v_msg as character).
   if v_msg = "" then do:
      assign v_msg = os-getenv("_TMP") + "/nfsvpaco" 
                   + string(year (today),"9999")
                   + string(month(today),"99")
                   + string(day  (today),"99").
   end.
   output stream s_msg to value(v_msg) append.
   put stream s_msg unformatted 
       "nfs/v/nfsvpaco " + string(string(now),"x(19)")      + " " 
                         + string(userid("Dictdb"),"x(08)") + " "
                         + var_msg skip.
   output stream s_msg close.
end function.                                                         /*f_log*/
def var t as int no-undo.
def var nro_cce as int initial 0      no-undo.
def var qtd_cce as int initial 0      no-undo. 
def var tot_cce as int initial 0      no-undo. 
def var cod_pac as int initial 0      no-undo.
def var pac_nft as int initial 0      no-undo.
def var rec_not as int initial 0      no-undo.
def var dat_ven as dat initial today no-undo.
def var seq_not as int initial 0  no-undo.

def var fre_def as cha                no-undo.
def var sta_nfe as cha                no-undo.
def var flg_aut as log initial false  no-undo.

/** variavel utilizada no novo processo de impresao de nota fiscal ******/
define variable num_seq_imp as integer no-undo.

define new shared variable Opc_Prg  as character                       no-undo.
define new shared variable Par_Prg  as character                       no-undo.
define     shared variable cod_mnu  as character                       no-undo.
define     shared variable syst_nam as character format "x(30)"        no-undo.
define     shared variable id       like Usuario.UsrIdt                no-undo.
define     shared variable password like Usuario.UsrPas                no-undo.
define     shared variable grp_cha  as character                       no-undo.

&IF DEFINED(pp) &THEN def input param par_in as cha no-undo.
                &ELSE def var par_in as cha no-undo.
      assign par_in = "C,S,F,1,6,510202,ENT,13627038,0,0,C,simula,3213300002,".
&ENDIF

/*Significado das entradas *****************************************************
  1  Cliente/Fornecedor
  2  Entrada/Saida
  3  Emitir/Faturar
  4  Codigo empresa
  5  Codigo da empresa contabil
  6  Codigo Fiscal
  7  Codigo Frete
  8  Codigo Carga
  9  Codigo pacote
  10 Recnotfis
  11 Tipo destinatatio "tipped.tipdst"
  12 Tipo de pedido
******************************************************************************/
def shared var var_wai  as int no-undo initial 60.
def shared var max_bp   as int no-undo initial 64.
def shared var buf_dec  as cha no-undo.
def        var var_waio as int no-undo initial 1.
def        var rsp      as cha no-undo.
def        var ult_eve  as cha no-undo initial "return".
def        var cod_cfo  as int no-undo.
def        var tip_emi  as cha no-undo.
def        var sit_prc  as cha case-sensitive no-undo.
def        var cod_fre  as cha no-undo.
def        var cod_car  as int no-undo.
def        var asd_ped  as int64 no-undo.
def        var int_64   as int64 no-undo.
def        var cod_emp  as int no-undo.
def        var emp_ctb  as int no-undo.
def        var emp_emi  as int no-undo.
def        var emp_dst  as int no-undo.
def        var asd_trf_ini  as int no-undo init -1.
def        var asd_trf  as int no-undo.
def        var rep_car  as cha no-undo initial "" format "x(52)".
def        var varf_msg as cha no-undo.
def        var val_cnv  as dec no-undo.
def        var cod_est  as int no-undo.

def        var ins_est  as cha no-undo.
def        var lph_wms  as cha no-undo.
def        var dsc_cfo  as cha no-undo format "x(78)".
def        var i        as int.
def        var nro_not  as int.
def        var hor_ini  as int.
def        var max_ite  as int.
def        var nro_ite  as int.
def        var msg_fat  as cha format "x(52)".
def        var a_tel    as cha format "X".
def        var cap_tnf  as int format "9999". 
def        var cap_inf  as int format "9999". 
def        var v_com    as cha no-undo.
def        var pes_cnt  as cha no-undo.
def        var consumo  as log no-undo init false.
def        var sit_dvg  as cha no-undo.
def        var rec_ref  as int no-undo initial 0.
def        var trn_cd   as int no-undo.
def        var pla_cd   like pacote.numpla.
def        var rtn_val  as cha no-undo.
def        var lst_pac  as cha no-undo.
def        var cod_cxa  as int no-undo.
def        var cod_trn_pac as int no-undo.
def var pdf_fil as cha no-undo.

def var nro_rty  as int no-undo.

{ nfs/.i/nfs_itrb.i  "new shared" "transaction" }
{ nfs/.i/nota.i      "w_not" "FN_01" "define"   }
{ nfs/.i/w_ideso.i                              }

&IF DEFINED(GLPI440) &THEN 
def temp-table w_pedtok like pedtok
    field valnot    as dec.
&ENDIF
def temp-table w_emlcrt no-undo
    field dscmsg    as cha.

def temp-table w_ufprd no-undo
    field codest    as int  format "99"     label "UF"
    field nronot    as int  format ">>9"    label "NFs"
    field nroinf    as int  format ">>9"    label "ITs"
    field stripd    as cha  format "x(50)"  label "Produtos"
index codest is primary codest.

def buffer b_pac  for {&pac}.
def buffer b_cli  for {&cli}.
def buffer bf_sta for {&sta}.
def buffer f_ectb for {&ectb}.
def buffer f_tpd  for tipped.
def buffer b_sta  for {&est}.
def buffer bf_uf  for {&sta}.
def buffer bf_cli for {&cli}.

/* gilson (27-03-18) */ 
def buffer x_car  for carga.
def buffer x_ped  for pedido. 
def buffer x_sap  for sapreqtrf.
def buffer x_pac  for pacote. 
def buffer x_isap for sapitereqtrf.
def buffer x_ectb for {&ectb}.

{ trg/.funcao/fcdatliv.i }
{ trg/.funcao/fcf_msg.i  }
{ trg/.funcao/fcstaple.i }
function v_aidf returns character ( input emp_ctb as integer,
                                    input dat_emi as date,
                                    input nr_aidf as character
                                  ).
   def var a as cha no-undo .                                  
   for each  {&aidf} no-lock
       where {&aidf}.empctb  = emp_ctb
       and   {&aidf}.dtaidf <= dat_emi   
       and   (if {&aidf}.datlim >= dat_emi                    and
                 {&aidf}.frmini + {&aidf}.nrofrm > {&aidf}.frmatu
                 then true
                 else false
             ) by {&aidf}.datlim by {&aidf}.dtaidf:
      if {&aidf}.nraidf <> nr_aidf then do:
         assign a = a + (if a = "" then "" else "\n")
                  + "AIDF: ~"" + nr_aidf 
                  + "~" NAO CONFERE C/ ~"" + {&aidf}.nraidf
                  + "~"".
      end. else do:
         assign a = a + (if a = "" then "" else "\n") + "ok".
      end.
   end.
   assign a = (if a = "" 
                  then "AIDF: " + nr_aidf + " NAO ENCONTRADA\n" +
                       "DATA: " + string(dat_emi,"99/99/9999")
                  else a
               ).
   return a.
end function.                                                        /*v_aidf*/
 
function emite_nota returns logical (buffer b_not for {&not} ).
   def buffer b_cfo for {&cfo}.
   for last  b_cfo  no-lock 
       where b_cfo.codcfo     = b_not.codcfo
       and   b_cfo.datultatu <= b_not.datmov:
      if b_cfo.flgnumtbs then return true.
   end.
   return false.
end function.                                                   /*emite_nota*/

function f_centra returns character ( input dsc as character,
                                      input tam as integer
                                    ).
   assign dsc = trim(dsc)
          dsc = fill(" ",int(trunc((tam - length(dsc)) / 2,0))) + dsc.
   return dsc.          
end function.                                                      /*f_centra*/
function key_code returns character ( input cod_sit as character ).
    return string(keycode(cod_sit),"999").
end function.                                                      /*key_code*/
function f_dscacao returns character ( input st_acao as character,
                                       input i       as integer
                                     ).
   case st_acao:
      when "L" then assign st_acao = "faturar,faturamento".
      when "T" then assign st_acao = "transferir,transferencia".
      otherwise st_acao = ",".
   end.
   return entry(i,st_acao).
end function.                                                     /*f_dscacao*/
function v_cfop returns integer ( input max_ite as integer,
                                  input cod_cfo as integer
                                ).

   if max_ite > 90 then assign max_ite = 90.
   if entry(1,string(cod_cfo,"9,99999"),".") = "6" and
      max_ite > 90 then assign max_ite = 90.
   return max_ite.
end.
function fv_maxite returns integer ( input emp_ctb as integer,
                                     input cod_cfo as integer
                                   ).
   def var max_ite as int no-undo.                                   
   def buffer b_pnf for {&pnfe}.
   def buffer b_cfo for {&cfo}.
   def buffer b_onf for {&onf}.
   def buffer b_mna for {&mna}.

   for first {&pnfe} no-lock where {&pnfe}.empctb = emp_ctb:
     assign max_ite = v_cfop ({&pnfe}.maxinfsai  , cod_cfo ).
   end.
   if max_ite = 0 then return 0.
   find last  b_cfo no-lock where b_cfo.codcfo = cod_cfo.
   find first b_onf of b_cfo no-lock where 
              b_onf.desobsnotfis <> "." no-error.
   find first b_mna of b_cfo no-lock no-error.
   assign max_ite = v_cfop ({&pnfe}.maxinfsai  , cod_cfo )
                  - (if not avail b_mna then 0 else 1)
                  - (if not avail b_onf then 0 else 1)
                  - (if b_cfo.DesMsgFis[1] = "" then 0 else 1)
                  - (if b_cfo.DesMsgFis[2] = "" then 0 else 1)
                  - (if b_cfo.DesMsgFis[3] = "" then 0 else 1).
   return max_ite.
end function.                                                     /*fv_maxite*/

function vl_dspace returns decimal ( buffer b_ped for {&ped},
                                     buffer b_cfo for {&cfo},
                                     buffer b_tpd for tipped,
                                     buffer b_cli for {&cli},
                                     input cfo_opb as integer
                                   ).
   if cfo_opb > 0
   or b_tpd.pedb2c
   or b_ped.numordcom <> "" and lookup(b_ped.idtdspace,"C") = 0
   or b_ped.idtdspace = "L"
   or b_cli.cmndspace = "ISENTO"
   or not b_cfo.IndAtuCtr
   or not b_cfo.flgcreicm
   or lookup(b_tpd.tipdst,"C") = 0
   or lookup(b_tpd.grpprd,"CNS,CRB,DFT,DOA,FEI,PBO,SAC,TRF,MGL") > 0
   or lookup(b_ped.tipped,"PST,PRT,PSF,PFM,PFH,PD,PDD,PRC,PRM") > 0
   or lookup(b_cfo.tipopesrf,"028") > 0 /*GLPI18320*/ 
      then return 0.00.

   for first {&liv} no-lock
       where {&liv}.codliv = {&ped}.codliv
       and   (if {&liv}.valdspace > 0 {&tru}):
      return {&liv}.valdspace.    
   end.

   RETURN
   if {&ped}.codliv >= 202027 then 7.99 else
   if {&ped}.codliv >= 201827 then 6.49 else
   if {&ped}.codliv >= 201740 then 5.99 else
   if {&ped}.codliv >= 201623 then 5.49 else
   if {&ped}.codliv >= 201501 then 4.99 else
   if {&ped}.codliv >= 201306 then 3.99 else 
   if {&ped}.codliv >= 201028 then 3.49 else
   if {&ped}.codliv >= 200748 then 2.99 else 2.49.
   
end function.                                                     /*vl_dspace*/

function f_rsp return character (input flg_aut as logical,
                                 input sml_fat as logical
                                ).
   if flg_aut 
   or sml_fat then return ";Tecle <R>e-iniciar <A>bortar;R;A".
   return ";Tecle <T>entar <R>e-iniciar <A>bortar;T;R;A".
end function.                                                         /*f_tra*/

def frame f_1
   w_trn.tipo     label    "Emissao por...."
                  {&VLD}(lookup({&le},"end-error") > 0  or 
                         lookup(input frame f_1 w_trn.tipo,
                                {&pdt}) > 0
                        ,"Tipo de emissao invalido")
                  help "<P>acote <C>arga <N>nota <F4>Volta"         skip
   w_trn.codemp   label "Empresa........"
                  {&VLD}(lookup({&le},"end-error,cursor-up") > 0 or 
                         can-find(first {&emp} no-lock 
                                  where {&emp}.codemp = 
                                        input frame f_1 w_trn.codemp
                                  )
                         ,"Empresa nao cadastrada")
                  help "F4_Volta Codigo empresa"                    space(4)
   {&emp}.nomemp  no-label                                          skip
   w_trn.empctb   label "Emp Contabil..."
                  {&VLD}(lookup({&le},"end-error,cursor-up") > 0 or
                         can-find(first {&ectb} no-lock 
                                  where {&ectb}.empctb = 
                                        input frame f_1 w_trn.empctb
                                  and   {&ectb}.cnpj begins {&emp}.cgcemp
                                 )
                         ,"Empresa contabil nao cadastrada")                 
                  help "F4_Volta Codiga da contabil"                space(3)
   {&ectb}.nomfts no-label    format "x(55)"                        skip
   w_trn.codfre   label "Tipo frete....."
                  {&VLD}(lookup({&le},"end-error,cursor-up") > 0 or 
                         can-find(first {&tfr} no-lock 
                                  where {&tfr}.codfre = 
                                        input frame f_1 w_trn.codfre
                                 )
                        ,"Tipo de frete nao cadastrado")
                  help "Tipo de frete"                              space(2)
   {&tfr}.desfre  no-label                                          skip
   w_trn.codemb   label "Embalagem......"
                 {&VLD}(lookup({&le},"end-error,cursor-up") > 0  or 
                         can-find(first {&emb} no-lock 
                                  where {&emb}.codemb = 
                                        input frame f_1 w_trn.codemb
                                 )
                        ,"Embalagem nao cadastrada")
                  help "Codigo da embalagem"                        space(3)
   {&emb}.desemb  no-label                                          skip
   w_trn.codvia   label "Via transporte."
                  {&VLD}(lookup({&le},"end-error,cursor-up") > 0 or
                         can-find(first {&via} no-lock 
                                  where {&via}.codviatrn = 
                                        input frame f_1 w_trn.codvia
                                 )
                        ,"Via de transporte nao cadastrada")
                  help "Codigo da via de transporte"                space(4)
   {&via}.desvia  no-label                                          skip
   w_trn.codtrn   label "Transportador.." format ">>>>" 
                  {&VLD}(lookup({&le},"end-error,cursor-up") > 0 or 
                         input frame f_1 w_trn.codtrn = 0        or
                         can-find(first {&trn} no-lock 
                                  where {&trn}.codtrn = 
                                        input frame f_1 w_trn.codtrn
                                  )
                         ,"Transportador nao cadastrado")
                  help "Codigo transporte."                         space(1)
    w_trn.nomtrn  no-label help "Nome transportador. F2_lista"      skip
    "Endereco.......:" space(6) w_trn.endtrn no-label               skip
    "Municipio......:" space(6) w_trn.muntrn no-label               skip
    "Estado.........:" space(6)
    w_trn.stapla  no-label 
                  {&VLD}(lookup({&le},"end-error,cursor-up") > 0 or 
                         input frame f_1 w_trn.stapla = ""       or
                         can-find(first {&sta} no-lock 
                                  where {&sta}.sigest = 
                                        input frame f_1 w_trn.stapla
                                 )
                        ,"Estado nao cadastrado")
                  help "(F2_lista) Sigla do estado"                 space(2)
    {&sta}.nomest no-label                                          skip
    "Placa veiculo..." space(6)
    w_trn.plavei  no-label
                  {&VLD}(lookup({&le},"end-error,cursor-up") > 0 or 
                         input frame f_1 w_trn.plavei = ""       or
                         length(trim(input frame f_1 w_trn.plavei)) = 7
                        ,"Placa do veiculo invalida")
                  help "Placa do veiculo"                           skip
    dsc_cfo       no-label                                          skip
    w_trn.codpac  label "Pacote........."
                  {&VLD}(lookup({&le},"end-error,cursor-up") > 0 or 
                         can-find(first {&pac} no-lock 
                                  where {&pac}.codpac = 
                                         input frame f_1 w_trn.codpac
                                 )
                        ,"Pacote nao cadastrado")
                  help "Numero do pacote"                           skip
    w_trn.codcar  label "Carga.........."
                  {&VLD}(lookup({&le},"end-error,cursor-up") > 0 or 
                         can-find(first {&car} no-lock 
                                  where {&car}.codcar = 
                                        input frame f_1 w_trn.codcar
                                 )
                        ,"Carga nao cadastrada")
                  help "Numero da carga"                            space(1)
    rep_car       no-label                                          skip
    w_trn.datemi  label "Data de emissao"
                  {&VLD}(lookup({&le},"end-error,cursor-up") > 0 or 
                         input frame f_1 w_trn.datemi <= to_day
                        ,"Data de emissao invalida")
                  Help "Data de emissao da nota"                    skip

    msg_fat       no-label format "x(78)"                           skip
    w_trn.notinf  label "Nota fiscal de." format ">>>>>>>>9"        space(2) 
    a_tel         no-label                                          space(1)
    w_trn.notsup  no-label format ">>>>>>>>9"                       skip
    w_trn.notref  label "Nota Ori Refat."                           space(10)
    w_trn.qtdemb  label "Qtd Embalagens."                           skip
    w_trn.datsai  label "Data de saida.."
                  {&VLD}(lookup({&le},"end-error,cursor-up") > 0 or 
                         input frame f_1 w_trn.datsai = ?                 or
                         input frame f_1 w_trn.datsai >= to_day and
                         input frame f_1 w_trn.datsai <= to_day + 5
                        ,"Data de saida invalida0")
                  Help "Data para saida do veiculo"
with side-labels 1 down row 2 centered overlay title "" V6FRAME.

{ nfs/.i/nfs_itrf.i "new shared" }

def temp-table nf_fat field recnotfis as int index recnotfis recnotfis.
def temp-table nf_srt field recnotfis as int index recnotfis recnotfis.

def temp-table w_pspr
    field asdped    as int64 FORMAT ">9999,99999"
    field numnot    as int format ">>>>>>>>9"
    field tipwms    as cha
    field flgati    as log
    field sitcar    as cha
    field movrsv    as int init 1
    field nrobox    as int
index asdped is primary asdped.    
def temp-table ipd_a_f 
    field ridipd as row
    field nrolin as int init 0
    field idprod as int
    field uffatu as int init 0
    &IF DEFINED(GLPI643) &THEN
    field prdbrd as cha init "N" &ENDIF
index idprod idprod
index uficms uffatu idprod.

/*Inicio dos triggers*********************************************************/
on cursor-up,end-error             of w_trn.tipo   in frame f_1 or
   cursor-up,end-error             of w_trn.codemp in frame f_1 or
   cursor-up,end-error             of w_trn.empctb in frame f_1 or    
   cursor-up,end-error             of w_trn.codfre in frame f_1 or
   cursor-up,end-error             of w_trn.codemb in frame f_1 or
   cursor-up,end-error             of w_trn.codvia in frame f_1 or
   cursor-up,end-error,cursor-down of w_trn.codtrn in frame f_1 or
   cursor-up,end-error             of w_trn.nomtrn in frame f_1 or
   cursor-up,end-error             of w_trn.endtrn in frame f_1 or
   cursor-up,end-error             of w_trn.muntrn in frame f_1 or
   cursor-up,end-error             of w_trn.stapla in frame f_1 or
   cursor-up,end-error             of w_trn.plavei in frame f_1 or
   cursor-up,end-error             of w_trn.codpac in frame f_1 or
   cursor-up,end-error             of w_trn.codcar in frame f_1 or
   cursor-up,end-error             of w_trn.datemi in frame f_1 or
   cursor-up,end-error             of w_trn.notinf in frame f_1 or
   cursor-up,cursor-left,end-error of w_trn.notsup in frame f_1 or
   cursor-up,end-error             of w_trn.notref in frame f_1 or
   cursor-up,end-error             of w_trn.qtdemb in frame f_1 or
   cursor-up,end-error             of w_trn.datsai in frame f_1 do:
   apply {&le}.
   return no-apply.
end.
on end-error of frame f_1 do:
   if input frame f_1 w_trn.tipo   = "" and w_trn.tipo:sensitive in frame f_1
   or input frame f_1 w_trn.codemp = 0  and w_trn.codemp:sensitive in frame f_1
   or input frame f_1 w_trn.empctb = 0  and w_trn.empctb:sensitive in frame f_1
   or input frame f_1 w_trn.codfre = "" and w_trn.codfre:sensitive in frame f_1

   or not w_trn.codfre:sensitive in frame f_1 and
      not w_trn.empctb:sensitive in frame f_1 and
      not w_trn.codemp:sensitive in frame f_1 and
      /*not w_trn.tipo:sensitive in frame f_1   and*/
      input frame f_1 w_trn.codemb = ""
      then do:
      apply "window-close" to this-procedure.
      return no-apply.  
   end.
   clear frame f_1 no-pause.    
   delete w_trn. create w_trn. find first w_trn.
   assign w_trn.tipo   = tip_emi
          w_trn.codemp = cod_emp
          w_trn.empctb = emp_ctb
          w_trn.codfre = cod_fre
          w_trn.codpac = cod_pac
          w_trn.codcar = cod_car.
   disp w_trn.tipo
        w_trn.codemp
        w_trn.empctb
        w_trn.codfre
        w_trn.codpac
        w_trn.codcar    with frame f_1.
   run A_W_TRN (buffer {&emp},buffer {&ectb},buffer {&tfr},buffer {&trn}).
   run D_F1.
   if w_trn.tipo:sensitive in frame f_1 then
      apply "entry" to w_trn.tipo in frame f_1.     else
   if w_trn.codemp:sensitive in frame f_1 then
      apply "entry" to w_trn.codemp in frame f_1.   else
   if w_trn.empctb:sensitive in frame f_1 then
      apply "entry" to w_trn.empctb in frame f_1.   else
   if w_trn.codfre:sensitive in frame f_1 then
      apply "entry" to w_trn.codfre in frame f_1.   else
   if w_trn.codemb :sensitive in frame f_1 then
      apply "entry" to w_trn.codemb in frame f_1.      
   return no-apply.
end.
on help of w_trn.empctb in frame f_1 do: {&hm}.
   assign buf_dec = "cnpj=" + string({&emp}.cgcemp).
   run hlp/cod/empctb.p.
   assign buf_dec = "".
   if return-value <> "" then do:
      assign emp_ctb         = int(entry(1,return-value))
             w_trn.empctb    = emp_ctb
             entry(4,par_in) = entry(1,return-value).
      for first {&ectb} no-lock where {&ectb}.empctb   = emp_ctb: 
          disp w_trn.empctb {&ectb}.nomfts with frame f_1.
      end.
   end.    
   return no-apply.
end.
on help of w_trn.codtrn in frame f_1 do: {&hm}.
   assign buf_dec = (if lookup(input frame f_1 w_trn.codfre,"CIF,FOB") > 0
                        then "natcam=T@flgati=A"
                        else "natcam=A;F@flgati=A"
                       ).
   run hlp/cod/codtrn.p.
   assign buf_dec = "".
   if return-value <> "" then 
      disp int(return-value) @ w_trn.codtrn with frame f_1.
   return no-apply.
end.      
on help of w_trn.codpac in frame f_1 do: {&hm}.
   if caps(entry(3,par_in)) = "F" then do:
      run MT/LG/mtlgasep.p ('P,5,5,P,@SELECIONAR,SELECAO'). /*,T TESTE*/
   end. else do:
      run MT/LG/mtlgasep.p ('P,6,6,P,@SELECIONAR,SELECAO'). /*,T TESTE*/
   end.
   if return-value <> "" then do:
      disp int(return-value) @ w_trn.codpac with frame f_1.
   end.
end.
on get of w_trn.codpac in frame f_1 do: {&hm}.
   if caps(entry(3,par_in)) = "E" then return no-apply.
   run nfs/v/nfsvpfat.p ( input frame f_1 w_trn.empctb ).
   assign pac_nft = int(return-value) no-error.
   if return-value <> "" then disp pac_nft @ w_trn.codpac with frame f_1.
   return no-apply.      
end.
on help of w_trn.notinf in frame f_1 do: {&hm}.
   for each  {&not} no-lock 
       where {&not}.datemi = w_trn.datemi 
       and   {&not}.codemp = w_trn.codemp
       and   {&not}.empctb = w_trn.empctb
       and   (if {&not}.flgclifor = entry(1,par_in) and
                 {&not}.flgnotfis = entry(2,par_in)
                 {&tru}
             ) by {&not}.numnotfis:
      if entry(1,par_in) = "C" and entry(2,par_in) = "S"
      or emite_nota( buffer {&not} ) then do:
         disp {&not}.numnotfis @ w_trn.notinf with frame f_1.
         return.
      end.
   end.
   apply "cursor-up" to w_trn.notinf in frame f_1.
   return no-apply.
end.
on help of w_trn.notsup in frame f_1 do: {&hm}.
   for each  {&not} no-lock 
       where {&not}.datemi = w_trn.datemi 
       and   {&not}.codemp = w_trn.codemp 
       and   {&not}.empctb = w_trn.empctb
       and   (if {&not}.flgclifor = entry(1,par_in) and
                 {&not}.flgnotfis = entry(2,par_in)
                 {&tru}
             )
       and   (if {&not}.flgclifor = entry(1,par_in) and
                 {&not}.flgnotfis = entry(2,par_in)
                 {&tru}
             ) by {&not}.numnotfis desc:
      if entry(1,par_in) = "C" and entry(2,par_in) = "S"
      or emite_nota( buffer {&not} ) then do:
         disp {&not}.numnotfis @ w_trn.notsup with frame f_1.
         return.
      end.
   end.
   return no-apply.
end.
on any-printable of w_trn.tipo in frame f_1 do:
   if entry(3,par_in) = "F" and lookup({&le},"n,N") > 0 then return no-apply.
   case {&le}:
      when "p" or when "P" then assign w_trn.tipo = "PACOTE".
      when "c" or when "C" then assign w_trn.tipo = "CARGA".
      when "n" or when "n" then assign w_trn.tipo = "NOTA".
      otherwise return no-apply.
   end. 
   disp w_trn.tipo with frame f_1.
   apply "tab" to w_trn.tipo in frame f_1.
   return no-apply.
end.
on entry of w_trn.tipo in frame f_1 do: {&hm}.
   if {&pdt} <> "" and num-entries({&pdt}) = 1 then do:
      disp {&pdt} @ w_trn.tipo with frame f_1.
      return no-apply.
      /*apply "tab".*/
   end.
end.
on leave of w_trn.tipo in frame f_1 do: {&hm}.
   if lookup({&le},"cursor-up,end-error") > 0 then return.
   if not w_trn.tipo:validate() in frame f_1 then return no-apply.
   if lookup(w_trn.tipo,{&pdt}) = 0 then do:
      message "Tipo invalido".
      return no-apply.
   end.
   disp w_trn.tipo with frame f_1.
   run D_F1.
   disp rep_car with frame f_1.
   run ENA_F_1 ( 2 ).
end.
on leave of w_trn.codemp in frame f_1 do: {&hm}.
   if lookup({&le},"end-error,cursor-up") > 0 then return.
   if not w_trn.codemp:validate() in frame f_1 then return no-apply.
   assign frame f_1 w_trn.codemp.
   for first {&emp} no-lock where  {&emp}.codemp = w_trn.codemp:
      disp {&emp}.nomemp with frame f_1.
   end.   
end.
on leave of w_trn.empctb in frame f_1 do: {&hm}.
   if lookup({&le},"end-error,cursor-up") > 0 then return.
   if not w_trn.empctb:validate() in frame f_1 then return no-apply.
   assign frame f_1 w_trn.empctb.
   for first {&ectb} no-lock where {&ectb}.empctb = w_trn.empctb: end.
   if not avail {&ectb} then return no-apply.
   disp {&ectb}.nomfts with frame f_1.
   if not par_in begins "C,S,F" then return.
   assign cod_cfo= (if num-entries(par_in) >= 6 
                       then integer(entry(6,par_in)) 
                       else 0
                   ).
   for first cfotrs no-lock
       where cfotrs.empctb = w_trn.empctb
       and   (if cfotrs.tiptrs = "vda" {&tru})
      ,last  {&cfo} no-lock
       where {&cfo}.codcfo = cfotrs.codcfo:
      assign cod_cfo = cfotrs.codcfo.    
   end.    
end.
on leave of w_trn.codfre in frame f_1 do: {&hm}.   
   if lookup({&le},"end-error,cursor-up") > 0 then return.
   if not w_trn.codfre:validate() then return no-apply.
   assign frame f_1 w_trn.codfre.
   for first {&tfr} no-lock 
       where {&tfr}.codfre = input frame f_1 w_trn.codfre.
      disp w_trn.codfre {&tfr}.desfre with frame f_1.
      if {&tfr}.codobstrn <> "" then do:
         assign w_trn.nomtrn = w_trn.nomtrn
                w_trn.endtrn = {&tfr}.codObsTrn
                w_trn.stapla = ""
                w_trn.muntrn = ""
                w_trn.plavei = "".  
         disp "" @ {&sta}.nomest with frame f_1.
      end. else do:
         assign w_trn.endtrn = if w_trn.nomtrn = "" 
                                  then "" 
                                  else w_trn.endtrn.
      end.
      run D_F1.
   end.
end.
on leave of w_trn.codemb in frame f_1 do: {&hm}.
   if lookup({&le},"end-error,cursor-up") > 0 then return.
   if input frame f_1 w_trn.codemb = "" then 
      disp "G" @ w_trn.codemb with frame f_1.
   disp caps(input frame f_1 w_trn.codemb) @ w_trn.codemb with frame f_1.
   if not w_trn.codemb:validate() in frame f_1 then return no-apply.
   assign frame f_1 w_trn.codemb.
   for first {&emb} no-lock where {&emb}.codemb = w_trn.codemb:
      disp w_trn.codemb {&emb}.desemb with frame f_1. 
   end. 
end.
on leave of w_trn.codvia in frame f_1 do: {&hm}.
   if lookup({&le},"end-error,cursor-up") > 0 or frame-field = "empctb"
      then return.
   if w_trn.codvia:screen-value in frame f_1 = "" then 
      disp "R" @ w_trn.codvia with frame f_1.
   if not w_trn.codvia:validate() in frame f_1 then return no-apply.
   assign frame f_1 w_trn.codvia
          w_trn.codvia = caps(w_trn.codvia).
   for first {&via} no-lock where {&via}.codviatrn = w_trn.codvia:
      disp w_trn.codvia {&via}.desvia with frame f_1.
      return.
   end.
   return no-apply.
end.
on leave,F98 of w_trn.codtrn in frame f_1 do: {&hm}.
   if lookup({&le},"end-error,cursor-up") > 0 then return.
   if not w_trn.codtrn:validate() in frame f_1 then return no-apply.
   /*GLPIxxxx Liberar armazem com transporte proprio*/
   if input frame f_1 w_trn.codtrn = 4556 then do:
      run rt/rtedmsg.p (60,"   A T E N C A O   ", "", 
                    "\n\nTRANSPORTADOR ~"4556~" INVALIDO\n\n"
                    ) no-error.
   end.
   assign frame f_1 w_trn.codtrn
          w_trn.nomtrn:sensitive in frame f_1 = true
          w_trn.endtrn:sensitive in frame f_1 = true
          w_trn.plavei:sensitive in frame f_1 = true
          w_trn.muntrn:sensitive in frame f_1 = true
          w_trn.stapla:sensitive in frame f_1 = true.
   for first {&trn} no-lock 
       where {&trn}.codtrn = w_trn.codtrn
      ,first {&mun} of {&trn} no-lock
      ,first {&sta} of {&mun} no-lock:
      assign w_trn.nomtrn = {&trn}.nomtrn
             w_trn.stapla = {&sta}.sigest.
      /* Se transportador sem cpf *****/
      if not {&trn}.flgrpa and {&trn}.codcnpjcpf = "" then do:
         run rt/rtedmsg.p(30,"ATENCAO","",
               "Transportador sem CNPJ/CPF entre em contato com Transporte")
               no-error.
      end.
      if not avail {&tfr} or {&tfr}.codobstrn = "" then do:
         assign w_trn.endtrn = {&trn}.desend
                w_trn.plavei = (if avail {&pac} /*and 
                                   ({&pac}.codtrn = 2061 or 
                                    {&pac}.codtrn = 2554 or
                                    {&pac}.codtrn = 5286 or
                                    {&pac}.codtrn = 5287 or
                                    {&pac}.codtrn = 4781)*/
                                then {&pac}.numpla else {&trn}.numpla)
                w_trn.muntrn = {&mun}.desmun
                w_trn.stapla = {&sta}.sigest
                w_trn.nomtrn:sensitive in frame f_1 = false
                w_trn.endtrn:sensitive in frame f_1 = false
                w_trn.plavei:sensitive in frame f_1 = false
                w_trn.muntrn:sensitive in frame f_1 = false
                w_trn.stapla:sensitive in frame f_1 = false.
      end.
      leave.
   end.
   apply 399 to w_trn.codtrn in frame f_1.
   if rsp = "no-apply" then return no-apply.
   for first {&sta} no-lock 
       where {&sta}.sigest = input frame f_1 w_trn.stapla:    
   end.
   disp w_trn.codtrn
        (if frame f_1 w_trn.codemp = 5 then "O PROPRIO" else w_trn.nomtrn) 
         @ w_trn.nomtrn
        w_trn.endtrn
        w_trn.muntrn
        w_trn.plavei
        w_trn.stapla
        {&sta}.nomest when avail {&sta}
   with frame f_1.
   if lookup({&le},"cursor-down") > 0 then do:
      case {&te}:
         when "PACOTE" then apply "entry" to w_trn.codpac in frame f_1.
         when "CARGA"  then apply "entry" to w_trn.codcar in frame f_1.
         when "NOTA"   then apply "entry" to w_trn.datemi in frame f_1.
      end case.
      return no-apply.
   end.
end.
ON F99 of w_trn.codtrn in frame f_1 do: {&hm}.   
   assign rsp = "ok".
   if entry(3,par_in) = "E" then return no-apply.
   /*cotta - 12/06/18
   if (avail {&tfr} and {&tfr}.flgtomseremp and {&tfr}.codobstrn = "" and
       (avail {&trn} and lookup({&trn}.natcam,"A,F") = 0) or 
       avail {&trn} and lookup({&tfr}.codfre,"CIF,FOB") > 0 and 
       {&trn}.natcam <> "T") and 
      {&pac}.codtrn <> 2061 and /*Precisa*/ 
      {&pac}.codtrn <> 2554 and {&pac}.codtrn <> 4781 /*Uruacu*/ then do:
      message "Natureza do transportador invalida".
      assign rsp = "no-apply".
      return no-apply.
   end.
   */
   if avail {&trn} and not {&trn}.flgati then do:
      message "Transportador invalido".
      assign rsp = "no-apply".
      return no-apply.
   end.
end.
on leave of w_trn.nomtrn in frame f_1 or
   leave of w_trn.endtrn in frame f_1 or
   leave of w_trn.muntrn in frame f_1 do:
   assign frame f_1 w_trn.nomtrn
          frame f_1 w_trn.endtrn
          frame f_1 w_trn.muntrn
          frame f_1 w_trn.plavei
          frame f_1 w_trn.stapla.
end.
on leave of w_trn.stapla in frame f_1 do: {&hm}.
   if lookup({&le},"end-error,cursor-up") > 0 then return.
   if not w_trn.stapla:validate() in frame f_1 then return no-apply.
   assign frame f_1 w_trn.stapla.   
   if w_trn.stapla = "" then return.
   for first {&sta} no-lock where {&sta}.sigest = w_trn.stapla:
      disp {&sta}.nomest with frame f_1.
      return.
   end. 
   message "Estado nao cadastrado".
   return no-apply.
end.                                                                  

on leave of w_trn.plavei in frame f_1 do: {&hm}.
   if lookup({&le},"end-error,cursor-up") > 0 then return.
   if not w_trn.plavei:validate() in frame f_1 then return no-apply.
end.

on leave of w_trn.codpac in frame f_1 do: {&hm}.
   if {&le} = "cursor-up" and 
      (avail {&trn} or input frame f_1 w_trn.stapla = "") then do:
      apply "entry" to w_trn.codtrn in frame f_1.
      return no-apply.
   end.
   if lookup({&le},"end-error,cursor-up") > 0 then return.

   /************************************************************************
   if (input frame f_1 w_trn.codpac = 553749 or 
       input frame f_1 w_trn.codpac = 553924) and
      Userid("Dictdb") <> "salgado" and
      Userid("Dictdb") <> "cotta"
      then return no-apply.
   ************************************************************************/

  /*98182 98183 98111 bloqueio SAC - Isaac | Salgado */ 
  run nfs/v/nfsvprdblo.p(input frame f_1 w_trn.codpac).
  if return-value <> "ok"
     then do:
     message "Pacote apresenta produtos bloqueados pelo S.A.C".
     return no-apply.
  end.

   /*PPB PAC*********************************/
    assign c_aux = ""
           e_msg = "".
       
    for each  {&ped} of carga no-lock,
        each  {&ipd} of {&ped} no-lock,
        first {&prd} no-lock
        where {&prd}.codgrp = {&ipd}.codgrp
        and   {&prd}.codprd = {&ipd}.codprd:

        if lookup({&prd}.nrosernot,"S")     > 0 and
           lookup({&prd}.codncm,"84715010") > 0
           then do:

           if {&ipd}.qtdate > 0 then do:
                             
              find first nrserieipd no-lock
                   where nrserieipd.asdped = {&ped}.asdped
                   and   nrserieipd.codgrp = {&prd}.codgrp
                   and   nrserieipd.codprd = {&prd}.codprd no-error. 
              
              select count (*)
              into qtd_inf from nrserieipd 
              where codgrp = {&prd}.codgrp
              and   codprd = {&prd}.codprd
              and   asdped = {&ped}.asdped.
               
              if not avail nrserieipd or 
                 qtd_inf < {&ipd}.qtdate then do:
                 assign c_aux = if c_aux = "" then 
                                string({&prd}.idprod) + " "
                                + if qtd_inf < {&ipd}.qtdate 
                                then "(" 
                                + string(qtd_inf)
                                +
                                "/" 
                                +
                                string({&ipd}.qtdate)
                                + ")"
                                else ""
                                else c_aux + "," + string({&prd}.idprod)
                                + " "
                                + if qtd_inf < {&ipd}.qtdate
                                then "("
                                + string(qtd_inf)
                                +
                                "/" + string({&ipd}.qtdate)
                                + ")"
                                else ""
                                .
              end.
              else do:
              
                 find first detprd no-lock
                      where detprd.idprod  = {&prd}.idprod
                      and   detprd.nserie = nrserieipd.nroser
                      no-error.
              
                 if not avail detprd then do:
                     assign e_det = if e_det = "" then 
                            string({&prd}.idprod) 
                            else e_det + "," + string({&prd}.idprod).
                 end.
             end.
               
           end. /*qtdate*/
                        
        end. /*ns*/  
        
    end. /*for*/

    if c_aux <> "" then do:
                        
       assign e_msg = if num-entries(c_aux,",") > 1 then 
                      "Produtos: " 
                      else "Produto: "
                      e_msg = e_msg + c_aux 
                            + " Falta Num.Serie".
                            
                            
                            
                                     
   end.
  
   if e_det <> "" then assign e_msg = e_msg + "\n" + 
   if num-entries(e_det,",") > 1 then
   "Produto "
   else "Produtos: "
   + e_det 
   + " nao foram informados na entrada da mercadoria!" .
   
   if e_msg <> "" then do:
      run rt/rtedmsg.p (60,"   A T E N C A O   ", "", e_msg ) no-error.
      return no-apply.
   end.
 
   &IF DEFINED(GLPI20339) &THEN
   run nfs/r/nfsrvbri.p ("P," + string(input frame f_1 w_trn.codpac)) no-error.
   if return-value <> "ok" then return no-apply.
   &ENDIF

   /**************************************************
     Rodrigo 2013
     Inibe o faturamento de notas fiscais com brinde

   if w_trn.empctb = 1 then do:
      run nfs/v/vbrinde.p(input frame f_1 w_trn.codpac).
      if return-value <> "ok" 
         then do:
         message "Nao pode faturar (*brindes*)".
         return no-apply.
      end.
   end.
   **************************************************/
   
   /* Se pacote de filial varejo com quebra de pacote (gilson - 27-03-18) */ 

   /*GLPIxxxx não quebrar etiqueta do armazem**********************************
   if current-value(amb_sap) = 0 and 
      entry(3,par_in) <> "E"     then do:
   for first x_pac no-lock 
       where x_pac.codpac = input frame f_1 w_trn.codpac 
      ,each  x_car no-lock 
       where x_car.codpac = x_pac.codpac 
      ,each  x_ped no-lock 
       where x_ped.codcar = x_car.codcar 
       and   (if x_ped.codcli = 76176 
              or x_ped.codcli = 76177 then true else false)
      ,first x_ectb no-lock
       where x_ectb.codcli = x_ped.codcli
       and   (if x_ectb.codset > 0 then true else false)
      ,first x_sap no-lock 
       where x_sap.asdped = x_ped.asdped:
       if x_pac.sitpac <> "L" then do:
          if f_msg("Pacote (" 
                 + trim(string(x_pac.codpac)) 
                 + ") nao liberado para faturamento") then do:
          end.        
          return no-apply. 
       end.
       for first ctrlpacfil no-lock 
           where ctrlpacfil.codpac = input frame f_1 w_trn.codpac:
           if ctrlpacfil.flagpac <> "F" then do:
              if f_msg("Pacote (" 
                     + trim(string(input frame f_1 w_trn.codpac)) 
                     + ") sem processamento de quebra de pedidos da filial")
                     then do:
              end.
              return no-apply.  
           end.
       end.
       if not avail ctrlpacfil then do:
           if f_msg("Pacote (" 
                  + trim(string(input frame f_1 w_trn.codpac)) 
                  + ") sem processamento de quebra de pedidos da filial")
                  then do:
           end.
           return no-apply.  
      end.
      leave.
   end.
   end.
   ***************************************************************************/

   if not w_trn.codpac:validate() in frame f_1 then return no-apply.
   if pac_nft <> 0 and pac_nft <> input {&f1} w_trn.codpac then do:
      if f_msg ( "PACOTE: " + string(pac_nft) + 
                 "DEVE SER FATURADO PRIMEIRO"
               ) then do:
      end.
   end.
   assign frame f_1 w_trn.codpac.
   if entry(3,par_in) = "F" then do:
      run mpd/mpdppaco.p ("opcprg=LS;codpac=" + string(w_trn.codpac)).
      if entry(1,return-value,";") <> "Ok" then return no-apply.
   end.

   find first {&pac} no-lock where
              {&pac}.codpac = w_trn.codpac no-error.

   if current-value(seq_crr) = 1 then do:
     /* a pedido de cici e albertinho - 19/04/2016 - cotta */
     if lookup({&pac}.codrot,"TRN,CIF,RET") = 0 then do:
       for each  {&car} of {&pac} no-lock,
           each  {&ped} of {&car} no-lock,
           first {&cli} of {&ped} no-lock,
           first estado of {&cli} no-lock
           where (if lookup(estado.sigest,
                     "BA,SE,AL,PE,PB,RN,CE,PI,MA,PA,TO,MT") > 0
                  then true else false):
         leave.
       end.
       if {&pac}.dattercar = ? and avail estado then do:
         assign rsp = "Pacote nao terminou o carregamento"
                    + ",Pressione qualquer tecla".
         run trg/trgclrsp.p (input-output rsp, false).
         return no-apply.
       end.
     end.
   end.

   for each  carga of {&pac} no-lock,
       each  pedido of carga no-lock
       where (if lookup(pedido.tipped,"PSF,PRC,PRM") > 0 then true else false):
     for first sacchm no-lock
         where sacchm.asdped = pedido.asdped
        ,first tiporeclamacao no-lock
         where tiporeclamacao.codtiprcl = sacchm.codtiprcl
         and   (if tiporeclamacao.codtiprcl = "03" then true else false):
     end.
     if avail sacchm and avail tiporeclamacao then do:
       rsp = "Pacote vai gerar email com modelo de devolucao de recolhimento,".
       run trg/trgclrsp.p (input-output rsp, false).
       leave.
     end.
   end.   
   
   if entry(3,par_in) = "F" and w_trn.codemp <> w_trn.empctb then do:
      for first {&trfc} no-lock
          where {&trfc}.codpac       = w_trn.codpac
          and   {&trfc}.recnotfisdst = 0 
          and   (if {&trfc}.empctbdst = w_trn.empctb {&tru}):
         if f_msg ( "NOTA FISCAL DE ENTRADA NA FILIAL NAO EFETIVADA\n" +
                    string({&trfc}.recnotfisemi) + " de "    +
                    string({&trfc}.empctbemi)    + " para "  +
                    string({&trfc}.empctbdst)
                  ) then do:
         end.
         return no-apply.
      end.
   
      for first {&trfc} no-lock
          where {&trfc}.codpac       = w_trn.codpac
          and   (if {&trfc}.empctbdst = w_trn.empctb {&tru})
         ,first {&not} no-lock
          where {&not}.recnotfis = {&trfc}.recnotfisdst
          and   (if {&not}.nronotsap = "" then true else false):
         if f_msg ( "NOTA DE ENTRADA NA FILIAL NAO ENVIADA PARA O SAP\n" +
                    string({&trfc}.recnotfisemi) + " de "    +
                    string({&trfc}.empctbemi)    + " para "  +
                    string({&trfc}.empctbdst)
                  ) then do:
         end.
         return no-apply.
      end.
   end.

   /*Para nao faturar determinada rota****************************************
   if lookup({&pac}.codrot,"RN") > 0 and entry(3,par_in) = "F" then do:
      if f_msg ( "PACOTE DO RIO GRANDE DO NORTE") then do: end.
      if Userid("Dictdb") <> "salgado" then return no-apply.
   end.
   **************************************************************************/
   if not sml_fat then
   for each  {&car} of {&pac} no-lock                  
       where {&car}.codemp = w_trn.codemp
       and   (if lookup(key_code({&car}.codsitcar),"102,116") > 0
                 {&tru}
             ):
      assign rsp = "i\nCarga " + trim(string({&car}.codcar,">>>>>>>>>>>9"))
                 + (if keycode({&car}.codsitcar) = 102
                       then " sendo faturada."
                       else " sendo transferida."
                   )
                 + "@m\nProcesso sera abortado."
                 + ",Pressione qualquer tecla".
      run trg/trgclrsp.p (input-output rsp, false).
      return no-apply.
   end.

   for each  {&car} of {&pac} no-lock,
       each  {&ped} of {&car} no-lock,
       first {&cli} of {&ped} no-lock,
       first estado of {&cli} no-lock:
     if lookup(estado.sigest,"GO,DF") > 0 then do:
       assign rsp = "Fazer o pagamento das GNRs das notas do pacote,"
                  + "Tecle <O>k,O".
       run LB/PRG/LBPRGcrsp.p (input-output rsp).
       leave.
     end.
   end.
         
   if {&pac}.codtrn = 2554 and {&pac}.codtrn = 5286 and
      {&pac}.codtrn = 5287 and {&pac}.codtrn = 5575 and
      input frame f_1 w_trn.codfre <> "CIF" and
      entry(3,par_in) = "F" then do:
     assign rsp = "Frete deve ser CIF,Pressione qualquer tecla".
     run trg/trgclrsp.p (input-output rsp, false).
     return no-apply.
   end.

   assign cod_trn_pac = {&pac}.codtrn.
   if {&pac}.codtrn <> 0 /***and 
      input frame f_1 w_trn.codtrn <> {&pac}.codtrn***/ then do:
      assign rsp = "Transportador diferente! Copiar codigo transportador,"
                 + "Tecle <S>im <N>ao,S,N". 
      run trg/trgclrsp.p (input-output rsp, false).
      if rsp = "S" then do:
         assign trn_cd = {&pac}.codtrn
                cod_trn_pac = {&pac}.codtrn.
         for first introt of {&pac} no-lock:
         end.
         if (avail introt and introt.tiprot = "C") or
            (w_trn.empctb = 1 and
             lookup({&pac}.codrot,"CVC,CV1,CV2,CV3,TI1,TI2,TI3,T04,TI5,TI6," +
                                  "TI7,TI8,TI9,T10,TIN,CP1,CPB," +
                                  "AJ1,AJ2,AJ3,AJU,CBM,BAR,BRR,CCF,I01,I02," +
                                  "I03,I04,I04,I06,I07,I08,I09,I10,I11,I12," +
                                  "I14,ITA,JUZ,JU1,JU2,JU3,JU4,JU5,JU6,JU7," +
                                  "JU8,PET,CMC,CM1,CM2,CM3,CNV,"             +
                                  "CVC,CV1,CV2,CV3"                          + 
                                  "GRJ,RST,SAL,SA1,SA2,RSF,SPT,CDV,CRN,THE," +
                                  "CCE,CC1,CC2,CC3,CSC,CS1,CS2,CS3,CCG,CDS," +
                                  "CCF,CAS,CDU,CBE,CB1,MAC,DF") > 0) or
            (w_trn.empctb = 3 and
             lookup({&pac}.codrot,"JUZ,JU1,JU2,JU3,JU4,JU5,JU6,JU7,JU8,SAL," +
                                  "SA1,SA2,BAR,BRR,PET,PI,PI1,P12,PI3") > 0)
         then do:
           assign lst_pac = "" cod_cxa = 0.
           for first cxapacote no-lock
               where cxapacote.codpac = {&pac}.codpac:
             assign cod_cxa = cxapacote.codcxa.
           end.
           for each cxapacote no-lock
               where cxapacote.codcxa = cod_cxa:
             assign lst_pac = lst_pac
                            + (if lst_pac <> "" then "," else "")
                            + string(cxapacote.codpac).
           end.
           for first paccd no-lock
               where lookup(string(paccd.codpac),lst_pac) > 0,
                     /*paccd.codpac = {&pac}.codpac,*/
               first b_pac no-lock
               where b_pac.codpac = paccd.codpaccd:
             assign trn_cd = b_pac.codtrn
                    pla_cd = b_pac.numpla.
           end.
         end.
         disp /*{&pac}.codtrn*/ 
              trn_cd @ w_trn.codtrn
              pla_cd @ w_trn.plavei with frame f_1.
         if {&pac}.datsolnot >= to_day then
            disp {&pac}.datsolnot @ w_trn.datsai with frame f_1.
         apply "leave" to w_trn.codtrn in frame f_1.
         if rsp = "no-apply" then return no-apply.
      end.
   end.

   /**************************************************************************
   if (num-entries(par_in) < 11 or entry(11,par_in) <> "M") and
      avail {&tfr}                                          and 
      {&tfr}.flgtomseremp and {&tfr}.codobstrn = ""         and 
      input frame f_1 w_trn.codtrn = 0 then do:
      message "Transportador obrigatorio".
      return no-apply.
   end.    
   ***************************************************************************/
   apply 399 to w_trn.codtrn in frame f_1.
   if rsp = "no-apply" then return no-apply.
   find first {&car} no-lock where rowid({&car}) = ? no-error.
   if entry(3,par_in) = "F" then do:
      for each  {&car} of {&pac} no-lock
          where {&car}.codemp = w_trn.codemp
          and   (if {&car}.flgati {&tru})                 
         ,first {&stc} of {&car} no-lock 
          where not {&stc}.flglibfat:
         message "Carga <" + 
                 string({&car}.codemp) + "." +
                 string({&car}.coddiv) + "." +
                 string({&car}.codcar) +
                 "> nao liberada para faturar".
         return no-apply.
      end.
      if {&pac}.sitpac = "A" then do:
         message "Pacote nao foi atualizado".
         return no-apply.
      end.
      for first atuisp no-lock
          where atuisp.codpac = {&pac}.codpac
          and   (if atuisp.rescon = "" {&tru}): {&hm}.
         message "Falha na atualizacao do pacote nao foi atualizado".
         message "Pedido " + string(atuisp.asdped,">9999,99999 ").
         return no-apply.
      end.
      
      if can-find(first atuisp no-lock
                  where atuisp.codpac = {&pac}.codpac
                  and   atuisp.rescon = "") then do:
         message "Falha na atualizacao do pacote nao foi atualizado".
         return no-apply.
      end.
      if keycode({&pac}.sitpac)  = 65 
      or keycode({&pac}.sitpac) <> 70 and
         keycode({&pac}.sitpac) <> 76 and
         keycode({&pac}.sitpac) <> 84 then do:            
         run MT/LG/verpaclog.p ({&pac}.codpac).
         if return-value = "nao" then do:
            
            run ftp/colsenha.p.
            if return-value <> "ok" then do:
              if f_msg ( "Logistica nao foi atualizado" ) then do: end.
              return no-apply.
            end.
            else do:
              for first separa where separa.codpac = {&pac}.codpac and
                                     keycode(separa.sitsep) = 102:

                assign separa.sitsep = "F".
              end.
            end.
         end.
      end.
      if {&pac}.datsolnot <> ? and {&pac}.datsolnot >= to_day then do:
         disp {&pac}.datsolnot @ w_trn.datsai with frame f_1.
      end.
   end. else do:
      for each  {&car} of {&pac} no-lock 
          where {&car}.codemp = w_trn.codemp
          and   keycode({&car}.codsitcar) <> 70 and 
                lookup({&car}.codsitcar,"X,T") = 0:
         message "Carga <" + 
                 string({&car}.codemp) + "." +
                 string({&car}.coddiv) + "." +
                 string({&car}.codcar) +
                 "> nao faturada. Situacao <" + {&car}.codsitcar + ">".
         leave.
      end.
   end. 

   if avail {&car} then return no-apply.
   if lookup(entry(3,par_in),"E") > 0 then return.

   for first {&tfr} no-lock where rowid({&tfr}) = ?: end.
   assign fre_def = "ENT,RET".
   for each  introt of {&pac} no-lock
       where introt.grprot = "TT":
      assign fre_def = "CIF,CIF".
   end.
   for each  {&car} of {&pac} no-lock 
       where {&car}.numtotpedlib > 0 
       and   {&car}.codfre   <> input frame f_1 w_trn.codfre
      ,each  {&tfr} no-lock
       where {&tfr}.codfre   <> input frame f_1 w_trn.codfre
       and   (if {&tfr}.codfre = {&car}.codfre 
              or {&car}.codfre = "" and 
                 {&tfr}.codfre = entry(int(string({&car}.flgpedhor,"2/1")),
                                       fre_def)
                 then true
                 else false
             ):
      message "Tipo de frete ~"" + trim({&tfr}.codfre) + "~" da carga " +
              string({&car}.codcar) + " diferente do frete informado".
      return no-apply.
   end.
   if avail {&tfr} then return no-apply.
   for first {&tfr} no-lock where 
       {&tfr}.codfre = input frame f_1 w_trn.codfre: 
   end.
   if not avail {&tfr} then do:
      message "Frete nao cadastrado".
      return no-apply.
   end.
   /* GLPI 24961 Aviso sobre produtos para Amazon sem EAN */
   if {&pac}.codrot = "SPA" or {&pac}.codrot = "GRA" then do: 
     run nfs/v/amazonean.p ({&pac}.codpac).
     if return-value <> "ok" then do: 
       return no-apply.
     end.
   end.
  /* GLPI 24961 */

END.

/*ppb*/
on leave of w_trn.codcar in frame f_1 do: {&hm}.
   if lookup({&le},"end-error,cursor-up") > 0 then return.
   if not w_trn.codcar:validate() in frame f_1 then return no-apply.
   assign frame f_1 w_trn.codcar.
   if cod_car <> 0 and w_trn.codcar <> cod_car then do:
      assign w_trn.codcar = cod_car.
      disp w_trn.codcar with frame f_1.
      return no-apply.
   end.
   find first {&car} no-lock where
              {&car}.codcar = w_trn.codcar no-error.

   if {&car}.codpac <> 0 then do:
      if f_msg ( "CARGA DO PACOTE: " + trim(string({&car}.codpac,"zzzz,zz9")))
         then do:
      end.
      return no-apply.
   end.
   if cod_car <> 0 and {&car}.codcar <> cod_car then do:
      disp cod_car @ w_trn.codcar with frame f_1.
      return no-apply.
   end.
   if not {&car}.flgpedhor then do:
     if f_msg ( "CARGA NAO E PARA PEDIDO DE HORA" ) then do: end.
   end.

   &IF DEFINED(GLPI20339) &THEN
   run nfs/r/nfsrvbri.p ("C," + string(w_trn.codcar)) no-error.
   if return-value <> "ok" then return no-apply.
   &ENDIF

   for each  {&ped} of {&car} no-lock,
       first cliente of {&ped} no-lock,
       first estado of cliente no-lock:
     if lookup(estado.sigest,"GO,DF") > 0 then do:
       assign rsp = "Fazer o pagamento da GNR da nota,Tecle <O>k,O".
       run LB/PRG/LBPRGcrsp.p (input-output rsp).
     end.
/*     &IF DEFINED(GLPI28245) &THEN
     if lookup({&ped}.tipped,pdf_fil) > 0 and 
        {&ped}.empctb <> input frame f_1 w_trn.empctb then do:
        message "Pedido ~"" + string({&ped}.asdped)
                 + "~" para faturamento na empresa " 
                 + string({&ped}.empctb).
        return no-apply.         
     end.   
     &ENDIF*/
   end.
               
    /*PPB CARGA*********************************/
    assign c_aux = ""
           e_msg = "".
       
    for each  {&ped} of {&car} no-lock
        where (if asd_ped = 0
               or {&ped}.asdped = asd_ped
                  then true else false)
       ,each  {&ipd} of {&ped} no-lock,
        first {&prd} no-lock
        where {&prd}.codgrp = {&ipd}.codgrp
        and   {&prd}.codprd = {&ipd}.codprd:

        if lookup({&prd}.nrosernot,"S")     > 0 and
           lookup({&prd}.codncm,"84715010") > 0
           then do:

           if {&ipd}.qtdate > 0 then do:
           
                             
              find first nrserieipd no-lock
                   where nrserieipd.asdped = {&ped}.asdped
                   and   nrserieipd.codgrp = {&prd}.codgrp
                   and   nrserieipd.codprd = {&prd}.codprd no-error. 
              
              select count (*)
              into qtd_inf from nrserieipd 
              where codgrp = {&prd}.codgrp
              and   codprd = {&prd}.codprd
              and   asdped = {&ped}.asdped.
               
              if not avail nrserieipd or 
                 qtd_inf < {&ipd}.qtdate then do:
                 assign c_aux = if c_aux = "" then 
                                string({&prd}.idprod) + " "
                                + if qtd_inf < {&ipd}.qtdate 
                                then "(" 
                                + string(qtd_inf)
                                +
                                "/" 
                                +
                                string({&ipd}.qtdate)
                                + ")"
                                else ""
                                else c_aux + "," + string({&prd}.idprod)
                                + " "
                                + if qtd_inf < {&ipd}.qtdate
                                then "("
                                + string(qtd_inf)
                                +
                                "/" + string({&ipd}.qtdate)
                                + ")"
                                else ""
                                .
              end.
              else do:
              
                 find first detprd no-lock
                      where detprd.idprod  = {&prd}.idprod
                      and   detprd.nserie = nrserieipd.nroser
                      no-error.
              
                 if not avail detprd then do:
                     assign e_det = if e_det = "" then 
                            string({&prd}.idprod) 
                            else e_det + "," + string({&prd}.idprod).
                 end.
             end.
               
           end. /*qtdate*/
                        
        end. /*ns*/  
        
    end. /*for*/

    if c_aux <> "" then do:
                        
       assign e_msg = if num-entries(c_aux,",") > 1 then 
                      "Produtos: " 
                      else "Produto: "
                      e_msg = e_msg + c_aux 
                            + " Falta Num.Serie".

   end.
  
   if e_det <> "" then assign e_msg = e_msg + "\n" + 
   if num-entries(e_det,",") > 1 then
   "Produto "
   else "Produtos: "
   + e_det 
   + " nao foram informados na entrada da mercadoria!" .
   
   if e_msg <> "" then do:
      run rt/rtedmsg.p (60,"   A T E N C A O   ", "", e_msg ) no-error.
      return no-apply.
   end.
 
   /******************************************/
   
   run v_mpdapedi.
   if return-value <> "ok" then return no-apply.
   assign rep_car = string({&car}.codemp) + "."
                  + string({&car}.coddiv) + "/"
                  + (if {&car}.codrepint > 0 
                        then (string({&car}.codrepint) + "-") else "")
                  + string({&car}.codrep).
   if entry(3,par_in) = "F" then do:
      for first {&car} no-lock 
          where {&car}.codcar = w_trn.codcar
         ,first {&stc} of {&car} no-lock 
          where not {&stc}.flglibfat
          or    lookup({&stc}.codsit,"A") > 0:
         message "Carga <" + 
                 string({&car}.codemp) + "." +
                 string({&car}.coddiv) + "." +
                 string({&car}.codcar) +
                 "> nao liberada para faturar".
         return no-apply.
      end.
   end. else do:
      for first {&car} no-lock 
          where {&car}.codcar = w_trn.codcar 
          and   keycode({&car}.codsitcar) <> 70:
         message "Carga <" + 
                 string({&car}.codemp) + "." +
                 string({&car}.coddiv) + "." +
                 string({&car}.codcar) +
                 "> nao faturarada".
         return no-apply.
      end.
   end. 
   disp rep_car with frame f_1.
   if entry(3,par_in) = "E" then return.
   for first {&tfr} no-lock
       where {&tfr}.codfre   <> input frame f_1 w_trn.codfre
       and   (if {&tfr}.codfre = {&car}.codfre 
              or {&tfr}.codfre = "ENT" and {&car}.codfre = ""
                                       and not {&car}.flgpedhor
              or {&tfr}.codfre = "RET" and {&car}.codfre = ""
                                       and {&car}.flgpedhor
                 then true
                 else false
             ):
      message "Tipo de frete ~"" + trim({&tfr}.codfre) + "~" da carga " +
              string({&car}.codcar) + " diferente do frete informado".
      return no-apply.
   end.
   if avail {&tfr} then return no-apply.
   for first {&tfr} no-lock 
       where {&tfr}.codfre = input frame f_1 w_trn.codfre: 
   end.
   if not {&car}.flgpedhor and 
      ({&car}.codfre <> {&tfr}.codfre and {&car}.codfre <> "" or
       {&car}.codfre = "" and {&tfr}.codfre <> "ENT"
      ) then do:
      message "Tipo de frete da carga diferente do frete informado".
      return no-apply.
   end.
   if {&car}.codpac = 0 and not {&car}.flgpedhor then do:
      message "Voce deve gerar um pacote para a carga".
      return no-apply.
   end.

   for first {&trfc} no-lock
       where {&trfc}.codcar       = w_trn.codcar
       and   {&trfc}.recnotfisdst = 0 
       and   (if {&trfc}.empctbdst = w_trn.empctb {&tru})
             while entry(3,par_in) = "F" and w_trn.codemp <> w_trn.empctb:
      if f_msg ( "NOTA FISCAL DE ENTRADA NA FILIAL NAO EFETIVADA\n" +
                 string({&trfc}.recnotfisemi) + " de "    +
                 string({&trfc}.empctbemi)    + " para "  +
                 string({&trfc}.empctbdst)
      ) then do:
      end.
      return no-apply.
   end.
   
   apply 399 to w_trn.codtrn in frame f_1.
   if rsp = "no-apply" then return no-apply.
   if {&car}.codtrn > 0 and 
      {&car}.codtrn <> input frame f_1 w_trn.codtrn then do:
      message "Transportador da carga difere do informado".
      return no-apply.
   end.
   /***************************************************************************
   if (num-entries(par_in) < 11 or entry(11,par_in) <> "M") and
      {&tfr}.flgtomseremp and {&tfr}.codobstrn = ""         and 
      input frame f_1 w_trn.codtrn = 0 then do:
      message "Transportador obrigatorio".
      return no-apply.
   end.    
   ***************************************************************************/
end.
on entry of w_trn.datemi in frame f_1 do: {&hm}.
   for first {&not} no-lock where {&not}.recnotfis = rec_not:
      assign w_trn.datemi = {&not}.datemi
             w_trn.notinf = {&not}.numnotfis
             w_trn.notsup = {&not}.numnotfis.
      disp w_trn.datemi w_trn.notinf w_trn.notsup with frame f_1.
   end.    
end.
on leave of w_trn.datemi in frame f_1 do: {&hm}.
   assign frame f_1 w_trn.datemi no-error.
   if error-status:error then do:
      if lookup({&le},"end-error,cursor-up") > 0 then do:
         apply "clear" to w_trn.datemi in frame f_1.
         return.
      end.
      message "Formato da data de emissao invalido".
      return no-apply.
   end.
   if lookup({&le},"end-error,cursor-up") > 0 then return.
   if not w_trn.datemi:validate() then return no-apply.
end.
on cursor-down of w_trn.notinf in frame f_1 do: {&hm}.
   apply "tab" to w_trn.notinf in frame f_1.
   return no-apply.
end.
on leave of w_trn.notinf in frame f_1 do: {&hm}.
   if lookup({&le},"end-error,cursor-up") > 0 then return.
   assign frame f_1 w_trn.notinf.
   for first {&not} no-lock 
       where {&not}.numnotfis = w_trn.notinf    
       and   {&not}.flgclifor = entry(1,par_in) 
       and   {&not}.flgnotfis = entry(2,par_in) 
       and   {&not}.datemi    = w_trn.datemi    
       and   {&not}.codemp    = w_trn.codemp
       and   {&not}.empctb    = w_trn.empctb:
      assign frame f_1 w_trn.notinf.
      if {&not}.codtrn <> 0 /***and 
         input frame f_1 w_trn.codtrn <> {&not}.codtrn***/ then do:
         assign rsp = "Transportador diferente! Copiar codigo transportador,"
                    + "Tecle <S>im <N>ao,S,N". 
         run trg/trgclrsp.p (input-output rsp, false).
         if rsp = "S" then do:
            disp {&not}.codtrn    @ w_trn.codtrn with frame f_1.
            apply 398 to w_trn.codtrn in frame f_1.
            if rsp = "no-apply" then return no-apply.
         end.
      end.
      /*********************************************************************
      if string({&not}.codcfo,"999999") begins "532" or 
         string({&not}.codcfo,"999999") begins "632" then do:
        if f_msg ("Nota fiscal nao pode ser emitida neste programa") then do:
        end.
        return no-apply.
      end.
      *********************************************************************/
      return.
   end.
   message "Nota fiscal nao emitida em " + string(w_trn.datemi).
   return no-apply.
end.
on leave of w_trn.notsup in frame f_1 do: {&hm}.
   if lookup({&le},"end-error,cursor-left,cursor-up") > 0 then return.
   assign frame f_1 w_trn.notsup.
   if w_trn.notsup < w_trn.notinf then do:
      message "Nota superior menor que nota inferior".
      return no-apply.
   end.
   for first {&not} no-lock 
       where {&not}.numnotfis = w_trn.notsup    
       and   {&not}.flgclifor = entry(1,par_in) 
       and   {&not}.flgnotfis = entry(2,par_in) 
       and   {&not}.datemi    = w_trn.datemi    
       and   {&not}.codemp    = w_trn.codemp
       and   {&not}.empctb    = w_trn.empctb:
      assign frame f_1 w_trn.notsup.
      /*******************************************************************
      if string({&not}.codcfo,"999999") begins "532" or 
         string({&not}.codcfo,"999999") begins "632" then do:
        if f_msg ("Nota fiscal nao pode ser emitida neste programa") then do:
        end.
        return no-apply.
      end.
      ********************************************************************/
      leave.
   end.
   if not available {&not} then do:
     message "Nota fiscal nao emitida em " + string(w_trn.datemi).
     return no-apply.
   end.
end.
on leave of w_trn.notref in frame f_1 do: {&hm}.
   if lookup({&le},"end-error,cursor-left,cursor-up") > 0 then return.
   assign frame f_1 w_trn.notref
          rec_ref = 0.
   if w_trn.notref <> 0 then do:
     for first {&not} no-lock 
         where {&not}.numnotfis = w_trn.notref    
         and   {&not}.flgclifor = entry(1,par_in) 
         and   {&not}.flgnotfis = entry(2,par_in) 
         and   {&not}.codemp    = w_trn.codemp
         and   {&not}.empctb    = w_trn.empctb:
       assign rec_ref = {&not}.recnotfis.
     end.
     if not available {&not} then do:
       message "Nota fiscal nao cadastrada".
       return no-apply.
     end.
   end.  
end.
on leave of w_trn.qtdemb in frame f_1 do: {&hm}.
   if lookup({&le},"end-error,cursor-left,cursor-up") > 0 then return.
   assign frame f_1 w_trn.qtdemb.
   if w_trn.notref <> 0 then do:
     if avail {&not} and w_trn.qtdemb = 0 then do:
       message "Qtd embalagens deve ser digitada".
       return no-apply.
     end.
     if avail {&not} and w_trn.qtdemb > {&not}.qtdemb then do:
       message "Qtd embalagens maior que nota de origem".
       return no-apply.
     end.
   end.  
end.
on leave of w_trn.datsai in frame f_1 do: {&hm}.
   assign frame f_1 w_trn.datsai no-error.
   if error-status:error then do:
      if lookup({&le},"end-error,cursor-up") > 0 then do:
         apply "clear" to w_trn.datsai in frame f_1.
         return.
      end.
      message "Formato da data de saida invalido".
      return no-apply.
   end.
   if lookup({&le},"end-error,cursor-up") > 0 
   or frame-field <> "datsai" then return.

   if not w_trn.datsai:validate() in frame f_1 then return no-apply.
   if w_trn.notsup:visible in frame f_1 and
      input frame f_1 w_trn.notsup > 0 and 
      {&not}.datemi > input frame f_1 w_trn.datsai 
   or entry(3,par_in) = "F" and input frame f_1 w_trn.datsai < to_day then do:
      message "Data de saida invalida".
      return no-apply.
   end.   

   if current-value(amb_sap) = 0 then do:
      run nfs/nfssefaz.p.        /*GLPI15391*/
      if lookup(return-value,"ok") = 0 then return no-apply.
   end.

   assign frame f_1 w_trn.notinf
          frame f_1 w_trn.notsup
          frame f_1 w_trn.codemp
          frame f_1 w_trn.empctb
          frame f_1 w_trn.codtrn
          frame f_1 w_trn.codcar
          frame f_1 w_trn.codpac
          frame f_1 w_trn.tipo  
          frame f_1 w_trn.datsai
          frame f_1 w_trn.datemi
          frame f_1 w_trn.nomtrn
          frame f_1 w_trn.endtrn
          frame f_1 w_trn.muntrn
          frame f_1 w_trn.plavei
          frame f_1 w_trn.stapla
          w_trn.nomtrn = caps(w_trn.nomtrn)
          w_trn.endtrn = caps(w_trn.endtrn)
          w_trn.muntrn = caps(w_trn.muntrn)
          w_trn.plavei = caps(w_trn.plavei)
          w_trn.stapla = caps(w_trn.stapla).
   run D_F1.
   assign rsp = (if entry(3,par_in) = "E"
                    then "Confirmacao da emissao,"
                    else "Confirme os dados acima,"
                )
              + "Tecle <P>rosseguir <R>efazer <I>gnorar,I,R,P".
   run trg/trgclrsp.p (input-output rsp, false).
   if rsp = "R" then return.
   if rsp = "I" then do:
      apply "end-error" to frame f_1.
      return no-apply.
   end.
   /*
   do transaction:
      find _MyConnection exclusive-lock.
      _MyConnection._MyConn-NumSeqBuffers = max_bp.
   end.      
   */
   for first {&emp}  no-lock where {&emp}.codemp  = w_trn.codemp: end.
   for first {&ectb} no-lock where {&ectb}.empctb = w_trn.empctb: end.

   // BHFS.001 - Validações e atualizações referentes à itens do pacote - Colocado neste ponto a pedido do Salgado, por ser último campo da tela
   &IF DEFINED(GLPI29549) &THEN
       IF input frame f_1  w_trn.codpac = 0 AND
          input frame f_1  w_trn.codcar = 0 THEN DO: 
          MESSAGE "Pacote / Carga estao vazios".
          RETURN NO-APPLY.
       END.          
   
       RUN nfes/nfsv_Vld_Itens.p (input frame f_1  w_trn.codpac,
                                  input frame f_1 w_trn.codcar) no-error.

       if error-status:error then do:
          run rt/rtedmsg.p (60,"   A T E N C A O   ", "",
              "\n\nOcorreu o erro(" + 
              error-status:get-message(error-status:num-messages) +
              ")\n\n"
              ) no-error.
          return no-apply.
       end.

       if RETURN-VALUE <> "ok" then DO: 
          run rt/rtedmsg.p (60,"   A T E N C A O   ", "",
              "\n\n" + 
              substring(entry(2,return-value),1,60) +
              "\n" +
              substring(entry(2,return-value),61,120) + 
              "\n" 
              ) no-error.
           return no-apply.
       end.

   &ENDIF
   // BHFS.001 - FIM

   run value("SEPARA_" + caps(entry(3,par_in)) ).

   if can-find(first nf_srt no-lock) then run nro_sorte.

   {&hm}.
   case entry(1,return-value):
      when "troca/falta" then do: run nfs/v/nfsvtroc.p.
                                  return.
      end.
      when "Empresa"     then do: run nfs/v/nfsvectb.p ( {&ectb}.empctb, ? ).
                                  return.
      end.
      when "refaz"       then return.
   end case.

   repeat while caps(entry(3,par_in)) = "F":
      if entry(1,return-value) <> "ok" then do:
         assign rsp = "separador=;,m\nFaturamento da empresa (" 
                    + replace({&emp}.nomemp,";",",") 
                    + ")@I\nNAO TERMINOU;Tecle F4_Voltar;V;end-error".
         run trg/trgclrsp.p (input-output rsp, false).
         color display normal msg_fat with frame f_1.
         assign a_tel                             = ""
                msg_fat                           = ""
                w_trn.notinf:visible in frame f_1 = false
                w_trn.notsup:visible in frame f_1 = false
                a_tel:visible        in frame f_1 = false.
         disp msg_fat with frame f_1.
         return no-apply.
      end.
      assign rsp = "P;E".
      if w_trn.tipo = "PACOTE" then run t_emitir ( w_trn.codpac ).
      if lookup("E",rsp) > 0 and entry(3,par_in) = "F" then do:
         for each i_not no-lock:
            run trb/rt/trbrtvld.p ( "C", i_not.recnotfis, 1 ) no-error.
         end.
      end.
      
      assign rsp = "separador=;,Faturamento da empresa (" 
                 + replace({&emp}.nomemp,";",",") 
                 + ") terminou"
                 + ";Tecle" 
                 + (if lookup("E",rsp,";") > 0 then " <E>mitir" else "")
                 + " <P>rosseguir;" + rsp.
                 
      run trg/trgclrsp.p (input-output rsp, false).

      /* GLPI 21361 - API Whatsapp 
         No final do faturamento do pacote
         enviar mensagem pela API do Valdeir */
      if avail w_trn and w_trn.codpac > 0 then do:
         run LB/PRG/apival.p ("FIMFAT", w_trn.codpac).
         run LB/PRG/emailcorte.p (w_trn.codpac) no-error.
      end.
      
      assign msg_fat = "".
      disp msg_fat with frame f_1.
      color display normal msg_fat with frame f_1.
      if rsp= "P" then leave.

      for each  f_ectb no-lock
          where f_ectb.cnpj begins {&emp}.cgcemp
          and   (if input frame f_1 w_trn.empctb = 0
                 or f_ectb.empctb = input frame f_1 w_trn.empctb
                    then true
                    else false
                ):
         if rsp <> "E" then run value("E_" + w_trn.tipo ) ( f_ectb.empctb ).
      end.
      leave.
   end.

   assign w_trn.codpac = 0
          w_trn.codcar = 0
          w_trn.datemi = ?
          w_trn.notinf = 0
          w_trn.notsup = 0
          w_trn.datsai = ?.

   run D_F1.
   case w_trn.tipo:
      when "pacote" then apply "entry" to w_trn.codpac in frame f_1.
      when "carga"  then apply "entry" to w_trn.codcar in frame f_1.
      when "nota"   then apply "entry" to w_trn.datemi in frame f_1.
   end.
   return no-apply.
end.
on return of w_trn.datsai in frame f_1 do:
  apply "tab" to w_trn.datsai in frame f_1.
  return no-apply.
end.
on entry of w_trn.codpac in frame f_1 do: {&hm}.
   if input frame f_1 w_trn.codpac = 0 and entry(3,par_in) = "F" and
      can-find(first notxped no-lock
               where notxped.codemp = {&ectb}.empctb)       then do:
      apply "get" to w_trn.codpac in frame f_1.
   end.   
end.
on m,M of w_trn.codcar in frame f_1 do:
   if {&car}.flgpedhor then run v_permissao.
   return no-apply.
end.   
on any-printable of w_trn.plavei in frame f_1 do: {&hm}.
   case w_trn.plavei:cursor-offset in frame f_1:
      when 1 or when 2 then do: if last-event:function >= "A" and
                                   last-event:function <= "Z" then return.
                            end.
      when 3           then do: if last-event:function >= "A" and
                                   last-event:function <= "Z" or
                                   last-event:function = "-" then return.
                            end.
      otherwise              do: if last-event:function >= "0" and
                                    last-event:function <= "9" then return.
                            end.
   end.
   message "Caracter invalido".
   return no-apply.
end.
on clear of w_trn.codtrn in frame f_1 do:
   assign w_trn.codtrn = 0
          w_trn.nomtrn = "" 
          w_trn.endtrn = ""
          w_trn.plavei = ""
          w_trn.muntrn = ""
          w_trn.stapla = "".
   disp w_trn.codtrn
        w_trn.nomtrn
        w_trn.endtrn
        w_trn.muntrn
        w_trn.plavei
        w_trn.stapla
        ""              @ {&sta}.nomest
   with frame f_1.
end.
/* TERMINO COLHE DADOS DO TRANSPORTADOR***************************************/
{&hm}.
pause 0 before-hide.
if current-value(ctlfat) = 0 then do:
   run rt/rtedmsg.p (60,"   A T E N C A O   ", "", 
                    "\n\nOPCOES DE EMISSAO DE NOTAS FISCAIS INATIVAS\n\n"
                    ) no-error.
   return "ok".
end.

assign par_in = trim(par_in).
run mta_parametro.
if return-value <> "ok" then return.


/***** TESTE DATA DE FATURAMENTO PARA PEDIDO DE HORA (ALBERTO PORTUGAL) ******/
if cod_car > 0 and current-value(nrddfatu,dictdb) > 0 and
   today > 01/01/1990 + current-value(dtfatura,dictdb) then do:
   run rt/rtedmsg.p ( 30,"   ATENCAO   ", "",
                      "\n\nDATA DE FATURAMENTO " + 
                      string(01/01/1990 + current-value(dtfatura,dictdb),
                             "99/99/9999") +
                      "\n\n  PROCURAR ALBERTO PORTUGAL\n\n" 
                    ) no-error.
   return "ok".
end.

&IF DEFINED(GLPI21191) &THEN
if current-value(amb_sap) = 0 and
   lookup(entry(3,par_in),"E") = 0 and day(today) < 10 and
   not v_prcmedmes( output rsp ) then do:
   run rt/rtedmsg.p ( 60,"   PRECO CONVENIO MG   ", "",
                      "\n\n" + rsp + "\n\n" 
                    ) no-error.
   return "ok".
end.
&ENDIF

for last  {&cfo}  no-lock where {&cfo}.codcfo    = cod_cfo: end.
for first {&tfr}  no-lock where {&tfr}.codfre    = cod_fre: end. 
for first {&pac}  no-lock where {&pac}.codpac    = cod_pac: end. 
for first {&car}  no-lock where {&car}.codcar    = cod_car: end.
for first {&not}  no-lock where {&not}.recnotfis = rec_not: end.
for first {&emp}  no-lock where {&emp}.codemp    = cod_emp: end.
for first {&ectb} no-lock where {&ectb}.empctb   = emp_ctb: end.

if avail {&cfo} then do:
   assign dsc_cfo = string({&cfo}.codcfo,"9,999,99").
   if {&cfo}.indtiptrs <> "T" then do:
      case substr(dsc_cfo,1,1):
         when "1" or when "2" then assign dsc_cfo = "(1/2)" + substr(dsc_cfo,2).
         when "5" or when "6" then assign dsc_cfo = "(5/6)" + substr(dsc_cfo,2).
      end case.
   end.
   assign dsc_cfo = dsc_cfo + " " + trim({&cfo}.desfis)
          /*
          dsc_cfo = fill(" ",int(trunc((78 - length(dsc_cfo)) / 2,0)))
                  + dsc_cfo.
          */        
          dsc_cfo = f_centra ( dsc_cfo, 78 ).
end.

if cod_cfo = 515201 and cod_car = 0 and 
   (if weekday(today) = 6 then
     (if (month(today + 1) <> month(today) or 
          month(today + 2) <> month(today) or
          month(today + 3) <> month(today)) then true else false)
    else
    if weekday(today) = 7 then
      (if (month(today + 1) <> month(today) or
           month(today + 2) <> month(today)) then true else false)
    else
      (if month(today + 1) <> month(today) then true else false))
then do:
  {&hm}. 
  message "Pedido da filial nao pode ser faturado no ultimo dia do mes".
  return no-apply.
end.

create w_trn.
assign w_trn.tipo   = tip_emi
       w_trn.codemp = cod_emp
       w_trn.empctb = emp_ctb
       w_trn.codfre = cod_fre
       w_trn.codpac = cod_pac
       w_trn.codcar = cod_car
       w_trn.tipo   = (if entry(3,par_in) = "F" and tip_emi = ""
                          then "PACOTE"
                          else tip_emi
                      )
       {&pdt}       =  (if entry(3,par_in) = "F"
                           then w_trn.tipo
                           else "PACOTE,CARGA,NOTA"
                       )
       {&pdt}       =  (if w_trn.tipo = "" then {&pdt} else w_trn.tipo)
       w_trn.tipo:help in frame f_1 = "F4_Volta"
            + (if lookup("PACOTE",{&pdt}) > 0
                  then " <P>ACOTE"
                  else ""
              )
            + (if lookup("CARGA",{&pdt}) > 0
                  then " <C>ARGA"
                  else ""
              )
            + (if lookup("NOTA",{&pdt}) > 0
                  then " <N>OTA"
                  else ""
              )
       tip_emi = w_trn.tipo.

for each f_tpd no-lock
   where (if f_tpd.idtfat = "F" then true else false):
   assign pdf_fil = pdf_fil + (if pdf_fil = "" then "" else ",") 
                  + trim(f_tpd.tipped).
end.
              
if rec_not > 0 and avail {&not} then
   assign w_trn.notinf = {&not}.numnotfis
          w_trn.notsup = {&not}.numnotfis
          w_trn.datemi = {&not}.datemi.
find current w_trn no-lock no-error.

if sml_fat then do:
   run p_simula.
   return.
end. else
if flg_aut then do:
   run p_automatico.
   return.
end. 
run D_F1. 
run A_W_TRN (buffer {&emp},buffer {&ectb},buffer {&tfr},buffer {&trn}).
run ENA_F_1 ( 1 ).

assign tip_emi = input frame f_1 w_trn.tipo.
apply "entry" to w_trn.tipo in frame f_1.
wait-for window-close of this-procedure
         focus w_trn.tipo in frame f_1 pause var_wai.
hide frame f_1 no-pause.
if can-find(first nf_srt no-lock) then run nro_sorte.
{&hm}.
procedure p_automatico:

   run A_W_TRN_A (buffer {&emp},buffer {&ectb},buffer {&tfr},buffer {&trn}).
   if not return-value begins "ok" then return "undo".
   run SEPARA_F.
   for each i_not no-lock:
      run trb/rt/trbrtvld.p ( "C", i_not.recnotfis, 1 ) no-error.
   end.
        
end procedure.                                                 /*p_automatico*/

procedure p_simula:

   run A_W_TRN_A (buffer {&emp},buffer {&ectb},buffer {&tfr},buffer {&trn}).
   if not return-value begins "ok" then return "undo".
   run SEPARA_F.
        
end procedure.                                                 /*p_simula*/

procedure t_emitir:
   def input param cod_pac as int no-undo.
   for each  {&pac} no-lock 
       where {&pac}.codpac = cod_pac
      ,each  {&car} of {&pac} no-lock 
       where {&car}.codemp = {&emp}.codemp
      ,each  {&not} of {&car} no-lock 
       where (if {&not}.empctb = emp_ctb {&tru}) by {&not}.recnotfis:
      assign rsp = (if {&not}.numnotfis = w_trn.notinf 
                       then "P;E"
                       else "P"
                   ).
      return.
   end.
   assign rsp = "P".
end procedure.                                                     /*t_emitir*/

procedure g_trfitem:         
   def input param emp_emi as int no-undo.
   def input param emp_dst as int no-undo.
   def input param cod_pac as int no-undo.
   def input param cod_car as int no-undo.
   
   a_trfitem:
   repeat transaction on error  undo a_trfitem, return "UNDO"
                      on endkey undo a_trfitem, return "UNDO"
                      on stop   undo a_trfitem, return "UNDO"
                      on quit   undo a_trfitem, return "UNDO":
      for each w_inf no-lock query-tuning ( cache-size 50 row ):
         for first {&ipd} exclusive-lock 
             where rowid({&ipd}) = w_inf.idaux 
                   query-tuning ( cache-size 5 row ):
            assign {&ipd}.indsitite = "T".
         end.
         if not avail {&ipd} then do:
            if f_msg( "Falha na transferencia" ) then do: end.
            undo a_trfitem, return "UNDO".
         end.
         if {&cfo}.indmovqtdest = "N" then next. /*NAO TRANSF SPL REMESSA*/
         
         run nfs/r/nfsrgtrf.p (input-output asd_trf_ini,
                               input-output asd_trf,
                               "R",
                               input        emp_emi,
                               input        emp_dst,
                               input        cod_pac,
                               input        cod_car,
                               input        w_inf.codgrp,
                               input        w_inf.codprd,
                               input        w_inf.qtdate,
                               input        w_inf.valliquni /*,
                               input        0*/
                              ).
         case entry(1,return-value):
            when "ok"    then do: end.
            when "refaz" then undo a_trfitem, return "refaz".
            when "retry" then undo a_trfitem, return "retry".
            when "undo"  then undo a_trfitem, return "undo,tranfere".
            otherwise    undo a_trfitem, return "undo".
         end case.
      end.
      leave a_trfitem.
   end.                                                         /*a_trfitem*/
   find first {&ipd} of {&ped} no-lock no-error.
   find first {&trfc} no-lock 
        where {&trfc}.asdtrf = asd_trf 
        and   {&trfc}.empctbemi = emp_emi
        and   {&trfc}.empctbdst = emp_dst no-error.
   find first {&trfi} no-lock 
        where {&trfi}.asdtrf = asd_trf 
        and   {&trfi}.empctbemi = emp_emi
        and   {&trfi}.empctbdst = emp_dst no-error.
   return "ok".
end procedure.                                                  /*g_trfitem*/

procedure g_nf_transferencia:
   def input param asd_trf as int no-undo.
   def var par_out as cha no-undo.
   def var tit_trf as cha no-undo.
   def var i       as int no-undo.
   def var fec_not as log no-undo init false.
   
   def buffer b_ectb for {&ectb}.
   assign par_out = "C,S,F,1,1,ENT,1,615221,3,215271,471933,E,,,,S"
          tit_trf = "TRANSFERENCIA " + string(asd_trf,"9999,9,9999")
          entry(4,par_out)  = string({&emp}.codemp)
          entry(6,par_out)  = input frame f_1 w_trn.codfre
          entry(14,par_out) = string(asd_trf)
          entry(15,par_out) = string(input frame f_1 w_trn.codtrn).
          
   {&hm}. message "Aguarde! Gerando notas de transferencia".

   V_NOTAS:
   repeat &IF defined(FAT_PARCIAL) = 0 &THEN transaction &ENDIF
                      on error  undo V_NOTAS, return "undo"
                      on endkey undo V_NOTAS, return "undo"
                      on stop   undo V_NOTAS, return "undo"
                      on quit   undo V_NOTAS, return "undo"
                      while can-find(first w_trf no-lock):
      find first w_trf no-lock no-error.
      for first b_ectb no-lock where b_ectb.empctb = w_trf.empctbdst
                                     query-tuning ( cache-size 1 row ): 
      end.
      
      for first cfotrs no-lock
          where cfotrs.empctb = w_trf.empctbdst
          and   cfotrs.tiptrs = "tpf":
         assign entry(08,par_out) = string(cfotrs.codcfo).    
      end.
      for first cfotrs no-lock
          where cfotrs.empctb = w_trf.empctbdst
          and   cfotrs.tiptrs = "tef":
         assign entry(10,par_out) = string(cfotrs.codcfo).    
      end.
      
      assign entry(07,par_out) = string(w_trf.empctbemi)
             entry(09,par_out) = string(w_trf.empctbdst)
             entry(11,par_out) = string(b_ectb.codcli)
             entry(14,par_out) = string(w_trf.asdtrf).
      assign frame f_1:visible = false
             buf_dec = "datsai=" + string(w_trn.datsai,'99/99/9999')
             buf_dec = (if w_trn.datsai = ? then "" else buf_dec).      
      
      /*if f_log( "g_nf_transferencia: nfs/t/nfstitem.p", input-output v_msg
              ) then do:
      end.*/
      run nfs/t/nfstitem.p ( par_out, tit_trf ) no-error.
      assign fec_not = (entry(1,return-value) = "fecha_nota") 
             frame f_1:visible = (not fec_not).

      if error-status:error
      or entry(1,return-value) <> "ok" then do:
         if not fec_not and entry(1,return-value) <> "retry" and
            f_msg ( "FALHA AO GERAR NF DE TRANSFERENCIA\n" +
                    "TRANSFERE: " + trim(string(asd_trf,"9999,9,9999")) + "\n"
                    +
                    (if error-status:error
                        then {&err_msg} + "\n"
                        else ""
                    ) +
                    (if entry(1,return-value) <> "ok"
                        then return-value
                        else ""
                    )
                    
                  ) then do:
         end.
         {&hm}. message "Aguarde! Desfazento transacao transferencia".
         undo V_NOTAS, return "undo".
      end.
      /*
      if f_log( "g_nf_transferencia: nfs/t/nfstitem.p ___", input-output v_msg
              ) then do:
      end.*/
      for first {&trfi} no-lock where {&trfi}.asdtrf = w_trf.asdtrf
                                      query-tuning ( cache-size 1 row ): 
      end.
      for first {&trfc} no-lock where rowid({&trfc}) = w_trf.ridtrf
                                      query-tuning ( cache-size 1 row ): 
      end.
      delete w_trf.

      repeat i = 2 to num-entries(return-value + ","):
         for first {&not} no-lock
             where {&not}.recnotfis = int(entry(i,return-value))
             and   {&not}.recnotfis > 0 query-tuning ( cache-size 1 row ):
            run g_i_not ( buffer {&not}, buffer i_not ).
            if return-value = "ok" then do:
               create nf_fat. assign nf_fat.recnotfis = {&not}.recnotfis.
               assign i_not.id_not    = rowid({&not})
                      i_not.numnot    = {&not}.numnot
                      i_not.recnotfis = {&not}.recnotfis
                      w_trn.notsup    = {&not}.numnot
                      w_trn.notinf    = (if w_trn.notinf = 0
                                            then w_trn.notsup
                                            else w_trn.notinf
                                        )
                      a_tel        = "A".
            end.
         end.
         if not fec_not and a_tel = "A" and not flg_aut and not sml_fat then
            disp w_trn.notinf w_trn.notsup a_tel with frame f_1.
      end.
    end.
    find first {&trfi} no-lock where {&trfi}.asdtrf = asd_trf no-error.
    find first {&trfc} no-lock where {&trfc}.asdtrf = asd_trf no-error.

    return "ok".

end procedure.                                           /*g_nf_transferencia*/

procedure g_w_trf:
   def input param emp_ctb as int no-undo.
   def input param cod_pac as int no-undo.
   if cod_pac = 0 then return.

   for each  {&trfc} no-lock
       where {&trfc}.codpac       = cod_pac
       and   {&trfc}.recnotfisemi = 0
       and   (if {&trfc}.empctbemi = emp_ctb
                 then true
                 else false
             )
       and   not can-find(first w_trf no-lock
                          where w_trf.asdtrf = {&trfc}.asdtrf):
      for first trfitem no-lock
          where trfitem.asdtrf    = {&trfc}.asdtrf
          and   trfitem.empctbemi = {&trfc}.empctbemi
          and   trfitem.empctbdst = {&trfc}.empctbdst
          and   (if trfitem.recnotfisemi = {&trfc}.recnotfisemi
                    then true else false
                ):
         create w_trf.
         buffer-copy {&trfc} to w_trf assign 
                                w_trf.ridtrf = rowid({&trfc}).
      end.
   end.
   for each w_trf no-lock by w_trf.asdtrf:
      assign asd_trf      = w_trf.asdtrf
             asd_trf_ini  = asd_trf.
   end.
end procedure.                                                      /*g_w_trf*/
procedure v_mpdapedi:
   def var i as int no-undo.
   { LB/INC/prg_exec.i "i" "mpdapedi." "ok" }   
   if f_msg ( "CARGA PARA PEDIDO DE HORA" ) then do: end.
   return "undo".
end procedure.                                                   /*v_mpdapedi*/
procedure ENA_F_1:
   def input param nro_chm as int no-undo.
   disable all with frame f_1.
   enable w_trn.tipo        when num-entries({&pdt}) > 1 
                            or   nro_chm = 1
          w_trn.codemp      when cod_emp = 0
          w_trn.empctb      when emp_ctb = 0
          w_trn.codfre      when entry(3,par_in) = "F"
                            and  (cod_fre = "" 
                                  or
                                  avail {&car} and {&car}.flgpedhor
                                 )
          w_trn.codemb
          w_trn.codvia
          w_trn.codtrn
          w_trn.nomtrn
          w_trn.endtrn      when not avail {&tfr}
                            or   {&tfr}.codobstrn = ""
          w_trn.muntrn      when not avail {&tfr}
                            or   {&tfr}.codobstrn = ""
          w_trn.stapla      when not avail {&tfr}
                            or   {&tfr}.codobstrn = ""
          w_trn.plavei      when not avail {&tfr}
                            or   {&tfr}.codobstrn = ""
          w_trn.codpac      when w_trn.tipo = "PACOTE"
          w_trn.codcar      when w_trn.tipo = "CARGA"
          w_trn.datemi      when w_trn.tipo = "NOTA"
          w_trn.notinf      when w_trn.tipo = "NOTA"
          w_trn.notsup      when w_trn.tipo = "NOTA"
          w_trn.notref      when avail {&car} and {&car}.flgpedhor
          w_trn.qtdemb      when avail {&car} and {&car}.flgpedhor
          w_trn.datsai
   with frame f_1.
   for first {&not} no-lock where {&not}.recnotfis = rec_not
                                  query-tuning ( cache-size 5 row ):
      assign w_trn.datemi = {&not}.datemi
             w_trn.notinf = {&not}.numnotfis
             w_trn.notsup = {&not}.numnotfis
             w_trn.datemi:sensitive in frame f_1 = false
             w_trn.notinf:sensitive in frame f_1 = false
             w_trn.notsup:sensitive in frame f_1 = false.
      disp w_trn.datemi w_trn.notinf w_trn.notsup with frame f_1.
   end.
   pause 0 before-hide.
end procedure.                                                      /*ENA_F_1*/
                                            
procedure g_i_ped:         
   def input-output param fal_atu as cha no-undo.
   def input-output param ped_mg  as cha no-undo.
   def input-output param lpd_cfn as cha no-undo. /*Pedidos consumidor final*/
   def       output param sit_dvg as cha no-undo.
   def input        param to_day  as dat no-undo.
   def param buffer bi_ped for i_ped.
   
   def var int_aux  as int no-undo.

   def buffer p_cli  for {&cli}.
   def buffer b_cli  for {&cli}.
   def buffer b_bas  for {&bas}.
   def buffer b_cfo  for {&cfo}.
   def buffer b_ectb for {&ectb}.
   
   create bi_ped.
   assign i             = i + 1            
          bi_ped.numseq = i
          bi_ped.empctb = 1
          bi_ped.ridped = rowid({&ped})
          bi_ped.ridcar = rowid({&car})
          bi_ped.ridpac = (if avail {&pac} then rowid({&pac}) else ?)
          bi_ped.codpac = {&car}.codpac
          bi_ped.codcar = {&ped}.codcar
          bi_ped.numped = {&ped}.numped
          bi_ped.asdped = {&ped}.asdped
          bi_ped.sitped = (if sml_fat then "L" else {&car}.codsitcar).

   for first f_tpd no-lock where f_tpd.tipped = {&ped}.tipped:
      assign bi_ped.pedmkt = f_tpd.pedmkt.
   end.

   for first b_cli no-lock where rowid(b_cli) = rowid({&cli}): end.
   if lookup({&ped}.tipped,"RAS") > 0 then do:
      for first b_ectb no-lock
          where b_ectb.empctb = 1
         ,first b_cli no-lock
          where b_cli.codcli = b_ectb.codcli:
         assign bi_ped.empctb = 1
                int_aux = f_stapri (buffer b_cli, buffer p_cli).
      end.
   end. else do:
      assign int_aux = f_stapri (buffer {&cli}, buffer p_cli).
      for first w_ufempctb no-lock
          where w_ufempctb.codest = int_aux
                query-tuning ( cache-size 1 row ):
         assign bi_ped.empctb = w_ufempctb.empctb.
      end.
      for first b_ectb no-lock
          where b_ectb.empctb = {&ped}.empctb:
         assign bi_ped.empctb = {&ped}.empctb
                bi_ped.tipemp = b_ectb.tipemp.         
      end.
   end.
   
   for last  {&ipd} of {&ped} no-lock
       where (if {&ipd}.qtdate > 0 {&tru}) while not sml_fat:
      assign bi_ped.sitped = {&ipd}.indsitite.    
   end.
   
   for first atuisp no-lock
       where atuisp.asdped = {&ped}.asdped
       and   (if atuisp.rescon = "" {&tru})
             query-tuning ( cache-size 5 row ):
      assign fal_atu = fal_atu + trim(string({&ped}.asdped,">9999,99999"))
                               + " ".
   end.

   if bi_ped.empctb = 1 and int_aux = 31 then do:
      assign ped_mg = ped_mg + trim(string({&ped}.asdped,">9999,99999"))
                             + " ".
   end.

   if f_consumo(bi_ped.empctb, b_cli.codcli, output sit_dvg) then do: end.

   &IF DEFINED(GLPI17955) &THEN
   assign bi_ped.stadst = int(entry(14,sit_dvg,";")) no-error. &ENDIF
   if entry(1,sit_dvg,";") <> entry(2,sit_dvg,";")
   or entry(1,sit_dvg,";") <> "Rev" and 
      lookup(entry(07,sit_dvg,";"),"{&UF_BLK}") > 0 then do:
      assign lpd_cfn = lpd_cfn + " " + string({&ped}.asdped,">9999,99999")
             lpd_cfn = trim(lpd_cfn).
      return.
   end.

end procedure.                                                      /*g_i_ped*/

procedure SEPARA_F:
   def var fal_atu as cha no-undo.
   def var ped_mg  as cha no-undo.
   def var lpd_cfn as cha no-undo. /*Lista de pedidos consumidor final*/
   def var sta_dst as int no-undo.
   def var st_acao as cha case-sensitive no-undo init " ".
   def var rsp_fat as cha no-undo.
   def buffer b_cfo for {&cfo}.
   
   if current-value(ctlfat) = 0 then do:
      run rt/rtedmsg.p (60,"   A T E N C A O   ", "", 
                        "\n\nOPCOES DE EMISSAO DE NOTAS FISCAIS INATIVAS\n\n"
                       ) no-error.
      return "refaz".
   end.

   for each i_ped no-lock: delete i_ped. end.
   
   /*************************************************************************
     st_acao = "L" Faturar para cliente
     st_acao = "T" Transferir para filial
   *************************************************************************/  
   assign st_acao = (if {&ectb}.cnpj = {&emp}.cgcemp + {&emp}.cgccplemp
                        then "L"
                        else "T"
                    )
          i       = 0.
   empty temp-table w_ctr no-error.
   for each w_ctr no-lock: delete w_ctr. end.

   if w_trn.tipo = "PACOTE" then do:
      {&hm}. message "Aguarde! Selecionando pedidos do pacote".
      for first {&trfc} no-lock
          where {&trfc}.codpac       = w_trn.codpac
          and   {&trfc}.empctbemi    = w_trn.empctb
          and   {&trfc}.recnotfisdst = 0 
          while st_acao = "T" and entry(3,par_in) = "F"
                query-tuning ( cache-size 5 row ):
         assign rsp = "i\nPacote " + trim(string(w_trn.codpac,">>>>,>>9"))
                    + " nao entrou na filial."
                    + "@m\nProcesso sera abortado."
                    + ",Pressione qualquer tecla".
         run trg/trgclrsp.p (input-output rsp, false).
         return "refaz".
      end.
      
      if not sml_fat then
      for each  {&pac} no-lock 
          where {&pac}.codpac = w_trn.codpac
         ,each  {&car} of {&pac} no-lock                  
          where {&car}.codemp = {&emp}.codemp
          and   (if keycode({&car}.codsitcar) = 102
                 or keycode({&car}.codsitcar) = 116
                    {&tru}
                ):
         assign rsp = "i\nCarga " + trim(string({&car}.codcar,">>>>>>>>>>>9"))
                    + (if keycode({&car}.codsitcar) = 102
                          then " sendo faturada."
                          else " sendo transferida."
                      )
                    + "@m\nProcesso sera abortado."
                    + ",Pressione qualquer tecla".
         run trg/trgclrsp.p (input-output rsp, false).
         return "refaz".
      end.

      for each  {&car} no-lock
          where {&car}.codpac = w_trn.codpac
         ,each  {&ped} of {&car} no-lock
          where (if {&ped}.flgpedati and
                    lookup({&ped}.tipped,"PD") = 0 {&tru}) /*GLPI10884*/
         ,first orgaopb no-lock
          where orgaopb.codcli = {&ped}.codcli
          and   (if lookup(orgaopb.tipopb,"MG,M,E") > 0 {&tru})
          and   (if lookup({&ped}.tipped,"RAS") = 0 {&tru}):
         for each  empenho no-lock
             where empenho.codcli = orgaopb.codcli
             and   empenho.asdped = {&ped}.asdped
             and   can-find(last  {&cfo} no-lock
                            where {&cfo}.codcfo = empenho.codcfo):
            leave.
         end.
         if not avail empenho then do:
            assign rsp = "EMPENHO NAO CADASTRADO PARA O PEDIDO "
                       + string({&ped}.asdped,">9999,99999").
            run trg/trgclrsp.p (input-output rsp, false).
            return "refaz".
         end.
      end.

      for each  {&pac} no-lock 
          where {&pac}.codpac = w_trn.codpac
         ,each  {&car} of {&pac} no-lock                  
          where {&car}.codemp = w_trn.codemp                   
         ,each  {&ped} of {&car} no-lock 
         ,first {&cli} of {&ped} no-lock
         ,each  {&not} no-lock
          where {&not}.codcar = {&ped}.codcar
          and   {&not}.numped = {&ped}.numped
          and   (if {&not}.datemi >= {&pac}.datatupac {&tru})
                by {&not}.recnotfis desc:
         assign i = (if i < {&not}.seqent then {&not}.seqent else i).
      end.   

      for each  {&pac} no-lock 
          where {&pac}.codpac = w_trn.codpac
         ,each  {&car} of {&pac} no-lock                  
          where {&car}.codemp = w_trn.codemp                   
          and   (if {&car}.flgati {&tru} )
         ,first {&stc} of {&car} no-lock 
          where (if {&stc}.flglibfat and {&stc}.codsit <> "A" {&tru} )
         ,each  {&ped} of {&car} no-lock 
          where (if {&ped}.flgpedati {&tru})
          and   (if w_trn.empctb = 1
                 or w_trn.empctb = {&ped}.empctb {&tru}) /*24/09/2012*/
         ,first {&cli} of {&ped} no-lock
         ,first {&ipd} of {&ped} no-lock 
          where (if lookup({&ipd}.indsitite,"L,T") > 0 and {&ipd}.qtdate > 0
                    then true
                    else false
                )
            by {&ped}.numseqped by {&car}.numseqcar
            by {&ped}.codcar    by {&ped}.numped
               query-tuning ( cache-size 200 row ):

          if entry(1,{&cli}.idtagtcob) <> "N" then do:
             run ver_agtcobrador ( buffer {&ped}, buffer {&cli} ).
             if lookup(entry(1,return-value),"ok") = 0 then return "refaz".
          end.
          /*
          if {&ped}.empctb = 8 and w_trn.codpac <> 293687 then do:
             assign rsp = "i\nPedido: " + string({&ped}.asdped,"99999,99999")
                        + " da filial Maranhao".
             run trg/trgclrsp.p (input-output rsp, false).
             return "refaz".
          end.
          */
          run g_i_ped ( input-output fal_atu, 
                        input-output ped_mg,
                        input-output lpd_cfn,
                              output sit_dvg,
                        to_day,
                        buffer i_ped
                      ).
      end.
   end. else do:
      {&hm}. message "Aguarde! Selecionando pedidos da carga".
      for each  {&car} no-lock 
          where {&car}.codcar = w_trn.codcar                    
          and   {&car}.codemp = {&emp}.codemp                   
          and   (if {&car}.flgati or sml_fat {&tru})
         ,each  {&stc} of {&car} no-lock 
          where ({&stc}.flglibfat or sml_fat)
         ,each  {&ped} of {&car} no-lock 
          where (if {&ped}.flgpedati or sml_fat {&tru})   
          and   (if not {&car}.flgpedhor
                 or {&ped}.flgpedlib
                 or sml_fat
                    then true 
                    else false
                )
          and   (if asd_ped = 0
                 or {&ped}.asdped = asd_ped then true else false)
          &IF DEFINED(GLPI28245) &THEN
          and   (if lookup({&ped}.tipped,pdf_fil) = 0
                 or {&ped}.empctb = w_trn.empctb
                    then true else false
                )
          &ENDIF
         ,first {&cli} of {&ped} no-lock
         ,first {&ipd} of {&ped} no-lock 
          where {&ipd}.qtdate > 0
          and   (lookup({&ipd}.indsitite,"L,T") > 0 or sml_fat)
          by {&ped}.numseqped by {&car}.numseqcar
          by {&ped}.codcar    by {&ped}.numped
             query-tuning ( cache-size 200 row ):
         run g_i_ped ( input-output fal_atu, 
                       input-output ped_mg,
                       input-output lpd_cfn,
                             output sit_dvg,
                       to_day,
                       buffer i_ped
                     ).
      end.

   end.

   if trim(fal_atu) <> "" then do:
      assign rsp = "Pedidos nao atualizados " + trim(fal_atu).
      if length(rsp) > 80 then do:
         assign overlay(rsp,r-index(substr(rsp,1,80)," "),1) = "@".
         if length(entry(2,rsp,"@")) > 80 then do:
            assign fal_atu = entry(2,rsp,"@")
                   overlay(fal_atu,
                           r-index(substr(fal_atu,1,80)," "),1) = ","
                   entry(2,rsp,"@") = entry(1,fal_atu,",")
                   fal_atu          = entry(2,fal_atu,",")
                  fal_atu = substr(fal_atu,1,r-index(substr(fal_atu,1,70)," "))
                   rsp     = rsp + "," + fal_atu.
         end.
      end.
      run trg/trgclrsp.p (input-output rsp, false).
      return "refaz".
   end. 

   if lpd_cfn <> "" then do:
      if i_ped.empctb <> 1 and lookup(i_ped.tipemp,"E") = 0 /* BHFS */
      or i_ped.stadst <> 31 then do:

      assign rsp = "Divergencia cad: " + trim(lpd_cfn) 
                 + " " + string(i_ped.empctb) 
                 + " " + string(i_ped.stadst)
                 + " " + i_ped.tipemp
                 .
      if length(rsp) > 80 then do:
         assign overlay(rsp,r-index(substr(rsp,1,80)," "),1) = "@".
         if length(entry(2,rsp,"@")) > 80 then do:
            assign lpd_cfn = entry(2,rsp,"@")
                   overlay(lpd_cfn,
                           r-index(substr(lpd_cfn,1,80)," "),1) = ","
                   entry(2,rsp,"@") = entry(1,lpd_cfn,",")
                   lpd_cfn          = entry(2,lpd_cfn,",")
                   lpd_cfn = substr(lpd_cfn,1,r-index(substr(lpd_cfn,1,70)," "))
                   rsp     = rsp + "," + lpd_cfn.
         end.
      end.
      run trg/trgclrsp.p (input-output rsp, false).
      return "refaz".
      end.
   end.
   
   /*
   if entry(17,sit_dvg,";") begins "I" then do:
      assign rsp = "Cliente suframa@Atualizar cadastro suframa".
      run trg/trgclrsp.p (input-output rsp, false).
      return "refaz".
   end.
   */

   /************convencioMG
   if trim(ped_MG) <> "" and 
      lookup(userid("Dictdb"),"salgado") = 0 then do:
      assign rsp = "Pedidos para MG " + trim(ped_MG).
      if length(rsp) > 80 then do:
         assign overlay(rsp,r-index(substr(rsp,1,80)," "),1) = "@".
         if length(entry(2,rsp,"@")) > 80 then do:
            assign ped_MG = entry(2,rsp,"@")
                   overlay(ped_MG,
                           r-index(substr(ped_MG,1,80)," "),1) = ","
                   entry(2,rsp,"@") = entry(1,ped_MG,",")
                   ped_MG          = entry(2,ped_MG,",")
                  ped_MG = substr(ped_MG,1,r-index(substr(ped_MG,1,70)," "))
                   rsp     = rsp + "," + ped_MG.
         end.
      end.
      run trg/trgclrsp.p (input-output rsp, false).
      return "refaz".
   end.
   *************************************************/
   
   {&hm}. message "Aguarde! Verificando pedidos selecionados".

   for each  i_ped no-lock
      ,first {&ped} no-lock 
       where rowid({&ped}) = i_ped.ridped
      ,first tipped no-lock
       where tipped.tipped = {&ped}.tipped
       and   (if tipped.tipopesrf = "014" 
                 then true else false
             )
      ,first b_cfo no-lock
       where b_cfo.codcfo = tipped.codcfo
       and   (if b_cfo.codmsgnotfisant <> 0
                 then true else false
             ):
      assign i_ped.notant = 0.
      do transaction on error  undo, return "undo"
                     on endkey undo, return "undo"
                     on stop   undo, return "undo"
                     on quit   undo, return "undo":
         for first {&psr} exclusive-lock
             where {&psr}.asdped = {&ped}.asdped
            ,last  {&cfo} no-lock
             where {&cfo}.codcfo = {&psr}.codcfo
                  query-tuning ( cache-size 2 row ):
            assign i_ped.notant = {&psr}.recnotfis.
            for first {&not} no-lock
                where {&not}.recnotfis = {&psr}.recnotfis
                      query-tuning ( cache-size 1 row ):
               if lookup({&not}.codvld,"101,102,301,302") > 0
               or lookup(string({&not}.codsitreg,"99"),"17,18,24,25") > 0
                  then do:
                  assign i_ped.notant = 0.
               end. else assign i_ped.empctb = {&not}.empctb.
            end.
         end.
      end.                                                  /*do transaction*/
   end.
   
   if can-find(first i_ped no-lock where i_ped.notant = 0) then do:
      return "troca/falta".
   end.

   &IF DEFINED(GLPI440) &THEN
   ASSIGN rsp = "".
   L:
   FOR EACH  i_ped no-lock
       where (if f_tpd.pedb2c 
              or sml_fat then false else true)
      ,FIRST {&ped} no-lock 
       WHERE ROWID({&ped}) = i_ped.ridped
      ,FIRST {&pgt} OF {&ped} NO-LOCK
       WHERE (IF not sml_fat and LOOKUP({&pgt}.forpgt,"R") > 0 {&tru}):
      FOR FIRST {&mppg} NO-LOCK
          WHERE {&mppg}.referenceNum = STRING({&ped}.asdped)
          AND   LOOKUP({&mppg}.situacao,"RE") > 0:
         NEXT L.
      END.

      ASSIGN rsp = rsp + (IF rsp = "" THEN "" ELSE " ")
                 + STRING({&ped}.asdped,"z999999999").
   END.

   IF rsp <> "" THEN DO:
      RUN rt/rtedmsg.p ( 30, "A T E N C A O","",
                             "PEDIDO"                                       + 
                             TRIM(STRING(NUM-ENTRIES(rsp," ") > 1,"S/"))    +
                             " COM CARTAO DE CREDITO SEM RESERVA"
                       ) NO-ERROR.
   
      RETURN "refaz".
   END.
   &ENDIF

   for each  i_ped no-lock
      ,first {&ped} no-lock 
       where rowid({&ped}) = i_ped.ridped      
      ,each {&ipd} of {&ped} no-lock 
       where (if {&ipd}.codgrp = 98  and 
                 {&ipd}.qtdate > 0   and
                 ({&ipd}.codprd = 028 or 
                  {&ipd}.codprd = 029 or
                  {&ipd}.codprd = 030 or
                  {&ipd}.codprd = 111 or
                  {&ipd}.codprd = 183
                 )
              or lookup({&ped}.tipped,"JRN,CLD") > 0 and {&ipd}.qtdate > 0   
              then true 
              else false
             ) query-tuning ( cache-size 5 row ):
      assign i_ped.notant = 0.
      return "troca/falta".
   end.

   if st_acao = "L"                                     and 
      can-find(first i_ped no-lock
               where i_ped.empctb = {&ectb}.empctb)     and
      can-find(first i_ped no-lock
               where i_ped.empctb <> {&ectb}.empctb)    then do:
      run nfs/v/nfsvectb.p ( {&ectb}.empctb, {&ectb}.empctb ).
      if int(return-value) <> {&ectb}.empctb then return "refaz".
      assign rsp = "separador=;,m\nATENCAO! FATURAR PEDIDOS CNPJ "
                 + string({&ectb}.cnpj,"xx.xxx.xxx/xxxx-xx")
                 + "@m\n" 
                 + replace({&ectb}.nomfts,";",",") + " (" 
                 + replace({&ectb}.desend,";",",") + ")"
                 + ";m\nTecle <A>bortar ou <CODIGO DA EMPRESA>_para prosseguir"
                 + ";A;end-error;" + string({&ectb}.empctb).
      run trg/trgclrsp.p (input-output rsp, false).
      if int(rsp) <> {&ectb}.empctb then return "refaz".
      for each  i_ped no-lock where i_ped.empctb <> {&ectb}.empctb:
         delete i_ped.
      end.    
   end.   
   
   assign rsp = "".
   for each  i_ped no-lock where (if i_ped.idtfat = "" {&tru}):

      assign i_ped.idtfat = string(i_ped.empctb = {&ectb}.empctb
                                   or sml_fat,"F/T")
             rsp          = rsp + (if rsp = "" then "" else ",")
                          + i_ped.idtfat + i_ped.sitped + st_acao.

      if entry(3,par_in) = "F" and                                
         not sml_fat and
         lookup(i_ped.idtfat + i_ped.sitped + st_acao,
                "FLT,FTT,TLL,FLL,TTL") = 0
      or entry(3,par_in) = "E" and
         lookup(i_ped.idtfat + i_ped.sitped + st_acao,"TTL") = 0
         then do:
         if f_msg ( w_trn.tipo + " " + (if w_trn.tipo = "PACOTE"
                                           then string(w_trn.codpac)
                                           else string(w_trn.codcar)
                                       ) 
                                     + "\nPEDIDO " 
                                     + string(i_ped.asdped,">9999,99999") 
                                     + "\nCodigo de controle (" 
                                     + i_ped.idtfat + i_ped.sitped + st_acao
                                     + ")\nInformar ao analista" )
            then do:
         end.
         return "refaz".
      end.
   end.

   /***************************************************************************
      FLT - Faturar direto na filial
      TLL - Transferir para a filial 
      FTT - Faturar na filial apos transferencia
      FLL - Faturar na matriz
      TTL - Pedidos ja gravados em TRFCTL e TRFITEM
   ***************************************************************************/
   assign sit_prc = "TLL,FTT,FLL".

   if lookup("FLT",rsp) > 0 then do:
      assign rsp = "separador=;,i\n"
                 + caps(substr(f_dscacao( st_acao,2 ),1,1))
                 + lc  (substr(f_dscacao( st_acao,2 ),2))
                 + " "  + caps(w_trn.tipo) + " " 
                 + (if w_trn.tipo = "PACOTE"
                       then string(w_trn.codpac)
                       else string(w_trn.codcar)
                    ) 
                 + " nao efetuada"
                 + "@i\nFaturar direto na filial?"
                 + ";Tecle <S>im <N>ao;N;S".
      if not sml_fat then do:
         run trg/trgclrsp.p (input-output rsp, false).
         if rsp <> "S" 
         or w_trn.tipo = "PACOTE" 
            /*and {&ectb}.empctb <> 6*/ then return "refaz".
      end.
      assign sit_prc = "FLT".
   end.

   for each  i_ped no-lock
       where lookup(i_ped.idtfat + i_ped.sitped + st_acao,sit_prc) = 0:
      delete i_ped.
   end.
   assign sit_prc = "".
   for each  i_ped no-lock
       where lookup(i_ped.idtfat + i_ped.sitped + st_acao,sit_prc) = 0:
      if lookup(i_ped.idtfat + i_ped.sitped + st_acao,sit_prc) = 0 then do:
         assign sit_prc = sit_prc + (if sit_prc = "" then "" else ",")
                        + i_ped.idtfat + i_ped.sitped + st_acao.
      end.
   end.
   if not flg_aut and not sml_fat then
      disp f_centra ( dsc_cfo + " - " + sit_prc, 78 ) @ dsc_cfo 
           with frame f_1.
   if caps(entry(3,par_in)) = "E" then do:
      run trf_efetivada( {&ectb}.empctb, w_trn.codpac, w_trn.codcar ).
      return trim(return-value).
   end.
   
   if asd_trf_ini <> -1 and a_asdtrf( input-output asd_trf_ini,
                                      input-output asd_trf
                                     ) then do:
   end.
   assign asd_trf = 0 asd_trf_ini = -1.
   for each w_trf no-lock: delete w_trf. end.

   if st_acao = "L"                                         and
      w_trn.tipo = "PACOTE"                                 and
      not can-find(first i_ped no-lock
                   where i_ped.empctb = {&ectb}.empctb)    and
      can-find(first {&trfc} no-lock
               where {&trfc}.codpac = w_trn.codpac)     
      then do transaction: {&hmn}.
      run g_w_trf ( {&ectb}.empctb, w_trn.codpac ).
   end.
   
   if not can-find(first i_ped no-lock) and
      not can-find(first w_trf no-lock) then do:
      {&hm}.
      if f_msg ( {&auto} + 
                 w_trn.tipo + " " + (if w_trn.tipo = "PACOTE"
                                        then string(w_trn.codpac)
                                        else string(w_trn.codcar)
                                    ) +
                 " nao tem pedido para " + f_dscacao( st_acao,1 )
               ) then do:
      end.
      return "refaz".
   end.

   if lookup("TLL",sit_prc) > 0 and lookup("FLL",sit_prc) > 0
   or can-find(first i_ped no-lock 
               where i_ped.empctb <> {&ectb}.empctb
               and   i_ped.idtfat <> "T") 
   or can-find(first i_ped no-lock
               where i_ped.empctb <> {&ectb}.empctb
               and   i_ped.idtfat = "T")        and
      can-find(first i_ped no-lock
               where i_ped.empctb = {&ectb}.empctb
               and   i_ped.idtfat = "F")        then do:
      return "Empresa".
   end.   

   {&hm}.
   if w_trn.codpac > 0 then do:
      for last  {&trfc} no-lock
          where {&trfc}.codpac = w_trn.codpac
         ,first {&not} no-lock
          where {&not}.recnotfis = {&trfc}.recnotfisemi
                query-tuning ( cache-size 5 row ):
         run a_w_ctr ( {&trfc}.empctbdst, buffer {&not} ).
      end.
   end.
   if can-find(first ctrapoio no-lock
               where ctrapoio.empctb = {&ectb}.empctb) and
      not can-find(first w_ctr no-lock
                   where w_ctr.codaux = w_trn.empctb) then do:
      run g_w_ctr ( {&ectb}.empctb ).
   end.
   for first w_ctr no-lock where w_ctr.codaux = {&ectb}.empctb
                                 query-tuning ( cache-size 1 row ): 
   end.

   if not flg_aut and not sml_fat then do:
      assign rsp = "separador=;,i\nConfirmacao final@m\n"
                 + (if avail w_ctr then w_ctr.munloc else "" )
                 + ";Tecle <P>rosseguir <R>efazer;R;P".
      run trg/trgclrsp.p (input-output rsp, false).
      if rsp = "R" then return "refaz".
   end.

   for first i_ped no-lock 
       where i_ped.notant <> ?
       and   i_ped.idtfat = "T"
      ,first {&ped} no-lock 
       where rowid({&ped}) = i_ped.ridped while entry(3,par_in) = "F":

      if {&ped}.valtotateped > 800 then do: /*GLPI14393*/
          run rt/rtedmsg.p 
                (15,"AVISO", "Pressione F4 para voltar@v,V", 
                 "PEDIDO..: " + string(i_ped.asdped,">9999,99999") + 
                 " EMPRESA: " + string({&ped}.empctb)              +
                 " SIMPLES REMESSA\nATENDIDO: "                    +
                 trim(string({&ped}.valtotateped,">>>,>>>,>>9.99"))
                ) no-error.
      end.

   end.

   for each  i_ped no-lock
       where (if i_ped.idtfat = "" {&tru}):
      assign i_ped.idtfat = string(i_ped.empctb = {&ectb}.empctb,"F/T").
   end.
   assign i = 0.
   
   assign i = 0 nro_not = 0 hor_ini = time.
   etime(true).

   find first i_ped no-lock no-error.
   find first notxped no-lock where rowid(notxped) = ? no-error.

   /*Para numerar as notas de um pacote em sequencia**************************/
   if lookup(w_trn.tipo,"PACOTE") > 0 then do:
      run v_notxped. {&hm}.
      if entry(1,return-value) <> "ok" then do:
         if f_msg ( "FALHA NA NUMERACAO DAS NOTAS DOS PEDIDOS" ) then do: end.
         return "refaz".
      end.
      /*if f_log ( st_acao + string(w_trn.codpac,"ZZZZZZZ") + " LER_I_PEDIDO",
                 input-output v_msg
               ) then do:
      end.*/
   end.

   LER_I_PEDIDO:
   REPEAT on error  undo, leave ler_i_pedido
          on endkey undo, leave ler_i_pedido
          on stop   undo, leave ler_i_pedido
          on quit   undo, leave ler_i_pedido
          while can-find(first i_ped no-lock):

      run F_PEDIDO.
      if return-value <> "ok" then do:
         for each w_trf no-lock: delete w_trf. end.
      end.
      assign rsp     = return-value
             rsp_fat = return-value.
      {&hm}.
      hide frame FN_01   no-pause.
      hide frame f_res   no-pause.
      hide frame f_dup   no-pause.
      find first {&pac}  no-lock no-error.
      find first {&car}  no-lock no-error.
      find first {&ped}  no-lock no-error.
      find first {&cli}  no-lock no-error.
      find first {&dup}  no-lock no-error.
      find first {&ccr}  no-lock no-error.
      find first {&met}  no-lock no-error.
      find first {&gsz}  no-lock no-error.
      find first {&ipd}  no-lock no-error.
      find first {&prd}  no-lock no-error.
      find first {&est}  no-lock no-error.
      find first {&not}  no-lock no-error.
      find first {&inf}  no-lock no-error.
      find first {&esr}  no-lock no-error.
      find first {&pnf}  no-lock no-error.
      find first {&pnfe} no-lock no-error.
      find first {&trfi} no-lock no-error.
      find first {&trfc} no-lock no-error.
      find first notxped no-lock no-error.
      find first textoirregularidade no-lock no-error.
      find first {&pac}  no-lock where {&pac}.codpac = w_trn.codpac no-error.
      find first {&car}  no-lock where {&car}.codcar = w_trn.codcar no-error.
      find first {&sep}  no-lock where {&sep}.codpac = w_trn.codpac no-error.
      if avail w_not and lookup(entry(1,rsp),"UNDO,refaz") = 0 then do:
         for first {&not}  no-lock 
             where {&not}.recnotfis = w_not.recnotfis
             and   (if {&not}.sitnfe = "" {&tru})
            ,last  {&cfo}  no-lock 
             where {&cfo}.codcfo     = {&not}.codcfo
             and   {&cfo}.datultatu <= {&not}.datmov:
         end.    
      end.
      if lookup(entry(1,rsp),"UNDO,refaz") > 0
      or not can-find(first i_ped no-lock) then do:
         assign w_trn.datemi:visible in frame f_1 = false
                w_trn.notinf:visible in frame f_1 = false
                w_trn.notsup:visible in frame f_1 = false
                w_trn.notref:visible in frame f_1 = false
                w_trn.qtdemb:visible in frame f_1 = false.
         disp "" @ a_tel
              "" @ msg_fat
         with frame f_1.
         leave.
      end.

   END.                                                        /*LER_I_PEDIDO*/
   
   if can-find(first nf_fat no-lock) then run envia_nf_fat.                
   if can-find(first nf_srt no-lock) then run nro_sorte.

   &IF DEFINED(FAT_PARCIAL) &THEN
   /*if w_trn.tipo = "PACOTE" and
      f_log ( st_acao + string(w_trn.codpac,"ZZZZZZZ") + " T_NF_TRANSF",
              input-output v_msg
            ) then do:
   end.*/
   
   T_NF_TRANSF:
   repeat on error  undo T_NF_TRANSF, return "UNDO,T_NF_TRANSF"
          on endkey undo T_NF_TRANSF, return "UNDO,T_NF_TRANSF"
          on stop   undo T_NF_TRANSF, return "UNDO,T_NF_TRANSF"
          on quit   undo T_NF_TRANSF, return "UNDO,T_NF_TRANSF"
          while can-find(first w_trf no-lock):
      run g_nf_transferencia ( asd_trf ).
      case entry(1,return-value):
         when "ok"    then do: end.
         when "retry" then undo T_NF_TRANSF, retry T_NF_TRANSF.
         otherwise    undo T_NF_TRANSF, leave T_NF_TRANSF.
      end case.
      assign frame f_1:visible = true
             asd_trf_ini = (if not can-find(first w_trf no-lock)
                               then -1
                               else asd_trf_ini
                           ).    
   end.
   
   /*if w_trn.tipo = "PACOTE" and
      f_log ( st_acao + string(w_trn.codpac,"ZZZZZZZ") + " T_NF_TRANSF__",
              input-output v_msg
            ) then do:
   end.*/
   &ENDIF
   
   if asd_trf_ini <> -1 and a_asdtrf( input-output asd_trf_ini,
                                      input-output asd_trf
                                     ) then do:
   end.
   if can-find(first nf_fat no-lock) then run envia_nf_fat.                
   assign rsp = rsp_fat.
   hide frame NF_01 no-pause.
   hide frame f_res no-pause.
   run exporta_separacao ( return-value ).

   return trim(return-value).
end procedure.                                                     /*SEPARA_F*/

procedure nro_sorte:
   def var v_str as cha no-undo.
   assign v_str   = os-getenv("_CNFPGS") + "/_FIFO/batch_srt".
   hide message no-pause.
   message "Aguarde! Preparando numero da sorte".
   output stream s_rel3 to value(v_str) append unbuffered.
   for each nf_srt no-lock:
      put stream s_rel3 unformatted 
          string(nf_srt.recnotfis,"999999999") skip.
      delete nf_srt.
   end.
   output stream s_rel3 close.
   hide message no-pause.
end procedure.                                                    /*nro_sorte*/

procedure ver_agtcobrador:
   def param buffer b_ped for {&ped}.
   def param buffer b_cli for {&cli}.
   for first {&pgt} of b_ped no-lock
      ,first {&cct} of {&pgt} no-lock 
       where (if {&cct}.FlgEmiBolCtr then true else false):
      find first {&ppg} of b_cli no-lock no-error.
      run rt/rtbcocli.p ( buffer b_cli, buffer {&ppg}, 
                                       "tem," + string(b_ped.empctb)
                     ).
      if return-value <> "" then leave.                     
      assign rsp = "i\nCliente nao autorizado a pagar com boleta@"
                 + "Pedido: " + string(b_ped.asdped,">9999,99999").
      run trg/trgclrsp.p (input-output rsp, false).
      return "refaz".
   end.
   return "ok".
end procedure.                                              /*ver_agtcobrador*/
procedure trf_efetivada:
   def input param emp_ctb as int no-undo.
   def input param cod_pac as int no-undo.
   def input param cod_car as int no-undo.
   def buffer b_trfc for {&trfc}.
   def buffer b_trfi for {&trfi}.
   
   for each  b_trfc no-lock
       where b_trfc.recnotfisemi = 0
       and   b_trfc.empctbemi    = emp_ctb
       and   (if cod_pac > 0 and b_trfc.codpac = cod_pac
              or cod_car > 0 and b_trfc.codcar = cod_car
                 then true else false
             )
       ,first b_trfi no-lock
        where b_trfi.asdtrf    = b_trfc.asdtrf
        and   b_trfi.empctbemi = b_trfc.empctbemi
        and   (if b_trfi.qtdtrf - b_trfi.qtdsrm > 0 {&tru}):
      if f_msg ( "TRANSFERENCIA NAO EFETIVADA " ) then do: end.
      return "retry".
   end.
   return "ok".
end procedure.                                               /*trf_efetivada*/
procedure SEPARA_E:
   run SEPARA_F.
   if return-value <> "Ok" then return "refaz".   
   {&hm}.
   if can-find(first i_ped no-lock) then do:
      {&hm}. message "PEDIDOS NAO FATURADOS".
      return "refaz".
   end.
   
   /* Thiago - Transmissao de ficha Automatica 18/08/2016 */

      assign opc_prg = "P,I"
                     + "/<P>rosseguir <I>gnorar"
                     + "/Gerando produto perigoso e Ibama"
                     + "/Escolha uma das opcoes"
                     + "/P".
            par_prg = "MT/LG/MTLGpernew.p[fichas-perigoso-e-ibama,,,,80," +
                      string(w_trn.codpac). /* Entry 6 */
     run LB/PRG/CHMrel.p.

   /* Fim Thiago */
   
   run value( "E_" + w_trn.tipo ) ( {&ectb}.empctb ).

end procedure.                                                     /*SEPARA_E*/

procedure cta_nrserie:
   &scoped-define sipd   tambasa.nrserieipd

   def input param asd_ped as int64 NO-UNDO.
   def input param cod_grp as int no-undo.
   def input param cod_prd as int no-undo.
   def input-output param nro_inf as int no-undo.
   def var lin_nsr as cha no-undo.

   if asd_ped = 0 then return "ok".
   for first {&sipd} no-lock
       where {&sipd}.asdped = asd_ped 
       and   (if {&sipd}.codgrp = cod_grp and
                 {&sipd}.codprd = cod_prd
                 then true else false
             ):
   end.
   if not avail {&sipd} then return "ok".
   run mpd/mpdmtnsr.p (asd_ped, cod_grp, cod_prd, output lin_nsr) no-error.
   if error-status:error and
      f_msg ("Falha ao gerar numero de serie pedido\n"              + 
             trim(string(- error-status:num-messages,"(ZZZZZ)"))    +
             {&err_msg} ) then do:
      return "undo".
   end.
   assign nro_inf = nro_inf + num-entries(lin_nsr,"\n").
   return "ok".
end procedure.                                                  /*cta_nrserie*/

procedure cta_notas:
   def param buffer bi_ped for i_ped.
   def param buffer b_ped  for {&ped}.
   def var cod_est as int no-undo.
   def var nro_inf as int no-undo.
   def var max_ite as int no-undo.
   def var nro_not as int no-undo.
   &IF DEFINED(GLPI643) &THEN
   def var nro_brd as int no-undo.  /*Numero de brindes*/ &ENDIF
   def var i       as int no-undo.
   def var tab_sta as cha no-undo.
   def var tab_inf as cha no-undo.

   def buffer b_not for {&not}.
   def buffer b_inf for {&inf}.
   def buffer b_sta for {&sta}.
   def buffer b_cli for {&cli}.
   assign max_ite = fv_maxite ( bi_ped.empctb, cod_cfo ).

   for first tipped no-lock where tipped.tipped = b_ped.tipped
                                  query-tuning ( cache-size 1 row ):
      assign max_ite = fv_maxite ( bi_ped.empctb, tipped.codcfo ).
   end.

   if lookup(tipped.tipdst,"M") = 0 then do:

      for each  {&ipd} of b_ped no-lock
          where (if {&ipd}.qtdate > 0 and 
                    lookup({&ipd}.indsitite,"L,T") > 0
                    {&tru}
                ):
         &IF DEFINED(GLPI643) &THEN
            assign nro_inf = nro_inf 
                           + (if {&ipd}.PerDscPrd <> 99 then 1 else 0)
                   nro_brd = nro_brd 
                           + (if {&ipd}.PerDscPrd = 99 then 1 else 0). 
         &ELSE 
             ASSIGN nro_inf = nro_inf + 1.
         &ENDIF

         /*********************************************************************
         run cta_nrserie( b_ped.asdped, 
                          {&ipd}.codgrp, 
                          {&ipd}.codprd,
                          input-output nro_inf
                        ).
         if return-value <> "ok" and 
            f_msg ("Falha ao verificar numero de serie: " +
                    string({&ipd}.codgrp,"99") + "."      +
                    string({&ipd}.codprd,"999")           +
                   " pedido " + string(b_ped.asdped,"9999,9,99999") 
                  ) then return "undo".
         *********************************************************************/

      end.
      if nro_inf &IF DEFINED(GLPI643) &THEN + nro_brd &ENDIF <= 0 then 
         return "undo". 

      &IF DEFINED(GLPI19319) = 0 &THEN
      assign bi_ped.nronot = trunc(nro_inf / max_ite,0)
                           + 1 when nro_inf > 0
                           - (if nro_inf mod max_ite = 0 and nro_inf > 0
                                 then 1 else 0).
      &ELSE
      assign bi_ped.nronot = 1
                           + trunc(nro_inf / max_ite,0)
                           - (if nro_inf mod max_ite = 0 then 1 else 0).
      &ENDIF
      for first {&pgt} of b_ped no-lock
          where (if LOOKUP({&pgt}.forpgt,"R") > 0 THEN TRUE ELSE FALSE)
          WHILE bi_ped.nronot > 1:
          RETURN "undo".
      end.

      &IF DEFINED(GLPI19319) &THEN
      assign bi_ped.nronot = bi_ped.nronot
                           + 1
                           + trunc(nro_brd / max_ite,0)
                           - (if nro_brd mod max_ite = 0 then 1 else 0).
      &ELSEIF DEFINED(GLPI643) &THEN
      assign bi_ped.nronot = bi_ped.nronot
                           + trunc(nro_brd / max_ite,0)
                           + 1 when nro_brd > 0
                           - (if nro_brd mod max_ite = 0 and nro_brd > 0
                                 then 1 else 0).
      &ENDIF
      return "ok".
   end.

   for first b_cli of b_ped no-lock query-tuning ( cache-size 1 row ): 
   end.
   
   &IF DEFINED(GLPI27820) &THEN
       run nfs/r/nfsrclcn.p ( buffer b_ped, 
                              max_ite, 
                              input-output bi_ped.nronot,
                              output TABLE w_ufprd
                            ).
      if not return-value begins "ok" then return "undo".
      return "ok".
   &ELSE   
   for each  {&ipd} 
       where {&ipd}.codcar = b_ped.codcar
       and   {&ipd}.numped = b_ped.numped
       and   (if lookup({&ipd}.indsitite,"L,T") > 0 and {&ipd}.qtdate > 0
                 {&tru}
             )
      ,first {&prd} no-lock
       where {&prd}.idprod = {&ipd}.idprod:
 
      for first b_not no-lock
          where b_not.recnotfis    = {&prd}.recnotfis
         ,first b_inf of b_not no-lock 
          where b_inf.numitenotfis = {&prd}.numitenotfis
         ,first b_sta of b_not no-lock
          where b_sta.numinfcep <> b_sta.numsupcep
                query-tuning ( cache-size 5 row ):
      end.

      if not avail b_sta then do:
         for first b_sta no-lock where b_sta.codest = b_cli.codest
                                       query-tuning ( cache-size 1 row ): 
         end.
      end.
      if lookup(string(b_sta.codest,"99"),tab_sta) = 0 then do:
         assign tab_sta = tab_sta + (if tab_sta = "" then "" else ",")
                        + string(b_sta.codest,"99")
                tab_inf = tab_inf + (if tab_inf = "" then "" else ",")
                        + "0". 
      end.
      assign i = lookup(string(b_sta.codest,"99"),tab_sta)
             entry(i,tab_inf) = string(int(entry(i,tab_inf)) + 1).
   end.
   
   if  num-entries(tab_sta) > 1 AND
       f_msg ("PEDIDO CONSUMO COM MAIS DE UM ESTADO") then return "undo".

   repeat i = 1 to num-entries(tab_sta):
      assign nro_inf = int(entry(i,tab_inf)).
      if nro_inf = 0 then return "undo".
      assign bi_ped.nronot = bi_ped.nronot + 1 + trunc(nro_inf / max_ite,0)
                           - (if nro_inf mod max_ite = 0 then 1 else 0).
   end.
   return "ok".
   &ENDIF
end procedure.                                                    /*cta_notas*/

procedure v_notxped:

   def buffer bi_ped for i_ped.
   def buffer b_ped  for {&ped}.
   def buffer b_sta  for {&sta}.
   def var max_ite as int no-undo.

   {&hm}.  message "Aguarde! Verificando numeracao da(s) nota(s)".

   V_NOTAS:
   repeat transaction on error  undo V_NOTAS, return "UNDO,01"
                      on endkey undo V_NOTAS, return "UNDO,02"
                      on stop   undo V_NOTAS, return "UNDO,03"
                      on quit   undo V_NOTAS, return "UNDO,04":
   
      for each  bi_ped no-lock
          where bi_ped.empctb = {&ectb}.empctb
         ,first b_ped no-lock
          where rowid(b_ped) = bi_ped.ridped
         ,first notxped no-lock
          where notxped.asdped = b_ped.asdped
                query-tuning ( cache-size 200 row ):
         assign bi_ped.prinot = notxped.numnotfis
                bi_ped.nronot = notxped.nronot
                bi_ped.empctb = notxped.codemp.
      end.

      if not can-find(first bi_ped no-lock
                      where bi_ped.empctb = {&ectb}.empctb
                      and   bi_ped.prinot = 0) then leave V_NOTAS.

      find first {&pnfe} of {&ectb} no-lock no-error.
      if not avail {&pnfe} then undo V_NOTAS, return "undo,05".
      assign rsp = v_aidf ({&pnfe}.empctb, today, {&pnfe}.nraidf ).
      if rsp <> "ok" and f_msg ( rsp ) then do:
         undo V_NOTAS, return "UNDO,06".
      end.

      for each  bi_ped no-lock
          where bi_ped.empctb = {&ectb}.empctb
          and   (if bi_ped.prinot = 0 {&tru})
         ,first b_ped no-lock 
          where rowid(b_ped) = bi_ped.ridped by bi_ped.numseq
                query-tuning ( cache-size 200 row ):
         run cta_notas ( buffer bi_ped, buffer b_ped ).
         if entry(1,return-value) <> "ok" 
         or bi_ped.nronot <= 0    then undo V_NOTAS, return "UNDO,07".
      end.

      l_pnfe:
      repeat:
         if sml_fat
            then find first {&pnfe} of {&ectb} no-lock no-wait no-error.
            else find first {&pnfe} of {&ectb} exclusive-lock no-wait no-error.
         if locked {&pnfe} then do:
            run trg/trgrgblo.p (string({&ectb}.empctb),
                               "{&pnfe},empctb,I,R,Parametro NF bloqueado").
            if return-value matches "*liberou" and var_waio < 10
               then next l_pnfe.
               
            assign rsp = "separador=;," + return-value 
                       + ";Tecle <A>bortar <T>entar;T;A;T".
            run colhe_rsp.
            if rsp = "A" then undo V_NOTAS, return "UNDO,08".
            undo V_NOTAS, retry V_NOTAS.
         end.
         assign var_waio = 1.
         leave l_pnfe.
      end.                                                           /*l_pnfe*/

      assign var_waio = 1.
      l_pnf:
      repeat while {&ectb}.empctb = 1:
         if sml_fat
            then find first {&pnf} of {&emp} no-lock no-wait no-error.
            else find first {&pnf} of {&emp} exclusive-lock no-wait no-error.
         if locked {&pnf} then do:
            run trg/trgrgblo.p (string({&emp}.codemp),
                               "{&pnf},codemp,I,R,Parametro NF bloqueado").
            if return-value matches "*liberou" and var_waio < 10
               then next l_pnf.
            assign rsp = "separador=;," + return-value 
                       + ";Tecle <T>entar <A>bortar;T;A".
            run colhe_rsp.
            case rsp:
               when "T" then undo V_NOTAS, retry V_NOTAS.
               otherwise undo V_NOTAS, return "UNDO,09".
            end.
         end.
         assign {&pnfe}.nroatusai      = {&pnf}.numultnotfissai.
         leave l_pnf.
      end.                                                         /*l_pnf*/

      /*NOTAS FISCAIS JA EMITIDAS COM ESTA NUMERACAO EM 31/03/2010 ***********/
      if {&pnfe}.empctb = 1 then repeat 
         while {&pnfe}.nroatusai + 1 >= 040415 and {&pnfe}.nroatusai < 040416
         or    {&pnfe}.nroatusai + 1 >= 765081 and {&pnfe}.nroatusai < 765110:
            assign {&pnfe}.nroatusai = {&pnfe}.nroatusai + 1.
      end.
      /***********************************************************************/
      
      assign var_waio = 1.
      for each  bi_ped no-lock 
          where bi_ped.empctb = {&ectb}.empctb 
          and   (if bi_ped.prinot = 0 {&tru})
         ,first b_ped no-lock 
          where rowid(b_ped) = bi_ped.ridped by bi_ped.numseq
                query-tuning ( cache-size 200 row ):

/*NOTAS FISCAIS JA EMITIDAS COM ESTA NUMERACAO EM 31/03/2010 *****************/
if {&pnfe}.empctb = 1 then repeat 
   while {&pnfe}.nroatusai + 1 >= 765081 and {&pnfe}.nroatusai < 765110:
        assign {&pnfe}.nroatusai = {&pnfe}.nroatusai + 1.
end.
/*****************************************************************************/

         /*cotta*/

         assign bi_ped.prinot     = {&pnfe}.nroatusai + 1
                {&pnfe}.nroatusai = {&pnfe}.nroatusai + bi_ped.nronot
                                  + (if b_ped.tipped = "PVA"
                                        then bi_ped.nronot else 0).
         if {&ectb}.empctb = 1 then do:
            assign {&pnf}.numultnotfissai = {&pnfe}.nroatusai.
         end.

         create notxped.
         assign notxped.asdped    = b_ped.asdped
                notxped.numnotfis = bi_ped.prinot
                notxped.nronot    = bi_ped.nronot
                notxped.codemp    = {&ectb}.empctb
                notxped.usridt    = Userid("Dictdb")
                notxped.nraidf    = {&pnfe}.nraidf.
      end.

      if {&pnfe}.nroatusai >= {&pnfe}.nromaxsai
      or {&pnfe}.nromaxsai - {&pnfe}.nroatusai < 1000 then do:
         run v_numero.
         if entry(1,return-value) = "UNDO" then undo V_NOTAS, return "UNDO,10".
      end.
      leave V_NOTAS.
   end.
   
   find first {&pnfe} of {&ectb} no-lock no-error.
   find first notxped no-lock where rowid(notxped) = ? no-error.
   return "ok".
end procedure.                                                  /*v_notxped*/

procedure F_PEDIDO:
   def var cfo_opb  as int no-undo.
   def var sta_opb  as int no-undo.
   def var idn_opb  as int no-undo.
   def var nro_box  as int no-undo init ?.
   def var per_cns  as dec no-undo init ?.
   def var pap_nor  as dec no-undo init 1.
   def var stc_ant  as cha.
   def var not_ped  as int.
   def var sta_dst  as int no-undo.
   def var per_com  as dec no-undo.
   def var per_fre  as dec no-undo.
   def var sta_ras  as int no-undo. /*UF destinataria quando for prd ANP*/
   def var lst_ite  as log no-undo init false.
   def var idt_wms  as cha.
   def var rid_trf  as row.
   
   def buffer b_not  for {&not}.
   def buffer b_inf  for {&inf}.
   def buffer b_sta  for {&sta}.
   def buffer bi_ped for i_ped.
   def buffer x_not for {&not}.
   def buffer x_dup for {&dup}.

   assign var_waio = 1 nro_rty = 0
          per_cns  = (if sml_fat then 1 else per_cns).

   empty temp-table w_ufprd no-error.

   T_F_PEDIDO:
   repeat transaction on error  undo T_F_PEDIDO, return "UNDO,T_F_PEDIDO"
                      on endkey undo T_F_PEDIDO, return "UNDO,T_F_PEDIDO"
                      on stop   undo T_F_PEDIDO, return "UNDO,T_F_PEDIDO"
                      on quit   undo T_F_PEDIDO, return "UNDO,T_F_PEDIDO"
                      while can-find(first i_ped no-lock):
      assign nro_rty = nro_rty + 1.
      /*if nro_rty mod 20 = 0 then undo T_F_PEDIDO, return "UNDO,T_F_PEDIDO".*/
      if can-find(first nf_fat no-lock) then do:
         run envia_nf_fat.                
         next T_F_PEDIDO.
      end.
      find first {&not} no-lock where rowid({&not}) = ? no-error.
      if not avail i_ped then find first i_ped use-index idxpri no-error.
      repeat while i_ped.ridpac <> ?:   
         find {&pac} where rowid({&pac}) = i_ped.ridpac
              exclusive-lock no-wait no-error.
         if locked {&pac} then do:
            assign rsp = "{&pac},rowid,W,R,Pacote "
                       + string(w_trn.codpac,">,>>>,>>9")
                       + " bloqueado".
            run trg/trgrgblo.p (string(i_ped.ridpac), rsp ).
            assign rsp = "separador=;," + return-value 
                       + ";Tecle <T>entar <A>bortar;T;A".
            run colhe_rsp.
            case rsp:
               when "T" then undo T_F_PEDIDO, retry T_F_PEDIDO.
               otherwise     UNDO T_F_PEDIDO, return "refaz".
            end.
         end.
         assign var_waio = 1.
         find first {&sep} exclusive-lock 
              where {&sep}.codpac = {&pac}.codpac no-wait no-error.
         if locked {&sep} then do:
            assign rsp = "{&sep},codpac,I,R,Separacao pacote "
                       + string({&pac}.codpac,">,>>>,>>9")
                       + " bloqueado".
            run trg/trgrgblo.p (string({&pac}.codpac), rsp ).
            assign rsp = "separador=;," + return-value 
                       + ";Tecle <T>entar <A>bortar;T;A".
            run colhe_rsp.
            case rsp:
               when "T" then undo T_F_PEDIDO, retry T_F_PEDIDO.
               otherwise     UNDO T_F_PEDIDO, return "refaz".
            end.
         end.
         assign var_waio = 1.
         leave.
      end.
      assign not_ped = 0
             idt_wms = "".
      empty temp-table w_htri no-error.       /*Para montar por pedido*****/
      empty temp-table ipd_a_f no-error.
      empty temp-table w_ufprd no-error.
      for each w_htri  no-lock: delete w_htri.  end.
      for each ipd_a_f no-lock: delete ipd_a_f. end.

      B_PEDIDO:
      repeat on error  undo T_F_PEDIDO, return "UNDO"
             on endkey undo T_F_PEDIDO, return "UNDO"
             on stop   undo T_F_PEDIDO, return "UNDO"
             on quit   undo T_F_PEDIDO, return "UNDO" 
             while can-find(first i_ped no-lock):
         if not avail i_ped then find first i_ped use-index idxpri no-error.
         find first {&car} exclusive-lock
              where rowid({&car}) = i_ped.ridcar no-wait no-error.
         if locked {&car} then do:
            assign rsp = "{&car},rowid,W,R,Carga "
                       + string(w_trn.codcar,">>,>>>,>>9")
                       + " bloqueada".
            run trg/trgrgblo.p (string({&pac}.codpac), rsp ).
            assign rsp = "separador=;," + return-value
                       + f_rsp( flg_aut, sml_fat ).
            run colhe_rsp.
            case rsp:
               when "T" then undo T_F_PEDIDO, retry T_F_PEDIDO.
               when "R" then UNDO T_F_PEDIDO, retry T_F_PEDIDO.
               otherwise     UNDO T_F_PEDIDO, return "refaz".
            end.
         end.
         assign var_waio = 1.
         find {&stc} of {&car} no-lock no-error.
         if (not {&stc}.flglibfat and not sml_fat
         or not {&car}.flgati) and not sml_fat then do:
            assign rsp = "Carga <"   + string({&car}.codcar) + "> Situacao: " 
                       + {&car}.codsitcar + " " 
                       + trim(string({&stc}.flglibfat,"Liberada/Nao lib")) 
                       + " "         
                       + trim(string({&car}.flgati,"Ativa/Inativa"))
                       + ". Nao pode ser faturada,Tecle <A>bortar,A".
            run trg/trgclrsp.p (input-output rsp, false).
            undo T_F_PEDIDO, return "refaz".
         end.
         find first {&ped} exclusive-lock
              where rowid({&ped}) = i_ped.ridped no-wait no-error.
         if locked {&ped} then do:
            assign rsp = "{&ped},rowid,W,R,Pedido " 
                       + string(i_ped.asdped,">9999,99999")
                       + " bloqueado".
            run trg/trgrgblo.p ( string(i_ped.ridped),  rsp ).
            assign rsp = "separador=;," + return-value 
                       + f_rsp( flg_aut, sml_fat ).
            run colhe_rsp.
            case rsp:
               when "T" then undo T_F_PEDIDO, retry T_F_PEDIDO.
               when "R" then UNDO T_F_PEDIDO, retry T_F_PEDIDO.
               otherwise     UNDO T_F_PEDIDO, return "refaz".
            end.
         end.

         &IF DEFINED(GLPI21345) &THEN
         for each  {&ipd} 
             where {&ipd}.codcar = {&ped}.codcar
             and   {&ipd}.numped = {&ped}.numped
             and   (if {&ipd}.qtdate > 0 and not sml_fat and
                       lookup({&ipd}.indsitite,"L,T,F") = 0
                       {&tru}
                    ):
            if f_msg ( "PEDIDO " + string({&ped}.asdped,">9999,99999") 
                                 + " PRODUTO " + string({&ipd}.idprod)
                                 + " COM SITUACAO <" 
                                 + trim({&ipd}.indsitite) + "> INVALIDA"
                     ) then do:
            end.
            undo T_F_PEDIDO, return "refaz".
         end.
         &ENDIF

         find first sapreqtrf exclusive-lock
              where sapreqtrf.asdped = {&ped}.asdped no-wait no-error.
         if locked sapreqtrf then do:
            assign rsp = "sapreqtrf,asdped,I,R,Requisicao Pedido " 
                       + string(i_ped.asdped,">9999,99999")
                       + " bloqueado".
            run trg/trgrgblo.p ( string({&ped}.asdped),  rsp ).
            assign rsp = "separador=;," + return-value
                       + f_rsp( flg_aut, sml_fat ).
            run colhe_rsp.
            case rsp:
               when "T" then undo T_F_PEDIDO, retry T_F_PEDIDO.
               when "R" then UNDO T_F_PEDIDO, retry T_F_PEDIDO.
               otherwise     UNDO T_F_PEDIDO, return "refaz".
            end.
         end.
         if avail sapreqtrf then assign rid_trf = rowid(sapreqtrf).

         assign var_waio = 1.
         if not sml_fat and not {&ped}.flgpedati then do:
            if f_msg ( "PEDIDO " + string({&ped}.asdped,">9999,99999") 
                                 + " INATIVO"
                     ) then do:
            end.
            undo T_F_PEDIDO, return "refaz".
         end.
         
         assign stc_ant = {&car}.codsitcar
                sta_ras = 0.
         for first {&cli} of {&ped} no-lock
            ,first {&bas} of {&cli} no-lock
            ,first {&raz} of {&bas} no-lock query-tuning ( cache-size 5 row ):
         end.
         if lookup({&ped}.tipped,"RAS") > 0 then do:
            assign sta_ras = {&cli}.codest.   
            for first {&cli} no-lock
                where {&cli}.codcli = 1
               ,first {&bas} of {&cli} no-lock
               ,first {&raz} of {&bas} no-lock:
            end.
         end.
         if not avail {&cli} 
         or not avail {&bas}
         or not avail {&bas} then do:
            if f_msg ( "FALHA CADASTRO DO CLIENTE " + string({&ped}.codcli) )
               then do:
            end.
            undo T_F_PEDIDO, return "refaz".
         end.

         /*Variaveis inicializadas por pedido********************************/
         if f_consumo({&ped}.empctb,{&cli}.codcli,output sit_dvg) then do: end.
         if entry(1,sit_dvg,";") <> entry(2,sit_dvg,";") 
         or entry(1,sit_dvg,";") <> "Rev" and 
            lookup(entry(07,sit_dvg,";"),"UF_BLK") > 0 then do:
            if f_msg( "CLIENTE..: " + string({&cli}.codcli,"z,zzz,zzz    ") +
                "\nPEDIDO...: " + STRING({&ped}.asdped,">9999,99999")       +
                "\nInscricao: " + entry(11,sit_dvg,";")                     +
                "\nDivergencia no cadastro" 
               ) then do:
            end.
            return "undo".
         end.
         /*GLPI17955*/
         for first f_ectb no-lock where f_ectb.empctb = i_ped.empctb
                                        query-tuning (cache-size 1 row): 
         end.
         assign consumo = (entry(1,sit_dvg,";") = "Cns")
                &IF DEFINED(GLPI17955) &THEN
                consumo = (if {&ped}.dstmer = "U" and i_ped.stadst = 31 and
                              {&ectb}.codest = i_ped.stadst
                              then true
                              else consumo
                          ) &ENDIF
                pes_cnt = string({&raz}.tippes,"J/F") 
                        + string(consumo,"N/C")
                sta_opb = 0
                cfo_opb = 0
                idn_opb = 0.

         assign not_ped = not_ped + 1
                nro_not = nro_not + 1
                i       = int(round((time - hor_ini) / nro_not,0)).

         assign msg_fat:screen-value in frame f_1 =
                trim(string({&ped}.numseqped,"Seq >>9"))        + " "   +
                trim(string({&ped}.asdped,"Pedido z999999999")) + " "   +
                trim(string(not_ped,">>>9"))                    +
                (if avail i_ped and i_ped.nronot > 1
                    then "/" + string(i_ped.nronot)
                    else ""
                )                                               + " "  +
                trim(string({&car}.codemp,"Repr >>9"))          + "."   +
                trim(string({&car}.coddiv,">>9"))               + "-"   +
                trim(string({&car}.codrep,">>>>>9"))            + " "   +
                trim(string(nro_not,"Notas >>>,>>9"))           + " "   +
                substr(string(i,"HH:MM:SS"),4,5)                + " "   +
                string(round(etime / nro_not,0)).
         if not flg_aut and not sml_fat then
            color display messages msg_fat with frame f_1.

         l_orgaopb:
         for first orgaopb no-lock
             where orgaopb.codcli = {&ped}.codcli
             and   (if lookup(orgaopb.tipopb,"MG,M,E") > 0 {&tru})
             and   (if lookup({&ped}.tipped,"RAS,PD") = 0 {&tru}): 
                                            /*GLPI10884*/
            /**
            if userid("Dictdb") <> "salgado" and
               f_msg ( "PEDIDO " + string({&ped}.asdped,"9999,9,99999") + 
                       "\nORGAO PUBLICO DE MINAS GERAIS" +
                       "\nCOMUNICAR COM O ROBERTO" ) then do:
               undo T_F_PEDIDO, return "refaz".
            end.
            **/
            assign cfo_opb = ?.
            for each  empenho no-lock
                where empenho.codcli = orgaopb.codcli
                and   empenho.asdped = {&ped}.asdped 
                and   can-find(last  {&cfo} no-lock
                               where {&cfo}.codcfo = empenho.codcfo):
               assign cfo_opb = empenho.codcfo.
                      idn_opb = (if empenho.nomloc = ""
                                    then 5
                                    else 6
                                ).
               leave.
            end.
            for each  empenho no-lock
                where empenho.codcli   = orgaopb.codcli
                and   empenho.asdped   = {&ped}.pedori
                and   can-find(last  {&cfo} no-lock
                               where {&cfo}.codcfo = empenho.codcfo)
                        while cfo_opb = ? and {&ped}.pedori > 0:
               assign cfo_opb = empenho.codcfo.
                      idn_opb = (if empenho.nomloc = ""
                                    then 5
                                    else 6
                                ).
               for first tipped no-lock
                   where tipped.tipped = {&ped}.tipped
                   and   (if tipped.tipopesrf = "014" 
                             then true else false
                         ):
                  assign cfo_opb = tipped.codcfo.
               end.
               leave.
            end.
         end.

         if cfo_opb = ? then do:
            if f_msg ( "EMPENHO NAO CADASTRADO PARA O PEDIDO " +
                        string(i_ped.asdped,">9999,99999") ) then do:
            end.
            undo T_F_PEDIDO, return "refaz".
         end.

         for first tipped of {&ped} no-lock query-tuning (cache-size 1 row): 
            assign cod_cfo = tipped.codcfo.
         end.

         for first f_tpd of {&ped} no-lock query-tuning (cache-size 1 row): 
         end.

         &IF DEFINED(GLPI27820) &THEN
            if lookup(f_tpd.tipdst,"M") > 0 and 
               not can-find(first w_ufprd no-lock) then do:
               assign max_ite = fv_maxite ( {&ectb}.empctb, cod_cfo ).
               run nfs/r/nfsrclcn.p ( buffer {&ped}, 
                                      max_ite, 
                                      input-output max_ite,
                                      output TABLE w_ufprd
                                    ).
               if not return-value begins "ok" then return "undo".
            end.
         &ENDIF

         find last  {&cfo} no-lock where {&cfo}.codcfo = cod_cfo.
         for first {&bai} of {&cli} no-lock
            ,first {&mun} of {&bai} no-lock
            ,first {&sta} of {&mun} no-lock:
         end.
         for first {&pgt} of {&ped} no-lock
            ,first {&cct} of {&pgt} no-lock
            ,first {&cvd} of {&ped} no-lock:
         end.

         find first {&pnfe} of f_ectb no-lock.
         find first {&str} of {&cfo} no-lock.
         find first r_not no-lock where rowid(r_not) = ? no-error.
         for  first {&psr} exclusive-lock
              where {&psr}.asdped = {&ped}.asdped
             ,last  {&cfo} no-lock
              where {&cfo}.codcfo = {&psr}.codcfo
                    query-tuning (cache-size 2 row):
            for first r_not no-lock where r_not.recnotfis = {&psr}.recnotfis
                                          query-tuning (cache-size 1 row):
            end.
         end.

         if ( {&cfo}.indtiptrs <> "T"   or 
              lookup({&cfo}.indtippre,"M,T") = 0 ) and
            can-find(first {&ectb} no-lock
                     where {&ectb}.codcli = {&cli}.codcli) 
         or ( {&cfo}.indtiptrs = "T" or 
              lookup({&cfo}.indtippre,"M,T") > 0 )     and
            ( lookup(f_tpd.grpprd,"DOA") = 0 and
             not can-find(first {&ectb} no-lock
                          where {&ectb}.codcli = {&cli}.codcli)
            ) then do:
            if f_msg ( "CODIGO FISCAL " + string({&cfo}.codcfo,"9,999,99")
                                        + " INVALIDO PARA O CLIENTE\n" + 
                       "PEDIDO "        + string({&ped}.asdped,">9999,99999")
                                        + " Tipo " + {&ped}.tipped
                     ) then do:
            end.
            undo T_F_PEDIDO, return "refaz".
         end.

         for each w_not no-lock: delete w_not. end.
         for each w_dup no-lock: delete w_dup. end.
         for each w_ccr no-lock: delete w_ccr. end.
         for each w_met no-lock: delete w_met. end.
         for each w_inf no-lock: delete w_inf. end.
         for each w_gnr no-lock: delete w_gnr. end.
         assign {&ped}.perdscped    = (if {&cfo}.indtippre = "M"
                                       or {&cfo}.indtiptrs = "T"
                                       or f_tpd.pedmkt
                                          then 0
                                          else {&ped}.perdscped
                                      ).
         create w_not.
         assign w_not.seqent        = i_ped.numseq
                w_not.flgclifor     = "C"
                w_not.conrev        = ({&cli}.flgconrev or {&ped}.flgconrev)
                w_not.conrev        = consumo
                w_not.conicms       = string(w_not.conrev,"NC/CI")
                w_not.idtcnsfin     = string(consumo,"S/N")
                w_not.dstmer        = {&ped}.dstmer
                w_not.cfoopb        = cfo_opb
                w_not.uficms        = 0
                w_not.flgnotfis     = "S"
                w_not.tipespdoc     = "NF"
                w_not.numnotfis     = 0
                w_not.PerAcrCusUnt  = 0
                w_not.QtdEmb        = 0
                w_not.recnotfis     = 0
                w_not.numordcom     = 0
                w_not.DesObs        = ""
                w_not.DatTrn        = ?
                w_not.DatEmi        = to_day
                w_not.DatMov        = to_day
                w_not.hormov        = time
                w_not.NomRes        = userid("Dictdb")
                w_not.codsernotfis  = {&pnfe}.codsersai
                w_not.codCliFor     = {&cli}.codCli
                w_not.codCar        = {&ped}.codCar
                w_not.NumPed        = {&ped}.NumPed
                w_not.codcfo        = {&cfo}.codcfo
                w_not.codSitReg     = {&cfo}.codSitReg
                w_not.empctb        = f_ectb.empctb
                w_not.codemp        = {&emp}.codemp
                w_not.coddiv        = {&car}.coddiv
                w_not.codrep        = if {&ped}.codrep > 0 
                                         then {&ped}.codrep
                                         else {&car}.codrep
                w_not.codTrn        = w_trn.codTrn
                w_not.numpla        = w_trn.plavei
                w_not.codEmb        = w_trn.codEmb
                w_not.codViaTrn     = w_trn.codVia
                w_not.DatSai        = w_trn.datsai
                w_not.codFre        = if avail {&tfr}
                                         then {&tfr}.codFre else ""
                w_not.idr_not       = if avail r_not
                                         then rowid(r_not) else ?
                w_not.valdspace     = vl_dspace( buffer {&ped}, 
                                                 buffer {&cfo}, 
                                                 buffer f_tpd,
                                                 buffer {&cli}, cfo_opb)
                                    - i_ped.valdspace
                w_not.valdspace     = (if w_not.valdspace < 0 /*GLPI18320*/
                                          then 0
                                          else w_not.valdspace
                                      )
                w_not.qtdemb        = w_trn.qtdemb
                i_ped.valdspace     = i_ped.valdspace 
                                    + w_not.valdspace
                i                   = 0
                cod_est             = {&sta}.codest.

         if f_tpd.pedb2c then do:
            assign w_not.valtotfre = {&ped}.valtotinfped - {&ped}.valtotpdzped
                   w_not.valtotfre = (if w_not.valtotfre < 0
                                         then 0
                                         else w_not.valtotfre
                                     ).
         end.
         &IF DEFINED(GLPI14387) &THEN
         if w_not.cfoopb <> 0 and w_not.cfoopb <> ? and /*GLPI14387*/
            avail orgaopb and lookup(orgaopb.tipopb,"MG") = 0 then do:
            if (orgaopb.tipopb <> "M" and orgaopb.tipopb <> "E") or
               (orgaopb.tipopb = "M" and lookup({&ped}.tipped,"VOM,ROM") = 0) or
               (orgaopb.tipopb = "E" and lookup({&ped}.tipped,"VOP,ROP") = 0)
            then
               assign w_not.codcfo = w_not.cfoopb
                      w_not.cfoopb = 0
                      cfo_opb      = 0.
         end.
         &ENDIF

         assign max_ite = (if w_not.cfoopb > 0
                              then fv_maxite ( w_not.empctb, w_not.cfoopb )
                              else fv_maxite ( w_not.empctb, w_not.codcfo )
                          )
                        - idn_opb
                        - (if {&cli}.flgendpri
                           or {&cli}.codinsest <> "" then 0 else 1).
                           
         if max_ite <= 1 then do:
            if f_msg ( "FALHA AO DEFINIR O NUMERO DE ITENS POR NOTA" ) then do:
            end.
            undo T_F_PEDIDO, return "refaz".
         end.
         for first w_ctr no-lock
             where w_ctr.codaux = w_not.empctb query-tuning (cache-size 1 row):
            assign w_not.ctrapo = w_ctr.ctrapo
                   max_ite      = max_ite - 2.
         end.
         assign max_ite = (if i_ped.idtfat = "T" then 999999 else max_ite)
                ins_est = (if {&cli}.codinsest <> ""
                              then {&cli}.codinsest
                              else {&bas}.codinsest
                          ).
         
         if {&raz}.tippes and not {&cli}.flgendpri and 
            {&cli}.codinsest = "" and
            ( f_lentrega( {&bas}.codatv ) or w_not.cfoopb > 0) then do:
            for first b_cli of {&bas} no-lock where b_cli.flgendpri
                                              query-tuning (cache-size 1 row):
               assign cod_est = b_cli.codest
                      ins_est = {&bas}.codinsest.
            end.
         end.
         for first b_sta no-lock where b_sta.codest = cod_est: end.

         case string({&raz}.tippes,"J/F"):
         when "J" then run rt/rtinsest.p (true,  b_sta.sigest, ins_est).
         when "F" then run rt/rtinsest.p (false, b_sta.sigest, ins_est).
         end case.
         
         if return-value matches "erro*" and
            f_msg ( "PEDIDO "    + string({&ped}.asdped,">9999,99999") +
                    "\nCliente " + string({&cli}.codcli)               +
                    "\nESTADO "  + b_sta.nomest                        +
                    "\nINSCRICAO ESTADUAL INVALIDA"
                     ) then do:
            undo T_F_PEDIDO, return "refaz".
         end.
         
         assign w_not.stadst = (if sta_ras > 0 then sta_ras else cod_est)
                w_not.staemi = f_ectb.codest
                sta_opb      = (if w_not.cfoopb = 0 then 0 else cod_est).

         for first bf_sta where bf_sta.codest = cod_est no-lock
                                query-tuning (cache-size 1 row): 
         end.
         if lookup(f_ectb.tipemp,"M,E") = 0 and 
            cod_est <> f_ectb.codest then do:

            run rt/rtedmsg.p ( 20, "ATENCAO", "Tecle <V>oltar@v,V,end-error",
                                   "PEDIDO.: "                              +
                                        string({&ped}.asdped,">9999,99999") +
                                   "\nESTADO.: " + bf_sta.nomest            +
                                   "\nEMPRESA: " + f_ectb.nomfts
                             ) no-error.            
            undo T_F_PEDIDO, return "refaz".

         end.
         assign w_not.codinsest = (if {&cli}.codinsest = ""
                                      then caps({&bas}.CodInsEst)
                                      else caps({&cli}.CodInsEst)
                                  )
                w_not.cpfcnpj   = (if {&raz}.TipPes 
                                      then string({&raz}.CodCGCCliFor,"x(8)")
                                         + string({&bas}.CGCCli,"x(6)") 
                                      else string({&raz}.CodCGCCliFor,"x(11)")
                                  )
                w_not.codinsest = replace(w_not.codinsest,".","")
                w_not.codinsest = replace(w_not.codinsest,"/","")
                w_not.codinsest = replace(w_not.codinsest,"-","")
                w_not.cpfcnpj   = replace(w_not.cpfcnpj,".","")
                w_not.cpfcnpj   = replace(w_not.cpfcnpj,"/","")
                w_not.cpfcnpj   = replace(w_not.cpfcnpj,"-","").

         repeat on error  undo T_F_PEDIDO, return "UNDO"
                on endkey undo T_F_PEDIDO, return "UNDO"
                on stop   undo T_F_PEDIDO, return "UNDO"
                on quit   undo T_F_PEDIDO, return "UNDO"
                while not can-find(first ipd_a_f no-lock):

            for each  {&ipd} 
                where {&ipd}.codcar = {&ped}.codcar
                and   {&ipd}.numped = {&ped}.numped
                and   (if {&ipd}.qtdate > 0 {&tru})
                and   (if lookup({&ipd}.indsitite,"L,T") > 0
                       or sml_fat
                          {&tru}
                      ):
               create ipd_a_f.
               assign ipd_a_f.ridipd = rowid({&ipd})
                      ipd_a_f.idprod = {&ipd}.idprod
                      &IF DEFINED(GLPI643) &THEN
                      ipd_a_f.prdbrd = (if i_ped.idtfat = "T"
                                           then "N"
                                       else string({&ipd}.perdscprd < 99,"N/S")
                                       )
                      &ENDIF
                      .
               &IF DEFINED(GLPI27820) &THEN
                  if lookup(f_tpd.grpprd,"CNS") > 0 then
                  for each w_ufprd no-lock
                      where (if lookup(string({&ipd}.idprod,"999999")
                                       ,w_ufprd.stripd," ") > 0
                                then true else false
                            ):
                     assign ipd_a_f.uffatu = w_ufprd.codest.
                  end.
               &ENDIF

               /***************************************************************
               /*F_pedido*/
               run cta_nrserie( {&ped}.asdped, 
                                {&ipd}.codgrp, 
                                {&ipd}.codprd,
                                input-output ipd_a_f.nrolin
                              ).
               ****************************************************************/
               
               if lookup({&cfo}.tipmer,"M") > 0 /*coMercializacao*/         and
                  f_tpd.tipdst    = "C"        /*Cliente*********/         and
                  f_tpd.tippre    = "L"        /*Livro***********/         and
                  lookup({&ped}.tipped,
                                   "PE,PTE,PEH,PST,PSF,PFM,PFH,PD,PDD") = 0 and
                  lookup({&ped}.tipped,"PRT,PRC,PRM") = 0                   and
                  {&ipd}.perdscprd < 99                                     and
                  (per_cns = ? and w_not.conrev and cod_est <> f_ectb.codest 
                   /*ESTUDAR COMO TRATAR LOCAL DE ENTREGA FORA DA UF ORIGEM*/
                  )
                  then do:
                  assign pap_nor = (if {&ped}.codcndpgt = "LS" 
                                       then 1.1 
                                       else 1
                                   ).
                  run v_prcvenda (input-output per_cns, sta_opb, pap_nor ).
                  case entry(1,return-value):
                    when "refaz" then undo T_F_PEDIDO, return "refaz".
                    when "ok"    then do: end.
                    otherwise    undo T_F_PEDIDO, return "undo,v_prcvenda".
                  end.

               end.

            end.
            leave.

         end.

         assign nro_ite = 0.
         for each ipd_a_f no-lock: 
            assign nro_ite = nro_ite + 1 + ipd_a_f.nrolin. 
         end.
         if not can-find(first ipd_a_f no-lock) then do:
            if f_msg ( "PEDIDO: "          +
                       string({&ped}.asdped,">9999,99999")    +
                       "SEM ITENS A FATURAR"
                     ) then do:
            end.
            undo T_F_PEDIDO, return "refaz".
         end.
         assign per_cns = (if per_cns = ? then 1 else per_cns)
                i       = 0.

         L_IPD: /*CLASSIFICA ITENS NA NOTA*/
         for each  ipd_a_f no-lock
            ,first {&ipd}
             where rowid({&ipd}) = ipd_a_f.ridipd
            ,first {&prd} no-lock
             where {&prd}.idprod = {&ipd}.idprod
            ,first w_alm no-lock
             where w_alm.codalm = {&prd}.codalm
                   break by ipd_a_f.uffatu
                         &IF DEFINED(GLPI643) &THEN
                         by ipd_a_f.prdbrd &ENDIF
                         by (if w_not.codcfo = 592704
                                then ipd_a_f.idprod
                                else 0
                            )
                         by {&prd}.flgperig desc        /*nova classificacao*/
                         by w_alm.seqalm by ipd_a_f.idprod:

            /******************************************************************
             Tipo operacao receita federal "tipopesrf"
             Material consumo      - "010"
             Mercadoria danificada - "011" 
             Pedidos Roubo carga   - "014"
            ******************************************************************/

            if lookup(f_tpd.tipdst,"M") > 0
            or lookup({&cfo}.tipnot,"CR,TC,MD") > 0
            or lookup(f_tpd.tipopesrf,"010,011")  > 0
            or lookup({&ped}.tipped,"PRH,PMH,PCH,C29,C31,C32,C36,C38") > 0
            or lookup(f_tpd.grpprd,"CNS,DFT,CRB") > 0 then do:
               assign cod_est = (if ipd_a_f.uffatu <> 0
                                    then ipd_a_f.uffatu
                                    else {&cli}.codest
                                ).

               for first b_not no-lock
                   where b_not.recnotfis    = {&prd}.recnotfis
                  ,first b_inf of b_not no-lock 
                   where b_inf.numitenotfis = {&prd}.numitenotfis
                         query-tuning (cache-size 2 row): 
                 assign cod_est      = b_not.codest.
               end.

               assign w_not.uficms = (if w_not.uficms = 0
                                         then cod_est 
                                         else w_not.uficms
                                      ).
               if w_not.uficms <> 0 and w_not.uficms <> cod_est and
                  (cod_est = {&cli}.codest or w_not.uficms = {&cli}.codest) 
                  then next L_IPD.

               for first {&sta} no-lock where {&sta}.codest = cod_est
                                              query-tuning (cache-size 1 row):
               end.
               for first bf_sta no-lock where bf_sta.codest = w_not.uficms
                                              query-tuning (cache-size 1 row): 
               end.
               if bf_sta.numinfcep <> bf_sta.numsupcep   and
                  {&sta}.numinfcep <> {&sta}.numsupcep  
               or bf_sta.numinfcep = bf_sta.numsupcep   and
                  {&sta}.numinfcep = {&sta}.numsupcep    
                  then assign cod_est = w_not.uficms.
               for first {&sta} of {&cli} no-lock
                   query-tuning (cache-size 1 row): 
               end.

               if cod_est <> w_not.uficms then next L_IPD.
               assign w_not.staemi = w_not.uficms.

            end.

            if can-find(first w_inf no-lock
                        where w_inf.codgrp = {&ipd}.codgrp
                        and   w_inf.codprd = {&ipd}.codprd) then do:
               //aqui-gilson 
               FOR FIRST itemnflote NO-LOCK 
                   WHERE itemnflote.codcar    = {&ped}.codcar
                   AND   itemnflote.numped    = {&ped}.numped
                   AND   itemnflote.numiteped = {&ipd}.numiteped
                   AND   itemnflote.idprod    = {&ipd}.idprod:
               END.
 
               IF NOT AVAIL itemnflote THEN DO:
                  IF f_msg ( "DUPLICOU PRODUTO: "          +
                             string({&ipd}.codgrp,"99")    +
                             string({&ipd}.codprd,"999")
                           ) then do:
                  end.
                  undo T_F_PEDIDO, return "refaz".
               END.   
            end.

            assign lst_ite = last-of(ipd_a_f.uffatu).

            &IF DEFINED(GLPI643) &THEN
                assign lst_ite = ( last-of(ipd_a_f.uffatu) or 
                                   last-of(ipd_a_f.prdbrd)
                                 )
                       w_not.codcfo    = (if ipd_a_f.prdbrd = "S"
                                             then 591025 else w_not.codcfo
                                         )
                       w_not.valdspace = (if ipd_a_f.prdbrd = "S"
                        /*GLPI18320*/        then 0 else w_not.valdspace
                                         ).
            &ENDIF
            
            delete ipd_a_f.
            assign per_com = 0 per_fre = 0.
            /*Comprenet*/
            if {&ped}.codcli = 677468
            or {&ped}.codcli = 750667
            or {&ped}.codcli = 116181 then do:
              for first comissao no-lock
                  where comissao.codcom = {&ipd}.codcom:
                case {&ipd}.codgrpcom:
                     when 1 then per_com = comissao.PerComPriGrp.
                     when 2 then per_com = comissao.PerComSegGrp. 
                     when 3 then per_com = comissao.PerComTerGrp. 
                     when 4 then per_com = comissao.PerComQuaGrp. 
                end.
              end.
            end.
            else 
            /*VENDA A ORDEM REND BRASIL LTDA */
            if {&ped}.codcli = 619037 then do:
              for first pedvdaord no-lock
                  where pedvdaord.asdped = {&ped}.asdped,
                  first bf_cli of pedvdaord no-lock,
                  first bf_uf of bf_cli no-lock:
                case bf_uf.sigest:
                     when "MG" then assign per_fre = -0.80.
                     when "SP" then assign per_fre = -0.68.
                     when "RJ" then assign per_fre = -1.80.
                     when "PR" then assign per_fre = 0.70.
                     when "DF" then assign per_fre = 0.20.
                     when "GO" then assign per_fre = 0.20.
                     when "MS" then assign per_fre = 0.
                     when "ES" then assign per_fre = -1.30.
                     when "BA" then assign per_fre = 0.
                     when "AL" then assign per_fre = 1.
                     when "SE" then assign per_fre = 1.
                     when "TO" then assign per_fre = 2.70.
                     when "MT" then assign per_fre = 2.70.
                     when "RO" then assign per_fre = 4.50.
                     when "PE" then assign per_fre = 3.80.
                     when "PB" then assign per_fre = 4.70.
                     when "RN" then assign per_fre = 3.80.
                     when "PI" then assign per_fre = 3.80.
                     when "CE" then assign per_fre = 3.80.
                     when "MA" then assign per_fre = 3.30.
                     when "PA" then assign per_fre = 3.20.
                     when "AM" then assign per_fre = 6.35.
                     when "RR" then assign per_fre = 5.20.
                     when "AC" then assign per_fre = 5.60.
                     when "AP" then assign per_fre = 3.40.
                     when "RS" then assign per_fre = 0.70.
                     when "SC" then assign per_fre = 0.70.
                end.     
              end.
            end.
            create w_inf.
            assign i                  = i + 1
                   nro_ite            = nro_ite - 1
                   w_inf.idaux        = rowid({&ipd})
                   w_inf.NumIteNotFis = i
                   w_inf.idprod       = {&prd}.idprod
                   w_inf.codGrp       = {&ipd}.codGrp
                   w_inf.codPrd       = {&ipd}.codPrd
                   w_inf.codcfo       = w_not.codcfo

                   w_inf.valratfre    = 0
                   w_inf.valratseg    = 0
                   w_inf.valratdsc    = 0
                   w_inf.valratdsp    = 0
                   w_inf.cuscomicm    = 0
                   w_inf.cussemicm    = 0

                   w_inf.valpis       = 0
                   w_inf.valcof       = 0
                   w_inf.permva       = 0

                   w_inf.codTri       = ""
                   w_inf.QtdAte       = {&ipd}.QtdAte
                   w_inf.preprd       = {&ipd}.PrePrd 
                                      * (if sml_fat
                                         or f_tpd.pedmkt and 
                                            lookup({&ped}.sitped,"F") = 0
                                            then 1
                                            else (1 + {&ped}.peracr / 100)
                                                 * (1 - per_com / 100)
                                                 * (1 + per_fre / 100)
                                        )
                   w_inf.preprd       = (if {&ipd}.PerDscPrd = 99
                                            then {&ipd}.PrePrd 
                                            else w_inf.preprd
                                        )
                   w_inf.PerDscPrd    = {&ipd}.PerDscPrd
                   w_inf.perdscprd    = 0 /*Ja considerado no pedido*/
                   w_inf.codcom       = {&ipd}.codcom
                   w_inf.grpcom       = {&ipd}.codgrpcom
                   w_inf.valipi       = 0
                   w_inf.aliipi       = 0
                   w_inf.codtri       = (if w_not.uficms <> 0 and avail b_inf
                                            and false /*Convenio MG*/
                                            then b_inf.codtri
                                            else w_inf.codtri
                                        )
                   w_inf.temsub       = (if w_not.uficms <> 0 and avail b_inf
                                            and b_inf.valsubtri = 0
                                            then 0
                                            else w_inf.temsub
                                        )
                   w_inf.temsub       = (if w_not.uficms <> 0 and avail b_inf
                                            and b_inf.valsubtri > 0
                                            then 1
                                            else w_inf.temsub
                                        ).

            &IF DEFINED(GLPI20339) &THEN
            if {&ipd}.PerDscPrd = 99 then do:
               for last  {&hpd} no-lock
                   where {&hpd}.idprod = w_inf.idprod
                   and   {&hpd}.cfobri > 0:
                  assign w_inf.codcfo = 
                         int(entry(1,string(w_inf.codcfo,"9,99999"),".") +
                             entry(2,string({&hpd}.cfobri,"9,99999"),".")).
               end.
            end.
            &ENDIF

            /* Cliente consumidor final fora do Estado emitente***************/
            if per_cns <> 1 then do:
               assign w_inf.preprd = round(w_inf.preprd * per_cns,2).
               run ftp/crr/enviaemail_sem_anexo.p 
                    ( "nfsvpaco@tambasa.com.br", "",
                      "rsalgado@tambasa.com.br;gilson@tambasa.com.br",
                      "AMBIENTE " + os-getenv("_AMB") + "\n" +
                      "PEDIDO: " + string({&ped}.asdped) + " <" 
                                 + {&ped}.dstmer   + "><"
                                 + string({&ped}.papnor) + "><" 
                                 + string(per_cns) + ">",
                      "CLIENTE: " + string({&ped}.codcli) + "\n\n" + sit_dvg
                    ) no-error. 
            end.

            if w_not.flgnotfis = "S" and w_inf.preprd < 0.20 and
               f_ectb.codest <> w_not.stadst then do:
               /*Para correção do erro 694************************************
                694 REJEICAO: NAO INFORMADO O GRUPO DE ICMS PARA A UF Destino
               **************************************************************/
               assign w_inf.preprd = 0.20.
            end.

            if lst_ite
            or i >= max_ite
            or i + nro_ite > max_ite and nro_ite < i
                                     and nro_ite < max_ite then leave.
         end.                                                         /*L_IPD*/

         if sml_fat and {&ped}.sitped = "FF" then do: /*Verificar*/
            for each  {&not} of {&ped} no-lock
                where (if substr(string({&not}.codcfo,"999999"),2) = 
                          substr(string(w_not.codcfo,"999999"),2) {&tru})
               ,each  {&inf} of {&not} no-lock
                where (if {&inf}.valsubtri > 0 then true else false)
               ,first w_inf no-lock
                where w_inf.idprod = {&inf}.idprod:
               assign w_inf.preprd = {&inf}.preprd.
               
            end.
         end.
         
         for each g_ccr no-lock: delete g_ccr. end.
        //if f_log( "Chama nfsrnota.p", input-output v_msg) then do: end.
         pause 1 no-message.
         run nfs/r/nfsrnota.p ( buffer w_not, buffer w_inf, buffer w_htri,
                                output TABLE w_ideso
                              ) no-error.
         if error-status:error then do:
            if f_msg ( "Erro (1000) " + {&err_msg} ) then do: end.
            undo T_F_PEDIDO, return "UNDO,Falha nfsrnota.p".
         end.

         //if f_log( "Saiu  nfsrnota.p", input-output v_msg) then do: end.
         if w_not.valtotconnot = ? then do:
            undo T_F_PEDIDO, return "UNDO,VALTOTCON NULO".
         end.

         case entry(1,return-value):
            when "retry" then undo T_F_PEDIDO, retry T_F_PEDIDO.
            when "refaz" then undo T_F_PEDIDO, return "refaz".
            when "UNDO"  then undo T_F_PEDIDO, return trim(return-value).
            when "ok"    then do: end.
            otherwise    undo T_F_PEDIDO, return "undo,nfsrnota".
         end case.
         
         /*
         if f_tpd.pedb2c and w_not.valtotnotfis <> {&ped}.valtotinfped then do:
            run rt/rtedmsg.p (30,"   A T E N C A O   ","", 
                             "Pedido " + string({&ped}.asdped,"9999999999")
                           + "\nValor informado " 
                           + trim(string({&ped}.valtotinfped,">>>,>>>,>>9.99"))
                           + "\nValor calculado " 
                           + trim(string(w_not.valtotnotfis,">>>,>>>,>>9.99"))
                             ) no-error.
            undo T_F_PEDIDO, return "undo,Valor nf e-commerce".                 
         end.
         */

         if can-find(first w_inf no-lock where w_inf.valliqprd = 0)
            then undo T_F_PEDIDO, return "UNDO,INF ZERADO".
         for last  {&cfo} no-lock where {&cfo}.codcfo = w_not.codcfo: end.

         if {&car}.flgpedhor and f_tpd.idtwms <> "N" and not sml_fat then do:
            if idt_wms = "" then do:
               run m_lph_wms.   
               if lookup(return-value,"E,N") = 0 then
                  undo T_F_PEDIDO, return "refaz".
               assign rsp = "I" + chr(10)
                       + "Enviar pedido "
                       + string({&ped}.asdped,">9999,99999") 
                       + " cliente " + trim(string({&ped}.codcli,">,>>>,>>9"))
                       + " para separacao no armazem,Tecle "
                       + (if return-value = "E" 
                             then "<E>nviar <A>bortar,E,A"
                             else "<N>ao enviar <A>bortar,N,A"
                          ).
               run trg/trgclrsp.p (input-output rsp, false).
               if rsp = "A" then undo T_F_PEDIDO, return "refaz".
               if rsp = "E" and lookup("FLT",sit_prc) > 0 then do:
                  if f_msg ("FATURAMENTO PELA FILIAL NAO ENVIA WMS") then do:
                  end.
                  undo T_F_PEDIDO, return "refaz".
               end.
               assign idt_wms       = rsp.
            end.
            
            create w_pspr.
            assign w_pspr.asdped = {&ped}.asdped
                   w_pspr.tipwms = idt_wms
                   w_pspr.movrsv = (if w_not.codemp = w_not.codemp
                                       then w_pspr.movrsv
                                       else 0
                                   ).
            find current w_pspr no-lock.       
         end.
         assign {&ped}.empctb = w_not.empctb.
         if i_ped.idtfat = "F" then do:
            if not flg_aut and not sml_fat then
               disp w_trn.notinf a_tel w_trn.notsup with frame f_1.
            /*if f_log( "Chama G_W_CCR...", input-output v_msg) then do: end.*/
            if {&cfo}.IndAtuCom and {&car}.codrep <> 991 /*Comprenet*/ and
            
               {&car}.codrep <> 994
               then do:
               run G_W_CCR.
               case entry(1,return-value):
                  when "retry" then undo T_F_PEDIDO, retry T_F_PEDIDO.
                  when "refaz" then undo T_F_PEDIDO, return "refaz".
                  when "UNDO"  then undo T_F_PEDIDO, return trim(return-value).
               end case.
            end.
            if {&cfo}.IndAtuCtr    then do:
              /*if f_log("Chama G_W_DUP...",input-output v_msg) then do: end.*/
               run G_W_DUP.
               case entry(1,return-value):
                  when "retry" then undo T_F_PEDIDO, retry T_F_PEDIDO.
                  when "refaz" then undo T_F_PEDIDO, return "refaz".
                  when "UNDO"  then undo T_F_PEDIDO, return trim(return-value).
               end case.
            end.
            if {&cfo}.FlgAtuMedVen then run G_W_MET.
            
            /*if f_log( "Chama A_NOT.....", input-output v_msg) then do: end.*/
            run A_NOT.
            
            /*if f_log( "Saiu  A_not.....", input-output v_msg) then do: end.*/
            /*if f_log( return-value, input-output v_msg) then do: end.*/

            case entry(1,return-value):
               when "retry" then undo T_F_PEDIDO, retry T_F_PEDIDO.
               when "refaz" then undo T_F_PEDIDO, return "refaz".
               when "UNDO"  then undo T_F_PEDIDO, return trim(return-value).
            end case.
    
            for first empenho exclusive-lock
                where empenho.asdped = {&ped}.asdped while cfo_opb > 0
                      query-tuning (cache-size 1 row):
               assign empenho.recnotfis = w_not.recnotfis
                      empenho.valicm    = icm_epn.  
            end.
            if avail empenho then find current empenho no-lock.
            create nf_fat. assign nf_fat.recnotfis = w_not.recnotfis.
            for first {&not} no-lock 
                where {&not}.recnotfis = w_not.recnotfis 
                      query-tuning (cache-size 1 row):
            end.
         end. else do:
            if not avail {&cfo} then do:
               undo T_F_PEDIDO, return "UNDO".
            end.

            /* if f_log( "Entrou trfitem", input-output v_msg) then do: end.*/
            
            if not {&car}.flgpedhor then do:
               run g_trfitem( w_not.codemp, w_not.empctb, w_trn.codpac, 0 ).
            end. else do:
               run g_trfitem( w_not.codemp, w_not.empctb, 0, w_trn.codcar ).
            end.
            /* if f_log( "Saiu trfitem", input-output v_msg) then do: end.*/

            case entry(1,return-value):
               when "retry" then undo T_F_PEDIDO, retry T_F_PEDIDO.
               when "refaz" then undo T_F_PEDIDO, return "refaz".
               when "UNDO"  then undo T_F_PEDIDO, return trim(return-value).
            end case.
            
            assign {&car}.codsitcar = "T"
                   {&ped}.sitped    = "T"
                   {&ped}.flgconrev = w_not.conrev
                   stc_ant          = (if {&car}.numtotpedlib = 1
                                          then {&car}.codsitcar
                                          else stc_ant
                                      ).
            if avail {&pac} then do:
               assign {&pac}.sitpac= "T" {&sep}.sitsep = "T".
            end.
         end.

         if {&ped}.tipped = "PVA" then do:
           run CM/MN/nfremord.p (w_not.recnotfis, buffer notxped) no-error.
           if error-status:error or return-value = "erro" then
             undo T_F_PEDIDO, return "undo".
         end.

         &IF DEFINED(GLPI440) &THEN
         IF AVAIL {&not} AND i_ped.idtfat = "F" AND 
            not sml_fat and not f_tpd.pedb2c and 
                LOOKUP({&pgt}.forpgt,"R") > 0 THEN DO:
            FOR FIRST {&mppg} NO-LOCK
                WHERE {&mppg}.referenceNum = STRING(i_ped.asdped)
                        AND   LOOKUP({&mppg}.situacao,"RE") > 0:
               FOR FIRST w_pedtok NO-LOCK
                   WHERE w_pedtok.asdped = i_ped.asdped:
               END.
               IF NOT AVAIL w_pedtok THEN DO:
                  CREATE w_pedtok. 
                  ASSIGN w_pedtok.asdped = i_ped.asdped
                         w_pedtok.token  = {&mppg}.token.
                  for each x_not of {&ped} no-lock
                      where rowid(x_not) <> rowid({&not})
                     ,each x_dup of x_not no-lock
                      where lookup(x_dup.codctr,"13") > 0:
                     assign w_pedtok.valnot = w_pedtok.valnot + x_dup.valdup. 
                  end.       
               END.
                 
               IF w_pedtok.valnot > {&mppg}.reservado THEN DO:
                  run rt/rtedmsg.p ( 20, "ATENCAO", "",
                                "Valor " 
                             + TRIM(STRING({&mppg}.reservado,">>>,>>>,>>9.99"))
                             + " reservado no cartao menor que valor da nota "
                             + TRIM(STRING(w_pedtok.valnot,">>>,>>>,>>9.99"))
                                   ).
                  UNDO T_F_PEDIDO, return "refaz".
               END. 
            END.
            IF NOT CAN-FIND(FIRST w_pedtok
                            WHERE w_pedtok.asdped = i_ped.asdped) THEN DO:
               UNDO T_F_PEDIDO, RETURN "refaz".
            END.
         END.
         &ENDIF

         /*Para verificar a nota fiscal***************************************/
         REPEAT while grp_cha = "1" and i_ped.idtfat = "F"
                or    sml_fat
                or    current-value(amb_sap) = 4 and i_ped.idtfat = "F"
                or    lookup(Userid("Dictdb"),"") > 0
                      and i_ped.idtfat = "F" and avail {&not}
                or    avail {&not} and i_ped.idtfat = "F"
                and   (w_not.cfoopb > 0 
                       or {&car}.flgpedhor
                       or  w_not.uficms > 0 and 
                           (grp_cha = "1"
                            or
                            can-find(first {&inf} of {&not} no-lock 
                                     where {&inf}.alqicm = 7)
                           )
                       ):
            if flg_aut then leave.
            run verifica_nota.
            case entry(1,return-value):
               when "leave" then leave.
               when "retry" then undo T_F_PEDIDO, retry T_F_PEDIDO.
               when "refaz" then undo T_F_PEDIDO, return "refaz".
               when "undo"  then undo T_F_PEDIDO, return trim(return-value).
            end case.
         end.   
         /*Fim da verificacao da nota fiscal**********************************/

         if can-find(first ipd_a_f no-lock) then do:
            /*dep ant*/ next B_PEDIDO.
         end.
         
         empty temp-table w_htri no-error.       /*Para montar por pedido*****/
         
         for first {&not} no-lock
            where {&not}.recnotfis = w_not.recnotfis
           ,first {&inf} of {&not} no-lock query-tuning (cache-size 2 row):
         end.
         for first estcon no-lock query-tuning (cache-size 1 row): end.
         if avail sapreqtrf then
            assign sapreqtrf.sitreq = "F".
         
         if not {&car}.flgpedhor then do:
            if i_ped.idtfat = "T" then do:
               delete i_ped validate(true,"").      /*GLPI 440*/
               &IF DEFINED(FAT_PARCIAL) &THEN leave B_PEDIDO. &ELSE
               if can-find(first i_ped no-lock) then next B_PEDIDO.
               run g_nf_transferencia ( asd_trf ).
               assign frame f_1:visible = true.
               case entry(1,return-value):
                  when "retry" then undo T_F_PEDIDO, retry T_F_PEDIDO.
                  when "refaz" then undo T_F_PEDIDO, return "refaz".
                  when "ok"    then leave T_F_PEDIDO.
                  otherwise    undo T_F_PEDIDO, return trim(return-value).
               end case.
               &ENDIF
            end.
            assign {&car}.codsitcar = (if {&car}.flgati 
                                          then stc_ant 
                                          else "F"
                                      )
                   {&pac}.sitpac = {&car}.codsitcar.
            if {&pac}.sitpac = "F" and 
               can-find(first {&car} of {&pac} no-lock
                        where {&car}.codsitcar = stc_ant) then do:
               assign {&pac}.sitpac = stc_ant.
            end.
            delete i_ped validate(true,"").

            /*PARA FECHAR A TRANSACAO POR PEDIDO*************************/
            &IF DEFINED(FAT_PARCIAL) &THEN leave B_PEDIDO. &ELSE
            
            /*PARA FECHAR A TRANSACAO POR PACOTE*************************/
            if can-find(first i_ped no-lock) then next B_PEDIDO.
            leave b_pedido.
            &ENDIF
            
         end.

         /*PEDIDO DE HORA*****************************************************/
         if i_ped.idtfat = "T" 
         or w_not.empctb = 1 
         or sit_prc = "FLT"     then do:
            for first w_pspr no-lock where w_pspr.asdped    = {&ped}.asdped
                                           query-tuning (cache-size 1 row):
            end.
            assign {&car}.codsitcar = (if {&car}.flgati 
                                          then stc_ant
                                          else "F"
                                      ).
         end.
         delete i_ped validate(true,"").
         if can-find(first i_ped no-lock) then next B_PEDIDO.
         
         for each w_pspr no-lock query-tuning (cache-size 50 row):
            for first {&ped} exclusive-lock
                where {&ped}.asdped = w_pspr.asdped
               ,first {&car} of {&ped} exclusive-lock
                      query-tuning (cache-size 2 row):
               assign w_pspr.flgati    = {&ped}.flgpedati
                      w_pspr.sitcar    = {&car}.codsitcar
                      .
            end.
            case w_pspr.tipwms:
               when "E" then run MT/LG/mtlgmped.p ( w_pspr.asdped, "G,L,O" ).
               when "N" then run MT/LG/mtlgmped.p ( w_pspr.asdped, "N,L,O" ).
            end case.
            assign {&ped}.flgpedati = w_pspr.flgati
                   {&car}.codsitcar = w_pspr.sitcar.
            if entry(1,return-value) <> "ok" then do:
               if f_msg( "FALHA NA GERACAO DA SEPARACAO\n" +
                         "PEDIDO " + string(w_pspr.asdped,">9999,99999")
                       ) then do:
               end.
               undo T_F_PEDIDO, return "refaz".
            end.
            
            assign int_64 = codpac_separa ( w_pspr.asdped ).
            for first {&sep} where {&sep}.codpac = int_64:
               assign {&sep}.codtrn = cod_trn_pac /*cotta w_trn.codtrn*/.
               if lookup(w_pspr.tipwms,"N") > 0 then do:
                  assign {&sep}.datexp = {&sep}.datfor
                         {&sep}.horexp = {&sep}.horfor
                         {&sep}.datpdu = {&sep}.datfor
                         {&sep}.horpdu = {&sep}.horfor
                         {&sep}.datatu = {&sep}.datfor
                         {&sep}.horatu = {&sep}.horfor
                         {&sep}.sitsep = (if {&car}.codsitcar = "T" 
                                             then "T" 
                                             else "F"
                                         ).
               end. else do:
                  case f_tpd.grpprd:
                     when "CNS" or                         /*CONSUMO*/
                     when "CRB" or                         /*CARGA ROUBADA*/
                     when "DFT" or                         /*MERC DANIFICADA*/
                     when "FEI" then do:                   /*PEDIDO FEIRA*/
                        assign {&sep}.nrobox = 999
                               {&sep}.nroseq = 999.
                     end.
                     otherwise do:
                        assign {&sep}.nrobox = next-value(seqboxpho)
                               {&sep}.nroseq = {&sep}.nrobox.
                     end.
                  end case.
                  assign w_pspr.nrobox = {&sep}.nrobox .
               end.
            end.
            for first {&sep} no-lock where {&sep}.codpac = w_pspr.asdped
                                           query-tuning (cache-size 1 row): 
            end.
         end.

         if can-find(first w_trf no-lock) then do:
            run g_nf_transferencia ( asd_trf ).
            assign frame f_1:visible = true.
            case entry(1,return-value):
               when "retry" then undo T_F_PEDIDO, retry T_F_PEDIDO.
               when "refaz" then undo T_F_PEDIDO, return "refaz".
               when "ok"    then do: end.
               otherwise    undo T_F_PEDIDO, return trim(return-value).
            end case.
         end.      
         assign nro_rty = 0.
         find first   {&not}           no-lock no-error.
         find current {&car}           no-lock no-error.
         find current {&ped}           no-lock no-error.
         if can-find(first i_ped no-lock) then next B_PEDIDO.

         /*PARCIAL*********************************************************/
         leave T_F_PEDIDO.
      end.                                                       /*B_PEDIDO*/

      assign nro_rty = 0.
      find first {&ipd}  no-lock no-error.
      find first {&ped}  no-lock no-error.
      find first {&car}  no-lock no-error.
      find first {&cli}  no-lock no-error.
      find first {&dup}  no-lock no-error.
      find first {&ccr}  no-lock no-error.
      find first {&met}  no-lock no-error.
      find first {&gsz}  no-lock no-error.
      find first {&prd}  no-lock no-error.
      find first {&est}  no-lock no-error.
      find first {&not}  no-lock no-error.
      find first {&inf}  no-lock no-error.
      find first {&esr}  no-lock no-error.
      find first {&pnf}  no-lock no-error.
      find first {&pnfe} no-lock no-error.
      find first {&trfi} no-lock no-error.
      find first {&trfc} no-lock no-error.
   end.                                                          /*T_F_PEDIDO*/
   find current {&sep} no-lock no-error.
   find current itesep no-lock no-error.
   find first   pedatu no-lock no-error.

   /*Procedimento incluido no programa pdz/pdzipedi.p**************************
   &IF DEFINED(GLPI5457) &THEN if current-value(amb_sap) = -1 then
   for first {&strf} no-lock
       where rowid({&strf}) = rid_trf
      ,first {&ped} no-lock
       where {&ped}.asdped = {&strf}.asdped
      ,each  {&itrf}
       where {&itrf}.codcli    = {&strf}.codcli
       and   {&itrf}.numreqtrf = {&strf}.numreqtrf
       and   (if {&itrf}.qtdate = 0
                 then true else false
             ):
      
      RUN sap/sap0058e.p ( sapreqtrf.numreqtrf, 
                           sapitereqtrf.idprod
                         ) no-error.  
   END.
   &ENDIF
   ***************************************************************************/
   return "ok".
end procedure.                                                    /*F_PEDIDO*/

procedure m_lph_wms:
   def var arq_ped as cha no-undo.
   if w_not.uficms <> 0     and 
     {&ped}.tipped <> "PCH" and
     {&ped}.tipped <> "C29" and
     {&ped}.tipped <> "C32" and
     {&ped}.tipped <> "C36" and
     {&ped}.tipped <> "C38" and
     {&ped}.tipped <> "C31" and
     f_tpd.idtwms <> "E" then return "N".

   T_A_WMS:
   repeat transaction on error  undo T_A_WMS, return "UNDO"
                      on endkey undo T_A_WMS, return "UNDO"
                      on stop   undo T_A_WMS, return "UNDO"
                      on quit   undo T_A_WMS, return "UNDO":
      if lph_wms = "" then do:
         assign arq_ped = os-getenv("_CNFPGS") + "/_LIBERA/pedhorawms".
         if search(arq_ped) = ? then return "undo".
         input from value(arq_ped) no-echo.
         import unformatted lph_wms.
         input close.
      end.
      assign arq_ped = lph_wms + "/" 
                     + trim(string({&ped}.asdped,">999999999")).
      if search(arq_ped) = ? then do:
         if input frame f_1 w_trn.codtrn > 0 
         or input frame f_1 w_trn.codfre <> "RET" then do:
            assign rsp = "Liberacao do pedido " 
                       + string({&ped}.asdped,">9999,99999")
                       + " nao efetuada"
                       + ",Tecle <T>entar <A>bandonar,T,A".
            message entry(1,rsp,","). pause 1 no-message.
            run trg/trgclrsp.p (input-output rsp, false).
            if rsp = "T" then undo T_A_WMS, next T_A_WMS.
            undo T_A_WMS, return "undo".
         end.
         if input frame f_1 w_trn.codtrn = 0 and 
            input frame f_1 w_trn.codfre = "RET" then do:
            return "E".
         end.
         assign rsp = "Tipo de frete:" + input frame f_1 w_trn.codfre 
                    + " Transporte: "  + string(input frame f_1 w_trn.codtrn)
                    + " invalido".
         run trg/trgclrsp.p (input-output rsp, false).
         return "E".
      end.
      input from value(arq_ped) no-echo.
      import unformatted arq_ped no-error.
      input close.
      if error-status:error then do:
         assign rsp = "Falha no arquivo de liberacao do pedido".
         run trg/trgclrsp.p (input-output rsp, false).
         return "undo".
      end.
      if num-entries(arq_ped," ") <> 2                 or
         int64(ENTRY(1,arq_ped," ")) <> {&ped}.asdped  or
         lookup(entry(2,arq_ped," "),"E,N") = 0        then return "undo". 
      return trim(entry(2,arq_ped," ")).
   end.
   return "undo".
end procedure.                                                    /*m_lph_wms*/
procedure exporta_separacao:
   def input param rtn_val as cha no-undo.
   find first w_pspr no-lock 
        where w_pspr.tipwms = "E" no-error.
   A_EXPORTA:
   repeat transaction on error  undo A_EXPORTA, leave A_EXPORTA
                      on endkey undo A_EXPORTA, leave A_EXPORTA
                      on stop   undo A_EXPORTA, leave A_EXPORTA
                      on quit   undo A_EXPORTA, leave A_EXPORTA
                      while avail w_pspr:
      assign par_prg = "MT/LG/EX/mtlgexph.p,T,,,80,E," 
                     + string(w_pspr.asdped).
      run MT/LG/EX/mtlgexph.p.
      if return-value <> "ok" and
         f_msg( "FALHA NO ENVIO AUTOMATICO PARA O WMS ARMAZEM\n"   +
                "PEDIDO: " + string(w_pspr.asdped,">9999,99999")   + 
                "\nAVISAR AO SETOR DE LOGISTICA"
              ) then do:
      end.
      delete w_pspr.
      find first w_pspr no-lock 
           where w_pspr.tipwms = "E" no-error.
   end.                                                          /*A_EXPORTA*/
   return rtn_val.
end procedure.                                            /*exporta_separacao*/
procedure v_prcvenda:
   def input-output param per_cns as dec.
   def input        param sto_opb as int.
   def input        param pap_nor as dec.
   def var dat_ilv as dat no-undo.
   def var cod_set as int no-undo init ?.
   def var pre_clc as dec no-undo.
   def var str_psq as cha no-undo.                       

   for first livro no-lock
       where livro.codliv = {&ped}.codliv
      ,last  livroproduto no-lock use-index idprod
       where livroproduto.idprod = {&ipd}.idprod
       and   livroproduto.codliv <= {&ped}.codliv
             query-tuning (cache-size 2 row):
   end.
   if not avail livro or not avail livroproduto then do:
      if f_msg ( "FALHA NA PRECIFICACAO DO PRODUTO " +
                    string({&ipd}.codgrp,"99") + "." +
                    string({&ipd}.codprd,"999") 
                 + "\nPEDIDO " +
                    string({&ped}.asdped,">9999,99999")
               ) then do:
      end.
      return "refaz".
   end.
   assign dat_ilv = f_datliv ( livro.datini, "L" ).
   for each  setmun no-lock 
       where setmun.codest = {&cli}.codest  
       and   setmun.codmun = {&cli}.codmun  
       and   setmun.datinc <= dat_ilv 
       and   setmun.datexc >= dat_ilv
      ,each  setor no-lock 
       where setor.codset = setmun.codset:
      assign cod_set = setmun.codset.
      leave.
   end.
   if cod_set = ? then return "undo".
   
   /*Alterado codliv >= 200744*/
      run mpd/mpdclcpv.p ( string({&ped}.empctb) + ",S",
                           {&cli}.codest,
                           bf_sta.codest,
                           sto_opb,      /*Estado de orgao publico MG       */
                           pes_cnt,      /*Tipo de destinatario JC,JN,FC,FN */
                           w_not.conrev,
                           cod_set,
                           dat_ilv,
                           {&ipd}.codgrp,
                           {&ipd}.codprd,
                           {&ped}.codliv,
                           livroproduto.perdscpro,
                           {&ped}.perajuset,
                           livroproduto.preven,
                           pap_nor,
                           input-output per_cns,
                           input-output pre_clc,
                           output str_psq
                        ).
   assign pre_clc = trunc(pre_clc * (1 - ({&ipd}.perdscprd / 100)),2)
          per_cns = (if {&ipd}.preprd * 1.05 < pre_clc then per_cns else 1).

   for each hw_htri no-lock: delete hw_htri. end.
   for each w_htri  no-lock: delete w_htri.  end.

   if per_cns = 1 then return "ok".

   /*Bloco para verificacao ajuste de consumidor final************************/

   run ftp/crr/enviaemail_sem_anexo.p 
        ( "nfsvpaco@tambasa.com.br", "",
          "rsalgado@tambasa.com.br;gilson@tambasa.com.br",
         "AMBIENTE " + os-getenv("_AMB") + "\n" +
         "PEDIDO: " + string({&ped}.asdped) + " <" + {&ped}.dstmer   + "><"
                    + string({&ped}.papnor) + "><" + string(per_cns) + ">",

         "ACRESCIMO CONSUMO FINAL " + string(per_cns) + "\n"   +
         "CLIENTE: " + string({&ped}.codcli)          + "\n\n" + sit_dvg
        ) no-error. 
   
   if f_msg ( "FALHA NA PRECIFICACAO DO PRODUTO " +
                    string({&ipd}.codgrp,"99") + "." +
                    string({&ipd}.codprd,"999") 
                 + "\nPEDIDO " +
                    string({&ped}.asdped,">9999,99999")
               ) then do:
   end.
   return "refaz".
   /*Fim do bloco para verificacao********************************************/

end procedure.                                                   /*v_prcvenda*/
        
procedure A_NOT:
   def var rid_cta   as row.
   def var seq_ccr   as int initial 0.
   def var nsq_epn   as int no-undo.
   def buffer b_epn for empenho.
   def buffer b_ped for pedido.
   def buffer b_not for notafiscal.

   T_A_NOT:
   repeat transaction on error  undo T_A_NOT, return "UNDO"
                      on endkey undo T_A_NOT, return "UNDO"
                      on stop   undo T_A_NOT, return "UNDO"
                      on quit   undo T_A_NOT, return "UNDO":

      A_CLI:                       /*Atualiza a data de movimento dos produtos*/
      REPEAT while not sml_fat and {&cli}.datultmov < w_not.datmov 
             or    not sml_fat and {&cli}.datultmov = ?:
         find {&cli} of {&ped} exclusive-lock no-wait no-error.
         if locked {&cli} then do:
            run trg/trgrgblo.p (string({&ped}.codcli),
                               "cliente,codcli,I,R,Cliente "       + 
                               string({&ped}.codcli,">,>>>,>>9")     +
                               " bloqueado").
            assign rsp = return-value
                       + ",Tecle <T>entar <R>e-iniciar  <A>bortar,T,R,A".
            run trg/trgclrsp.p (input-output rsp, false).
            case rsp:
               when "T" then UNDO T_A_NOT, retry T_A_NOT.
               when "R" then UNDO T_A_NOT, return "RETRY".
               otherwise     UNDO T_A_NOT, return "UNDO".
            end.
         end.
         if not avail {&cli} then undo T_A_NOT, return "UNDO".
         assign {&cli}.datultmov = w_not.datmov no-error.
         if error-status:error then undo T_A_NOT, return "UNDO".
      end.
      assign {&ped}.flgpedati = can-find(first ipd_a_f)
             {&ped}.sitped    = "F"
             {&ped}.flgconrev = w_not.conrev
             {&ped}.idtmovsap = (if {&cfo}.IndAtuCtr 
                                    then "D" else {&ped}.idtmovsap
                                )
             {&car}.flgati    = if can-find(first {&ped} of {&car} 
                                            where {&ped}.flgpedati)
                                   {&tru}
             {&car}.codsitcar = "f"
             {&car}.codfre    = if {&car}.flgati 
                                    then {&car}.codfre 
                                    else input frame f_1 w_trn.codfre
             {&car}.ValCusMedIte    = {&car}.ValCusMedIte    + w_not.semicm
             {&car}.ValCusMedIteICM = {&car}.ValCusMedIteICM + w_not.comicm
             {&car}.ValCusTotIte    = {&car}.ValCusTotIte    + w_not.precus.
      if avail sapreqtrf and sapreqtrf.asdped = {&ped}.asdped then
         assign sapreqtrf.sitreq = {&ped}.sitped.
      
      for first empenho no-lock
          where empenho.asdped = {&ped}.asdped query-tuning (cache-size 1 row):
      end.
      if not sml_fat and avail empenho then do:
         run cria_txt.
         case entry(1,return-value):
           when "RETRY" then undo T_A_NOT, return trim(return-value).
           when "UNDO"  then undo T_A_NOT, return trim(return-value).
         end.
      end.

      /*if f_log( "Chama a_ite.....", input-output v_msg) then do: end.*/
      if not sml_fat then do:
         run A_ITE no-error.  /*Atualizacoes itens da NF*/
         case entry(1,return-value):
           when "RETRY" then undo T_A_NOT, return trim(return-value).
           when "UNDO"  then undo T_A_NOT, return trim(return-value).
         end.
         if error-status:error then undo T_A_NOT, return "undo".
      end.
      for each w_met no-lock while not sml_fat:
         find first {&met} where 
                    {&met}.codemp = w_met.codemp and
                    {&met}.codrep = w_met.codrep and 
                    {&met}.codLiv = w_met.codliv
                    exclusive-lock no-wait no-error.
         if locked {&met} then do:
            run blq/blqmeta.p ( w_met.codemp, w_met.codliv, w_met.codrep ).
            assign rsp = "separador=;," + return-value
                       + f_rsp( flg_aut, sml_fat ).
            run colhe_rsp.           
            case rsp:
               when "T" then undo T_A_NOT, retry T_A_NOT.
               when "R" then undo T_A_NOT, return "retry".
               otherwise undo T_A_NOT, return "UNDO".
            end.
         end.
         assign var_waio = 1.
         if not avail {&met} then create {&met}.
         assign {&met}.codemp = w_met.codemp
                {&met}.codrep = w_met.codrep
                {&met}.codLiv = w_met.codliv
                {&met}.ValFat = {&met}.valfat + w_met.valfat.
         delete w_met.
      end.
      /*if f_log( "Entra T_A_ESR...", input-output v_msg) then do: end.*/

      T_A_ESR:                                   /*Atualiza estorno icms*/
      REPEAT while not sml_fat and {&cfo}.Flgesricm and w_not.valesr > 0:
         find first {&esr} exclusive-lock
              where {&esr}.numanomes = year(w_not.datmov) * 100 
                                     + month(w_not.datmov) no-wait no-error.
         if locked {&esr} then do:
            run trg/trgrgblo.p (string(year (w_not.datmov) * 100 + 
                                      month(w_not.datmov)
                                     ),
                               "{&esr},numanomes,I,R,Estormo bloqueado").
            assign rsp = "separador=;," + return-value
                       + f_rsp( flg_aut, sml_fat ).
            run colhe_rsp.           
            case rsp:
               when "T" then UNDO T_A_NOT, retry T_A_NOT.
               when "R" then UNDO T_A_NOT, return "RETRY".
               otherwise     UNDO T_A_NOT, return "UNDO".
            end.
         end.
         assign var_waio = 1.
         if not avail {&esr} then create {&esr}.
         assign {&esr}.numanomes = year(w_not.datmov) * 100  
                                 + month(w_not.datmov)
                {&esr}.valesr    = {&esr}.valesr + w_not.valesr
                no-error.
         if error-status:error then undo T_A_NOT, return "UNDO".
         leave T_A_ESR.
      end.
      
      /*if f_log( "Entra l_notxped.", input-output v_msg) then do: end.*/
      l_notxped:
      repeat while not sml_fat: 
         find first notxped exclusive-lock
              where notxped.asdped = {&ped}.asdped no-wait no-error.
         if locked notxped then do:
            run trg/trgrgblo.p (string({&ped}.asdped),
                               "notxped,asdpedp,I,R,Nota pedido bloqueado").
            if return-value matches "*liberou" and var_waio < 10
               then next l_notxped.
            assign rsp = "separador=;," + return-value
                       + f_rsp( flg_aut, sml_fat ).
            run colhe_rsp.
            case rsp:
               when "T" then undo T_A_NOT, retry T_A_NOT.
               when "R" then undo T_A_NOT, return "RETRY".
               otherwise undo T_A_NOT, return "UNDO".
            end.
         end.
         assign var_waio = 1.
         if not avail notxped then leave l_notxped.
         if notxped.nronot    <= 0 
         or notxped.numnotfis <= 0 then do:
            if f_msg( "FALHA NA NUMERACAO DE NOTA DO PEDIDO: " + 
                      string(notxped.asdped,">9999,99999") ) then do:
            end.
            undo T_A_NOT, return "UNDO".
         end.
         leave l_notxped.
      end.

      /*if f_log( "Entra l_pnfe....", input-output v_msg) then do: end.*/
      find first {&pnfe} of f_ectb no-lock.

      l_pnfe:
      repeat while not avail notxped and not sml_fat:
         find first {&pnfe} of f_ectb exclusive-lock no-wait no-error.
         if locked {&pnfe} then do:
            run trg/trgrgblo.p (string({&ectb}.empctb),
                               "{&pnfe},empctb,I,R,Parametro NF bloqueado").
            if return-value matches "*liberou" and var_waio < 10
               then next l_pnfe.
            assign rsp = "separador=;," + return-value
                       + f_rsp( flg_aut, sml_fat ).
            run colhe_rsp.
            case rsp:
               when "T" then undo T_A_NOT, retry T_A_NOT.
               when "R" then undo T_A_NOT, return "RETRY".
               otherwise undo T_A_NOT, return "UNDO".
            end.
         end.
        
         assign var_waio = 1.
         l_pnf:
         repeat while f_ectb.empctb = 1:
            find first {&pnf} of {&emp} exclusive-lock no-wait no-error.
            if locked {&pnf} then do:
               run trg/trgrgblo.p (string({&emp}.codemp),
                                  "{&pnf},codemp,I,R,Parametro NF bloqueado").
               if return-value matches "*liberou" and var_waio < 10
                  then next l_pnf.
               assign rsp = "separador=;," + return-value
                          + f_rsp( flg_aut, sml_fat ).
               run colhe_rsp.
               case rsp:
                  when "T" then undo T_A_NOT, retry T_A_NOT.
                  when "R" then undo T_A_NOT, return "RETRY".
                  otherwise undo T_A_NOT, return "UNDO".
               end.
            end.
            assign {&pnfe}.nroatusai      = {&pnf}.numultnotfissai
                   {&pnf}.numultnotfissai = {&pnf}.numultnotfissai + 1
                   var_waio = 1.
            leave l_pnf.
         end.                                                         /*l_pnf*/
         if {&pnfe}.nroatusai >= {&pnfe}.nromaxsai 
         or ({&pnfe}.nromaxsai - {&pnfe}.nroatusai) < 1000 and
            ({&pnfe}.nromaxsai - {&pnfe}.nroatusai) mod 10 = 0 then do:
            run v_numero.
            if entry(1,return-value) = "UNDO" then undo T_A_NOT, return "UNDO".
         end.
         leave l_pnfe.
      end.                                                           /*l_pnfe*/

      /*if f_log( "Saiu  l_pnfe....", input-output v_msg) then do: end.*/
      if w_not.recnotfis = 0 then do:
         if avail notxped then do:
            assign w_not.numnotfis        = notxped.numnotfis
                   w_not.nraidf           = notxped.nraidf
                   notxped.numnotfis      = notxped.numnotfis + 1
                   notxped.nronot         = notxped.nronot    - 1.
            if notxped.nronot = 0 then delete notxped.
         end. else if not sml_fat then do:
            assign {&pnfe}.nroatusai = {&pnfe}.nroatusai + 1
                   w_not.numnotfis   = {&pnfe}.nroatusai
                   w_not.nraidf      = {&pnfe}.nraidf.
            assign rsp = v_aidf ({&pnfe}.empctb, today, {&pnfe}.nraidf ).
            if rsp <> "ok" and f_msg ( rsp ) then do:
               undo T_A_NOT, return "UNDO".
            end.
         end.          
         assign w_not.recnotfis        = next-value(recnotfis,tambasa).
         create {&not}.
      end. else do:
         for first {&not} exclusive-lock
             where {&not}.recnotfis = w_not.recnotfis
                   query-tuning (cache-size 1 row):
         end.
         if not avail {&not} then pause message "Nota fiscal nao disponivel".
      end.

      /*if f_log( "Copia notafiscal", input-output v_msg) then do: end.*/

      buffer-copy w_not to {&not} no-error.
      if error-status:error then do:
         assign rsp = "ERRO DE EXECUCAO 001,,".
         run trg/trgclrsp.p (input-output rsp, false).
         undo T_A_NOT, return "UNDO".
      end.
      if not sml_fat and can-find(first w_gnr no-lock) and
         not can-find(first gnrnot no-lock
                      where gnrnot.recnotfis = {&not}.recnotfis) then do:
         /*if f_log( "Chama g_gnrnot..", input-output v_msg) then do: end.*/
         run g_gnrnot.
         case entry(1,return-value):
            when "tenta" then UNDO T_A_NOT, retry T_A_NOT.
            when "retry" then UNDO T_A_NOT, return "RETRY".
            when "undo"  then UNDO T_A_NOT, return "UNDO".
            when "ok"    then do: end.
            otherwise    undo T_A_NOT, retry T_A_NOT.
         end case.
      end.

      find first {&not} exclusive-lock 
           where  {&not}.recnotfis = w_not.recnotfis.
      if not avail {&pac} or {&pac}.codpac <> {&car}.codpac then
         find first {&pac} of {&car} no-lock no-error.

      if avail {&pac} and
         ({&pac}.codtrn = 0 or not can-find(first {&car} of {&pac} no-lock
                                     where {&car}.flgati)) then do:
         run a_pacote.
         case entry(1,return-value):
            when "tenta" then undo T_A_NOT, retry T_A_NOT.
            when "retry" then UNDO T_A_NOT, return "RETRY".
            when "undo"  then UNDO T_A_NOT, return "UNDO".
            when "ok"    then do: end.
            otherwise    retry.
         end case.
      end.
      assign w_trn.notinf = if w_trn.notinf = 0 
                               then {&not}.numnot
                               else w_trn.notinf
             a_tel        = "A" 
             w_trn.notsup = {&not}.numnot.
      if not flg_aut and not sml_fat then
         disp w_trn.notinf w_trn.notsup a_tel with frame f_1.

      if w_not.idr_not <> ? then do:
         for first r_not no-lock where rowid(r_not) = w_not.idr_not:
            find first {&rnf} exclusive-lock
                 where {&rnf}.recnotfisori = r_not.recnotfis
                 and   {&rnf}.recnotfisref = {&not}.recnotfis no-error.
            if not avail {&rnf} then create {&rnf}.
            assign {&rnf}.recnotfisori = r_not.recnotfis
                   {&rnf}.recnotfisref = {&not}.recnotfis.
         end.
      end.
      
      /*Inicio da geracao da GNR***********************************************
      for first {&sta} where {&sta}.CodEst = w_not.CodEst and {&sta}.TipAntIcm
          query-tuning (cache-size 1 row):
        assign dat_ven = to_day + 3
               dat_ven = if weekday(dat_ven) = 7 then dat_ven + 2
                         else
                         if weekday(dat_ven) = 1 then dat_ven + 1
                         else dat_ven
               val_cnv = 0.
      
        for each w_inf no-lock
           ,first Produto 
            where Produto.CodGrp = w_inf.CodGrp 
            and   Produto.CodPrd = w_inf.CodPrd no-lock
           ,first hisprd of produto no-lock
            where hisprd.datini <= NotaFiscal.datmov
            and   hisprd.datfin >= NotaFiscal.datmov,
           ,first VariacaoBase 
            where VariacaoBase.CodVarBas = hisprd.CodVarBas 
            and   VariacaoBase.CodEst    = w_not.CodEst      
            and   VariacaoBase.TipOpe    = w_not.flgnotfis no-lock
            break by VariacaoBase.CodCnvGnr query-tuning (cache-size 60 row):
          assign val_cnv = val_cnv + w_inf.ValSubTri.
          if last-of(VariacaoBase.CodCnvGnr) then do:
            if val_cnv <> 0 then do:
              create GNR.
              assign GNR.RecNotFis = {&not}.RecNotFis
                     GNR.CodCnv    = VariacaoBase.CodCnvGnr
                     GNR.DatGnr    = {&not}.DatMov 
                     GNR.ValGnr    = val_cnv
                     gnr.datven    = dat_ven.
              assign val_cnv = 0.
            end.
          end.
        end.
      end.
      Fim da geracao da GNR***************************************************/

      /*if f_log( "Fim ger gnr.....", input-output v_msg) then do: end.*/

      for each w_inf no-lock:
         find {&ipd} where rowid({&ipd}) = w_inf.idaux no-error.
         if error-status:error then do:
            assign rsp = "ERRO DE EXECUCAO 002,,".
            run trg/trgclrsp.p (input-output rsp, false).
            undo T_A_NOT, return "UNDO".
         end.

         create {&inf}.
         buffer-copy w_inf to {&inf} assign
                    {&inf}.recnotfis = w_not.recnotfis
                    {&ipd}.indsitite = "F"
                    {&ipd}.codcom    = w_inf.codcom
                    {&ipd}.ValSubTri = w_inf.dprc_stmg
                    {&ipd}.persubtri = (w_inf.ValSubTri * 100 
                                     / w_inf.valliqprd)
                    {&ipd}.persubtri = (if not avail f_tpd
                                        or not f_tpd.pedmkt
                                           then {&ipd}.persubtri
                                           else 0
                                       ).
            
         if lookup({&cfo}.indtippre,"M,C") > 0 
         or avail r_not           then do: 
            assign {&ped}.valtotateped = {&ped}.valtotateped
                                       - {&ipd}.preprd * {&ipd}.qtdate
                   {&ped}.valtotpdzped = {&ped}.valtotpdzped
                                       - {&ipd}.preprd * {&ipd}.qtdped
                   {&ipd}.PrePrd       = w_inf.preprd 
                                       / (if {&ipd}.perdscprd <> 99
                                             then (1 + {&ped}.peracr / 100)
                                             else 1
                                         )
                   {&ipd}.perdscprd    = (if {&ipd}.perdscprd <> 99
                                             then w_inf.perdscprd
                                             else {&ipd}.perdscprd
                                         )
                   {&ped}.valtotateped = {&ped}.valtotateped
                                       + {&ipd}.preprd * {&ipd}.qtdate
                   {&ped}.valtotpdzped = {&ped}.valtotpdzped
                                       + {&ipd}.preprd * {&ipd}.qtdped.
         end.

         for each  w_ideso no-lock
             where w_ideso.recnotfis = 0
             and   w_ideso.numite    = w_inf.numite:
            create {&IIte}.
            buffer-copy w_ideso to {&IIte} assign
                    {&IIte}.recnotfis = w_not.recnotfis
                    w_ideso.recnotfis = w_not.recnotfis.
            delete w_ideso.

         end.
         delete w_inf.
         
         /*Atualiza MATERIAS DE CONSUMO DO ALMOXARIFADO, Tabela ESTCON*****/
         if {&cfo}.IndMovQtdEst = "S"       and 
            {&not}.cpfcnpj = f_ectb.cnpj    and
            ( lookup({&cfo}.tipmer,"S") > 0 or {&cfo}.idtestcsm = "C") then do: 
            run rt/rtestcon.p({&not}.recnotfis,
                              {&inf}.codgrp,
                              {&inf}.codprd,
                              {&inf}.qtdate,
                              "E").
         end.

         /* BHFS.002 INI - Gravando informacoes da NF do pedido na tabela ItemNFLote */
         &IF DEFINED(GLPI29549) &THEN
         FOR EACH ItemNFLote EXCLUSIVE-LOCK USE-INDEX itemped 
             WHERE ItemNFLote.Codcar    = {&ped}.Codcar
               AND ItemNFLote.NumPed    = {&ped}.NumPed
               AND ItemNFLote.NumItePed = {&ipd}.NumItePed
               AND ItemNFLote.idprod    = {&ipd}.idprod.
              
               ASSIGN ItemNFLote.recnotfis    = {&inf}.recnotfis 
                      ItemNFLote.numitenotfis = {&inf}.numitenotfis NO-ERROR. 
           
               if error-status:error then do:
                  message "Ocorreu o erro (" +
                  error-status:get-message(error-status:num-messages) +
                  ")".  
                  undo T_A_NOT, retry T_A_NOT.
               end.    
         END.
         &ENDIF 
         /* BHFS.002 FIM */

      end.

      /*Roberto **************************************************************/
      if {&ped}.codcndven = 10 and {&ped}.codcndpgt = "AT" and
         can-find(first dpaped no-lock
                  where dpaped.asdped = {&ped}.asdped) then do:
         run nfs/v/nfsvpant.p ( buffer {&not}, buffer {&ped} ) no-error.
         if entry(1,return-value) <> "ok" 
         or error-status:error then undo T_A_NOT, return "undo".
      end.
      /***********************************************************************/
      
      /*Bruno Ferreira *******************************************************/
      if lookup({&ped}.codcndpgt,"BL,PX") > 0 and
         can-find(first occPayment no-lock
                  where occPayment.asdped = {&ped}.asdped) then do:
         run nfs/v/nfsvbpg.p ( buffer {&not}, buffer {&ped} ) no-error.
         if entry(1,return-value) <> "ok" 
         or error-status:error then undo T_A_NOT, return "undo".
      end.
      /***********************************************************************/


      for each w_dup no-lock query-tuning (cache-size 10 row):
         for first {&dup} exclusive-lock
             where {&dup}.recnotfis = {&not}.recnotfis
             and   {&dup}.numseqdup = w_dup.numseqdup
                   query-tuning (cache-size 1 row):
         end.
         if not avail {&dup} then create {&dup}.
         buffer-copy w_dup to {&dup} assign
           {&dup}.recnotfis = {&not}.recnotfis
           {&dup}.numnotfis = w_not.numnotfis
           {&dup}.datultatu = {&not}.datmov
           {&dup}.horultatu = {&not}.hormov
           {&dup}.codraz    = {&raz}.codraz.
         for first PzFixVenc no-lock 
             where PzFixVenc.codcli     = {&dup}.codclifor
             and   PzFixVenc.datfim     >= today
             and   (if PzFixVenc.codrep = {&car}.codrep and 
                       PzFixVenc.flgativo 
                       {&tru}
                   ):
         end.
         if avail PzFixVenc then do:
            run nfs/r/nfsrpzfx.p ({&dup}.codclifor,
                                  {&car}.codrep,
                                  w_dup.datvendup,
                                  output {&dup}.datvendup
                                  ) no-error.
            if error-status:error 
            or {&dup}.datvendup = ? 
            or {&dup}.datvendup < today then do:
               if f_msg ("Falha prazo fixo por cliente" ) then do: end.
               undo T_A_NOT, return "undo".
            end.
         end.

         delete w_dup.
         if {&ped}.codrep = 0    or {&ped}.codrep = 1000 or
            {&ped}.codrep = 2000 or {&ped}.codrep = 3000 or 
            {&ped}.codrep = 4000 then
            run CL/MN/altcol.p ( {&car}.codemp, 
                                 {&car}.codrep, 
                                 18,
                                "D," + string({&dup}.recnotfis) 
                                     + "," + string({&dup}.numseqdup)).
      end.
      for first {&dup} no-lock where {&dup}.recnotfis = {&not}.recnotfis: end.

      for each  w_ccr no-lock query-tuning (cache-size 10 row):
         assign w_ccr.tipespdoc    = w_not.tipespdoc    
                w_ccr.codsernotfis = w_not.codsernotfis 
                w_ccr.numnotfis    = w_not.numnotfis    
                w_ccr.flgnotfis    = w_not.flgnotfis    
                w_ccr.flgclifor    = w_not.flgclifor    
                w_ccr.codclifor    = w_not.codclifor
                w_ccr.recnotfis    = w_not.recnotfis.
         find last {&ccr} where 
                   {&ccr}.codemp = w_ccr.codemp and
                   {&ccr}.codrep = w_ccr.codrep no-lock no-error.
         if available {&ccr} 
            then seq_ccr = {&ccr}.numseqlan + 1.
            else seq_ccr = 1.
         for first {&ccr} no-lock where
                   {&ccr}.recnotfis    = w_not.recnotfis    and
                   {&ccr}.codtipope    = "CF"               and
                   {&ccr}.tipespdoc    = w_not.tipespdoc    and
                   {&ccr}.codsernotfis = w_not.codsernotfis and
                   {&ccr}.numnotfis    = w_not.numnotfis    and
                   {&ccr}.flgnotfis    = w_not.flgnotfis    and
                   {&ccr}.flgclifor    = w_not.flgclifor    and
                   {&ccr}.codclifor    = w_not.codclifor    and
                   (if {&ccr}.codemp = w_not.codemp   and
                       {&ccr}.codrep = w_ccr.codrep
                       {&tru}) query-tuning (cache-size 1 row):
            assign rid_cta = rowid({&ccr}).
            leave.
         end.
         if not available {&ccr} then do:
            create {&ccr}.
            buffer-copy w_ccr to {&ccr} assign
              {&ccr}.numseqlan = seq_ccr
              {&ccr}.datultatu = {&not}.datmov
              {&ccr}.horultatu = {&not}.hormov
              {&ccr}.nomres    = Userid("Dictdb")
              rid_cta          = rowid({&ccr}) no-error.
            if error-status:error then undo T_A_NOT, retry T_A_NOT.
         end.
         find first {&ccr} where rowid({&ccr}) = rid_cta
                                        exclusive-lock no-wait no-error.
         if locked {&ccr} then do:
            run trg/trgrgblo.p ( string(rid_cta)), 
                                "contacorrente,rowid,W,R,Conta corrente " + 
                                trim(string(w_not.codemp,">9"))    + "-"  +
                                trim(string(w_ccr.codrep,">,>>>,>>9")     + 
                                " bloqueado").
            assign rsp = "separador=;," + return-value
                       + f_rsp( flg_aut, sml_fat ).
            run colhe_rsp.
            case rsp:
               when "T" then undo T_A_NOT, retry T_A_NOT.
               when "R" then undo T_A_NOT, return "retry".
               otherwise     undo T_A_NOT, return "undo".
            end.
         end.
         assign var_waio = 1.
         if not avail {&ccr} then undo T_A_NOT, retry T_A_NOT.
         assign {&ccr}.ValLan    = w_ccr.vallan
                {&ccr}.DatUltAtu = {&not}.datmov
                {&ccr}.HorUltAtu = {&not}.hormov.
         /*
         run CL/MN/altcol.p ({&ccr}.CodEmp,{&ccr}.CodRep,19,
                             string({&ccr}.NumSeqLan)).
         */
         delete w_ccr.
         if can-find(first g_ccr) then run exporta_com.
      end.
      for first {&dup} no-lock
          where {&dup}.recnotfis = {&not}.recnotfis
                query-tuning (cache-size 1 row):
      end.
      for first {&ccr} no-lock
          where {&ccr}.recnotfis = {&not}.recnotfis:
      end.
      run g_i_not ( buffer {&not}, buffer i_not ).
      for first empenho exclusive-lock
          where empenho.asdped = {&ped}.asdped while w_not.cfoopb > 0:
         assign empenho.recnotfis = w_not.recnotfis. 
      end.
      if not avail empenho                and 
         avail r_not and w_not.cfoopb > 0 and
         w_not.codcfo = w_not.cfoopb then do:
         for first empenho no-lock where empenho.recnotfis = w_not.recnotfis:
         end.    
         for first b_epn no-lock where b_epn.recnotfis = r_not.recnotfi
                                 while not avail empenho:
            assign nsq_epn = 0.
            for each  empenho no-lock
                where empenho.codcli = b_epn.codcli
                and   empenho.uniexe = b_epn.uniexe
                and   empenho.nroepn = b_epn.nroepn by empenho.nroseq desc:
               assign nsq_epn = empenho.nroseq + 1.
               leave.
            end.
            create empenho.
            buffer-copy b_epn to empenho assign
                        empenho.nroseq    = nsq_epn
                        empenho.asdped    = {&ped}.asdped
                        empenho.recnotfis = w_not.recnotfis
                        empenho.valepn    = w_not.valtotcon
                        empenho.valicm    = w_not.valtoticm no-error.
            if error-status:error and
               f_msg ("Falha ao criar empenho para nf" ) then do:
              undo T_A_NOT, retry T_A_NOT.
            end.
            find current empenho no-lock.
         end.
         if not avail empenho then
         for first empenho no-lock
             where empenho.recnotfis = r_not.recnotfis:
         end.    
      end.
      if avail empenho then find current empenho no-lock.
      
      if rec_ref <> 0 then do:
         create notref.
         assign notref.recnotfis    = w_not.recnotfis
                notref.recnotfisori = rec_ref
                notref.datatu       = today
                notref.usridt       = userid("dictdb") no-error.
         if error-status:error and
            f_msg ("Falha ao criar notref" ) then do: 
            undo T_A_NOT, retry T_A_NOT. 
         end. 
         find current notref no-lock.
      end.
      /*com mapa nao
        sem mapa sim*/

      if avail {&ped} and 
         ({&ped}.tipped = "PMH" or /*requisicao*/
          {&ped}.tipped = "C29" or /*Material oficina*/
          {&ped}.tipped = "C32" or /*almoXarifado adm (jorgemar)*/
          {&ped}.tipped = "C36" or /*Almoxarifado deposito (breno)*/
          {&ped}.tipped = "C38" or /*almOxarifado obras*/
          {&ped}.tipped = "C31" or 
          {&ped}.tipped = "PDS" or /*danificado para seguradora*/
          {&ped}.tipped = "PDD" or /*danificado para doacao*/ 
          {&ped}.tipped = "PDP")   /*danificado para perda*/ then do:
        run CS/MN/atuestwms.p (w_not.recnotfis,"S",
                               (if {&ped}.tipped = "PMH" or
                                   {&ped}.tipped = "C29" or 
                                   {&ped}.tipped = "C32" or
                                   {&ped}.tipped = "C36" or 
                                   {&ped}.tipped = "C38" or
                                   {&ped}.tipped = "C31" then "A" else "V")
                              ) no-error.
        if (error-status:error or return-value <> "ok") and
           f_msg ("Falha ao exportar arquivo para WMS") then do:
          undo T_A_NOT, retry T_A_NOT.
        end.
      end.

      for first notrft no-lock
          where notrft.asdped = {&ped}.asdped:
      end.
      if avail notrft then do:
        run a_notrft.
        case entry(1,return-value):
            when "tenta" then undo T_A_NOT, retry T_A_NOT.
            when "retry" then UNDO T_A_NOT, return "RETRY".
            when "undo"  then UNDO T_A_NOT, return "UNDO".
            when "ok"    then do: end.
            otherwise    retry.
        end case.
      end.

      /* Chamado 9410 */
      if lookup({&ped}.tipped,"ROP,ROM,SRP") > 0 then do:
        for first b_ped no-lock
            where b_ped.asdped = {&ped}.pedori,
            first b_not of b_ped no-lock:
          find first {&rnf} exclusive-lock
               where {&rnf}.recnotfisori = b_not.recnotfis
               and   {&rnf}.recnotfisref = {&not}.recnotfis no-error.
          if not avail {&rnf} then create {&rnf}.
          assign {&rnf}.recnotfisori = b_not.recnotfis
                 {&rnf}.recnotfisref = {&not}.recnotfis.
        end.
      end.
      
      leave T_A_NOT.
   end.                                                             /*T_A_NOT*/
   for first {&ccr} no-lock where {&ccr}.recnotfis = {&not}.recnotfis: end.
   for first {&dup} no-lock where {&dup}.recnotfis = {&not}.recnotfis: end.

   /*
   /* Gera arquivo de NF da filial CEASA e envia para windows NT */
   if {&not}.codclifor = 998248 and comentado
      can-find(first pedido of {&not} 
               where lookup(pedido.tipped,"PFM,PFH") > 0) then do:
     run CM/MN/notfil.p ({&not}.recnotfis).
     if return-value <> "ok" then return "undo".
   end.
   */
   return "ok".
end procedure.                                                        /*A_NOT*/
/*****************************************************************************/
/*****************************************************************************/
procedure a_notrft:
   t_a_notrft:
   repeat transaction on error  undo t_a_notrft, return "UNDO"
                      on endkey undo t_a_notrft, return "UNDO"
                      on stop   undo t_a_notrft, return "UNDO"
                      on quit   undo t_a_notrft, return "UNDO":
      find first notrft where notrft.asdpedrft = {&ped}.asdped
           exclusive-lock no-wait no-error.
      if locked notrft then do:
         run trg/trgrgblo.p (string(notrft.asdpedrft),
                            "notrft,asdpedrft,I,R,NotRft bloqueado").
         assign rsp = return-value 
                    + ",Tecle <T>entar <R>e-iniciar  <A>bortar,T,R,A".
         run trg/trgclrsp.p (input-output rsp, false).
         case rsp:
            when "T" then UNDO t_a_notrft, retry t_a_notrft.
            when "R" then UNDO t_a_notrft, return "RETRY".
            otherwise     UNDO t_a_notrft, return "UNDO".
         end.
      end.
      assign notrft.recnotfisrft = w_not.recnotfis no-error.
      if error-status:error then undo t_a_notrft, return "UNDO".
      
      /*Em 06/07/2018 as 12:35*************************************************
      create referencianf no-error.
      if error-status:error then undo t_a_notrft, return "UNDO".
      assign referencianf.recnotfisori = notrft.recnotfisori
             referencianf.recnotfisref = notrft.recnotfisrft no-error.
      if error-status:error then undo t_a_notrft, return "UNDO".
      ************************************************************************/
      leave t_a_notrft.
   end.   

   return "ok".      
end procedure.                                                   /*t_a_notrft*/
procedure g_i_not:
   def param buffer b_not for {&not}.
   def param buffer bi_not for i_not.
   if b_not.nraidf <> "" and
      not can-find(first {&aidf} no-lock 
                   where {&aidf}.nraidf = b_not.nraidf 
                   and   {&aidf}.empctb = b_not.empctb
                   and   {&aidf}.dtaidf <= b_not.datemi
                   and   {&aidf}.datlim >= b_not.datemi) 
   or b_not.nraidf = "" and
      not can-find(first {&aidf} no-lock 
                   where {&aidf}.empctb = b_not.empctb
                   and   {&aidf}.dtaidf <= b_not.datemi
                   and   {&aidf}.datlim >= b_not.datemi) then do:
      if f_msg ("PROGRAMA: nfsvpaco.p\n"            + "\n" +
                "AIDF....: " + b_not.nraidf         + "\n" +
                "Empresa.: " + string(b_not.empctb)
                            + " nao disponivel" ) then do:
      end.
      return "nao".
   end.   
   
   for last  {&cfo}  no-lock 
       where {&cfo}.codcfo     = b_not.codcfo
       and   {&cfo}.datultatu <= b_not.datmov:
      if not {&cfo}.flgnumtbs then return "nao".
   end.
   create bi_not.
   assign bi_not.id_not    = rowid(b_not)
          bi_not.numnot    = b_not.numnotfis
          bi_not.recnotfis = b_not.recnotfis.
   return "ok".          
end procedure.                                                      /*g_i_not*/
procedure exporta_com:         
   if v_com = "" then do:
      input from value( os-getenv("_CNFPGS") +
                        "/_COMUNICA/comfatura"
                      ).
     import unformatted v_com.
     input close.
   end.
   output to value(v_com + trim(string({&not}.recnotfis))) 
                    page-size 0 unbuffered.
   for each g_ccr no-lock:
      export g_ccr.
   end.
   for each g_ccr no-lock: delete g_ccr. end.
   output close.
end procedure.                                                  /*exporta_com*/
procedure g_gnrnot:      
   t_g_gnrnot:
   repeat transaction on error  undo t_g_gnrnot, return "UNDO"
                      on endkey undo t_g_gnrnot, return "UNDO"
                      on stop   undo t_g_gnrnot, return "UNDO"
                      on quit   undo t_g_gnrnot, return "UNDO"
                      while can-find(first w_gnr no-lock):
      for each w_gnr no-lock:
         create gnrnot.
         buffer-copy w_gnr to gnrnot assign
                              gnrnot.recnotfis = {&not}.recnotfis
                              gnrnot.datgnr    = {&not}.datmov.
         delete w_gnr.
      end.
   end.
   return "ok".
end procedure.                                                     /*g_gnrnot*/
procedure a_pacote:
   def buffer b_trn for transporte.
   
   t_a_pacote:
   repeat transaction on error  undo T_A_PACOTE, return "UNDO"
                      on endkey undo T_A_PACOTE, return "UNDO"
                      on stop   undo T_A_PACOTE, return "UNDO"
                      on quit   undo T_A_PACOTE, return "UNDO":
      find first {&pac} of {&car} exclusive-lock no-wait no-error.
      if locked {&pac} then do:
         run trg/trgrgblo.p (string({&car}.codpac),
                            "pacote,codpac,I,R,Pacote "             + 
                            string({&car}.codpac,">,>>>,>>9")       +
                            " bloqueado").
         assign rsp = return-value 
                    + ",Tecle <T>entar <R>e-iniciar  <A>bortar,T,R,A".
         run trg/trgclrsp.p (input-output rsp, false).
         case rsp:
            when "T" then UNDO T_A_PACOTE, retry T_A_PACOTE.
            when "R" then UNDO T_A_PACOTE, return "RETRY".
            otherwise     UNDO T_A_PACOTE, return "UNDO".
         end.
      end.
      find first {&sep} exclusive-lock 
           where {&sep}.codpac = {&pac}.codpac no-wait no-error.
      if locked {&sep} then do:
         run trg/trgrgblo.p (string({&pac}.codpac),
                            "{&sep},codpac,I,R,Separa "             + 
                            string({&pac}.codpac,">,>>>,>>9")       +
                            " bloqueado").
         assign rsp = return-value 
                    + ",Tecle <T>entar <R>e-iniciar  <A>bortar,T,R,A".
         run trg/trgclrsp.p (input-output rsp, false).
         case rsp:
            when "T" then UNDO T_A_PACOTE, retry T_A_PACOTE.
            when "R" then UNDO T_A_PACOTE, return "RETRY".
            otherwise     UNDO T_A_PACOTE, return "UNDO".
         end.
      end.

      find first b_trn no-lock
           where b_trn.codtrn = cod_trn_pac /*cotta w_trn.codtrn*/ no-error.
      if avail b_trn and b_trn.natcam = "F" then
        find first caminhao no-lock
             where caminhao.natcam = b_trn.natcam
             and   caminhao.placam = b_trn.numpla no-error.
             
      assign {&pac}.codtrn = cod_trn_pac /*cotta w_trn.codtrn*/
             {&sep}.codtrn = cod_trn_pac /*cotta w_trn.codtrn*/
             {&pac}.natcam = (if avail b_trn then b_trn.natcam else "")
             {&pac}.nrocam = (if avail b_trn and avail caminhao 
                              then caminhao.nrocam else "")
             {&pac}.numpla = /*(if {&pac}.codtrn = 2061 or 
                                 {&pac}.codtrn = 2554 or
                                 {&pac}.codtrn = 5286 or
                                 {&pac}.codtrn = 5287 or
                                 {&pac}.codtrn = 4781 
                              then {&pac}.numpla else*/ w_trn.plavei /*)*/ .
             
      if can-find(first {&car} of {&pac} no-lock 
                  where (if {&car}.flgati {&tru}))
         then leave T_A_PACOTE.
      
      assign {&pac}.FlgAti    = false
             {&pac}.IdtAti    = "N"
             {&pac}.sitpac    = {&car}.codsit
             /*{&pac}.DatSaiVei = w_trn.datsai*/
             {&pac}.HorFatPac = substr(string(w_not.hormov,
                                              "HH:MM:SS"),1,2)    
                              + substring(string(w_not.hormov,
                                              "HH:MM:SS"),4,2)
             {&pac}.datfatpac = w_not.datmov 
             {&pac}.HoratuPac = if {&pac}.datatupac = ?
                                   then {&pac}.horfatpac
                                   else {&pac}.HoratuPac
             {&pac}.datatupac = if {&pac}.datatupac = ?
                                   then {&pac}.datfatpac
                                   else {&pac}.datatuPac 
             no-error.
      if error-status:error then undo T_A_PACOTE, return "UNDO".
      assign {&sep}.datfat = {&pac}.datfat
             {&sep}.horfat = w_not.hormov no-error.
      if error-status:error then undo T_A_PACOTE, return "UNDO".
             
      run g_cxapacote.
      case entry(1,return-value):
         when "tenta" then undo T_A_PACOTE, retry T_A_PACOTE.
         when "RETRY" then undo T_A_PACOTE, return "RETRY".
         when "UNDO"  then undo T_A_PACOTE, return "UNDO".
      end.
      leave T_A_PACOTE.
   end.   

   return "ok".      
end procedure.                                                   /*T_A_PACOTE*/
procedure g_cxapacote:
   def var cod_cxa as int.
   def var dat_cxa as dat no-undo.
   def var rot_cd_prc as char no-undo.
   
   def buffer b_trn for transporte.

   assign rot_cd_prc = "FLC,FLN,TRN,CIF,FMC".
   /*CD*/
   for each introt no-lock where introt.tiprot = "C":
     assign rot_cd_prc = rot_cd_prc
                       + (if rot_cd_prc <> "" then "," else "") 
                       + caps(introt.codrot).
   end.
   /*Praca*/
   for each introt no-lock where introt.grprot = "P":
     assign rot_cd_prc = rot_cd_prc
                       + (if rot_cd_prc <> "" then "," else "")
                       + caps(introt.codrot).
   end.
   run a_r_v ( "undo" ).
   find first b_trn no-lock
        where b_trn.codtrn = {&pac}.codtrn no-error.
   if not avail b_trn then return "ok".
   find first cxapacote no-lock
        where cxapacote.codpac = {&pac}.codpac no-error.
   if avail cxapacote then return "ok".                    
   find first parcaixa no-lock no-error.

   /*
   assign cod_cxa = 0.
   repeat dat_cxa = to_day - 2 to to_day 
          while lookup({&pac}.codrot,rot_cd_prc) = 0
          and   {&pac}.codtrn <> 2723:               /*O PROPRIO*/
      for each  b_pac no-lock
          where b_pac.codtrn = {&pac}.codtrn 
          and   b_pac.datfatpac = dat_cxa
          and   (if lookup(b_pac.codrot,rot_cd_prc) = 0 then true else false)
         ,each cxapacote of b_pac no-lock:
         assign cod_cxa = cxapacote.codcxa.
      end.
   end.
   */
   
   t_g_cxapacote:
   repeat transaction on error  undo t_g_cxapacote, return "UNDO"
                      on endkey undo t_g_cxapacote, return "UNDO"
                      on stop   undo t_g_cxapacote, return "UNDO"
                      on quit   undo t_g_cxapacote, return "UNDO":
      m_codcxa:
      repeat while cod_cxa = 0:
         find last caixafech exclusive-lock no-wait no-error.
         if locked caixafech then do:
            assign rsp = "Fechamento caixa bloqueado".
            for last caixafech no-lock: 
               run trg/trgrgblo.p ( string(rowid(caixafech)), 
                     "caixafech,rowid,W,R,Fecha caixa bloqueado").
               assign rsp = (if return-value = ""
                                then ""
                                else return-value
                            ).
            end.
            assign rsp = "separador=;," + rsp
                       + f_rsp( flg_aut, sml_fat ).
            run colhe_rsp.
            case rsp:
               when "T" then undo t_g_cxapacote, retry t_g_cxapacote.
               when "R" then undo t_g_cxapacote, return "retry".
               otherwise     undo t_g_cxapacote, return "UNDO".
            end case.
            next m_codcxa.
         end.
         assign cod_cxa = (if avail caixafech then caixafech.codcxa + 1 else 1)
                var_waio = 1.
         create caixafech.
         assign caixafech.codcxa    = cod_cxa
                caixafech.valdiaria = parcaixa.valdia
                caixafech.tipcam    = b_trn.tipcam.
      end.
      create CxaPacote.
      assign CxaPacote.CodPac    = {&pac}.codpac
             CxaPacote.NomRes    = userid("dictdb")
             CxaPacote.DatDigPac = to_day
             CxaPacote.CodCxa    = cod_cxa
             CxaPacote.PerCom1   = if b_trn.flgrpa or 
                                      (b_trn.natcam = "T" and
                                       b_trn.indcd = "S")
                                      then 0
                                      else parcaixa.percom1
             CxaPacote.PerCom2   = if b_trn.flgrpa or
                                      (b_trn.natcam = "T" and
                                       b_trn.indcd = "S")
                                      then parcaixa.percom2aut 
                                      else parcaixa.percom2
             CxaPacote.PerCom3   = if b_trn.flgrpa or
                                      (b_trn.natcam = "T" and
                                       b_trn.indcd = "S")
                                      then 0
                                      else parcaixa.percom3.
      leave t_g_cxapacote.
   end.                                                       /*t_g_cxapacote*/
   find first caixafech no-lock no-error.
   find first cxapacote no-lock no-error.
      
   return "ok".
end procedure.                                                  /*g_cxapacote*/
procedure v_numero:
   def var cod_sai as cha format "x(20)" no-undo.
   if {&pnfe}.nroatusai < {&pnfe}.nromaxsai then do:
      if f_msg ( "NUMERACAO DA NOTA FISCAL ESTA CHEGANDO AO LIMITE" +
                 "\nFALTAM " +
                    string(({&pnfe}.nromaxsai - {&pnfe}.nroatusai),"99999999") 
                    + 
                 " NOTA(S)"
               ) then do:
      end.
      return "ok".
   end.
   if f_msg ( "NUMERACAO DA NOTA FISCAL CHEGOU NO LIMITE\n" +
              "AVISE A CONTABILIDADE"
            ) then do:
   end.
   return "undo".
end procedure.   
procedure G_W_MET:
   create w_met.
   assign w_met.codemp = {&car}.codemp
          w_met.codLiv = {&ped}.codliv
          w_met.codrep = if {&ped}.codrep = 0 
                            then {&car}.codrep else {&ped}.codrep 
          w_met.ValFat = w_not.valtotconnotfis.
end procedure.                                                      /*G_W_MET*/
procedure G_W_DUP:

   def var dia_prz         as int extent 12   initial 0.
   def var nro_par         as int            initial 1.
   def var seq_dup         as int            initial 0.
   def var i_a             as int            initial 0.
   def var val_dup         as dec decimals 2 initial 0.
   def var val_sub         as dec decimals 2 initial 0.
   def var dat_ven         as dat            initial ?.
   def var dat_hoj         as dat            initial ?.
   def var dsc_crt         as cha.  /*GLPI 440*/
   def var num_ctr_ban     as cha no-undo.

   gg_w_dup:
   repeat transaction on error  undo gg_w_dup, return "UNDO,error"
                      on endkey undo gg_w_dup, return "UNDO,endkey"
                      on stop   undo gg_w_dup, return "UNDO,stop"
                      on quit   undo gg_w_dup, return "UNDO,quit":
     run a_r_v ( "undo" ).
     assign dat_hoj = (if w_not.datmov < today
                          then today - 1
                          else w_not.datmov - 1
                      )
            nro_par = if {&ped}.DiaPrz001 > 0 then {&ped}.diaprz001 else 1.   

      for first {&not} of {&ped} no-lock
          where (if {&not}.flgnotfis = "S" {&tru}):
         assign dat_hoj = {&not}.datcri - 1.
      end.

      for each  {&prz} no-lock 
          where {&prz}.codCndVen = {&ped}.codCndVen 
          and   {&prz}.codCndPgt = {&ped}.codCndPgt 
          and   {&prz}.codLiv    = {&ped}.codLiv
                by {&prz}.valmin while {&pgt}.FlgPrz and {&pgt}.flgfix:

         assign nro_par = (if {&ped}.DiaPrz001 > 0 
                              then {&ped}.diaprz001 else 1 ).
         run mpd/mpdmtprz.p ( buffer {&prz}, nro_par ).
         assign dat_ven = date(entry(1,entry(2,return-value,"@"))).
         repeat i_a = 1 to nro_par:
            assign dia_prz[i_a] = int(entry(i_a,entry(3,return-value,"@"))).
            if lookup({&pgt}.forpgt,"R") > 0 then do:
               assign dia_prz[i_a] = date(entry(i_a,entry(2,return-value,"@")))
                                   - dat_ven.
            end.
         end.

         if {&prz}.valmin >= {&ped}.ValTotAtePed 
         or {&prz}.valmax >= {&ped}.valtotateped and {&prz}.nummaxpar > 0
         or lookup({&pgt}.forpgt,"R") > 0 /*GLPI 440*/
            then leave.

      end.
      assign i_a = (if {&ped}.codrep = 0 then {&car}.codrep else {&ped}.codrep).

      for each  {&aut} no-lock 
          where {&aut}.codCndVen = {&ped}.codCndVen
          and   {&aut}.codCndPgt = {&ped}.codCndPgt
          and   {&aut}.codLiv    = {&ped}.codLiv
          and   (if {&aut}.codemp = {&car}.codemp or {&aut}.codemp = 0 
                    {&tru})
          and   (if {&aut}.codrep = i_a or {&aut}.codrep = 0
                    {&tru})
          and   (if {&aut}.codCli = {&ped}.codCli or {&aut}.codCli = 0 
                    {&tru})
               by {&aut}.codcli desc
               by {&aut}.codemp desc
               by {&aut}.codrep desc
               while dat_ven = ? and {&pgt}.FlgPrz and {&pgt}.flgfix:
         assign dat_ven    = {&aut}.DatIni 
                           - (if nro_par <= 1 then 0 else {&aut}.NumDiaPrz)
                dia_prz[2] = {&prz}.numdiaprz * (if nro_par = 2 then 2 else 1)
                dia_prz[3] = {&prz}.numdiaprz * 2.
         leave.
      end.

      if dat_ven = ? and {&pgt}.FlgPrz and not {&pgt}.flgfix then do:
         assign dat_ven    = dat_hoj
                dia_prz[1] = {&ped}.diaprz001
                dia_prz[2] = {&ped}.diaprz002
                dia_prz[3] = {&ped}.diaprz003
                dia_prz[4] = {&ped}.diaprz004
                dia_prz[5] = {&ped}.diaprz005.
         do nro_par = 1 to 4 while dia_prz[nro_par + 1] > 0: end.  
      end.
      if dat_ven = ? then
         assign dat_ven    = dat_hoj + 1
                nro_par    = 1
                dia_prz[1] = 0
                dia_prz[2] = 0
                dia_prz[3] = 0
                dia_prz[4] = 0
                dia_prz[5] = 0.

      /****MOTO SHINERAY******************************************************/
      if lookup(string({&ped}.codcndven,"99") + {&ped}.codcndpgt,"02P") > 0 
         then do:
         assign nro_par     = 12
                dat_ven     = dat_hoj
                dia_prz[01] = 30
                dia_prz[02] = 60
                dia_prz[03] = 90
                dia_prz[04] = 120
                dia_prz[05] = 150
                dia_prz[06] = 180
                dia_prz[07] = 210
                dia_prz[08] = 240
                dia_prz[09] = 270
                dia_prz[10] = 300
                dia_prz[11] = 330
                dia_prz[12] = 360.
      end.

      /*Para agrupar parcelas de notas com valor <= 200 **********************/
      if avail {&cct} and {&cct}.FlgEmiBolCtr              and
         nro_par > 1 and lookup({&pgt}.codcndpgt,"AT") = 0 and
         lookup({&pgt}.forpgt,"R") = 0                     and /*GLPI 440*/
         w_not.ValTotConNotFis + w_not.valclcsub - w_not.valdscpiscof < 200 
         and {&ped}.codcli <> 228755 then
         repeat nro_par = 2 to 12:
            assign dia_prz[1]       = dia_prz[1] + dia_prz[nro_par]
                   dia_prz[nro_par] = 0.
            if nro_par = 12 or dia_prz[nro_par + 1] = 0 then do:
               assign dia_prz[1] = dia_prz[1] / nro_par
                      dia_prz[1] = dia_prz[1] + 7
                      nro_par    = 1.
               leave.
            end.
      end.

      assign val_dup = truncate((w_not.ValTotConNotFis + w_not.valclcsub
                                                       - w_not.valdscpiscof)
                                / nro_par,2)
             val_sub = truncate(w_not.valclcsub / nro_par,2)
             seq_dup = if nro_par > 1 then 1 else 1.       

      if f_tpd.pedb2c or sml_fat then leave gg_w_dup.

      &IF DEFINED(GLPI440) &THEN
      ASSIGN num_ctr_ban = "".
      FOR FIRST {&mppg} NO-LOCK
          WHERE {&mppg}.referenceNum = STRING({&ped}.asdped)
          AND   {&mppg}.situacao     = "RE"
          and   not sml_fat
          WHILE LOOKUP({&pgt}.forpgt,"R") > 0:

         FOR FIRST mp_token NO-LOCK
             WHERE mp_token.token = {&mppg}.token:
            ASSIGN dsc_crt     = TRIM(mp_token.masked) 
                               + " " + mp_token.bandeira
                   num_ctr_ban = TRIM({&mppg}.processorTransactionID) + "\n" 
                               + TRIM(mp_token.cvv)                   + "\n" 
                               + TRIM(mp_token.masked).
         END.
      END.

      IF avail {&pgt} and lookup({&pgt}.forpgt,"R") > 0 and num_ctr_ban = ""
         and not sml_fat
      OR NOT AVAIL {&mppg} AND lookup({&pgt}.forpgt,"R") > 0 and 
         not sml_fat THEN DO:
         ASSIGN rsp = "separador=;,"
                + "RESERVA DO CARTAO NAO CADASTRADA".
         RUN trg/trgclrsp.p ( INPUT-OUTPUT rsp, FALSE).
         RETURN "undo".
      END.
      &ENDIF

      /*Alimenta tabela de trabalho para gerar nrosorte***********************
      if today >= 04/20/2019 and today <= 01/05/2020 and
         avail f_tpd and f_tpd.grpprd = "" and current-value(amb_sap) = 0 and 
         not can-find(first nf_srt no-lock
                      where nf_srt.recnotfis = w_not.recnotfis) then do:
         create nf_srt. assign nf_srt.recnotfis = w_not.recnotfis.
      end.
      **********************************************************************/

      leave gg_w_dup.

   end.

   C_W_DUP:
   repeat i_a = 1 to nro_par
          transaction on error  undo C_W_DUP, return "UNDO,error"
                      on endkey undo C_W_DUP, return "UNDO,endkey"
                      on stop   undo C_W_DUP, return "UNDO,stop"
                      on quit   undo C_W_DUP, return "UNDO,quit":

      create w_dup.
      assign w_dup.recnotfis    = w_not.recnotfis
             w_dup.codCliFor    = w_not.codCliFor
             w_dup.numnotfis    = w_not.numnotfis
             w_dup.empctb       = w_not.empctb
             w_dup.indlimcre    = {&ped}.indlimcre
             w_dup.NumSeqDup    = seq_dup
             w_dup.DatVenDup    = dat_ven + dia_prz[i_a] 
             w_dup.DatVenDup    = if w_dup.DatVenDup < w_not.datmov
                                     then w_not.datmov 
                                     else w_dup.DatVenDup
             w_dup.DatVenOriDup = w_dup.DatVenDup
             w_dup.codemp       = w_not.codemp
             w_dup.codCtr       = {&cct}.codCtr
             w_dup.numdupban    = dsc_crt       /*GLPI440*/
             w_dup.numctrban    = num_ctr_ban
             w_dup.DatUltAtu    = w_not.datmov
             w_dup.HorUltAtu    = w_not.hormov
             w_dup.codSitTit    = "G"
             w_dup.valdup       = val_dup 
                                + (if seq_dup <= 1 then 1 else 0)
                                * ((w_not.ValTotConNotFis
                                    + w_not.valclcsub
                                    - w_not.valdscpiscof)
                                    - val_dup * nro_par)
             w_dup.valsubtri    = val_sub
                                + (if seq_dup <= 1 then 1 else 0)
                                * (w_not.valclcsub - val_sub * nro_par)
             w_dup.ValJurDiaDup = w_dup.ValDup 
                                * {&pcr}.PerDiaJur / 100
             seq_dup            = seq_dup + 1.

      if w_dup.valdup <= 0 
      or not avail {&pgt} then do:
         return "undo".
      end.

      /******
      if lookup({&pgt}.forpgt,"R") > 0 then do:
         assign w_dup.datpgtdup = w_dup.DatVenDup
                w_dup.datliqdup = w_dup.DatVenDup
                w_dup.valpgtdup = w_dup.valdup
                w_dup.codsittit = "LQ".
      end.
      */

   end.

   return "ok".
end procedure.                                                      /*G_W_DUP*/
procedure G_W_CCR:                                                  /*G_W_CCR*/
   def var seq_cta  as int initial 0.
   def var d        as int initial 0.
   def var id_cte   as rec initial ?.
   def var nao_cal  as cha no-undo.
   run a_r_v ( "undo" ).
   assign nao_cal = "01000,02000,03000,04000,01183".
   if lookup(f_tpd.grpprd,"CNS,TRF,DFT,SAC,FEI,DOA") > 0 then return "ok".
   if lookup(string(w_not.codrep,"99999"),nao_cal) > 0 then
      assign w_not.valcom[2] = 0.
   if w_not.valcom[2] <> 0 
   or w_not.codcom[2] = 99 then 
      assign w_not.valcom[1] = w_not.valcom[1] * ({&pvd}.PerComRed / 100).

   C_CONTA:
   REPEAT d = 1 to 2:
      if w_not.repcom[d] = 0 or w_not.valcom[d] = 0 then next.
      create w_ccr.
      assign w_ccr.codemp       = {&car}.codemp
             w_ccr.codrep       = w_not.repcom[d]
             w_ccr.codtipope    = "CF"
             w_ccr.DatLan       = w_not.datmov
             w_ccr.numnotfis    = w_not.numnotfis
             w_ccr.codsernotfis = w_not.codsernotfis
             w_ccr.numseqlan    = d
             w_ccr.flgnotfis    = "S"
             w_ccr.recnotfis    = w_not.recnotfis
             w_ccr.ValLan       = w_not.valcom[d]
             w_ccr.flgclifor    = "C"
             w_ccr.codCliFor    = {&ped}.codCli
             w_ccr.DatUltAtu    = w_not.datmov
             w_ccr.HorUltAtu    = w_not.hormov
             w_ccr.desobs       = 
               if {&ped}.codemp = 1 and 
                  ({&ped}.codrep = 1000 or 
                   {&ped}.codrep = 2000 or
                   {&ped}.codrep = 3000 or
                   {&ped}.codrep = 4000 or
                   {&ped}.nomres begins "recupera"
                   ) then "Recuperado Automaticamente"
               else "".
      for first {&div} no-lock
          where {&div}.codemp = w_not.codemp
          and   {&div}.coddiv = w_not.coddiv:
         assign w_ccr.empctb = {&div}.empctb. 
      end.
   end.
   return "ok".
end procedure.                                                      /*G_W_CCR*/
procedure A_ITE:
   def var rid_aux   as row no-undo.
   def var mov_est   as int initial 0.
   def var qtd_rsv   as dec decimals 2 initial 0.
   def var qtd_rea   as dec decimals 2 initial 0.
   def var qtd_atu   as int initial 0.
   def var mov_dan   as int initial 0.
   def var qtd_dan   as dec decimals 2 initial 0.
   
   def var rsv_mat   as dec decimals 2 initial 0.
   def var sub_tri   as dec no-undo.
   run a_r_v ( "undo" ).

   disable triggers for load of {&prd}.
   assign var_waio = 1.

   t_estoque:
   repeat transaction on error  undo t_estoque, return "UNDO"
                      on endkey undo t_estoque, return "UNDO"
                      on stop   undo t_estoque, return "UNDO"
                      on quit   undo t_estoque, return "UNDO":
      for each w_inf no-lock:
         if avail w_pspr and w_pspr.asdped = {&ped}.asdped
                         and w_pspr.tipwms = "N" 
                         and w_pspr.movrsv = 0 then do:
            for first {&est} no-lock 
                where {&est}.codemp = w_not.codemp 
                and   {&est}.empctb = w_not.codemp
                and   {&est}.codgrp = w_inf.codgrp 
                and   {&est}.codprd = w_inf.codprd
                and   (if {&est}.empctb = w_not.codemp {&tru}):
               assign w_inf.ridestm = rowid({&est}).
            end.
         end.
         for first {&est} no-lock 
             where {&est}.codemp = w_not.empctb 
             and   {&est}.codgrp = w_inf.codgrp 
             and   {&est}.codprd = w_inf.codprd
             and   (if {&est}.empctb = w_not.empctb {&tru}):
            assign w_inf.ridest = rowid({&est}).
         end.
      end.                   
      for each w_inf no-lock:
         find first {&prd} no-lock 
              where {&prd}.codgrp = w_inf.codgrp
              and   {&prd}.codprd = w_inf.codprd no-wait no-error.
         assign mov_est = (if {&cfo}.IndMovQtdEst <> "N" 
                           or w_inf.codcfo = 155401
                           or w_inf.codcfo = 255401
                           or w_inf.codcfo = 555401
                           or w_inf.codcfo = 655401 then 1 else 0
                          )
                        * {&str}.idtmovreg
                qtd_rea = w_inf.qtdate * mov_est
                qtd_rsv = w_inf.qtdate * mov_est
                qtd_rsv = if qtd_rsv >= 0     then 0 else qtd_rsv
                qtd_rsv = if w_not.codcar = 0 then 0 else qtd_rsv
                qtd_atu = w_inf.qtdate
                        * (if mov_est = -1 and
                              lookup({&cfo}.tipopesrf,"010") > 0
                              then 1 else 0
                          )
                mov_dan = (if {&cfo}.idtqtddan = "N" then 0 else 1)
                        * {&str}.idtmovreg
                qtd_dan = w_inf.qtdate * mov_dan.

         if mov_est > 0 then do:
            rsp = "Codigo fiscal <" + string({&cfo}.codcfo,"9,999,99") 
                   + "> com problema,Tecle <A>bortar,A".
            run trg/trgclrsp.p (input-output rsp, false).
            undo t_estoque, return "UNDO".
         end.
      
         if w_not.codest = f_ectb.codest and w_not.uficms = 0 and      
            lookup(trim(w_inf.codtri),"2,5,6,8,9,X,23,27,28") > 0 then do:
            if weekday(today) = 7 or weekday(today) = 1 then do:
               run NF/sa/NFsasenh.p ( "c_senha" ).
               if return-value = "undo" then undo t_estoque, return "undo".
            end. else do:
               rsp = "separador=;,m\nFalha no calculo produto: " 
                   + string(w_inf.codgrp,"99")     + "." 
                   + string(w_inf.codprd,"999")
                   + ", parar o faturamento.@"
                   + "i\nComunicar imediatamente com o setor de Analise"
                   + ";Tecle <A>bortar;A".
               pause 1 before-hide.
               run trg/trgclrsp.p (input-output rsp, false).
               undo t_estoque, return "UNDO".
            end.
         end.

         A_PRD:                   /*Atualiza a data de movimento dos produtos*/
         REPEAT while {&prd}.datultmov < w_not.datmov or {&prd}.datultmov = ?: 
            find first {&prd} exclusive-lock 
                 where {&prd}.codgrp = w_inf.codgrp
                 and   {&prd}.codprd = w_inf.codprd no-wait no-error.
            if locked {&prd} then do:
               run blq/blqprodu.p ( w_inf.codgrp, w_inf.codprd ).
               assign rsp = "separador=;," + return-value
                          + f_rsp( flg_aut, sml_fat ).
               run colhe_rsp.
               case rsp:
                  when "T" then undo t_estoque, retry t_estoque.
                  when "R" then undo t_estoque, return "retry".
                  otherwise     undo t_estoque, return "UNDO".
               end case.
            end.
            if not avail {&prd} then undo t_estoque, return "UNDO".
            assign {&prd}.datultmov = w_not.datmov no-error.
            if error-status:error then undo t_estoque, return "UNDO".
            find current {&prd} no-error.
         end.

         A_GSZ:                                 /*Atualiza o giro do produtos*/
         REPEAT:
            if mov_est >= 0 
            or not {&cfo}.FlgAtuMedVen 
            or {&cfo}.FlgAtuQtdEstExt
            or w_not.codclifor = 998248 then leave A_GSZ.
            find first {&gsz} exclusive-lock 
                 where {&gsz}.codemp    = w_not.codemp
                 and   {&gsz}.anomesgir = year(w_not.datmov) * 100 
                                        + month(w_not.datmov) 
                 and   {&gsz}.codgrp    = w_inf.codgrp
                 and   {&gsz}.codprd    = w_inf.codprd no-wait no-error.
            if locked {&gsz} then do:
               run blq/blqgsazo.p ( w_not.codemp,
                                    w_inf.codgrp,
                                    w_inf.codprd,
                                    year(w_not.datmov) * 100 + 
                                    month(w_not.datmov)).
               assign rsp = "separador=;," + return-value
                          + f_rsp( flg_aut, sml_fat ).
               run colhe_rsp.
               case rsp:
                  when "T" then undo t_estoque, retry t_estoque.
                  when "R" then undo t_estoque, return "retry".
                  otherwise     undo t_estoque, return "UNDO".
               end case.
            end.
            if not avail {&gsz} then create {&gsz}.
            assign {&gsz}.codemp          = w_not.codemp
                   {&gsz}.codgrp          = w_inf.codgrp
                   {&gsz}.codprd          = w_inf.codprd
                   {&gsz}.anomesgir       = year(w_not.datmov) * 100  
                                          + month(w_not.datmov)
                   {&gsz}.idprod          = w_inf.codgrp * 1000 + w_inf.codprd
                   {&gsz}.qtdtotgirfat    = {&gsz}.qtdtotgirfat + w_inf.qtdate
                   no-error.
            if error-status:error then undo t_estoque, return "UNDO".
            find current {&gsz} no-error.
            leave A_GSZ.
         end.
         if w_inf.ridest = ? then do:
            assign rsp = "separador=;,I\nProduto: " 
                       + trim(caps(replace({&prd}.desprd,";",",")))
                       + " Codigo: "
                       + string({&prd}.codgrp,"99")     + "."
                       + string({&prd}.codprd,"999")    + "-"
                       + string({&prd}.dveprd,"9")      + ". "
                       + string({&prd}.coduni)          + "."
                       + (if avail {&ped} 
                             then "@m\nPedido: " + 
                                  string({&ped}.asdped,">9999,99999") + ", "
                             else "@m\n"
                         )
                      + "estoque nao disponivel na empresa " 
                      + string(w_not.empctb).
            /*if f_log( rsp, input-output v_msg) then do: end.*/
            run trg/trgclrsp.p (input-output rsp, false).
            undo t_estoque, return "UNDO,estoque".
         end.
         if avail w_pspr and w_pspr.asdped = {&ped}.asdped
                         and w_pspr.tipwms = "N" then do:
            assign rsv_mat = qtd_rsv
                   qtd_rsv = qtd_rsv * w_pspr.movrsv
                   rsv_mat = rsv_mat - qtd_rsv.
         end.
         assign rid_aux = w_inf.ridest.
         A_EST:                             /*Atualiza o estoque dos produtos*/
         REPEAT:
            if qtd_rsv = 0 and qtd_rea = 0 and rsv_mat = 0 then leave A_EST.
            if sml_fat
               then find first {&est} no-lock 
                         where rowid({&est}) = rid_aux no-wait no-error.
               else find first {&est} exclusive-lock 
                         where rowid({&est}) = rid_aux no-wait no-error.
            if locked {&est} then do:
               assign rsp = "{&est},rowid,W,R,Estoque produto "
                          + string({&prd}.codgrp,"99") + "."
                          + string({&prd}.codprd,"999") + "-"
                          + string({&prd}.dveprd,"9")   
                          + "; empresa " + string(w_not.empctb)
                          + " bloqueado". 
               run trg/trgrgblo.p ( string( rid_aux ) , rsp). 
               assign rsp = "separador=;," 
                          + replace(return-value,";",",")
                          + f_rsp( flg_aut, sml_fat ).
               run colhe_rsp no-error.
               case rsp:
                  when "T" then undo t_estoque, retry t_estoque.
                  when "R" then undo t_estoque, return "retry".
                  otherwise     undo t_estoque, return "UNDO".
               end case.
               if error-status:error then do:
                  message {&err_msg}. pause no-message.
                  undo t_estoque, return "UNDO".
               end.
            end.
            assign var_waio = 1.
            if w_inf.cuscomicm = 0 then do:
               assign w_inf.cuscomicm = (if {&est}.premedicm <= 0  or
                                         {&est}.qtdestrea <= 0
                                         then 0
                                         else {&est}.premedicm
                                     )
                                   * w_inf.qtdate 
                                   / (if {&est}.qtdestrea <= 0 
                                         then 1
                                         else {&est}.qtdestrea
                                     ).
                   w_inf.cussemicm = (if {&est}.premed <= 0  or
                                         {&est}.qtdestrea <= 0
                                         then 0
                                         else {&est}.premed
                                     )
                                   * w_inf.qtdate 
                                   / (if {&est}.qtdestrea <= 0 
                                         then 1
                                         else {&est}.qtdestrea
                                     ).
            end.
            if w_inf.cuscomicm = 0 or w_inf.cussemicm = 0 then do:
               run MT/MN/MTcusprd.p ( "nota,ordemnota",
                                       w_inf.codgrp, 
                                       w_inf.codprd,
                                       input-output w_inf.cuscomicm,
                                       input-output w_inf.cussemicm,
                                       input-output sub_tri
                                    ).
               assign w_inf.cuscomicm = w_inf.cuscomicm * w_inf.qtdate
                      w_inf.cussemicm = w_inf.cussemicm * w_inf.qtdate.
            end.
         
            case mov_est:
               when -1 then do:                            /*Sai do estoque*/
                  run mov_estoque( input-output mov_est,
                                   input-output qtd_rsv,
                                   input-output qtd_rea,
                                   input-output qtd_atu,
                                   input-output qtd_dan
                                 ).
                  if entry(1,return-value) <> "ok" then do:
                     /*if f_log(string(w_inf.codgrp) + 
                              string(w_inf.codprd) + "\t" +
                              string(mov_est) + "\t" +
                              string(qtd_rsv) + "\t" +
                              string(qtd_rea) + "\t" +
                              string(qtd_atu) + "\t" +
                              string(v_msg),
                            input-output v_msg) then do: end.                */ 
                     undo t_estoque, return "UNDO,estoque".
                  end.
               end.
               when  1 then do:                            /*Entra no estoque*/ 
                  run mov_estoque( input-output mov_est,
                                   input-output qtd_rsv,
                                   input-output qtd_rea,
                                   input-output qtd_atu, 
                                   input-output qtd_dan
                                 ).
                  if entry(1,return-value) <> "ok" then do:
                     /* if f_log(string(w_inf.codgrp) + 
                              string(w_inf.codprd) + "\t" +
                              string(mov_est) + "\t" +
                              string(qtd_rsv) + "\t" +
                              string(qtd_rea) + "\t" +
                              string(qtd_atu) + "\t" +
                              string(v_msg),
                              input-output v_msg) then do: end.*/
                     undo t_estoque, return "UNDO,estoque".
                  end.   
               end.
            end.
            find current {&est} no-error.
            if qtd_rsv <> 0 
            or qtd_rea <> 0 then do:
               undo t_estoque, return "undo,estoque".
            end.
            if qtd_rsv = 0 and qtd_rea = 0 and rsv_mat = 0 then leave A_EST.
            if rsv_mat <> 0 and w_inf.ridestm <> ? then do:
               assign rid_aux = w_inf.ridestm
                      qtd_rsv = rsv_mat
                      rsv_mat = 0.
               next A_EST.
            end.   
            undo t_estoque, return "undo".
         END.                                                         /*A_EST*/
         assign var_waio = 1.
      END.                                                   /*for each w_inf*/
      for first {&prd} no-lock: end.
      for first {&gsz} no-lock: end.
      for first {&est} no-lock: end.
      leave t_estoque.
   end.      /*t_estoque*/                                     /*transaction*/
   return "ok".
end procedure.                                                       /*A_ITE*/
procedure mov_estoque:
   def input-output param mov_est   as int.
   def input-output param qtd_rsv   as dec decimals 2 initial 0.
   def input-output param qtd_rea   as dec decimals 2 initial 0.
   def input-output param qtd_atu   as dec decimals 2 initial 0.
   def input-output param qtd_dan   as dec decimals 2 initial 0.

   def var rsp     as cha no-undo.

   /***************************************************************************
    Processo de transferencia p/ FILIAL.
    1- Ao efetivar transferencia Matriz-Filial
       - Subtrai qtdestrea e qtdestrsv do estoque MATRIZ
    2- Ao efetivar entrada da transferencia na FILIAL
       - Soma qtdestrea e qtdestrsv no estoque da FILIAL
    3- Ao efetivar o faturamento para os clientes
       - Subtrai qtdestrea e qtdestrsv do estoque da FILIAL
   ***************************************************************************/

   repeat on error  undo, return "undo"
          on endkey undo, return "undo"
          on stop   undo, return "undo"
          on quit   undo, return "undo":
      if {&est}.qtdestrea > 0 then do:
         assign {&est}.premed    = {&est}.premed 
                                 * ({&est}.qtdestrea + qtd_rea)
                                 / {&est}.qtdestrea
                {&est}.premedicm = {&est}.premedicm 
                                 * ({&est}.qtdestrea + qtd_rea) 
                                 / {&est}.qtdestrea.
      end.
      assign {&est}.qtdestrea = {&est}.qtdestrea + qtd_rea
             {&est}.qtdatu    = {&est}.qtdatu    + qtd_atu
             {&est}.qtdatu    = (if {&est}.qtdatu <= 0
                                    then {&est}.qtdatu
                                    else 0
                                )
             {&est}.qtddan    = {&est}.qtddan + qtd_dan
             {&est}.qtddan    = (if {&est}.qtddan < 0 
                                    then 0 
                                    else {&est}.qtddan
                                )
             {&est}.qtdpad    = (if lookup({&ped}.tipped,"PIC") > 0
                                    then 0 
                                    else {&est}.qtdpad
                                ).
      assign {&est}.qtdssi = {&est}.qtdssi
                           - (if {&cfo}.FlgAtuQtdEstExt
                                 then qtd_rea else 0
                             ).
      if {&est}.qtdestrea < 0 
      or {&est}.qtdsap + qtd_rea < 0 then do:
         assign rsp = "I\nProduto: " 
                    + trim(caps(replace({&prd}.desprd,","," ")))
                    + " Codigo: "
                    + string({&prd}.codgrp,"99")     + "."
                    + string({&prd}.codprd,"999")    + "-"
                    + string({&prd}.dveprd,"9")      + "."
                    + string({&prd}.coduni)          + "."
                    + (if avail {&ped} 
                          then "@Pedido: " + 
                               string({&ped}.asdped,">9999,99999") + "."
                          else ""
                      )
                    + " Estoque insuficiente na empresa "
                    + string({&est}.empctb).
         {&hm}. message rsp.
         run trg/trgclrsp.p (input-output rsp, false).
         undo, return "undo,estoque".
      end.

      IF LOOKUP(f_tpd.tipped,"PFE,PIC") > 0 and 
         w_not.codclifor <> 587 THEN DO:

         IF {&est}.qtdpad > 0 THEN DO:
            ASSIGN {&est}.qtdpad = {&est}.qtdpad + qtd_rea
                   qtd_rea       = 0.
            IF {&est}.qtdpad < 0 THEN
               ASSIGN qtd_rea       = {&est}.qtdpad
                      {&est}.qtdpad = 0.
         END.
         ASSIGN {&est}.qtddan = {&est}.qtddan + qtd_rea
                {&est}.qtddan = (if {&est}.qtddan < 0 
                                    then 0 
                                    else {&est}.qtddan
                                ).
      END.

      assign qtd_rea          = 0
             qtd_atu          = 0.
      assign {&est}.qtdestrsv = {&est}.qtdestrsv 
                              + (if lookup(f_tpd.tipped,"VOM,VOP") > 0
                                 then 0 else qtd_rsv) /*GLPI 26019*/
             {&est}.premed    = if {&est}.qtdestrea > 0 and {&est}.premed > 0
                                   then {&est}.premed 
                                   else 0
             {&est}.premedicm = if {&est}.qtdestrea > 0 and 
                                   {&est}.premedicm > 0
                                   then {&est}.premedicm 
                                   else 0
             {&est}.qtdestrsv = if {&est}.qtdestrsv < 0 
                                   then 0
                                   else {&est}.qtdestrsv.
      if {&est}.qtdestrsv < 0 and {&est}.codemp =  3   then do:
         assign {&est}.qtdestrsv = 0.
      end.
      if {&est}.qtdestrsv < 0 and qtd_rsv < 0 then do:
         assign rsp = "I\nProduto: " 
                    + trim(caps(replace({&prd}.desprd,","," ")))
                    + " Codigo: "
                    + string({&prd}.codgrp,"99")     + "."
                    + string({&prd}.codprd,"999")    + "-"
                    + string({&prd}.dveprd,"9")      + "."
                    + string({&prd}.coduni)          + "."
                    + (if avail {&ped} 
                          then "@Pedido: " + 
                               string({&ped}.asdped,">9999,99999") + "."
                          else ""
                      )
                    + " Reservado ficaria negativo".
         run trg/trgclrsp.p (input-output rsp, false).
         undo, return "undo,estoque".
      end.

      if {&est}.qtdestrea - abs({&est}.qtdestrsv) < 0 then do:
         assign rsp = "separador=;,I\nProduto: " 
                    + trim(caps(replace({&prd}.desprd,","," ")))
                    + " Codigo: "
                    + string({&prd}.codgrp,"99")     + "."
                    + string({&prd}.codprd,"999")    + "-"
                    + string({&prd}.dveprd,"9")      + ", "
                    + string({&prd}.coduni)          + ", E "
                    + string({&est}.empctb)
                    + (if avail {&ped} 
                          then "@Pedido: " + 
                               string({&ped}.asdped,">9999,99999") + "."
                          else ""
                      )
                    + " Saldo ficaria negativo "
                    + string({&est}.qtdestrea) + " " 
                    + string({&est}.qtdestrsv).
         pause 1 no-message.
         run trg/trgclrsp.p (input-output rsp, false).
         undo, return "undo,estoque".
      end.
      assign qtd_rsv          = 0.
      leave.
   end.
   return "ok".
end procedure.                                                  /*mov_estoque*/
procedure cria_txt:
  def var rid_txt as row no-undo.
  def var num_seq_txt as int initial 1.
  
  txt:
  repeat transaction on error  undo txt, return "undo"
                     on endkey undo txt, return "undo"
                     on stop   undo txt, return "undo"
                     on quit   undo txt, return "undo":
    find first basecliente of {&cli} no-lock no-error.
    for last  textoirregularidade no-lock 
        where textoirregularidade.codbascli = basecliente.codbascli
        and   (if textoirregularidade.destxtirr[1] =
                   "ORGAO PUBLICO - AVISAR CONTABILIDADE"
               then true else false):
      leave txt.
    end.
    for last  textoirregularidade no-lock 
        where textoirregularidade.codraz = basecliente.codraz
        and   (if textoirregularidade.destxtirr[1] =
                   "ORGAO PUBLICO - AVISAR CONTABILIDADE"
               then true else false):
      leave txt.
    end.

    find last  textoirregularidade exclusive-lock
         where textoirregularidade.codbascli = basecliente.codbascli 
               no-wait no-error. 
    if locked textoirregularidade then do:
       for last  textoirregularidade no-lock
           where textoirregularidade.codbascli = basecliente.codbascli:
          assign rid_txt = rowid(textoirregularidade).    
       end.
       assign rsp = "textoirregularidade,rowid,W,R,"
                  + "Texto irregularidade bloqueado". 
       run trg/trgrgblo.p ( string( rid_txt ) , rsp). 
       if return-value matches "*liberou" then next txt.
       assign rsp = return-value + ",Tecle <A>bortar <T>entar,A,T".
       run trg/trgclrsp.p (input-output rsp, false).
       if rsp = "A" then undo txt, return "UNDO".
       undo txt, return "retry".
    end.
    if avail textoirregularidade then 
      assign num_seq_txt = textoirregularidade.numseqtxtirr + 1.
 
    create textoirregularidade.  
    assign textoirregularidade.codbascli    = basecliente.codbascli
           textoirregularidade.numseqtxtirr = num_seq_txt
           textoirregularidade.datmovtxt    = to_day
           textoirregularidade.codpesirr    = 3
           textoirregularidade.nomres       = "SCI"
           textoirregularidade.destxtirr[1] = 
                                       "ORGAO PUBLICO - AVISAR CONTABILIDADE".
    leave txt.
  end.
  return "ok".
end procedure.                                                     /*cria_txt*/
procedure R_EMITE:
   def var tip_emi as cha no-undo init "".
   def var flg_gnr as logical no-undo.
   def var rec_ini like notafiscal.recnotfis no-undo.
   def var rec_fin like notafiscal.recnotfis no-undo.
   def var cod_emp as int no-undo.
   
   if not can-find(first i_not no-lock) then do: {&hm}.
      if f_msg ( "NENHUMA NOTA A EMITIR") then do: end.
      return.
   end. 

   
   for each i_not no-lock by i_not.recnotfis:  
      disp i_not.numnot @ w_trn.notinf with frame f_1.
      leave.
   end.
   for each  i_not no-lock 
       where i_not.numnot <> input frame f_1 w_trn.notinf
             by i_not.recnotfis desc:
      disp "a"          @ a_tel
           i_not.numnot @ w_trn.notsup with frame f_1.
      leave.
   end.
   empty temp-table w_cce no-error.
   empty temp-table w_sai no-error.
   empty temp-table w_vjet no-error.

   for each w_vjet no-lock: delete w_vjet. end.
   
   for first f_ectb no-lock where rowid(f_ectb) = rowid({&ectb}): end.
   create w_sai. find current w_sai no-lock.
   assign cap_tnf     = 0
          nro_cce     = 0
          qtd_cce     = 1
          tot_cce     = 0
          rec_ini     = 0
          rec_fin     = 0
          cod_emp     = 0
          flg_gnr     = false
          seq_not     = 0
          num_seq_imp = 0.
   
   /**GLPI 3940 WALMAR******************************************************/
   if w_trn.tipo = "PACOTE" then do:
   
      for first pacote no-lock 
          where pacote.codpac = int(w_trn.codpac:screen-value),
          each  carga of pacote no-lock,
          each  notafiscal of carga no-lock
         /*where notafiscal.codvld = "100" */,
          first municipio of notafiscal no-lock:
           
          find first munbalsa no-lock 
               where munbalsa.cdibge = municipio.cdibge
               and   munbalsa.situacao = "A" no-error.

          if avail munbalsa then do:     
             for first representante of notafiscal no-lock,
                 first gercob of representante no-lock:
             end.
             find first pacbalsa no-lock 
                  where pacbalsa.codpac = w_trn.codpac no-error.
             if not avail pacbalsa then do:     
                run TR/MN/nfbalsa.p(notafiscal.recnotfis,munbalsa.empresa,
                                   "INC","","",gercob.codgercob).
             end.
             else do:
                run TR/MN/nfbalsa.p(notafiscal.recnotfis,pacbalsa.empresa,
                                   "INC","","",gercob.codgercob).
             end.
          end.
      end.
      /**WALMAR****************************************************************/
   end.

   for each i_not no-lock
      ,each {&not} no-lock 
       where rowid({&not}) = i_not.id_not
       by {&not}.recnotfis
       by {&not}.numnotfis:
      if {&not}.codtrn = 0 and w_trn.codtrn <> 0 then do:
         run a_transporte ( {&not}.recnotfis, w_trn.codtrn ).
         if return-value <> "ok" and
            f_msg ("NOTA FISCAL " + string({&not}.numnotfis) 
                                  + " NAO EMITIDA"
                  ) then do: next.
         end.
      end.
       
      if tip_emi = "" then do: 
         run nfs/r/nfsrpcfo.p ( {&not}.codcfo, {&not}.datmov, buffer {&cfo} ). 
         assign tip_emi = string({&cfo}.indtiptrs = "T","T/P").
      end.
      
      assign rec_ini = (if rec_ini = 0 then {&not}.recnotfis else rec_ini)
             rec_fin = {&not}.recnotfis
             cod_emp = {&not}.codemp.

      if lookup(string({&not}.codest,"99"),"52,53") > 0
         then flg_gnr = true.

      if {&not}.codcfo = 615201 or 
         ({&not}.codcfo = 615271 and {&not}.empctb = 3) or
         ({&not}.codcfo = 615261 and {&not}.empctb = 8)
          then flg_gnr = true.
      assign msg_fat:screen-value in frame f_1 =
             "Gerando........: " + string(i_not.numnot,">>>>>>>>9")
             seq_not         = seq_not + 1
             cap_inf         = 0.
      if not connected("my2000") then run trg/trgcbase.p ("my2000").
      
      run nfs/e/nfse_not.p ( buffer {&not},
                             buffer w_trn,
                             buffer w_htri,
                             buffer w_sai,
                             input-output num_seq_imp
                           ) no-error.

      if entry(1,return-value) <> "ok"
      or error-status:error                 
      or seq_not <> w_sai.numseqpac then do:
         empty temp-table i_not no-error.
         empty temp-table w_cce no-error.
         empty temp-table w_sai no-error.
         disp "" @ msg_fat with frame f_1.
         color display normal msg_fat with frame f_1.
         output stream s_rel1 close.
         output stream s_rel2 close.
         output stream s_rel3 close.
         if avail w_sai then do:
            if w_sai.sairel1 <> "" then os-delete value(w_sai.sairel1).
            if w_sai.sairel2 <> "" then os-delete value(w_sai.sairel2).
            if w_sai.sairel3 <> "" then os-delete value(w_sai.sairel3).
         end.
         run nfs/e/nfse_sin.p ( input-output num_seq_imp ).
         undo, return "undo".
      end.
      if {&not}.datemi >= 04/01/2011 and {&cfo}.FlgNumTBS and
         lookup({&not}.sitnfe,"") > 0                     then do:
         run envia_nfe ( buffer {&not} ).
      end.
      disp {&not}.numnot @ w_trn.notsup with frame f_1. 
      /* Gera comprovante de entrega para todas as N.F. */ 
      if not w_sai.cgcdes begins "17.359.233/" then do:
         create w_cce. 
         buffer-copy w_sai to w_cce assign
                  i_not.nrocce = i_not.nrocce + 1. 
      end.
      
      /****** COMENTADO 08/09/2008 (FORMA ANTIGA) *************************
      if not w_sai.strpgt begins "A PRAZO" and w_sai.strpgt <> "" then do:
         create w_cce. 
         buffer-copy w_sai to w_cce assign
                     i_not.nrocce = i_not.nrocce + 1. 
      end. 
      *********************************************************************/
      
      for first {&pnfe} no-lock 
          where {&pnfe}.empctb = {&not}.empctb
         ,first {&stae} no-lock
          where {&stae}.empctb = {&not}.empctb
          and   {&stae}.stadst  = w_sai.staemi
          and   {&stae}.datter >= {&not}.datmov:
      end.
      find {&sta} of {&not} no-lock no-error.
      if {&sta}.FlgRegEsp then do:
        for first {&cli} no-lock
            where {&cli}.codcli = {&not}.codclifor:
          assign cap_inf = cap_inf + 
                     (if {&cli}.flgcre and {&sta}.sigest = "BA" then 0 else 1).
          if {&sta}.sigest <> "TO" and {&sta}.sigest <> "AL" and 
             not {&cli}.flgcre then do:
            assign cap_inf = cap_inf + 1.          
          end.
          if {&sta}.sigest = "AL" then assign cap_inf = cap_inf + 1.          
        end.
      end.
      /*Restricao produto covid 19 - Alberto 13/04/2020*/
      
      /*run nfs/v/rstprd.p({&not}.recnotfis).*/
      
      for last ItemNF of {&not} no-lock:
          assign cap_inf = cap_inf + {&inf}.numitenotfis.
      end.
      assign cap_inf = truncate(cap_inf / {&pnfe}.maximpsai,0) 
                     + (if cap_inf modulo {&pnfe}.maximpsai <> 0 then 1 else 0).
      assign cap_tnf = cap_tnf + cap_inf.
      
   end.
   disconnect my2000 no-error.
   for each w_cce no-lock:
       assign tot_cce = tot_cce + 1. 
   end. 
   if can-find(first w_vjet) then do:
      run emite_vjet.
   end.
   /* Gera comprovante de entrega na opcao emitir */ 
   if caps(entry(3,par_in)) = "E" then do: 
      for each w_cce no-lock 
          where (if w_cce.strped <> "" then true else false):
          run nfs/e/nfse_cce.p (buffer w_cce, 
                                input-output nro_cce, 
                                1, 
                                qtd_cce,
                                tot_cce).
          assign qtd_cce = qtd_cce + 1.       
      end. 
   end.

   find first w_sai no-lock no-error. 
   if w_sai.sairel1 <> "" then output stream s_rel1 close.
   if w_sai.sairel2 <> "" then output stream s_rel2 close.
   if w_sai.sairel3 <> "" then output stream s_rel3 close.
   if v_cce <> "" then do:
      run nfs/e/nfse_cce.p (buffer w_cce, 
                           input-output nro_cce, 
                           2, 
                           qtd_cce,
                           tot_cce).
   end.
   if w_sai.sairel1 <> "" then do:
      assign rsp = "Arquivos: " 
                 + trim(w_sai.sairel1) + " " 
                 + trim(w_sai.sairel2) + " " 
                 + trim(w_sai.sairel3) 
                 + ",Tecle im<P>rimir "
                 + "<V>oltar,V,P".
   end. else do:
      assign rsp = "Arquivos gerados,Tecle <V>oltar,V,end-error".
   end.
   assign msg_fat:screen-value in frame f_1 = "".
   if {&stae}.tipant = "D" then do:
   end.
   if flg_gnr /*and entry(3,par_in) = "F"*/ then do:
     /*cotta 13/10/2016
     assign dat_ven = (if to_day <= 04/24/2013 then 04/24/2013 else to_day + 3)
            dat_ven = if weekday(dat_ven) = 7 then dat_ven + 2
                      else
                      if weekday(dat_ven) = 1 then dat_ven + 1
                      else dat_ven.
     repeat while year(dat_ven) > year(to_day) or
            year(dat_ven) = year(to_day) and month(dat_ven) > month(to_day):
        assign dat_ven = dat_ven - 1.
     end.
     assign dat_ven = today.
     assign par_prg = "CN/LS/gnrbgr1.p[gnr,,,,80," 
                    + "E" /*Emissao*/
                    + ","
                    + string(w_trn.codpac) 
                    + "," 
                    + string(rec_ini) 
                    + "," 
                    + string(rec_fin) 
                    + "," 
                    + string(cod_emp)
                    + ","  
                    + string(dat_ven,"99/99/9999").
     run LB/PRG/CHMrel-1.p.
     */
     
     /* cotta 14/08/18
     assign Par_Prg = "CN/LS/vbasfat.p,,,,80," + 
                       string(w_trn.CodPac,"9999999").
     if w_trn.CodPac <> 0 then run LB/PRG/CHMrel-1.p.
     */
   end.
   /* Capa de lote do pacote */ 
   if w_trn.tipo = "PACOTE" then do:
      run nfs/e/nfse_clt2.p ( buffer w_sai, buffer w_trn).
      assign opc_prg = "E,I"
                     + "/<E>mitir <I>gnorar"
                     + "/Gerando relacao entrega pacote "
                     +  string(w_trn.CodPac,"9999999")
                     + "/Escolha uma das opcoes"
                     + "/E".
             par_prg = "MT/LG/AR/mtlgar80.p,B,,,80," + tip_emi + ","
                     + string(w_trn.CodPac,"9999999").
      run LB/PRG/CHMrel.p.
   end.           
   empty temp-table i_not no-error.
   for each i_not no-lock while error-status:error:
      delete i_not validate(true,"").
   end.
   run trg/trgclrsp.p (input-output rsp, false).
   assign w_sai.sairel1 = "" w_sai.sairel2 = "" w_sai.sairel3 = "".
   return "ok".
end procedure.                                                      /*R_EMITE*/
procedure a_transporte:
   def input param rec_not as int no-undo.
   def input param cod_trn as int no-undo.
   def buffer b_not for {&not}.
   a_transporte:
   repeat transaction on error  undo a_transporte, return "UNDO"
                      on endkey undo a_transporte, return "UNDO"
                      on stop   undo a_transporte, return "UNDO"
                      on quit   undo a_transporte, return "UNDO":
      find first b_not exclusive-lock
           where b_not.recnotfis = rec_not no-wait no-error.
      if locked b_not then do:
         run trg/trgrgblo.p (string(rec_not),
                             "{&not},recnotfis,I,R,Nota fiscal bloqueada").
         if return-value matches "*liberou" then next a_transporte.
         assign rsp = return-value + ",Tecle <A>bortar <T>entar,A,T".
         run trg/trgclrsp.p (input-output rsp, false).
         if rsp = "A" then undo a_transporte, return "UNDO".
         undo a_transporte, retry a_transporte.
      end.
      assign b_not.codtrn = cod_trn.
      leave a_transporte.
   end.   
   find first b_not no-lock where b_not.recnotfis = rec_not no-error.
   return "ok".   
end procedure.                                                 /*a_transporte*/

procedure envia_nf_fat:
   for each  nf_fat no-lock:
      for first {&not} no-lock
          where {&not}.recnotfis = nf_fat.recnotfis
          and   (if {&not}.datemi >= 04/01/2010 {&tru} )
          and   (if lookup({&not}.sitnfe,"") > 0 {&tru} )
         ,last  {&cfo} no-lock
          where {&cfo}.codcfo    = {&not}.codcfo
          and   {&cfo}.datultatu <= {&not}.datemi
          and   (if {&cfo}.datter >= {&not}.datemi {&tru})
          and   (if {&cfo}.FlgNumTBS {&tru}):
         run envia_nfe ( buffer {&not} ).
      end.
      delete nf_fat no-error.
   end.
end procedure.                                                 /*envia_nf_fat*/
procedure envia_nfe:
   def param buffer b_not for {&not}.
   if b_not.sitnfe <> "" 
   or lookup(entry(1,sta_nfe),"undo") > 0 then return.
   
   /*
   if sta_nfe = "" then do:
      run rt/ctrl_cnx.p ( "srv_nfe", 1 ).
      assign sta_nfe = return-value.
      if lookup(entry(1,sta_nfe),"undo") > 0 and 
         f_msg( {&auto} +
         "NOTAS FISCAIS NAO ENVIADAS PARA O SERVIDOR DA NF-E") then do:
      end.
   end.
   */

   find first pargernfes no-lock no-error. 
   if available pargernfes  and pargernfes.flagvalfat then do:
      if not connected( "correio" ) 
         then run trg/trgcbase.p ("correio") no-error.
      if not connected( "dne" )     
         then run trg/trgcbase.p ( "dne" ) no-error.
      if not connected( "correio" ) and
         f_msg ( {&auto} + " BANCO CORREIO NAO CONECTADO") then return "ok".
      if not connected( "dne" ) and
         f_msg ( {&auto} + " BANCO DNE NAO CONECTADO") then return "ok".
      run nfes/nfes001.p (b_not.empctb,"A",b_not.recnotfis) no-error.
      disconnect correio no-error.
      if error-status:error and
         f_msg ( {&auto} +
                 "ENVIO DE NOTA PARA A NF-e FALHOU\n" +
                 "IDNOTA: " + string(b_not.recnotfis) ) then do:
      end.           
   end.
   return "ok".
end procedure.                                                    /*envia_nfe*/
procedure emite_vjet:   
   def var nr_vjet as int no-undo.
   def var str_not as cha no-undo.
   def buffer bw_vjet for w_vjet.
   def var par_vjt as cha no-undo.
   def var mail_to as cha no-undo.
   assign mail_to = "-x4 "
                  + "-tmailto"
                  + ":mail.tambasa.com.br"             /*smtp_server*/
                  + ":faturamento"                     /*smpt_user*/
                  + ":25"                              /*port*/
                  + ":SISTEMA_TAMBASA"                 /*nome_remetente*/
                  + ":sistema@tambasa.com.br"          /*email_remetente*/
                  + ":rsalgado@tambasa"                /*nome_destinatario*/
                  + ":rsalgado@tambasa.com.br"         /*email_destinatario*/
                  + ":~"NOTA FISCAL TAMBASA~""         /*subject*/
                  + ":~"ANEXO COPIA NF NRO~""          /*message*/
                  .
   
   assign par_vjt = "@COMMAND@ print "
                  + "-pc:~\VisualJet~\PROJETOS~\TAMBASA~\"
                  + "nf716412009~\nf716412009.vjr"
                  + "-sN -yS -kS -vS "
                  .
   output stream s_rel1 to value(w_sai.sairel1) unbuffered page-size 0.
   for each  w_vjet no-lock
       where (if w_vjet.numvia < 3 then true else false)
             break by w_vjet.numseqpac 
                   by w_vjet.nrofol
                   by w_vjet.numvia:
                   
       assign nr_vjet = nr_vjet + 1.
       if first(w_vjet.numvia) then do:
          assign par_vjt = "@COMMAND@ print "
                         + "-pc:~\VisualJet~\PROJETOS~\TAMBASA~\"
                         + entry(1,w_vjet.nommsc,".") + "~\"
                         + w_vjet.nommsc 
                         + " -sN -yS -kS -vS ".
          /*if Userid("Dictdb") = "salgado" and mail_to <> "" then do:
             assign par_vjt = replace(par_vjt,"print","export")
                            + " " + mail_to.
          end.
          */
          put stream s_rel1 unformatted par_vjt skip.
       end.
       put stream s_rel1 unformatted
          (if nr_vjet mod 2 = 1 then "@BEGIN_PAGE PAGE1\n" else "") +
          "@BEGIN_RECORD " + (if nr_vjet mod 2 = 1 then "1" else "2") skip
          w_vjet.imgnot                                               skip
          "@END_RECORD\n"  + 
          (if nr_vjet mod 2 = 1 then "" else "@END_PAGE\n").
            
       if last(w_vjet.numseqpac) and nr_vjet mod 2 = 1 then do:
          put stream s_rel1 unformatted
              "@BEGIN_RECORD 2\n@END_RECORD\n@END_PAGE" skip.
       end.
   end.
   output stream s_rel1 close.
   
   assign par_vjt = replace(par_vjt,"n1st","n3st").
   output stream s_rel1 to value(replace(w_sai.sairel1,"/n1st","/n3st")) 
                           unbuffered page-size 0.
   for each  w_vjet no-lock
       where (if w_vjet.numvia >= 3 then true else false)
             break by w_vjet.numvia
                   by w_vjet.numseqpac 
                   by w_vjet.nrofol:
       assign nr_vjet = nr_vjet + 1.
       if first(w_vjet.nrofol) then do:
          put stream s_rel1 unformatted par_vjt skip.
       end.
       put stream s_rel1 unformatted
          (if nr_vjet mod 2 = 1 then "@BEGIN_PAGE PAGE1\n" else "") +
          "@BEGIN_RECORD " + (if nr_vjet mod 2 = 1 then "1" else "2") skip
          w_vjet.imgnot                                               skip
          "@END_RECORD\n"  + 
          (if nr_vjet mod 2 = 1 then "" else "@END_PAGE\n").
            
       if last(w_vjet.numseqpac) and nr_vjet mod 2 = 1 then do:
          put stream s_rel1 unformatted
              "@BEGIN_RECORD 2\n@END_RECORD\n@END_PAGE" skip.
       end.
       if Userid("Dictdb") = "salgadoo" then do:
          run nfs/e/nfse_aph.p ( w_vjet.imgnot, w_sai.sairel1 ).
       end.
   end.
   output stream s_rel1 close.
   assign w_sai.sairel1 = "".
end procedure.                                                   /*emite_vjet*/
procedure a_w_ctr:
   def input param emp_ctb as int no-undo.
   def param buffer b_not for {&not}.
   if can-find(first w_ctr no-lock where w_ctr.codaux = emp_ctb) then return.
   for first ctrapoio no-lock
       where ctrapoio.ctrapo = b_not.ctrapo
       and   not can-find(first w_ctr no-lock
                          where w_ctr.codaux = ctrapoio.ctrapo):
      create w_ctr.
      assign w_ctr.codaux = ctrapoio.empctb
             w_ctr.ctrapo = ctrapoio.ctrapo.
      return.       
   end.    
   create w_ctr.
   assign w_ctr.codaux = emp_ctb
          w_ctr.ctrapo = 0.
   return.   
end procedure.                                                      /*a_w_ctr*/
procedure g_w_ctr:
   def input param emp_ctb as int no-undo.
   for first w_ctr no-lock
       where w_ctr.codaux = emp_ctb:
      return " ok" . 
   end.
   if not sml_fat then run hlp/hlp/hlpctrapo.p ( emp_ctb ).
   if sml_fat or return-value = "" then do:
      create w_ctr.
      assign w_ctr.codaux = emp_ctb
             w_ctr.ctrapo = 0.
      return "ok".
   end.
   if return-value = "?" then return "undo".
   do transaction:
      create w_ctr.
      assign w_ctr.codaux = emp_ctb
             w_ctr.ctrapo = int(entry(1,return-value,";" ))
             w_ctr.endloc = entry(2,return-value,";" )
             w_ctr.bailoc = entry(3,return-value,";" )
             w_ctr.munloc = entry(4,return-value,";" )
             w_ctr.staloc = entry(5,return-value,";" )
             w_ctr.ceploc = int(entry(6,return-value,";" )).
   end.
   find current w_ctr no-lock no-error.
   return "ok".
end procedure.                                                      /*g_w_ctr*/

procedure E_PACOTE:
   def input param emp_ctb as int no-undo.

   /*
   if f_log ( string(w_trn.codpac,"ZZZZZZZZ") + " E_PACOTE",input-output v_msg
            ) then do:
   end.*/
   
   for each  {&car} no-lock
       where {&car}.codpac = w_trn.codpac
      ,first {&ped} of {&car} no-lock
       where (if {&ped}.flgpedati {&tru}):
      if {&ectb}.cnpj = {&emp}.cgcemp + {&emp}.cgccplemp and
         not can-find(first {&ipd} of {&ped} 
                      where {&ipd}.indsitite <> "T" and {&ipd}.qtdate > 0)
         then next.
      if f_msg ( "PEDIDO: "  + trim(string({&ped}.asdped,">9999,99999"))
                             + " NAO FATURADO"
               ) then do:
      end.
      return "undo".
   end.
   empty temp-table i_not no-error.
   for each i_not no-lock while error-status:error:
      delete i_not validate(true,"").
   end.
   
   for each  {&pac} no-lock 
       where {&pac}.codpac = w_trn.codpac
      ,each  {&car} of {&pac} no-lock 
       where {&car}.codemp = {&emp}.codemp
      ,each  {&not} of {&car} no-lock 
       where (if {&not}.flgclifor = entry(1,par_in) and
                 {&not}.flgnotfis = entry(2,par_in) and
                 {&not}.empctb    = emp_ctb
                 {&tru}):
      run g_i_not ( buffer {&not}, buffer i_not ).
   end.
   for each  {&trfc} no-lock
       where {&trfc}.codpac    = w_trn.codpac
       and   {&trfc}.empctbemi = w_trn.empctb
      ,first {&not} no-lock
       where {&not}.recnotfis = {&trfc}.recnotfisemi
       and   not can-find(first i_not no-lock 
                          where i_not.id_not = rowid({&not})):
      run g_i_not ( buffer {&not}, buffer i_not ).
   end.
   run R_EMITE.
   assign rtn_val = return-value.
   /*
   if f_log ( string(w_trn.codpac,"ZZZZZZZZ") + " E_PACOTE__",
              input-output v_msg
            ) then do:
   end.*/
   return trim(rtn_val).
end procedure.                                                     /*E_PACOTE*/
procedure E_CARGA:
   def input param emp_ctb as int no-undo.
   empty temp-table i_not no-error.
   for each i_not no-lock while error-status:error:
      delete i_not validate(true,"").
   end.   
   for each  {&car} no-lock 
       where {&car}.codcar = w_trn.codcar
      ,each  {&not} of {&car} no-lock 
       where (if {&not}.flgclifor = entry(1,par_in) and
                 {&not}.flgnotfis = entry(2,par_in) and
                 {&not}.empctb    = emp_ctb
                 {&tru}
             ):
      run g_i_not ( buffer {&not}, buffer i_not ).
   end.
   RUN R_EMITE.
   return trim(return-value).
end procedure.                                                      /*E_CARGA*/
procedure E_NOTA:
   def input param emp_ctb as int no-undo.
   empty temp-table i_not no-error.
   for each i_not no-lock while error-status:error:
      delete i_not validate(true,"").
   end.   
   for each  {&not} of {&emp} no-lock 
       where {&not}.datemi    = w_trn.datemi  
       and   {&not}.numnotfis >= w_trn.notinf 
       and   {&not}.numnotfis <= w_trn.notsup 
       and   (if {&not}.flgclifor = entry(1,par_in) and
                 {&not}.flgnotfis = entry(2,par_in) and
                 {&not}.empctb    = w_trn.empctb
                 {&tru}
              ):
      run g_i_not ( buffer {&not}, buffer i_not ).
   end.
   run R_EMITE.
   return trim(return-value).
end procedure.                                                       /*E_NOTA*/
procedure colhe_rsp:
   assign var_waio = (if var_waio >= int(os-getenv("_VARWAI")) 
                         then 1
                         else var_waio
                     )
          var_wai  = var_waio
          var_waio = var_waio + (if var_waio >= int(os-getenv("_VARWAI"))
                                    then 0 
                                    else 1
                                ).    

   if flg_aut or sml_fat then do:
      assign entry(1,rsp,";") = ""
             entry(2,rsp,";") = ""
             entry(3,rsp,";") = "".
      repeat while rsp begins ";": assign rsp = substr(rsp,2). end.        
      assign rsp = entry(1,rsp,";").
      pause 1 no-message.
   end. else do:
      run trg/trgclrsp.p (input-output rsp, false).
      assign var_wai = int(os-getenv("_VARWAI")).
   end.
end procedure.                                                    /*colhe_rsp*/
procedure a_r_v:
   def input param r_v as cha no-undo.
   return trim(r_v).
end procedure.                                                        /*a_r_v*/
procedure D_F1:
   disp w_trn.tipo with frame f_1.
   assign rsp = lower(substr(w_trn.tipo:screen-value in frame f_1,1,1))
          w_trn.codpac = (if {&te} = "PACOTE" then w_trn.codpac else 0)
          w_trn.codcar = (if {&te} = "CARGA"  then w_trn.codcar else 0)
          w_trn.datemi = (if {&te} = "NOTA"   then w_trn.datemi else ?)
          w_trn.notinf = (if {&te} = "NOTA"   then w_trn.notinf else 0)
          w_trn.notsup = (if {&te} = "NOTA"   then w_trn.notsup else 0)
          w_trn.codpac:visible in frame f_1 = ({&te} = "PACOTE")
          w_trn.codcar:visible in frame f_1 = ({&te} = "CARGA")
          w_trn.notinf:visible in frame f_1 = ({&te} = "NOTA")
          w_trn.notsup:visible in frame f_1 = ({&te} = "NOTA")
          w_trn.datemi:visible in frame f_1 = ({&te} = "NOTA")
          a_tel:visible        in frame f_1 = ({&te} = "NOTA")
          a_tel                         = (if {&te} = "NOTA" then "A" else "")
          rep_car                       = ""
          dsc_cfo:dcolor in frame f_1 = (if dsc_cfo = "" then 0 else 2)
          w_trn.notref:visible in frame f_1 = avail {&car} and {&car}.flgpedhor
          w_trn.qtdemb:visible in frame f_1 = avail {&car} and {&car}.flgpedhor.

   disp w_trn.tipo    
        dsc_cfo
        w_trn.codemp {&emp}.nomemp  when w_trn.codemp > 0   and avail {&emp}
        w_trn.empctb {&ectb}.nomfts when w_trn.empctb > 0   and avail {&ectb}
        w_trn.codfre {&tfr}.desfre  when w_trn.codfre <> "" and avail {&tfr}
        w_trn.codtrn  
        w_trn.endtrn  
        w_trn.nomtrn  
        w_trn.muntrn  
        w_trn.stapla {&sta}.nomest when w_trn.stapla <> "" and avail {&sta}
        w_trn.plavei  
        w_trn.codpac  when ({&te} = "PACOTE")
        w_trn.codcar  when ({&te} = "CARGA")
        rep_car       
        w_trn.datemi  when ({&te} = "NOTA")
        w_trn.notinf  when ({&te} = "NOTA")
        a_tel         when ({&te} = "NOTA")
        w_trn.notsup  when ({&te} = "NOTA")
        w_trn.datsai  
   with frame f_1.
end procedure.                                                         /*D_F1*/

procedure A_W_TRN:
   def param buffer b_emp  for {&emp}.
   def param buffer b_ectb for {&ectb}.
   def param buffer b_tfr  for {&tfr}.
   def param buffer b_trn  for {&trn}.

   for first {&car} no-lock 
       where {&car}.codcar = input frame f_1 w_trn.codcar:
      assign w_trn.codcar = {&car}.codcar
             w_trn.codemp = {&car}.codemp
             w_trn.tipo   = "CARGA"
             w_trn.codfre = if {&car}.codfre <> "" 
                               then {&car}.codfre else w_trn.codfre
             rep_car      = string({&car}.codemp) + "."
                          + string({&car}.coddiv) + "/"
                          + (if {&car}.codrepint > 0 
                                then (string({&car}.codrepint) + "-") else "")
                          + string({&car}.codrep).
      disp w_trn.codemp w_trn.codfre with frame f_1.
   end.
   for first {&emp} no-lock 
       where {&emp}.codemp = input frame f_1 w_trn.codemp:
   end.
   for first {&ectb} no-lock 
       where {&ectb}.empctb = input frame f_1 w_trn.empctb:
   end.
   for first {&tfr} no-lock 
       where {&tfr}.codfre = input frame f_1 w_trn.codfre:
      assign w_trn.codfre = {&tfr}.codfre
             w_trn.endtrn = {&tfr}.codObsTrn.       
   end.
   for first {&trn} no-lock 
       where {&trn}.codtrn = input frame f_1 w_trn.codtrn,
       first {&mun} of {&trn} no-lock,
       first {&sta} of {&trn} no-lock:
      assign w_trn.nomtrn = {&trn}.nomtrn.
      if avail {&tfr} and {&tfr}.codobstrn = "" then
         assign w_trn.endtrn = {&trn}.desend
                w_trn.plavei = {&trn}.numpla
                w_trn.muntrn = {&mun}.desmun
                w_trn.stapla = {&sta}.sigest.
      leave.
   end.
   for first {&sta} no-lock 
       where {&sta}.sigest = w_trn.stapla: 
   end.
   assign cod_emp      = w_trn.codemp
          emp_ctb      = w_trn.empctb
          frame f_1:title  = (if entry(3,par_in) = "E" 
                             then "   EMITIR" else "   GERAR"
                         )
                       + " NOTA FISCAL DE "
                       + (if entry(2,par_in) = "E" 
                             then "ENTRADA" else "SAIDA"
                       ) 
                       + " PARA "
                       + (if avail {&cfo} and {&cfo}.indtiptrs = "T"
                             then "TAMBASA" else
                             (if entry(1,par_in) = "C" 
                                then "CLIENTE" else "FORNECEDOR"
                             )
                         ) + "   "
                       + (if entry(3,par_in) = "E"
                             then ""
                             else "Fat: " + string(to_day)
                         ).
   run D_F1.
   disp {&te}             @ w_trn.tipo
        {&emp}.nomemp         when avail {&emp}
        {&ectb}.nomfts        when avail {&ectb}
        {&tfr}.desfre         when avail {&tfr}
        {&sta}.nomest         when avail {&sta} 
        rep_car
   with frame f_1.  
end procedure.                                                      /*A_W_TRN*/
procedure A_W_TRN_A:
   def param buffer b_emp  for {&emp}.
   def param buffer b_ectb for {&ectb}.
   def param buffer b_tfr  for {&tfr}.
   def param buffer b_trn  for {&trn}.
                           
   for first {&car} no-lock 
       where {&car}.codcar = w_trn.codcar
      ,first {&stc} of {&car} no-lock:
      if lookup(key_code({&car}.codsitcar),"102,116") > 0 and 
         not sml_fat then do:
         assign rsp = "Carga " + trim(string({&car}.codcar,">>>>>>>>>>>9"))
                    + (if keycode({&car}.codsitcar) = 102
                          then " sendo faturada."
                          else " sendo transferida."
                      ).
         return "undo;" + rsp.
      end.
      if not sml_fat and not {&stc}.flglibfat then do:
         assign rsp = "Carga " + trim(string({&car}.codcar,">>>>>>>>>>>9"))
                    + "nao liberada para faturar".
         return "undo;" + rsp.
      end.
      
      assign w_trn.codcar = {&car}.codcar
             w_trn.codemp = {&car}.codemp
             w_trn.tipo   = "CARGA"
             w_trn.codfre = if {&car}.codfre <> "" 
                               then {&car}.codfre else w_trn.codfre
             w_trn.codemb = "G"
             w_trn.codvia = "R"
             w_trn.datsai = today
             rep_car      = string({&car}.codemp) + "."
                          + string({&car}.coddiv) + "/"
                          + (if {&car}.codrepint > 0 
                                then (string({&car}.codrepint) + "-") else "")
                          + string({&car}.codrep).
   end.
   for first {&emp} no-lock 
       where {&emp}.codemp = w_trn.codemp:
   end.
   for first {&ectb} no-lock 
       where {&ectb}.empctb = w_trn.empctb:
   end.
   for first {&tfr} no-lock 
       where {&tfr}.codfre = w_trn.codfre:
      assign w_trn.codfre = {&tfr}.codfre
             w_trn.endtrn = {&tfr}.codObsTrn.       
   end.
   for first {&trn} no-lock 
       where {&trn}.codtrn = w_trn.codtrn,
       first {&mun} of {&trn} no-lock,
       first {&sta} of {&trn} no-lock:
      assign w_trn.nomtrn = {&trn}.nomtrn.
      if avail {&tfr} and {&tfr}.codobstrn = "" then
         assign w_trn.endtrn = {&trn}.desend
                w_trn.plavei = {&trn}.numpla
                w_trn.muntrn = {&mun}.desmun
                w_trn.stapla = {&sta}.sigest.
      leave.
   end.
   for first {&sta} no-lock 
       where {&sta}.sigest = w_trn.stapla: 
   end.
   assign cod_emp      = w_trn.codemp
          emp_ctb      = w_trn.empctb
          frame f_1:title  = (if entry(3,par_in) = "E" 
                             then "   EMITIR" else "   GERAR"
                         )
                       + " NOTA FISCAL DE "
                       + (if entry(2,par_in) = "E" 
                             then "ENTRADA" else "SAIDA"
                       ) 
                       + " PARA "
                       + (if avail {&cfo} and {&cfo}.indtiptrs = "T"
                             then "TAMBASA" else
                             (if entry(1,par_in) = "C" 
                                then "CLIENTE" else "FORNECEDOR"
                             )
                         ) + "   "
                       + (if entry(3,par_in) = "E"
                             then ""
                             else "Fat: " + string(to_day)
                         ).
   return "ok".
end procedure.                                                    /*A_W_TRN_A*/

procedure mta_parametro:
   assign entry(1,par_in) = caps(entry(1,par_in))
          entry(2,par_in) = caps(entry(2,par_in))
          entry(3,par_in) = caps(entry(3,par_in))
          cod_emp         = if num-entries(par_in) >= 4
                               then integer(entry(4,par_in)) 
                               else 0
          emp_ctb         = if num-entries(par_in) >= 5
                               then integer(entry(5,par_in)) 
                               else 0
          cod_cfo         = if num-entries(par_in) >= 6 
                               then integer(entry(6,par_in)) 
                               else 0
          cod_fre         = if num-entries(par_in) >= 7
                               then entry(7,par_in) 
                               else ""
          cod_car         = if num-entries(par_in) >= 8 
                               then integer(entry(8,par_in)) 
                               else 0
          cod_pac         = if num-entries(par_in) >= 9
                               then integer(entry(9,par_in)) 
                               else 0
          rec_not         = if num-entries(par_in) >=10
                               then integer(entry(10,par_in)) 
                               else 0
          tip_emi         = if cod_pac > 0 
                               then "PACOTE"
                               else if cod_car > 0 then "CARGA" 
                                                   else ""
          tip_emi         = if rec_not > 0 
                               then "NOTA" 
                               else tip_emi
          flg_aut         = (if num-entries(par_in) >= 12 and 
                                entry(12,par_in) = "PFE"
                                then true else false
                            )
          sml_fat         = (if num-entries(par_in) >= 12 and 
                                lookup(entry(12,par_in),"simula") > 0
                                then true else false
                            )
          asd_ped         = (if num-entries(par_in) >= 13 and
                                (flg_aut or sml_fat)
                                then int64(entry(13,par_in))
                                else 0
                            )
          no-error.

   for first {&ped} no-lock
       where {&ped}.asdped = asd_ped
      ,first {&car} of {&ped} no-lock:
      assign emp_ctb = {&ped}.empctb
             cod_car = {&ped}.codcar.
   end.

   for last  {&cfo}  no-lock where {&cfo}.codcfo    = cod_cfo: end.
   for first {&tfr}  no-lock where {&tfr}.codfre    = cod_fre: end. 
   for first {&pac}  no-lock where {&pac}.codpac    = cod_pac: end. 
   for first {&car}  no-lock where {&car}.codcar    = cod_car: end.
   for first {&not}  no-lock where {&not}.recnotfis = rec_not: end.
   for first {&emp}  no-lock where {&emp}.codemp    = cod_emp: end.
   for first {&ectb} no-lock where {&ectb}.empctb   = emp_ctb: end.
   if error-status:error                   or
      cod_cfo > 0   and not avail {&cfo}   or
      cod_fre <> "" and not avail {&tfr}   or
      cod_car > 0   and not avail {&car}   or    
      cod_pac > 0   and not avail {&pac}   or
      emp_ctb > 0   and not avail {&ectb}  or
      cod_emp > 0   and not avail {&emp}   or
      rec_not > 0   and not avail {&not}   or
      asd_ped > 0   and not avail {&ped}   or
      avail {&pac} and avail {&car}        or
      avail {&tfr} and avail {&cfo} and 
      {&tfr}.codfre <> {&cfo}.codfre and {&cfo}.codfre <> "" then do:
      assign rsp = "separador=;,Parametro <" 
                 + replace(par_in,";",",") + "> invalido." 
                 + (if cod_cfo > 0   and not avail {&cfo}  
                       then "Fiscal "  else "")
                 + (if cod_fre <> "" and not avail {&tfr}  
                       then "Frete "   else "")
                 + (if cod_pac > 0   and not avail {&pac}  
                       then "Pacote. " else "")  
                 + (if cod_car > 0   and not avail {&car}  
                       then "Carga "   else "")  
                 + (if cod_emp > 0   and not avail {&emp}  
                       then "Empresa " else "")  
                 + (if emp_ctb > 0   and not avail {&ectb} 
                       then "Ectb "   else "")  
                 + (if rec_not > 0   and not avail {&not}  
                       then "Nota "    else "")  
                 + (if asd_ped > 0   and not avail {&ped}
                       then "Pedido "  else "")
                 + (if avail {&tfr} and avail {&cfo} 
                                    and cod_fre <> {&cfo}.codfre 
                                    and {&cfo}.codfre <> ""
                       then "Fiscal/Frete" else "")  
                 + (if avail {&car} and avail {&pac} 
                       then "Carga/Pacote"  else "").
      run trg/trgclrsp.p (input-output rsp, false).       
      return "undo".
   end.
   return "ok".
end procedure.                                                /*mta_parametro*/
procedure verifica_nota:                       
   def var msg as cha no-undo.
   assign var_wai = int(os-getenv("_VARWAI"))
          rsp = ";end-error;A;C;L"
              + (if w_not.cfoopb = 0
                 or avail empenho and empenho.valepn = w_not.valtotcon
                    then trim(string(sml_fat,"/;P"))
                    else ""
                ).
   if lookup("P",rsp,";") > 0 and
      w_not.uficms > 0        and avail {&not} and
      can-find(first {&inf} of {&not} no-lock 
               where {&inf}.alqicm = 7) then do:
      assign rsp = replace(rsp,";P","").
   end.

   if w_not.cfoopb > 0 then do:
       assign msg = "Pausa para conferencia da nota fiscal"
                 + (if avail {&not} 
                       then " Id: " + string({&not}.recnotfis) 
                       else ""
                    )
                 + (if w_not.uficm > 0
                       then ", UF base " + string(w_not.uficm,"99") +
                            (if avail {&not} and
                                can-find(first {&inf} of {&not} no-lock 
                                         where {&inf}.alqicm = 7)
                                then " Aliquota de 7%"
                                else ""
                            )
                       else ""
                   )
                 + (if w_not.cfoopb > 0
                       then "\n\nORGAO PUBLICO DE MINAS GERAES" +
                            (if empenho.valicm > 0
                                then ". Icms " 
                                   + trim(string(empenho.valicm,">>>>>,>>9.99"))
                                else ""
                            )
                       else ""
                   )
                 + (if w_not.cfoopb > 0 and empenho.valepn <> w_not.valtotcon
                       then "\nVALOR " 
                            + trim(string(w_not.valtotcon,">>>>>,>>>,>>9.99"))
                            + " DIFERE DO EMPENHO "
                            + trim(string(empenho.valepn,">>>>>,>>>,>>9.99"))
                       else ""
                   )
                + (if avail {&not}
                      then ";Tecle <C>onsultar nf <A>bortar"
                      else ";Tecle <A>bortar"
                  )
                + (if lookup("P",rsp,";") > 0 then " <P>rosseguir" else "")
                + ";" + replace(substr(rsp,2),";",",").

      run rt/rtedmsg.p (30, "A T E N C A O", 
                               entry(2,msg,";") + "@" + entry(3,msg,";"),
                               entry(1,msg,";")
                          ) no-error.
      assign rsp = return-value.
   end. else do:
   assign rsp = "separador=;,Pausa para conferencia da nota fiscal"
              + (if avail {&not} then " Id:" + string({&not}.recnotfis) else "")
              + (if w_not.uficm > 0
                    then ", UF base " + string(w_not.uficm,"99") +
                         (if avail {&not} and
                             can-find(first {&inf} of {&not} no-lock 
                                      where {&inf}.alqicm = 7)
                             then " Aliquota de 7%"
                             else ""
                         )
                    else ""
                )
              + (if w_not.cfoopb > 0
                    then "@i\nORGAO PUBLICO DE MINAS GERAES" +
                         (if empenho.valicm > 0
                             then ". Icms " 
                                + trim(string(empenho.valicm,">>>>>,>>9.99"))
                             else ""
                         )
                    else ""
                )
              + (if w_not.cfoopb > 0 and empenho.valepn <> w_not.valtotcon
                    then ". Valor " 
                         + trim(string(w_not.valtotcon,">>>>>,>>>,>>9.99"))
                         + " DIFERE DO EMPENHO"
                    else ". Valor "
                         + trim(string(w_not.valtotcon,">>>>>,>>>,>>9.99"))
                )
             + (if avail {&not}
                   then ";Tecle <A>bortar <C>onsultar nf"
                   else ";Tecle <A>bortar"
               )
             + (if lookup("P",rsp,";") > 0 then " <P>rosseguir" else "")
             + rsp.
      run trg/trgclrsp.p (input-output rsp, false).
   end.

   if lookup(rsp,"A,end-error") > 0 then return "refaz".
   if rsp = "P" then return "leave".
   if rsp = "L" 
   or avail f_tpd and f_tpd.pedmkt 
   or sml_fat then do:
      run trb/rt/trbrtvld.p ( "V", {&not}.recnotfis, ? ) no-error.
   end.

   run CN/RF/CNRFmnot.p ( rowid({&not}), "retorna,recnotfis,rec" ).
   if not return-value matches "recnotfis=*" then return "refaz".
   if f_tpd.pedb2c and w_not.valtotnotfis <> {&ped}.valtotinfped then do:
            run rt/rtedmsg.p (30,"   A T E N C A O   ","", 
                             "Pedido " + string({&ped}.asdped,"9999999999")
                           + "\nValor informado " 
                           + trim(string({&ped}.valtotinfped,">>>,>>>,>>9.99"))
                           + "\nValor calculado " 
                           + trim(string(w_not.valtotnotfis,">>>,>>>,>>9.99"))
                             ) no-error.
      return "refaz".
   end.
   
   //disconnect fiscal no-error.
   return "leave".
end procedure.                                                /*verifica_nota*/
/*Fim do programa*************************************************************/

