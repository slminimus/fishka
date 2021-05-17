/*
-- drop table CARDS
create table CARDS(
   ID       XGUID   not null
  ,CARDNUM  XCHAR25
  ,RELEASED XDT
  ,FIO      XCHAR50
  ,PHONES   XCHAR50
  ,EMAIL    XCHAR25
  ,DISCONT  XINT
  ,STATE    XINT     default 0
  ,MODDT    XDT      default current_timestamp
  ,MODUSER  XGUID
);
alter table CARDS add constraint PK_CARDS primary key(ID);
create descending index CARDS_IDX1 on CARDS(RELEASED);
comment on table  CARDS is 'Скидочные карты клиентов';
comment on column CARDS.RELEASED is 'Дата выпуска';
comment on column CARDS.DISCONT is 'Скидка,%';
comment on column CARDS.STATE is 'Битовое поле:
 0 - удален
 1 - disabled
 2 - призрак
';
*/
create or alter function REALY(STATE XINT)
returns boolean deterministic
as
begin
  return bin_and(:STATE, 5) = 0; -- запись не удалена и не призрак
end;
grant execute on function REALY to public;
comment on function REALY is 'Проверяет поле STATE; true, если запись не удалена и не призрак';
----------------------------------------------

create or alter view CARDS$VWE
as
  select * from CARDS T
  where REALY(t.STATE)
;
grant select on CARDS to view CARDS$VWE;
grant select on CARDS$VWE to ARTIX, USAGE_ROLE;


create or alter procedure CARDS$EDIT(
   OPER$    XINT
  ,ID       XGUID_STR
  ,CARDNUM  type of column CARDS.CARDNUM  = null
  ,RELEASED type of column CARDS.RELEASED = null
  ,FIO      type of column CARDS.FIO      = null
  ,PHONES   type of column CARDS.PHONES   = null
  ,EMAIL    type of column CARDS.EMAIL    = null
  ,DISCONT  type of column CARDS.DISCONT  = null
  ,STATE    type of column CARDS.STATE    = null
)returns(
  ROW_ID  XGUID
)as
begin
  :ID = ASUUID(:ID);
  if (:OPER$ = 0) then
  begin
    :ID = coalesce(:ID, gen_uuid());
    insert into CARDS(ID,CARDNUM,RELEASED,FIO,PHONES,EMAIL,DISCONT,STATE,MODDT)
    values (:ID,:CARDNUM,:RELEASED,:FIO,:PHONES,:EMAIL,:DISCONT,
            BITSET(coalesce(:STATE,0),2,0),current_timestamp);
    if (row_count > 0) then
      :ROW_ID = :ID;
  end else
  if (:OPER$ = 1) then
  begin
    update CARDS t
    set t.CARDNUM = coalesce(:CARDNUM,t.CARDNUM),
        t.RELEASED = coalesce(:RELEASED,t.RELEASED),
        t.FIO = coalesce(:FIO,t.FIO),
        t.PHONES = coalesce(:PHONES,t.PHONES),
        t.EMAIL = coalesce(:EMAIL,t.EMAIL),
        t.DISCONT = coalesce(:DISCONT,t.DISCONT),
        t.STATE = BITSET(coalesce(:STATE,t.STATE),2,0),
        t.MODDT = current_timestamp
    where (t.ID = :ID);
    if (row_count > 0) then
      :ROW_ID = :ID;
  end else
  if (:OPER$ = 2) then
  begin
    :ROW_ID = :ID;
    update CARDS t
    set t.STATE = BITSET(BITSET(t.STATE,0,1),2,0),
        t.MODDT = current_timestamp
    where (t.ID = :ID);
  end else
    exception E_INVALID_OPER;
  suspend;
end;
grant execute on procedure CARDS$EDIT to ARTIX, USAGE_ROLE;
GRANT EXECUTE ON FUNCTION ASUUID TO PROCEDURE CARDS$EDIT;
GRANT EXECUTE ON FUNCTION BITSET TO PROCEDURE CARDS$EDIT;
GRANT USAGE ON EXCEPTION E_INVALID_OPER TO PROCEDURE CARDS$EDIT;
GRANT SELECT,INSERT,UPDATE ON CARDS TO PROCEDURE CARDS$EDIT;
-------------------------

