create domain XGUID as CHAR(16) character set OCTETS collate OCTETS;
create domain XGUID_STR as VarChar(36) character set OCTETS collate OCTETS;
create domain XINT as INTEGER;
create domain XOBJNAME as VARCHAR(32);
create domain XCOMMENT as VARCHAR(128);
create domain XSQL as VARCHAR(512);

-- drop table ENTITIES
create table ENTITIES(
     ID       XGUID     not null primary key
    ,NAME     XOBJNAME
    ,DESCR    XCOMMENT
--    ,STATE    XINT      default 0 not null
--    ,MODDT    XDT       default current_timestamp
--    ,MODUSER  XGUID     not null
);
comment on table ENTITIES is 'Сущности, хранимые в БД';

-- drop table SYS_OPERS
create table SYS_OPERS(
     OPER     XOBJNAME  not null primary key
    ,OPERTYPE XINT      not null
    ,DESCR    XCOMMENT
);
alter table SYS_OPERS add constraint UNQ1_SYS_OPERS unique(NAME);
comment on table SYS_OPERS is 'Полный перечень операций над сущностями (ENTITIES)';
comment on column SYS_OPERS.OPERTYPE is 'Вид операции:
  0: Select
  1: Insert
  2: Update
  3: Delete
';

-- drop table ENTITY_OPERS
create table ENTITY_OPERS(
     ID        XGUID     not null primary key
    ,ENTITY    XGUID
    ,OPER      XOBJNAME
    ,SQL       XSQL
);
alter table ENTITY_OPERS add constraint UNQ1_ENTITY_OPERS unique(ENTITY,OPER);
alter table ENTITY_OPERS add constraint FK_ENTITY_OPERS_1 foreign key(OPER) references SYS_OPERS(OPER);
alter table ENTITY_OPERS add constraint FK_ENTITY_OPERS_2 foreign key(ENTITY) references ENTITIES(ID);
comment on table ENTITY_OPERS is 'Операции сущностей';

-- drop table MAINTREE
create table MAINTREE(
     ID        XGUID     not null primary key
    ,NAME      XOBJNAME  not null
    ,PARENT    XGUID
    ,ENTITY    XGUID
);
alter table MAINTREE add constraint FK_MAINTREE_1 foreign key(PARENT) references MAINTREE(ID);
alter table MAINTREE add constraint FK_MAINTREE_2 foreign key(ENTITY) references ENTITIES(ID);
comment on table MAINTREE is '"Меню" программы';

-- drop table SYSPRIV
create table SYSPRIV(
     ID        XGUID     not null primary key
    ,SUBJ      XINTFK
    ,ENTOPER   XGUID
);
alter table SYSPRIV add constraint FK_SYSPRIV_1 foreign key(ENTOPER) references ENTITY_OPERS(ID);
comment on table SYSPRIV is
'Матрица привилегий субъектов (пользователи, роли) на операции над объектами (ENTITIES)';

