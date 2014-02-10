begin;
/*{{{*//*{{{*//* "Product" schema */
/*}}}*/
/*{{{*/create schema "Product";
/*}}}*/
/*{{{*//*{{{*//* Row versioning backend */
/*}}}*/
/*{{{*//*{{{*//* Row identification */
/*}}}*/
create table "Product"."identity"
  ( "identity" bigserial not null primary key
  , "code" text not null
  , unique ("code")
  )
;/*}}}*/
/*{{{*//*{{{*//* Row version journal */
/*}}}*/
create table "Product"."journal"
  ( "entry"     bigserial                not null primary key
  , "identity"  bigint                   not null references "Product"."identity"
  , "timestamp" timestamp with time zone not null default now()

  , unique ("entry", "timestamp")
  , unique ("entry", "identity" )
  )
;/*}}}*/
/*{{{*//*{{{*//* Row version revocation */
/*}}}*/
create table "Product"."revocation"
  ( "entry"           bigint                   not null primary key references "Product"."journal"
  , "start timestamp" timestamp with time zone not null -- redundant but required for time-efficient integrity
  , "end timestamp"   timestamp with time zone not null default now()

  , check ("start timestamp" <= "end timestamp")
  , unique ("entry", "end timestamp")
  , foreign key ("entry", "start timestamp") references "Product"."journal" ("entry", "timestamp")
  )
;/*}}}*/
/*{{{*//*{{{*//* Row version succession */
/*}}}*/
create table "Product"."succession"
  ( "entry"     bigint                   not null primary key references "Product"."revocation"
  , "successor" bigint                   not null unique      references "Product"."journal"
  , "timestamp" timestamp with time zone not null -- redundant but required for time-efficient integrity

  -- succession timestamp equals successor journal entry timestamp
  , unique      ("successor", "timestamp") -- implicit index may make foreign key checks more efficient
  , foreign key ("successor", "timestamp") references "Product"."journal" ("entry", "timestamp")

  -- revocation end timestamp equals successor journal entry creation timestamp
  , unique      ("entry", "timestamp") -- implicit index may make foreign key checks more efficient
  , foreign key ("entry", "timestamp") references "Product"."revocation" ("entry", "end timestamp")

  )
;/*}}}*/
/*{{{*//*{{{*//* Active row version tracking */
/*}}}*/
create table "Product"."active"
  ( "identity" bigint not null primary key references "Product"."identity"
  , "entry"    bigint not null unique      references "Product"."journal"

  , unique      ("identity", "entry") -- implicit index may make foreign key checks more efficient
  , foreign key ("identity", "entry") references "Product"."journal" ("identity", "entry")
  )
;/*}}}*//*}}}*/
/*{{{*//*{{{*//* Attributes */
/*}}}*/
/*{{{*//*{{{*//* "name" */
/*}}}*/
/*{{{*/create table "Product"."name state"
  ( "name state" bigserial not null primary key
  , "name" text not null
  )
;
/*}}}*/
/*{{{*/create table "Product"."name proxy"
  ( "entry" bigint not null primary key references "Product"."journal"
  , "name state" bigint not null references "Product"."name state"
  )
;
/*}}}*//*}}}*/
/*{{{*//*{{{*//* "price" */
/*}}}*/
/*{{{*/create table "Product"."price state"
  ( "price state" bigserial not null primary key
  , "price" money not null
  )
;
/*}}}*/
/*{{{*/create table "Product"."price proxy"
  ( "entry" bigint not null primary key references "Product"."journal"
  , "price state" bigint not null references "Product"."price state"
  )
;
/*}}}*//*}}}*//*}}}*/
/*{{{*//*{{{*//* Frontend */
/*}}}*/
/*{{{*//*{{{*//* Version view */
/*}}}*/
create view "Product"."version" as
  select
    "Product"."journal"."entry",
    "Product"."journal"."timestamp" as "journal timestamp",
    "Product"."revocation"."end timestamp",
    "Product"."succession"."successor",
    "Product"."identity"."code",
    "Product"."name state"."name",
    "Product"."price state"."price"
  from "Product"."identity" natural join "Product"."journal"
  left outer join "Product"."revocation" on ("Product"."journal"."entry" = "Product"."revocation"."entry")
  left outer join "Product"."succession" on ("Product"."journal"."entry" = "Product"."succession"."entry")
  left outer join (
    "Product"."name proxy" natural join "Product"."name state"
  ) on ("Product"."journal"."entry" = "Product"."name proxy"."entry")
  left outer join (
    "Product"."price proxy" natural join "Product"."price state"
  ) on ("Product"."journal"."entry" = "Product"."price proxy"."entry")
;/*}}}*/
/*{{{*//*{{{*//* Transactional view */
/*}}}*/
/*{{{*/create view public."Product" as
  select
    "Product"."identity"."code",
    "Product"."name state"."name",
    "Product"."price state"."price"
  from "Product"."active" natural join "Product"."identity" natural join "Product"."journal"
  left outer join (
    "Product"."name proxy" natural join "Product"."name state"
  ) on ("Product"."journal"."entry" = "Product"."name proxy"."entry")
  left outer join (
    "Product"."price proxy" natural join "Product"."price state"
  ) on ("Product"."journal"."entry" = "Product"."price proxy"."entry")
;
/*}}}*/
/*{{{*//*{{{*//* Row version tracking triggers */
/*}}}*/
/*{{{*//*{{{*//* Insert into view */
/*}}}*/
/*{{{*//*{{{*//* Function */
/*}}}*/
create function "Product"."view insert"
  ()
returns trigger
  language 'plpgsql'
  security definer
as
  $body$
    declare
      "new identity" bigint;
      "new entry"    bigint;
    begin
      select     "Product"."identity"."identity"
      into       "new identity"
      from       "Product"."identity"
      where      "Product"."identity"."code" = new."code"
      ;

      if not found then
        insert into "Product"."identity"
          ("code") values
          (new."code")
        returning "Product"."identity"."identity"
        into "new identity"
        ;
      end if;

      insert into "Product"."journal"
        (    "identity") values
        ("new identity")
      returning "Product"."journal"."entry" into "new entry"
      ;

      insert into "Product"."active"
        (    "identity",     "entry") values
        ("new identity", "new entry")
      ;

      return new;
    end;
  $body$
;/*}}}*/
/*{{{*//*{{{*//* Trigger */
/*}}}*/
create trigger "00 insert"
instead of insert on public."Product"
for each row execute procedure "Product"."view insert"();/*}}}*//*}}}*/
/*{{{*//*{{{*//* Delete from view */
/*}}}*/
/*{{{*//*{{{*//* Function */
/*}}}*/
create function "Product"."delete function"
  ()
returns trigger
  language 'plpgsql'
  security definer
as
  $body$
    begin
      insert into  "Product"."revocation" ("entry", "start timestamp")
      select       "Product"."journal"."entry", "Product"."journal"."timestamp"
      from         "Product"."active"
      natural join "Product"."identity"
      natural join "Product"."journal"
      where        "Product"."identity"."code" = old."code"
      ;

      delete from "Product"."active"
      using       "Product"."identity" natural join "Product"."journal"
      where       "Product"."active"."entry" = "Product"."journal"."entry"
      and         "Product"."identity"."code" = old."code"
      ;

      return old;
    end;
  $body$
;/*}}}*/
/*{{{*//*{{{*//* Trigger */
/*}}}*/
create trigger "00 delete"
instead of delete on public."Product"
for each row execute procedure "Product"."delete function"();/*}}}*//*}}}*/
/*{{{*//*{{{*//* Update view */
/*}}}*/
/*{{{*//*{{{*//* Function */
/*}}}*/
create function "Product"."update function"
  ()
returns trigger
  language 'plpgsql'
  security definer
as
  $body$
    declare
      "old entry" bigint;
      "new identity" bigint;
      "new entry" bigint;
    begin
      if new."code" is null then
        raise exception 'null value in column % violates not-null constraint', 'code';
      end if;

      select "Product"."active"."entry"
      into   "old entry"
      from   "Product"."active" natural join "Product"."identity"
      where  "Product"."identity"."code" = old."code"
      ;

      delete from public."Product"
      where       public."Product"."code" = old."code"
      ;

      select "Product"."identity"."identity"
      into   "new identity"
      from   "Product"."identity"
      where  "Product"."identity"."code" = new."code"
      ;
      if not found then
        insert into "Product"."identity"
          ("code") values
          (new."code")
        returning "Product"."identity"."identity"
        into "new identity"
        ;
      end if;

      insert into "Product"."journal"
        (    "identity") values
        ("new identity")
      returning "Product"."journal"."entry"
      into "new entry"
      ;

      insert into "Product"."active"
        (    "identity",     "entry") values
        ("new identity", "new entry")
      ;

      insert into "Product"."succession" ("entry", "successor", "timestamp")
      select      "old entry", "new entry", "Product"."revocation"."end timestamp"
      from        "Product"."revocation"
      where       "Product"."revocation"."entry" = "old entry"
      ;

      return new;
    end;
  $body$
;/*}}}*/
/*{{{*//*{{{*//* Trigger */
/*}}}*/
create trigger "00 update"
instead of update on public."Product"
for each row execute procedure "Product"."update function"();/*}}}*//*}}}*//*}}}*/
/*{{{*//*{{{*//* Column triggers */
/*}}}*/
/*{{{*//*{{{*//* "name" */
/*}}}*/
/*{{{*//*{{{*//* Function */
/*}}}*/
create function "Product"."insert or update name function"
  ()
returns trigger
  language 'plpgsql'
  security definer
as
  $body$
    declare
      "new name state" bigint;
    begin
      if
        new."name" is not null
      then
        if
          tg_op = 'INSERT'
          or not (old."name" is not null and old."name" = new."name")
        then
          insert into "Product"."name state"
            (    "name") values
            (new."name")
          returning   "Product"."name state"."name state"
          into        "new name state"
          ;
        else
          select     "Product"."name proxy"."name state"
          into       "new name state"
          from       "Product"."identity" natural join "Product"."active" natural join "Product"."journal"
          inner join "Product"."succession" on ("Product"."journal"."entry" = "Product"."succession"."successor")
          inner join "Product"."name proxy" on ("Product"."succession"."entry" = "Product"."name proxy"."entry")
          where      "Product"."identity"."code" = new."code"
          ;
        end if;

        insert into  "Product"."name proxy" ("entry", "name state")
        select       "Product"."active"."entry", "new name state"
        from         "Product"."identity" inner join "Product"."active" using ("identity")
        where        "Product"."identity"."code" = new."code"
        ;
      end if;

      return new;
    end;
  $body$
;/*}}}*/
/*{{{*//*{{{*//* Trigger */
/*}}}*/
create trigger "10 insert or update name"
instead of insert or update on public."Product"
for each row execute procedure "Product"."insert or update name function"();/*}}}*//*}}}*/
/*{{{*//*{{{*//* "price" */
/*}}}*/
/*{{{*//*{{{*//* Function */
/*}}}*/
create function "Product"."insert or update price function"
  ()
returns trigger
  language 'plpgsql'
  security definer
as
  $body$
    declare
      "new price state" bigint;
    begin
      if
        new."price" is not null
      then
        if
          tg_op = 'INSERT'
          or not (old."price" is not null and old."price" = new."price")
        then
          insert into "Product"."price state"
            (    "price") values
            (new."price")
          returning   "Product"."price state"."price state"
          into        "new price state"
          ;
        else
          select     "Product"."price proxy"."price state"
          into       "new price state"
          from       "Product"."identity" natural join "Product"."active" natural join "Product"."journal"
          inner join "Product"."succession" on ("Product"."journal"."entry" = "Product"."succession"."successor")
          inner join "Product"."price proxy" on ("Product"."succession"."entry" = "Product"."price proxy"."entry")
          where      "Product"."identity"."code" = new."code"
          ;
        end if;

        insert into  "Product"."price proxy" ("entry", "price state")
        select       "Product"."active"."entry", "new price state"
        from         "Product"."identity" inner join "Product"."active" using ("identity")
        where        "Product"."identity"."code" = new."code"
        ;
      end if;

      return new;
    end;
  $body$
;/*}}}*/
/*{{{*//*{{{*//* Trigger */
/*}}}*/
create trigger "10 insert or update price"
instead of insert or update on public."Product"
for each row execute procedure "Product"."insert or update price function"();/*}}}*//*}}}*//*}}}*/
/*{{{*//* Reference triggers */
/*}}}*//*}}}*//*}}}*//*}}}*/
commit;
