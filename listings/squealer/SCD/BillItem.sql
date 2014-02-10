begin;
/*{{{*//*{{{*//* "BillItem" schema */
/*}}}*/
/*{{{*/create schema "BillItem";
/*}}}*/
/*{{{*//*{{{*//* Row versioning backend */
/*}}}*/
/*{{{*//*{{{*//* Row identification */
/*}}}*/
create table "BillItem"."identity"
  ( "identity" bigserial not null primary key  , "order version" bigint not null references "Order"."journal" deferrable initially deferred  , "product version" bigint not null references "Product"."journal" deferrable initially deferred
  , unique ("order version", "product version")
  )
;/*}}}*/
/*{{{*//*{{{*//* Row version journal */
/*}}}*/
create table "BillItem"."journal"
  ( "entry"     bigserial                not null primary key
  , "identity"  bigint                   not null references "BillItem"."identity"
  , "timestamp" timestamp with time zone not null default now()

  , unique ("entry", "timestamp")
  , unique ("entry", "identity" )
  )
;/*}}}*/
/*{{{*//*{{{*//* Row version revocation */
/*}}}*/
create table "BillItem"."revocation"
  ( "entry"           bigint                   not null primary key references "BillItem"."journal"
  , "start timestamp" timestamp with time zone not null -- redundant but required for time-efficient integrity
  , "end timestamp"   timestamp with time zone not null default now()

  , check ("start timestamp" <= "end timestamp")
  , unique ("entry", "end timestamp")
  , foreign key ("entry", "start timestamp") references "BillItem"."journal" ("entry", "timestamp")
  )
;/*}}}*/
/*{{{*//*{{{*//* Row version succession */
/*}}}*/
create table "BillItem"."succession"
  ( "entry"     bigint                   not null primary key references "BillItem"."revocation"
  , "successor" bigint                   not null unique      references "BillItem"."journal"
  , "timestamp" timestamp with time zone not null -- redundant but required for time-efficient integrity

  -- succession timestamp equals successor journal entry timestamp
  , unique      ("successor", "timestamp") -- implicit index may make foreign key checks more efficient
  , foreign key ("successor", "timestamp") references "BillItem"."journal" ("entry", "timestamp")

  -- revocation end timestamp equals successor journal entry creation timestamp
  , unique      ("entry", "timestamp") -- implicit index may make foreign key checks more efficient
  , foreign key ("entry", "timestamp") references "BillItem"."revocation" ("entry", "end timestamp")

  )
;/*}}}*/
/*{{{*//*{{{*//* Active row version tracking */
/*}}}*/
create table "BillItem"."active"
  ( "identity" bigint not null primary key references "BillItem"."identity"
  , "entry"    bigint not null unique      references "BillItem"."journal"

  , unique      ("identity", "entry") -- implicit index may make foreign key checks more efficient
  , foreign key ("identity", "entry") references "BillItem"."journal" ("identity", "entry")
  )
;/*}}}*//*}}}*/
/*{{{*//*{{{*//* Attributes */
/*}}}*/
/*{{{*//*{{{*//* "quantity" */
/*}}}*/
/*{{{*/create table "BillItem"."quantity state"
  ( "quantity state" bigserial not null primary key
  , "quantity" integer not null
  )
;
/*}}}*/
/*{{{*/create table "BillItem"."quantity proxy"
  ( "entry" bigint not null primary key references "BillItem"."journal"
  , "quantity state" bigint not null references "BillItem"."quantity state"
  )
;
/*}}}*//*}}}*//*}}}*/
/*{{{*//*{{{*//* Frontend */
/*}}}*/
/*{{{*//*{{{*//* Version view */
/*}}}*/
create view "BillItem"."version" as
  select
    "BillItem"."journal"."entry",
    "BillItem"."journal"."timestamp" as "journal timestamp",
    "BillItem"."revocation"."end timestamp",
    "BillItem"."succession"."successor",
    "BillItem"."identity"."order version",
    "order version view"."code" as "order -> code",
    "BillItem"."identity"."product version",
    "product version view"."code" as "product -> code",
    "BillItem"."quantity state"."quantity"
  from "BillItem"."identity" natural join "BillItem"."journal"
  left outer join "BillItem"."revocation" on ("BillItem"."journal"."entry" = "BillItem"."revocation"."entry")
  left outer join "BillItem"."succession" on ("BillItem"."journal"."entry" = "BillItem"."succession"."entry")
  inner join "Order"."version" as "order version view" on ("BillItem"."identity"."order version" = "order version view"."entry")
  inner join "Product"."version" as "product version view" on ("BillItem"."identity"."product version" = "product version view"."entry")
  left outer join (
    "BillItem"."quantity proxy" natural join "BillItem"."quantity state"
  ) on ("BillItem"."journal"."entry" = "BillItem"."quantity proxy"."entry")
;/*}}}*/
/*{{{*//*{{{*//* Transactional view */
/*}}}*/
/*{{{*/create view public."BillItem" as
  select
    "BillItem"."identity"."order version",
    "order version view"."code" as "order -> code",
    "BillItem"."identity"."product version",
    "product version view"."code" as "product -> code",
    "BillItem"."quantity state"."quantity"
  from "BillItem"."active" natural join "BillItem"."identity" natural join "BillItem"."journal"
  inner join "Order"."version" as "order version view" on ("BillItem"."identity"."order version" = "order version view"."entry")
  inner join "Product"."version" as "product version view" on ("BillItem"."identity"."product version" = "product version view"."entry")
  left outer join (
    "BillItem"."quantity proxy" natural join "BillItem"."quantity state"
  ) on ("BillItem"."journal"."entry" = "BillItem"."quantity proxy"."entry")
;
/*}}}*/
/*{{{*//*{{{*//* Row version tracking triggers */
/*}}}*/
/*{{{*//*{{{*//* Insert into view */
/*}}}*/
/*{{{*//*{{{*//* Function */
/*}}}*/
create function "BillItem"."view insert"
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
      if new."order version" is not null then
        raise exception 'insertions into % view must not specify %', 'BillItem', 'order version';
      end if;

      if new."product version" is not null then
        raise exception 'insertions into % view must not specify %', 'BillItem', 'product version';
      end if;

      select     "BillItem"."identity"."identity", "Order"."active"."entry", "Product"."active"."entry"
      into       "new identity", new."order version", new."product version"
      from       "BillItem"."identity"
      inner join ("Order"."identity" natural join "Order"."journal" natural join "Order"."active") on ("BillItem"."identity"."order version" = "Order"."journal"."entry")
      inner join ("Product"."identity" natural join "Product"."journal" natural join "Product"."active") on ("BillItem"."identity"."product version" = "Product"."journal"."entry")
      where      "Order"."identity"."code" = new."order -> code"
      and        "Product"."identity"."code" = new."product -> code"
      ;

      if not found then
        select "Order"."active"."entry"
        into   new."order version"
        from   ("Order"."identity" natural join "Order"."journal" natural join "Order"."active")
        where  "Order"."identity"."code" = new."order -> code"
        ;
        if not found then
          raise exception 'no active % row matches % reference on insert into % table', 'Order', 'order', 'BillItem';
        end if;

        select "Product"."active"."entry"
        into   new."product version"
        from   ("Product"."identity" natural join "Product"."journal" natural join "Product"."active")
        where  "Product"."identity"."code" = new."product -> code"
        ;
        if not found then
          raise exception 'no active % row matches % reference on insert into % table', 'Product', 'product', 'BillItem';
        end if;

        insert into "BillItem"."identity"
          ("order version", "product version") values
          (new."order version", new."product version")
        returning "BillItem"."identity"."identity"
        into "new identity"
        ;
      end if;

      insert into "BillItem"."journal"
        (    "identity") values
        ("new identity")
      returning "BillItem"."journal"."entry" into "new entry"
      ;

      insert into "BillItem"."active"
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
instead of insert on public."BillItem"
for each row execute procedure "BillItem"."view insert"();/*}}}*//*}}}*/
/*{{{*//*{{{*//* Delete from view */
/*}}}*/
/*{{{*//*{{{*//* Function */
/*}}}*/
create function "BillItem"."delete function"
  ()
returns trigger
  language 'plpgsql'
  security definer
as
  $body$
    begin
      insert into  "BillItem"."revocation" ("entry", "start timestamp")
      select       "BillItem"."journal"."entry", "BillItem"."journal"."timestamp"
      from         "BillItem"."active"
      natural join "BillItem"."identity"
      natural join "BillItem"."journal"
      where        "BillItem"."identity"."order version" = old."order version"
      and          "BillItem"."identity"."product version" = old."product version"
      ;

      delete from "BillItem"."active"
      using       "BillItem"."identity" natural join "BillItem"."journal"
      where       "BillItem"."active"."entry" = "BillItem"."journal"."entry"
      and         "BillItem"."identity"."order version" = old."order version"
      and         "BillItem"."identity"."product version" = old."product version"
      ;

      return old;
    end;
  $body$
;/*}}}*/
/*{{{*//*{{{*//* Trigger */
/*}}}*/
create trigger "00 delete"
instead of delete on public."BillItem"
for each row execute procedure "BillItem"."delete function"();/*}}}*//*}}}*/
/*{{{*//*{{{*//* Update view */
/*}}}*/
/*{{{*//*{{{*//* Function */
/*}}}*/
create function "BillItem"."update function"
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
      if new."order -> code" is null then
        raise exception 'null value in column % violates not-null constraint', 'order -> code';
      end if;

      if new."product -> code" is null then
        raise exception 'null value in column % violates not-null constraint', 'product -> code';
      end if;

      if
        new."order version" is not null and
        old."order version" <> new."order version"
      then
        raise exception 'updates to % view must not set %', 'BillItem', 'order version';
      elsif
        new."order version" is null
        or old."order -> code" <> new."order -> code"
      then
        select "Order"."active"."entry"
        into   new."order version"
        from   "Order"."active" natural join "Order"."identity"
        where  "Order"."identity"."code" = new."order -> code"
        ;
        if not found then
          raise exception 'no active % row matches % reference on update to % row', 'Order', 'order', 'BillItem';
        end if;
      end if;

      if
        new."product version" is not null and
        old."product version" <> new."product version"
      then
        raise exception 'updates to % view must not set %', 'BillItem', 'product version';
      elsif
        new."product version" is null
        or old."product -> code" <> new."product -> code"
      then
        select "Product"."active"."entry"
        into   new."product version"
        from   "Product"."active" natural join "Product"."identity"
        where  "Product"."identity"."code" = new."product -> code"
        ;
        if not found then
          raise exception 'no active % row matches % reference on update to % row', 'Product', 'product', 'BillItem';
        end if;
      end if;

      select "BillItem"."active"."entry"
      into   "old entry"
      from   "BillItem"."active" natural join "BillItem"."identity"
      where  "BillItem"."identity"."order version" = old."order version"
      and    "BillItem"."identity"."product version" = old."product version"
      ;

      delete from public."BillItem"
      where       public."BillItem"."order version" = old."order version"
      and         public."BillItem"."product version" = old."product version"
      ;

      select "BillItem"."identity"."identity"
      into   "new identity"
      from   "BillItem"."identity"
      where  "BillItem"."identity"."order version" = new."order version"
      and    "BillItem"."identity"."product version" = new."product version"
      ;
      if not found then
        insert into "BillItem"."identity"
          ("order version", "product version") values
          (new."order version", new."product version")
        returning "BillItem"."identity"."identity"
        into "new identity"
        ;
      end if;

      insert into "BillItem"."journal"
        (    "identity") values
        ("new identity")
      returning "BillItem"."journal"."entry"
      into "new entry"
      ;

      insert into "BillItem"."active"
        (    "identity",     "entry") values
        ("new identity", "new entry")
      ;

      insert into "BillItem"."succession" ("entry", "successor", "timestamp")
      select      "old entry", "new entry", "BillItem"."revocation"."end timestamp"
      from        "BillItem"."revocation"
      where       "BillItem"."revocation"."entry" = "old entry"
      ;

      return new;
    end;
  $body$
;/*}}}*/
/*{{{*//*{{{*//* Trigger */
/*}}}*/
create trigger "00 update"
instead of update on public."BillItem"
for each row execute procedure "BillItem"."update function"();/*}}}*//*}}}*//*}}}*/
/*{{{*//*{{{*//* Column triggers */
/*}}}*/
/*{{{*//*{{{*//* "quantity" */
/*}}}*/
/*{{{*//*{{{*//* Function */
/*}}}*/
create function "BillItem"."insert or update quantity function"
  ()
returns trigger
  language 'plpgsql'
  security definer
as
  $body$
    declare
      "new quantity state" bigint;
    begin
      if
        new."quantity" is not null
      then
        if
          tg_op = 'INSERT'
          or not (old."quantity" is not null and old."quantity" = new."quantity")
        then
          insert into "BillItem"."quantity state"
            (    "quantity") values
            (new."quantity")
          returning   "BillItem"."quantity state"."quantity state"
          into        "new quantity state"
          ;
        else
          select     "BillItem"."quantity proxy"."quantity state"
          into       "new quantity state"
          from       "BillItem"."identity" natural join "BillItem"."active" natural join "BillItem"."journal"
          inner join "BillItem"."succession" on ("BillItem"."journal"."entry" = "BillItem"."succession"."successor")
          inner join "BillItem"."quantity proxy" on ("BillItem"."succession"."entry" = "BillItem"."quantity proxy"."entry")
          where      "BillItem"."identity"."order version" = new."order version"
          and        "BillItem"."identity"."product version" = new."product version"
          ;
        end if;

        insert into  "BillItem"."quantity proxy" ("entry", "quantity state")
        select       "BillItem"."active"."entry", "new quantity state"
        from         "BillItem"."identity" inner join "BillItem"."active" using ("identity")
        where        "BillItem"."identity"."order version" = new."order version"
        and          "BillItem"."identity"."product version" = new."product version"
        ;
      end if;

      return new;
    end;
  $body$
;/*}}}*/
/*{{{*//*{{{*//* Trigger */
/*}}}*/
create trigger "10 insert or update quantity"
instead of insert or update on public."BillItem"
for each row execute procedure "BillItem"."insert or update quantity function"();/*}}}*//*}}}*//*}}}*/
/*{{{*//*{{{*//* Reference triggers */
/*}}}*/
/*{{{*//*{{{*//* cascade on update to "order" */
/*}}}*/
/*{{{*//*{{{*//* Function */
/*}}}*/
create function "BillItem"."cascade update on BillItem view order reference"
  ()
returns trigger
  language 'plpgsql'
  security definer
as
  $body$
    begin
      update public."BillItem"
      set
        ( "order version"
        , "order -> code"
        )
      = ( null
        , "Order"."version"."code"
        )
      from  "Order"."version"
      where new."entry" = public."BillItem"."order version"
      and   new."entry" = "Order"."version"."entry"
      ;

      return new;
    end;
  $body$
;/*}}}*/
/*{{{*//*{{{*//* Trigger */
/*}}}*/
create trigger "20 cascade update on BillItem view order reference"
after insert on "Order"."succession"
for each row execute procedure "BillItem"."cascade update on BillItem view order reference"();/*}}}*//*}}}*/
/*{{{*//*{{{*//* restrict on delete to "order" */
/*}}}*/
/*{{{*//*{{{*//* Function */
/*}}}*/
create function "BillItem"."restrict delete on BillItem view order reference"
  ()
returns trigger
  language 'plpgsql'
  security definer
as
  $body$
    begin
      perform *
      from    public."BillItem"
      where   public."BillItem"."order version" = new."entry"
      limit   1
      ;
      if found then
        raise exception '% on % table breaks % table % reference'
        , 'delete'
        , 'Order'
        , 'BillItem'
        , 'order'
        ;
      end if;

      return new;
    end;
  $body$
;/*}}}*/
/*{{{*//*{{{*//* Trigger */
/*}}}*/
create constraint trigger "20 restrict delete on BillItem view order reference"
after insert on "Order"."revocation"
deferrable initially deferred
for each row execute procedure "BillItem"."restrict delete on BillItem view order reference"();/*}}}*//*}}}*//*}}}*//*}}}*//*}}}*//*}}}*/
commit;
