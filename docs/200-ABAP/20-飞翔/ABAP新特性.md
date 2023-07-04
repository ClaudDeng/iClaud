---
autoGroup-1: 基础语法
title: ABAP新特性2
---

# ABAP新特性

## ABAP类型变化

### 枚举 ENUM

使用enum可以定义枚举类型，**ABAP740以上支持**.

- 使用**base type** 可以指定字段类型，如init,char8。如果使用char类型长度最长为char8。
- 如果需要提供描述值，可以使用**value**。使用类**cl_abap_enumdescr**可以获得枚举类型的相关内容。其中members中的结构是{name,value}.

~~~ABAP
types:
      begin of enum mesg_code base type char8,
        na value IS INITIAL,
        success value '成功',
        err0001 value '90000' ,
        err0002 value '90002',
      end of enum mesg_code .

DATA(size) = VALUE mesg_code( ).

DATA(enum_descr) = CAST cl_abap_enumdescr(
  cl_abap_typedescr=>describe_by_data( size ) ).

cl_demo_output=>new(
  )->write_data( enum_descr->kind            "E, for elementary
  )->write_data( enum_descr->type_kind       "k, new for enumerated type
  )->write_data( enum_descr->base_type_kind  "I, the base type
  )->write_data( enum_descr->members         "Table of constants and values
  )->display( ).
  
  
*    ev_message = o_enum_descr->members[ name = v_code ]-value.
*    ev_code = |{ v_code case = (cl_abap_format=>c_upper) } |.
~~~



### 标识结构 Indicator structures

`TYPES`语句有了新的附加选项`[INDICATORS]`，可以为给定的结构类型定义一个[indicator structure](https://help.sap.com/doc/abapdocu_755_index_htm/7.55/en-US/index.htm?file=abenindicator_structure_glosry.htm)子结构。indicator structure可以在ABAP SQL读写语句中用作[ABAP SQL indicator](https://help.sap.com/doc/abapdocu_755_index_htm/7.55/en-US/index.htm?file=abenabap_sql_indicator_glosry.htm)。**ABAP755以上支持**

~~~ABAP
TYPES wa TYPE sflight WITH INDICATORS ind.

DATA itab TYPE TABLE OF wa WITH EMPTY KEY.

SELECT carrid, connid, fldate, price
       FROM sflight
       WHERE carrid = char`LH` AND
             connid = numc`0400` AND
             fldate = @sy-datum
       INTO CORRESPONDING FIELDS OF TABLE @itab.

IF sy-subrc  = 0.

  LOOP AT itab ASSIGNING FIELD-SYMBOL(<wa>).
    <wa>-price = '0.8'.
    <wa>-ind-price = '01'.
  ENDLOOP.

  UPDATE sflight FROM TABLE @itab INDICATORS SET STRUCTURE ind.

ENDIF.
~~~




