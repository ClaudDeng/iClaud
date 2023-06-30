# ABAP SQL新特性



## Join Expression

在特定的应用场景中，需要使用字符长度不一致的两个字段进行关联时，可以使用相应的表达式处理，但要注意表达式的位置，一般需要放在等式左边，如下例

~~~ABAP
"例：（NAST-OBJKY类型为CHAR30，EKKO-EBELN类型为CHAR10）
SELECT k~ebeln,
       t~kschl
  FROM nast AS t
 INNER JOIN ekko AS k ON left( t~objky, 10 ) = k~ebeln
  INTO TABLE @DATA(lt_data).

~~~





## Where

常用的条件语句，整理如下：

- [ NOT ] IN：除了 SELECT-OPTION，也支持多个自定义或者通过子查询获取的值
- ANY/SOME/ALL：允许将子查询获取到的结果集作为限制条件使用
- [ NOT ] BETWEEN … AND …：使用范围条件
- [ NOT ] LIKE … [ ESCAPE ]：使用模糊查询
- IS [ NOT ] NULL：判断是否有关联到相应的记录
- IS [ NOT ] INITIAL：判断是否为空值
- [ NOT ] EXISTS：根据指定条件到数据库表查询数据，判断查询结果是否存在
- like_regexpr：匹配正则表达式

~~~ABAP
SELECT i~*
  FROM scarr AS r
  LEFT OUTER JOIN spfli AS i ON i~carrid = r~carrid
 WHERE i~carrid IN ( 'AA' , 'CO' )
   AND i~carrid IN ( SELECT DISTINCT carrid FROM sflight ) 
   AND i~carrid = ANY ( SELECT DISTINCT carrid FROM sflight )
   AND i~carrid NOT BETWEEN 'BA' AND 'CA'
   AND ( i~carrid LIKE 'A%' OR i~carrid LIKE 'C%' )
   AND r~carrid IS NOT NULL
   AND i~carrid IS NOT INITIAL
   AND EXISTS ( SELECT carrid FROM sflight WHERE carrid = i~carrid )
  INTO TABLE @DATA(lt_data). 


where like_regexpr( pcre = '^[Aa]\s*\w+', value = mara~matnr ) > 0
更多精彩尽在se26: CL_ABAP_MATCHER, CL_ABAP_REGEX; 

~~~



## Group by



Grouping









## Offset

指定OFFSET时，结果集必须使用ORDER BY进行排序，与UP TO一起使用可以实现分页查询。**ABAP751以上版本**

~~~ABAP
DATA: lv_int TYPE i VALUE 1.
cl_demo_input=>request( CHANGING field = lv_int ).

SELECT carrid, connid, fldate
  FROM sflight
 ORDER BY carrid, connid, fldate
  INTO TABLE @DATA(lt_data)
 UP TO 10 ROWS
OFFSET @( ( lv_int - 1 ) * 10 ).


~~~



## Union

用union合并多个查询结果，各个查询结果列的数目、名称、顺序、类型一样，union不能与up to n rows一起使用；

~~~ABAP
SELECT char10, int4
    FROM demo_ddic_types
    WHERE char10 = char`hallowelt`
  union all
  SELECT  char10, int8 as int4
    FROM demo_ddic_types
  union distinct
  SELECT  char10, int8 as int4
    FROM demo_ddic_types
  INTO TABLE @DATA(lt_data7)


~~~





## Case

该语句不仅可以用于单值判断，也可以根据复杂条件进行判断；此外，WHEN OTHERS 不再适用，需要使用 ELSE 代替，语句结束时使用 END，而不是 ENDCASE，且需要定义别名.

~~~ABAP
SELECT CASE currcode
       WHEN 'EUR' THEN carrname
       ELSE url
       END AS case_simple,

       CASE
       WHEN currcode = 'EUR' THEN url
       WHEN carrname <> ' '  THEN carrname
       ELSE carrid && '@' && currcode
       END AS case_complex
  FROM scarr
  INTO TABLE @DATA(lt_data)
    UP TO 5 ROWS. 

~~~

![img](http://claudpic.oss-cn-hangzhou.aliyuncs.com/img/20210715114833823.png)

## Null

在使用 LEFT / RIGHT OUTER JOIN 关联外表时，如果主表中存在记录，但在外表中没有关联到数据，则外表的这部分字段的值在取数过程中始终为 NULL，在取数完成后传入数据对象时，NULL 会再转换成系统兼容的值，通常为初始值；
NULL 值用于数值计算或是字符串处理时返回结果仍为NULL值，可以在条件语句中用 IS [ NOT ] NULL 判断以及处理;
现在表缓存支持真null值了，null值不再被转换为类型初始值；
is initial，类型初始值；

~~~ABAP
DATA: lr_carrid TYPE RANGE OF s_carr_id.

lr_carrid = VALUE #( sign = 'I' option = 'EQ' ( low = 'AA' )
                                              ( low = 'CO' ) ).
SELECT DISTINCT
       r~carrid,
       CASE
       WHEN t~seatsocc IS NULL THEN 'IS NULL'
       WHEN t~seatsocc IS NOT NULL THEN 'IS NOT NULL'
       END AS field_status
  FROM scarr AS r
  LEFT OUTER JOIN sflight AS t ON t~carrid = r~carrid
  INTO TABLE @DATA(lt_data)
 WHERE r~carrid IN @lr_carrid. 


~~~



## BINTOHEX&HEXTOBIN

新的类型转换函数**[BINTOHEX](https://help.sap.com/http.svc/rc/abapdocu_752_index_htm/7.52/en-US/abensql_type_conv_func.htm)**和**[HEXTOBIN](https://help.sap.com/http.svc/rc/abapdocu_752_index_htm/7.52/en-US/abensql_type_conv_func.htm)**现在可以在SQL表达式中把byte strings转换为character strings，这种功能在CAST表达式中是不可行的。

~~~
SELECT SINGLE id AS uuid32, hextobin( id ) AS uuid16
       FROM iwreferenc
       WHERE tcode = 'SE38'
       INTO @DATA(wa).

IF sy-subrc = 0.
  DATA uuid16 LIKE wa-uuid16.
  cl_system_uuid=>convert_uuid_c32_static(
    EXPORTING
      uuid          =     wa-uuid32
    IMPORTING
      uuid_x16      =     uuid16 ).
  ASSERT wa-uuid16 = uuid16.
ENDIF.
~~~







## CAST

 CAST 实现字段的类型转换.以获取指定日期的汇率为例，汇率表中存储的日期不能直接使用，例如日期20180831 对应存储的数据为 79819168，所以我们在使用时需要转换类型
在程序中，我们可以调用 FM 来获取汇率，这里只用来测试 CAST 的使用，在CDS View中可能会被使用

~~~ABAP
DATA(lv_date) = CONV datum( '20180830' ).

SELECT MIN( CAST( CAST( @lv_date AS NUMC ) AS INT4 ) -
            ( 99999999 - CAST( CAST( gdatu AS NUMC ) AS INT4 ) ) ) AS time_differ,
       ukurs AS exchange_rate
  FROM tcurr
  INTO TABLE @DATA(lt_data)
 WHERE kurst = 'M'
   AND fcurr = 'CNY'
   AND tcurr = 'EUR'
 GROUP BY gdatu, ukurs
HAVING MIN( CAST( CAST( @lv_date AS NUMC ) AS INT4 ) -
            ( 99999999 - CAST( CAST( gdatu AS NUMC ) AS INT4 ) ) ) >= 0
 ORDER BY time_differ DESCENDING. 

DATA(lv_exchange_rate) = VALUE #( lt_rate[ 1 ]-exchange_rate OPTIONAL ).


~~~

![img](http://claudpic.oss-cn-hangzhou.aliyuncs.com/img/20210715115213493.png)



![在这里插入图片描述](http://claudpic.oss-cn-hangzhou.aliyuncs.com/img/6121040e535f4cf6911c53721e62e03c.png)



![在这里插入图片描述](http://claudpic.oss-cn-hangzhou.aliyuncs.com/img/1d4ab011def7405c8ac0492fe648c5fe.png)







## Coalesce

使用内嵌表达式 COALESCE( arg1, arg2, arg3 … argn )处理NULL值
该表达式用来返回第一个非 NULL 字段，参数至少有2个，至多255个，如果参数都为 NULL，则返回 NULL

~~~
DATA: lr_carrid TYPE RANGE OF s_carr_id.

lr_carrid = VALUE #( sign = 'I' option = 'EQ' ( low = 'AA' )
                                              ( low = 'CO' ) ).
SELECT r~carrid,
       SUM( 1 ) AS total_lines,
       SUM( t~seatsocc ) AS actual_occ,
       SUM( t~seatsocc + 1 ) AS total_occ,
       SUM( coalesce( t~seatsocc + 1 , 1 ) ) AS correct_occ 
  FROM scarr AS r
  LEFT OUTER JOIN sflight AS t ON t~carrid = r~carrid
  INTO TABLE @DATA(lt_data)
 WHERE r~carrid IN @lr_carrid
 GROUP BY r~carrid. 


~~~



~~~ABAP
REPORT demo_sql_expr_coalesce NO STANDARD PAGE HEADING.


CLASS demo IMPLEMENTATION.
  METHOD main.
    DATA itab LIKE TABLE OF wa WITH EMPTY KEY.
    out = cl_demo_output=>new(
     )->begin_section( `OUTER JOIN with Coalesce` ).
    SELECT t1~a AS a1, t1~b AS b1, t1~c AS c1, t1~d AS d1,
           COALESCE( t2~d, '--' ) AS d2,
           COALESCE( t2~e, '--' ) AS e2,
           COALESCE( t2~f, '--' ) AS f2,
           COALESCE( t2~g, '--' ) AS g2,
           COALESCE( t2~h, '--' ) AS h2
       FROM demo_join1 AS t1
         LEFT OUTER JOIN demo_join2 AS t2 ON t2~d = t1~d
       ORDER BY t1~d
       INTO CORRESPONDING FIELDS OF TABLE @itab.
    out->display( itab ).
  ENDMETHOD.
  METHOD class_constructor.
    DELETE FROM demo_join1.
    INSERT demo_join1 FROM TABLE @( VALUE #(
      ( a = 'a1' b = 'b1' c = 'c1'  d = 'uu' )
      ( a = 'a2' b = 'b2' c = 'c2'  d = 'uu' )
      ( a = 'a3' b = 'b3' c = 'c3'  d = 'vv' )
      ( a = 'a4' b = 'b4' c = 'c4'  d = 'ww' ) ) ).
    DELETE FROM demo_join2.
    INSERT demo_join2 FROM TABLE @( VALUE #(
      ( d = 'uu' e = 'e1' f = 'f1'  g = 'g1'  h = 'h1' )
      ( d = 'ww' e = 'e2' f = 'f2'  g = 'g2'  h = 'h2' )
      ( d = 'xx' e = 'e3' f = 'f3'  g = 'g3'  h = 'h3' ) ) ).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  demo=>main( ).
~~~

![image-20221007012235420](http://claudpic.oss-cn-hangzhou.aliyuncs.com/img/image-20221007012235420.png)



## Functions

### Windows Function



#### Lead && Lag

函数LEAD或LAG作为窗口函数需与**OVER**一起使用。**OVER**中，ORDER BY必须一起指定，partition可以选择性指定。适用于计算，例如确定当前行中的值与前一行或后一行的值之间的差异。

**LEAD*|*LAG( sql_exp1*[*, diff*[*, sql_exp2*] ]***

- **LEAD**：定位到位于当前行之后第[diff]行 。
- **LAG**：定位到位于当前行之前第[diff]行 。
- **diff** ：距离当前行的行数。如果diff没有指定，则默认值为1。即lead是的下一行，lag的前一行。
- **sql_exp1**&**sql_exp2**：SQL表达式。如果第diff行存在，则返回表达式sql_exp1所求的值；如果不存在，则返回表达式sql_exp2所求的值。如果**sql_exp2**未指定，则返回空值。

参考程序：demo_select_over_lead_lag。此例中使用的按照字段char1进行分区后按照字段char2进行排序。显示结果与SQL执行时的排序结果不一致，所以需要按照字段RNUM确定SQL执行时的排序推算结果。

~~~ABAP
    SELECT char1, char2,
           num1,
           ROW_NUMBER( )
             OVER( PARTITION BY char1 ORDER BY char2 )
               AS rnum,
           LEAD( CAST( num1 AS CHAR( 11 ) ), 1, 'Nope' )
             OVER( PARTITION BY char1 ORDER BY char2 )
               AS lead1,
           LAG( CAST( num1 AS CHAR( 11 ) ), 1, 'Nope' )
             OVER( PARTITION BY char1 ORDER BY char2 )
               AS lag1,
           LEAD( CAST( num1 AS CHAR( 11 ) ), 2, 'Nope' )
             OVER( PARTITION BY char1 ORDER BY char2 )
               AS lead2,
           LAG( CAST( num1 AS CHAR( 11 ) ), 2, 'Nope' )
             OVER( PARTITION BY char1 ORDER BY char2 )
               AS lag2,
           LEAD( CAST( num1 AS CHAR( 11 ) ), 10, 'Nope' )
             OVER( PARTITION BY char1 ORDER BY char2 )
               AS lead10,
           LAG( CAST( num1 AS CHAR( 11 ) ), 10, 'Nope' )
             OVER( PARTITION BY char1 ORDER BY char2 )
               AS lag10
           FROM demo_expressions
           ORDER BY char1, char2 "#EC CI_NOWHERE
           INTO TABLE @DATA(windowed_order_ascending) ##no_text.
    out->write( windowed_order_ascending ).
~~~


![image-20220929162813638](http://claudpic.oss-cn-hangzhou.aliyuncs.com/img/image-20220929162813638.png)



~~~ABAP
 SELECT char1, char2,
           num1,
           ROW_NUMBER( )
             OVER( PARTITION BY char1 ORDER BY char2 DESCENDING )
               AS rnum,
           LEAD( CAST( num1 AS CHAR( 11 ) ), 1, 'Nope' )
             OVER( PARTITION BY char1 ORDER BY char2 DESCENDING )
               AS lead1,
           LAG( CAST( num1 AS CHAR( 11 ) ), 1, 'Nope' )
             OVER( PARTITION BY char1 ORDER BY char2 DESCENDING )
               AS lag1,
           LEAD( CAST( num1 AS CHAR( 11 ) ), 2, 'Nope' )
             OVER( PARTITION BY char1 ORDER BY char2 DESCENDING )
               AS lead2,
           LAG( CAST( num1 AS CHAR( 11 ) ), 2, 'Nope' )
             OVER( PARTITION BY char1 ORDER BY char2 DESCENDING )
               AS lag2,
           LEAD( CAST( num1 AS CHAR( 11 ) ), 10, 'Nope' )
             OVER( PARTITION BY char1 ORDER BY char2 DESCENDING )
               AS lead10,
           LAG( CAST( num1 AS CHAR( 11 ) ), 10, 'Nope' )
             OVER( PARTITION BY char1 ORDER BY char2 DESCENDING )
               AS lag10
           FROM demo_expressions
           ORDER BY char1, char2 "#EC CI_NOWHERE
           INTO TABLE @DATA(windowed_order_descending) ##no_text.
    out->write( windowed_order_descending ).
~~~



![image-20220929162836074](http://claudpic.oss-cn-hangzhou.aliyuncs.com/img/image-20220929162836074.png)





#### First_value &Last_value

FIRST_VALUE函数返回一组已排序值的第一个值，LAST_VALUE返回一组排序值的最后一个值。

OVER和ORDER BY是必填项。

PARTITION BY是可选的。PARTITION 会将结果集划分为多个分区，FIRST_VALUE/LAST_VALUE函数将为每个分区返回一个结果（参见示例）。如果没有PARTITON BY子句，函数将在整个数据集中工作。

如果值为null或表达式为空，则返回null（参见示例，第H行）。

在Last_value中，framing是重要的的考虑因素。

**ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW**  默认参数。范围是从指定列的第一行到当前行。若指定列每行都一样，则返回当前行的值，若指定列的行是一样，则返回相等组中的最后一个值。如果。

**[ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING](adtcom:/sap/bc/adt/docu/abap/langu?object=abapselect_over&sap-language=EN&version=X).** 获取分组或数据集中的最后的值，可以使用次参数。



~~~ABAP
SELECT 
      id, 
      col1, 
      col2, 
      col3, 
      FIRST_VALUE( col2 ) OVER( PARTITION BY col1 ORDER BY col3 ) 
                  AS first_value, 
      LAST_VALUE( col2 ) OVER( PARTITION BY col1 ORDER BY col3 ) 
                  AS last_value, 
      LAST_VALUE( col2 ) OVER( PARTITION BY col1 ORDER BY col3 
                  ROWS BETWEEN UNBOUNDED PRECEDING 
                  AND UNBOUNDED FOLLOWING ) 
                  AS last_value_correct 
      FROM demo_update 
      INTO TABLE @DATA(result). 

 cl_demo_output=>display( result ). 
~~~

![image-20220929112952861](http://claudpic.oss-cn-hangzhou.aliyuncs.com/img/image-20220929112952861.png)

The column FIRST_VALUE returns the first value of COL2 for each partition. 

 The column LAST_VALUE does not return the last value. As described above, the default frame is from the first row to the current row. If COL3 contains duplicate values, the rows are considered equal and the last value from the group of equals is returned. To get the last value of COL2 of the partition, the frame size has to be specified explicitly, as demonstrated in LAST_VALUE_CORRECT. 



#### Runk & Dense_Rank

排名函数。如果一个结果集中不包含与排序条件相关的任何多行，RANK将生成与ROW_NUMBER相同的结果。由RANK确定的排名存在间隙，间隙为当前分组和排序条件下的与首行相同的行数。，使用DENSE_RANK可用于消除间隙。

可见**Over**中的Example 2中，Rank和SCHLANK列之前的差异。

![image-20220929192201476](http://claudpic.oss-cn-hangzhou.aliyuncs.com/img/image-20220929192201476.png)



#### NTILE

将结果集划分为n个bucket，遵循**OVER**中的partition和order by中指定的规则。如果无法整除，则按照余数，从小到大依次分配所有的bucket中。

~~~ABAP
”demo_select_over_ntile
    SELECT name,
           salary,
           NTILE( 5 ) OVER( ORDER BY salary ) AS ntile
           FROM demo_employees
           INTO TABLE @DATA(result).

    cl_demo_output=>display( result ).
~~~

![image-20220929204436824](http://claudpic.oss-cn-hangzhou.aliyuncs.com/img/image-20220929204436824.png)

#### Row_number

将函数为每行分配一个数据类型为INT8的行号，并且没有参数。此编号按照结果集中的处理顺序进行。该顺序未定义，或者可以通过在OVER之后指定添加order by来定义。如果在OVER之后未指定ORDER BY，则ROW_NUMBER仍会指定唯一的行号，但这些行号不会排序。

例如over中的Example 2中的列rnum.



#### Over

**Over** 紧跟在windows function后，用来限定win-function的操作区域。

**Over()**可以使用如下参数：

partition：指定分区条件，可以指定多个SQL表达式。如果未指定partition，则默认在所有结果集。

order by: 指定排序条件。默认是升序，使用descending可以改成降序。

rows between ... and... : 指定间隔行数。between和and的参数可以是 unbounded preceding | current row | (n) preceding | (n) following .

将当前行的值与窗口表达式的结果相结合，例如，当前结果集中某列的百分比，或与当前结果集的最小值或最大值的距离。

**Example1**

```ABAP
    DATA(out) = cl_demo_output=>new( ).
    SELECT char1, char2,
           num1,
           COUNT(*)      OVER( PARTITION BY char1 ) AS cnt,
           ROW_NUMBER( ) OVER( PARTITION BY char1 ) AS rnum,
           '-'                                      AS rank,
           '-'                                      AS schlank,
           MAX( num1 )   OVER( PARTITION BY char1 ) AS max,
           MIN( num1 )   OVER( PARTITION BY char1 ) AS min,
           SUM( num1 )   OVER( PARTITION BY char1 ) AS sum,
           division( 100 * num1, SUM( num1 ) OVER( PARTITION BY char1 ), 2 ) AS perc
           FROM demo_expressions
           ORDER BY char1, char2
           INTO TABLE @DATA(windowed_no_order).
    out->write( windowed_no_order ).

```

**partition**中指定了列char1，会按照char1中数值去重后分组，所以count(),max(),min(),sum()会按照分组条件进行计算。与group by类似，但是会保留原始明细数据。

![image-20220929191305900](http://claudpic.oss-cn-hangzhou.aliyuncs.com/img/image-20220929191305900.png)

**Exampl2**

~~~ABAP
    SELECT char1, char2,
           num1,
           COUNT(*)      OVER( PARTITION BY char1
                               ORDER BY char2 ) AS cnt,
           ROW_NUMBER( ) OVER( PARTITION BY char1
                               ORDER BY char2 ) AS rnum,
           RANK( )       OVER( PARTITION BY char1
                               ORDER BY char2 ) AS rank,
           DENSE_RANK( ) OVER( PARTITION BY char1
                               ORDER BY char2 ) AS schlank,
           MAX( num1 )   OVER( PARTITION BY char1
                               ORDER BY char2 ) AS max,
           MIN( num1 )   OVER( PARTITION BY char1
                               ORDER BY char2 ) AS min,
           SUM( num1 )   OVER( PARTITION BY char1
                               ORDER BY char2 ) AS sum,
           division( 100 * num1,
                     SUM( num1 ) OVER( PARTITION BY char1
                                       ORDER BY char2 ),
                     2 ) AS perc
           FROM demo_expressions
           ORDER BY char1, char2
           INTO TABLE @DATA(windowed_order_ascending).
    out->write( windowed_order_ascending ).
~~~

**partition**中指定了列char1分组，和char2列排序，count()，sum()会按照分组和排序条件进行计算，同一分组下，不同的char2，会逐级累加。例如  count()表达式中，char1=AA,char2=AA,CNT=4；当char1=AA,char2=AB时，CNT= 7 . 是 4[count(char2=AA)] +3[count(chart=AB)].。max()和min()仍然按照分组条件进行计算。

![image-20220929192201476](http://claudpic.oss-cn-hangzhou.aliyuncs.com/img/image-20220929192201476.png)

**Example 3**

~~~~ABAP
DELETE FROM demo_update.
    INSERT demo_update FROM TABLE @(
        VALUE #( ( id = 'A' col1 = '3' col2 = '7')
                 ( id = 'B' col1 = '5' col2 = '5')
                 ( id = 'C' col1 = '4' col2 = '5')
                 ( id = 'D' col1 = '1' col2 = '1')
                 ( id = 'E' col1 = '927' )
         ) ).

    SELECT id, col1, col2,
       COUNT( * ) OVER( ORDER BY id ROWS BETWEEN UNBOUNDED PRECEDING    “从第1行到当前行的行数
                                    AND CURRENT ROW )
                                    AS count,
       COUNT( * ) OVER( ORDER BY id ROWS BETWEEN CURRENT ROW			“从当前行到最后行的行数
                                    AND UNBOUNDED FOLLOWING )
                                    AS count_reverse,
       AVG( col1 ) OVER( ORDER BY id ROWS BETWEEN 1 PRECEDING			“当前行的前1行到后1行
                                     AND 1 FOLLOWING )
                                     AS average,
       SUM( col1 ) OVER( ORDER BY id ROWS BETWEEN UNBOUNDED PRECEDING   "
                                     AND CURRENT ROW )
                                     AS accumulate
       FROM demo_update
       INTO TABLE @DATA(result).
~~~~

![image-20220929194755669](http://claudpic.oss-cn-hangzhou.aliyuncs.com/img/image-20220929194755669.png)





### Aggregate Function



|      **关键字**      | **功能**                                                     | 常用方法                      |
| :------------------: | :----------------------------------------------------------- | ----------------------------- |
|         AVG          | 平均值                                                       |                               |
|        MEDIAN        | 中位数。如果数量是奇数，则取最中间的数，如果是偶数，则取中间两位数的平均数。 |                               |
|         MAX          | 最大值                                                       | MAX( col )  or Max(col1+col2) |
|         MIN          | 最小值                                                       |                               |
|         SUM          | 求和                                                         |                               |
|        STDDEV        | 标准差                                                       |                               |
|         VAR          | 方差                                                         |                               |
|         CORR         | 皮尔逊相关系数                                               |                               |
|    CORR_SPEARMAN     | 斯皮尔曼等级相关                                             |                               |
|      STRING_AGG      | 字符串拼接                                                   |                               |
|        COUNT         | 计数                                                         |                               |
|       GROUPING       | 用于判断字段是否参与聚合，用于GROUP BY GROUPING SETS 语法    |                               |
| ALLOW_PRECISION_LOSS | 允许精度丢失，用于加速浮点数计算                             |                               |
|       PRODUCT        |                                                              |                               |



#### String_agg

拼接sql_exp的结果到一行中。最长1333个字符。

~~~ABAP
DELETE FROM demo_expressions.
    INSERT demo_expressions FROM TABLE @( VALUE #(
      ( id = '5' char1 = 'nowhere' )
      ( id = '3' char1 = 'this' )
      ( id = '1' char1 = 'everybody' )
      ( id = '4' char1 = 'is' )
      ( id = '2' char1 = 'knows' ) ) ).

    SELECT STRING_AGG( char1,' ' )
           FROM demo_expressions
           INTO TABLE @DATA(unordered).
    cl_demo_output=>write( unordered ).  
"Output :  nowhere this everybody is knows
    SELECT STRING_AGG( char1,' ' ORDER BY id ASCENDING )
           FROM demo_expressions
           INTO TABLE @DATA(ordered_ascending).
    cl_demo_output=>write( ordered_ascending ).
"Output : everybody knows this is nowhere
    SELECT STRING_AGG( char1,' ' ORDER BY id DESCENDING )
           FROM demo_expressions
           INTO TABLE @DATA(ordered_descending).
    cl_demo_output=>write( ordered_descending ).
"Output : nowhere is this knows everybody
~~~



#### Count

统计数量。

~~~ABAP
DELETE FROM demo_expressions. 
INSERT demo_expressions 
       FROM TABLE @( VALUE #( 
        FOR i = 1 UNTIL i > 10 ( id = CONV #( i ) num1 = i ) ) ). 

SELECT FROM demo_expressions 
       FIELDS 
         COUNT( 
           CASE WHEN num1 < 4 THEN 'X' 
                WHEN num1 BETWEEN 4 AND 7 THEN 'Y' END ) AS cnt,   ”7
         COUNT( DISTINCT 
            CASE WHEN num1 < 4 THEN 'X' 
                 WHEN num1 BETWEEN 4 AND 7 THEN 'Y' END ) AS cntdist,   “2
         COUNT(*) AS cntstar   “10
       INTO TABLE @DATA(result). 

cl_demo_output=>display( result ). 

~~~

- 10 is the total number of rows in the result set determined using COUNT(*) and is independent of a single value.

- 7 is the number of rows determined using COUNT without DISTINCT in which [case distinction](adtcom:/sap/bc/adt/docu/abap/langu?object=abensql_simple_case&sap-language=EN&version=X) does not produce the null value.

- 2 is the number of distinct results "X" and "Y" determined using COUNT and DISTINCT of the [case distinction](adtcom:/sap/bc/adt/docu/abap/langu?object=abensql_simple_case&sap-language=EN&version=X) while ignoring the null value.



#### Grouping sets & Grouping

Grouping Sets 设置分组统计条件；grouping 判定维度是否在分组条件中，如果是分组条件，返回0，否则返回1。

~~~ABAP
  REPORT demo_grouping_function.

  SELECT FROM sflight
       FIELDS carrid,
              connid,
              planetype,
              SUM( seatsmax ) AS sum_seatsmax,
              grouping( carrid ) AS grouping_carrid,
              grouping( connid ) AS grouping_connid,
              grouping( planetype ) AS grouping_planetype
       WHERE carrid = 'LH'
       GROUP BY GROUPING SETS ( ( carrid, connid, planetype ),
                                ( carrid, connid ),
                                ( carrid, planetype ),
                                ( connid, planetype ),
                                ( carrid ),
                                ( connid ),
                                ( planetype ),
                                ( ) )
       INTO TABLE @DATA(result_grouping).

    cl_demo_output=>display( result_grouping ).
~~~

![image-20221006142245089](http://claudpic.oss-cn-hangzhou.aliyuncs.com/img/image-20221006142245089.png)



#### ALLOW_PRECISION_LOSS 

允许丢失一部分精度，来提高计算效率。

~~~ABAP
CREATE TABLE TESTTABLE (COL1 decimal(10,5), COL2 decimal);
INSERT INTO TESTTABLE VALUES(1.139999, 1.138888888);
INSERT INTO TESTTABLE VALUES(2.119999, 2.118888888);
INSERT INTO TESTTABLE VALUES(2.119999, 2.118888888);
INSERT INTO TESTTABLE VALUES(2.669999, 2.668888888);
				
-- The following query, which does not allow precision loss, returns 8.01, 8.01
SELECT SUM(TO_DECIMAL(COL1,10,2)), SUM(TO_DECIMAL(COL2,10,2)) FROM TESTTABLE;
				
-- The following query, which uses the ALLOW_PRECISION_LOSS function to allow precision loss, returns 8.04, 8.04
SELECT ALLOW_PRECISION_LOSS(SUM(TO_DECIMAL(COL1,10,2))), ALLOW_PRECISION_LOSS(SUM(TO_DECIMAL(COL2,10,2))) FROM TESTTABLE;
~~~







### Hierarchy_function????

[SELECT - FROM HIERARCHY - ABAP Keyword Documentation (sap.com)](https://help.sap.com/doc/abapdocu_753_index_htm/7.53/en-US/abenselect_hierarchy_definition.htm)



### Number Function

常见的数值表达式如下：

- ABS：获取绝对值
- CEIL：向上取整
- FLOOR：向下取整
- DIV：除法计算，取整数位
- DIVISION：除法计算，保留 N 位小数
- MOD：除法计算，取余数
- ROUND：计算舍入值

~~~ABAP
DATA(lv_dec) = CONV zdec_3_demo( '-123.456' ).

SELECT SINGLE
       @lv_dec AS original,
       abs( @lv_dec ) AS abs,
       ceil( @lv_dec ) AS ceil,
       floor( @lv_dec ) AS floor,
       div( -4 , -3 ) AS div,
       division( -4 , -3 , 2 ) AS division,
       mod( -4 , -3 ) AS mod,
       round( @lv_dec , 2 ) AS round_po,
       round( @lv_dec , -2 ) AS round_ne
  FROM sflight
  INTO @DATA(lwa_data). 


~~~

![img](http://claudpic.oss-cn-hangzhou.aliyuncs.com/img/20210714094020504.png)

### String Function

- CONCAT：连接字符串，参数固定为2个，各个表达式之间可以嵌套使用，CONCAT内部也可以使用 &&
- &&：连接字符串，参数没有个数限制，但不能将其他内嵌表达式当作参数使用，仅作为操作符使用，在非SELECT语句中也可以被使用
- CONCAT_WITH_SPACE：连接字符串，并用 N 个空格分隔，该表达式结果不能超过1333个字符
- INSTR：遍历字符串，查找指定字符 s1 并返回第一次出现的位置，没有查到则返回0
- LEFT/RIGHT：从字符串左/右侧开始取出 N 位字符，不忽略前导/尾部的空格
- LENGTH：计算字符串长度
- Lower / Upper: 大小写转换。
- Lpad /Rpad：指定字符将原字符串填充到指定长度。
- LTrim/RTrim：删除字符串开头或结尾的指定字符。
- Substring：*substring( char1,3,3)*从第3位截取char1，截取长度3.
- LIKE_REGEXPR ( PCRE = '\CA',  VALUE = char2 )：  满足正则表达式则返回1
- REPLACE_REGEXPR：满足正则表达式的值被value中指定的变量代理。
- OCCURRENCES_REGEXPR：  满足正则表达式的出现的次数。

~~~ABAP
REPORT demo_sql_function_string.

CLASS demo DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS main.
ENDCLASS.

CLASS demo IMPLEMENTATION.
  METHOD main.
    DELETE FROM demo_expressions.
    INSERT demo_expressions FROM TABLE @( VALUE #(
      ( id = 'X'
        char1 = ' 0123'
        char2 = 'aAaA' ) ) ).

    SELECT SINGLE
           char1 AS text1,
           char2 AS text2,
           CONCAT(              char1,char2 )     AS concat,
           CONCAT_WITH_SPACE(   char1,char2, 1 )  AS concat_with_space,
           INSTR(               char1,'12' )      AS instr,
           LEFT(                char1,3 )         AS left,
           LENGTH(              char1 )           AS length,
           LIKE_REGEXPR(        PCRE = '\CA',
                                VALUE = char2 )   AS like_regex,
           LOWER(               char2 )           AS lower,
           LPAD(                char1,10,'x' )    AS lpad,
           LTRIM(               char1,' ' )       AS ltrim,
           OCCURRENCES_REGEXPR( PCRE = '\CA',
                                VALUE = char2 )   AS occ_regex,
           REPLACE(             char1,'12','__' ) AS replace,
           REPLACE_REGEXPR(     PCRE = '\CA',
                                VALUE = char2,
                                WITH = 'bb' )     AS replace_regex,
           RIGHT(               char1,3 )         AS right,
           RPAD(                char1,10,'x' )    AS rpad,
           RTRIM(               char1,'3' )       AS rtrim,
           SUBSTRING(           char1,3,3 )       AS substring,
           UPPER(               char2 )           AS upper
           FROM demo_expressions
           INTO @DATA(result).

    cl_demo_output=>display( result ).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  demo=>main( ).
~~~

![image-20221007005216558](http://claudpic.oss-cn-hangzhou.aliyuncs.com/img/image-20221007005216558.png)

![image-20221007005232714](http://claudpic.oss-cn-hangzhou.aliyuncs.com/img/image-20221007005232714.png)





### Date&Time

- DATS_IS_VALID/TIMS_IS_VALID：校验日期/时间有效性，有效时返回 1，否则返回 0
- DATS_DAYS_BETWEEN：计算日期d1和d2相隔的天数
- DATS_ADD_DAYS：为指定日期加上N天
- DATS_ADD_MONTHS：为指定日期加上N月

~~~ABAP
DATA(lv_date) = CONV datum( '20181022' ). 

SELECT fldate AS original_date,
       dats_is_valid( fldate ) AS valid_date,
       tims_is_valid( @sy-uzeit ) AS valid_time, 
       dats_days_between( fldate , @lv_date ) AS between,
       dats_add_days( fldate , 10 ) AS add_days,
       dats_add_months( fldate , 3 ) AS add_months
  FROM sflight
  INTO TABLE @DATA(lt_data)
    UP TO 3 ROWS.

~~~

![img](http://claudpic.oss-cn-hangzhou.aliyuncs.com/img/20210714094231972.png)

### Time Stamp

TSTMP_IS_VALID：校验时间戳有效性，有效时返回 1，否则返回 0
TSTMP_CURRENT_UTCTIMESTAMP：返回当前时间戳
TSTMP_SECONDS_BETWEEN：计算时间戳 t1 和 t2 相隔的秒数，需要用赋值语句进行传参，可以添加相应的错误处理
TSTMP_ADD_SECONDS：为指定时间戳加上 N 秒，N 必须为 timestamp 类型

~~~ABAP
DATA(lv_stamp_now) = CONV timestamp( '20190603133559' ).
DATA(lv_stamp_past) = CONV timestamp( '20190602161408' ).

SELECT tstmp_is_valid( @lv_stamp_now ) AS valid_stamp,
       tstmp_current_utctimestamp( ) AS current_stamp,
       tstmp_seconds_between( tstmp1 = @lv_stamp_past,
                              tstmp2 = @lv_stamp_now,
                              on_error = @sql_tstmp_seconds_between=>set_to_null ) AS between,
       tstmp_add_seconds( tstmp = @lv_stamp_now,
                          seconds = @( CONV timestamp( 999 ) ),
                          on_error = @sql_tstmp_add_seconds=>set_to_null ) AS add_second
  FROM sflight
  INTO TABLE @DATA(lt_data)
    UP TO 1 ROWS.


~~~

![img](http://claudpic.oss-cn-hangzhou.aliyuncs.com/img/20210714094309697.png)



### TimeZone

- ABAP_USER_TIMEZONE：获取用户时区，不传参时默认获取当前用户当前 Client 的时区
- ABAP_SYSTEM_TIMEZONE：获取系统时区，不传参时默认获取当前 Client 的时区

~~~ABAP
SELECT abap_user_timezone( user = @( CONV uname( 'JIANGRE' ) ),
                           client = '130',
                           on_error = @sql_abap_user_timezone=>set_to_null ) AS user_zone,
       abap_system_timezone( client = '130',
                             on_error = @sql_abap_system_timezone=>set_to_null ) AS sys_zone
  FROM sflight
  INTO TABLE @DATA(lt_data)
    UP TO 1 ROWS. 


~~~



### **Date/Time Conversion**

- TSTMP_TO_DATS：将时间戳转换成对应时区的日期
- TSTMP_TO_TIMS：将时间戳转换成对应时区的时间
- TSTMP_TO_DST：根据时间戳获取对应时区的夏令时标识
- DATS_TIMS_TO_TSTMP：将日期和时间根据时区转换成时间戳

~~~
DATA(lv_stamp) = CONV timestamp( '20190603133559' ).

SELECT tstmp_to_dats( tstmp = @lv_stamp,
                      tzone = @( CONV tznzone( 'CET' ) ) ) AS dats,
       tstmp_to_tims( tstmp = @lv_stamp,
                      tzone = @( CONV tznzone( 'CET' ) ) ) AS tims,
       tstmp_to_dst( tstmp = @lv_stamp,
                     tzone = @( CONV tznzone( 'CET' ) ) ) AS dst,
       dats_tims_to_tstmp( date = @sy-datum,
                           time = @sy-uzeit,
                           tzone = @( CONV tznzone( 'CET' ) ) ) AS tstmp
  FROM sflight
  INTO TABLE @DATA(lt_data)
    UP TO 1 ROWS. 


~~~

![img](http://claudpic.oss-cn-hangzhou.aliyuncs.com/img/20210714094428372.png)





## Data Source

### Internal Table

在**ABAP 7.52** 后，支持将内表作为数据源使用内表作为数据源使用时，需要定义别名并使用**转义符@**，该用法可以用来代替 FOR ALL ENTRIES IN，但FROM 语句中最多使用一个内表。
dbtab~*选所有字段；

~~~ABAP
SELECT carrid, connid, countryfr, cityfrom
  FROM spfli INTO TABLE @DATA(lt_table) UP TO 3 ROWS.

SELECT s~*
  FROM scarr AS s
 INNER JOIN @lt_table AS l ON l~carrid = s~carrid
  INTO TABLE @DATA(lt_data). 

~~~

![在这里插入图片描述](http://claudpic.oss-cn-hangzhou.aliyuncs.com/img/20210716092514583.png)



### Subquery for MODIFY

![在这里插入图片描述](http://claudpic.oss-cn-hangzhou.aliyuncs.com/img/20210716092639102.png)

### Subquery for Select











## Open SQL中的代理服务???	

类[CL_OSQL_REPLACE](https://help.sap.com/http.svc/rc/abapdocu_752_index_htm/7.52/en-US/abencl_osql_replace.htm)可以在ABAP Unit单元测试中将数据库访问重定向至访问其它数据库的Open SQL。

该类只能在测试类中使用。示例程序：DEMO_CL_OSQL_REPLACE





## Access CDS View

### Access association 元素

~~~ABAP
SELECT id, 
       \_spfli_scarr-carrname AS carrier, 
       flight, 
       departure, 
       destination 
       FROM sap_technical 
       INTO TABLE @DATA(result).
~~~



