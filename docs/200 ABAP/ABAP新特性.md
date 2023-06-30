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





## 数据操作

### String template

字符串模板由两个“|”字符括起来，创建一个字符串.字面文本包含所有不在大括号{}内的字符。 大括号内可包括如下内容:

- data objects, 数据对象
- calculation expressions, 计算表达式
- constructor expressions, 构造器表达式
- table expressions, 内表表达式
- predefined functions,  预定义功能

字符串模板在 | ... | 之间定义，主要分为两部分，固定文本和变量。其中，变量只能在 { ... } 内使用，大括号之外的所有字符均作为固定文本使用，空格始终不会被忽略。在固定文本中，如果出现 | ，{ } 或 \ 等特殊字符时，需要使用转义符 \。


例如：

~~~ABAP
SELECT * FROM scarr INTO TABLE @DATA(lt_scarr) .
cl_demo_output=>display( |Carrier: { lt_scarr [ carrid = 'LH’]-carrname }l ).
~~~



#### **Concatenation 串联 **

```ABAP
data(lv_string) = |Hello, { ev_code } Word|.
```



#### **Width/Align/Pad 宽度/对齐/填充 **

~~~ABAP
WRITE / |{ 'LEFT'       WIDTH = 20 ALIGN = LEFT      PAD = '0' }|.
WRITE / |{ 'CENTRE'     WIDTH = 20 ALIGN = CENTER    PAD = '0' }|.
WRITE / |{ 'RIGHT'      WIDTH = 20 ALIGN = RIGHT     PAD = '0'  }|.
WRITE / |{ 'RIGHT'      WIDTH = 20 ALIGN = RIGHT     PAD = '0' Case = lower }|. “输出的right变成小写
~~~

![image-20220923191131566](http://claudpic.oss-cn-hangzhou.aliyuncs.com/img/image-20220923191131566.png)

#### **Case 大小写**

```ABAP
WRITE / |{ 'Text' CASE = (CL_ABAP_FORMAT=>C_RAW)   }|.  or Raw Unchanged
WRITE / |{ 'Text' CASE = (CL_ABAP_FORMAT=>C_UPPER) }|.  or UPPER  Uppercase 
WRITE / |{ 'Text' CASE = (CL_ABAP_FORMAT=>C_LOWER) }|.	or Lower Lowercase
```



#### **ALPHA conversion**

```ABAP

DATA(LV_VBELN) = '0000012345'.
WRITE / |{ LV_VBELN  ALPHA = OUT }|. "out 去前导零
"Output result: 12345"
DATA(LV_VBELN) = '12345'.
WRITE / |{ LV_VBELN  ALPHA = in }|. "in 补前导零
"Output result: 0000012345"
```



#### **Date conversion**

```ABAP
   WRITE / |{ PA_DATE DATE = ISO }|.           "Date Format YYYY-MM-DD
   WRITE / |{ PA_DATE DATE = USER }|.          "As per user settings
   WRITE / |{ PA_DATE DATE = ENVIRONMENT }|.   "Formatting setting of language environment  using SET COUNTRY.
```



#### **Time conversion**

~~~
   WRITE / |{ PA_DATE TIME = ISO }|.           "Time Format in 24-hour format using colons (:) as separators: "hh:mm:ss".
   WRITE / |{ PA_DATE TIME = USER }|.          "As per user settings
   WRITE / |{ PA_DATE TIME = ENVIRONMENT }|.   "Formatting setting of language environment
~~~





#### **Sign**

| Keyword    | Value of dobj or expr        | Effect                                      |
| ---------- | ---------------------------- | ------------------------------------------- |
| LEFT       | CL_ABAP_FORMAT=>S_LEFT       | "-" left without space, no "+"              |
| LEFTPLUS   | CL_ABAP_FORMAT=>S_LEFTPLUS   | "-" and "+" left without space              |
| LEFTSPACE  | CL_ABAP_FORMAT=>S_LEFTSPACE  | "-" left without space, blank left for "+"  |
| RIGHT      | CL_ABAP_FORMAT=>S_RIGHT      | "-" right without space, no "+"             |
| RIGHTPLUS  | CL_ABAP_FORMAT=>S_RIGHTPLUS  | "-" and "+" right without space             |
| RIGHTSPACE | CL_ABAP_FORMAT=>S_RIGHTSPACE | "-" left without space, blank right for "+" |

```ABAP
DATA(text) = |{ 1 SIGN = LEFTPLUS }| .  
+1 
```



#### **Decimals**

```ABAP
DATA(text) = |{                  - 2 / 3   DECIMALS = 3 },  
              {  CONV decfloat34( - 2 / 3 ) DECIMALS = 3 }, 
              {  CONV          f( - 2 / 3 ) DECIMALS = 3 }|.  
-1.000, -0.667, -0.667 
```



#### **Zero**

| Keyword | Value of dobj or expr | Effect                                                       |
| ------- | --------------------- | ------------------------------------------------------------ |
| YES     | CL_ABAP_FORMAT=>Z_YES | The value zero is represented as a numeric value in accordance with the current formatting. |
| NO      | CL_ABAP_FORMAT=>Z_NO  | The value zero is represented as an empty string.            |

~~~ABAP
DATA(text) = |{ 0 ZERO = NO }, { 0 ZERO = YES }|.  
, 0 
~~~



#### **XSD**

当数据参照以下 domain 时，会根据规则转换成相应的 XML 数据【 XSD = [ YES | NO ] 】

![img](http://claudpic.oss-cn-hangzhou.aliyuncs.com/img/4d3314540bd541b38b5394489adf4901.png)

~~~ABAP
DATA: 
  flag1 TYPE xsdboolean VALUE abap_true, 
  flag2 TYPE xsdboolean VALUE abap_false. 

DATA(text) = |{ flag1 XSD = YES }, { flag2 XSD = YES }|.  
true, false 
~~~



#### **Currency**

~~~ABAP
*根据指定货币 cur 调整数值的小数位，参考表 TCURX 【 CURRENCY = cur 】
DATA(lv_currency) =  |{ lv_dec  CURRENCY = 'AU5' }|.  "五位小数,输出123.45678
DATA(lv_currency1) = |{ lv_dec  CURRENCY = 'BHD' }|. "三位小数，输出12345.678

~~~



#### **Number**

| Keyword     | Value of dobj or expr         | Effect                                                       |
| ----------- | ----------------------------- | ------------------------------------------------------------ |
| RAW         | CL_ABAP_FORMAT=>N_RAW         | The decimal separator is the period (.) and no thousands separators are inserted. |
| USER        | CL_ABAP_FORMAT=>N_USER        | The decimal and thousands separators are based on the [user master record](adtcom:/sap/bc/adt/docu/abap/langu?object=abenuser_master_record_glosry&sap-language=EN&version=X). |
| ENVIRONMENT | CL_ABAP_FORMAT=>N_ENVIRONMENT | The decimal and thousands separators are determined according to the current [formatting setting](adtcom:/sap/bc/adt/docu/abap/langu?object=abencountry&sap-language=EN&version=X) of the [language environment](adtcom:/sap/bc/adt/docu/abap/langu?object=abenlanguage_environment_glosry&sap-language=EN&version=X) that can be set using [SET COUNTRY](adtcom:/sap/bc/adt/docu/abap/langu?object=abapset_country&sap-language=EN&version=X). |

~~~ABAP
SET COUNTRY 'US'. 
DATA(text) = |{ 1000000 NUMBER = ENVIRONMENT }|.  
1,000,000 
~~~



#### **Timestamp **

| Keyword     | Value of dobj or expr          | Effect                                                       |
| ----------- | ------------------------------ | ------------------------------------------------------------ |
| SPACE       | CL_ABAP_FORMAT=>TS_SPACE       | The time stamp is represented according to the SQL standard ISO 9075, where there is a space between date and time, and a period (.) is used as a decimal separator: "yyyy-mm-dd hh:mm:ss.fffffff". |
| ISO         | CL_ABAP_FORMAT=>TS_ISO         | The time stamp is represented according to ISO 8601 for date formats and time specifications, where the character "T" is between date and time and a comma (,) is used as the decimal separator: "yyyy-mm-ddThh:mm:ss,fffffff". |
| USER        | CL_ABAP_FORMAT=>TS_USER        | As SPACE, but the date and time format, and the decimal separator are taken from the [user master record](adtcom:/sap/bc/adt/docu/abap/langu?object=abenuser_master_record_glosry&sap-language=EN&version=X). |
| ENVIRONMENT | CL_ABAP_FORMAT=>TS_ENVIRONMENT | As SPACE. However, the date and time format, and the decimal separator are defined according to the current [formatting setting](adtcom:/sap/bc/adt/docu/abap/langu?object=abencountry&sap-language=EN&version=X) of the [language environment](adtcom:/sap/bc/adt/docu/abap/langu?object=abenlanguage_environment_glosry&sap-language=EN&version=X) that can be set using [SET COUNTRY](adtcom:/sap/bc/adt/docu/abap/langu?object=abapset_country&sap-language=EN&version=X). |
| -           | CL_ABAP_FORMAT=>TS_RAW         | The time stamp is formatted as an uninterrupted sequence of numbers without separators, except for a decimal separator (.) before fractions of seconds. |

~~~ABAP
*设置时间戳格式
*SPACE：使用空格连接 ISO 格式下的日期和时间，yyyy-mm-dd hh:mm:ss
*ISO：使用 ISO 标准格式，yyyy-mm-ddThh:mm:ss
*RAW：无格式，直接输出
*USER：使用用户设置
*ENVIRONMENT：使用系统环境设置
DATA(lv_stamp) = CONV timestamp( '20190601181609' ).
DATA(lv_timestamp_space) = |{ lv_stamp TIMESTAMP = SPACE }|.
DATA(lv_timestamp_iso) = |{ lv_stamp   TIMESTAMP = ISO }|.
DATA(lv_timestamp_raw) = |{ lv_stamp  }|."当无格式时，在{}中填写TIMESTAMP = RAW则会报错，所有不需要写任何格式
DATA(lv_timestamp_USER) = |{ lv_stamp  TIMESTAMP = USER }|.
DATA(lv_timestamp_EVI) = |{ lv_stamp   TIMESTAMP = ENVIRONMENT }|.

~~~

![img](http://claudpic.oss-cn-hangzhou.aliyuncs.com/img/374d7693b6794b6896aafd57635a6af8.png)

#### **Timezone**

~~~

*将时间戳转换成指定时区的时间戳，参考表 TTZZ 【 TIMEZONE = tz 】
DATA(lv_timezone) = |{ lv_stamp TIMEZONE = 'CET' }|.
DATA(lv_timezone1) = |{ lv_stamp TIMEZONE = 'JAPAN' }|.


~~~

![image-20220928151058662](http://claudpic.oss-cn-hangzhou.aliyuncs.com/img/image-20220928151058662.png)







### String Function

#### Strlen

~~~ABAP
"获取字符串长度，当字符串类型为 CHAR 时，尾部空格会被忽略，当字符串类型为 STRING 时，尾部空格不会被忽略，仍会按字符被计入长度内
例：
DATA(lv_strlen_c) = strlen( CONV char10( |1234567   | ) ). 
"output length of lv_strlen_c : 7
DATA(lv_strlen_s) = strlen( CONV string( |ACDEFGH   | ) ).
"output length of lv_strlen_s : 10
~~~



#### Distance

~~~ABAP
计算将 text1 改动成 text2 的最少操作次数，每次操作仅允许删除/添加/更改 1 个字符
MAX 参数用来限制最大操作次数，当最少次数大于 max 时，返回 max
例：
DATA(lv_distance) = distance( val1 = '123ABC'  val2 = '124AC' ).  "输出值2
DATA(lv_dis_max) = distance( val1 = '123ABC'  val2 = '124ACDE' max = 3 ). ” “这里最小操作次数为4，但规定了max = 3,所以输出值为3

~~~



#### Find

~~~~ABAP
*搜索指定字符串并计算偏移量，没有遍历到时返回 1-
*可以使用 SUB ( 固定文本 ) 或者 REGEX ( 正则表达式 ) 作为指定条件进行搜索。CASE = [ abap_true | abap_false ]：大小写检查，默认需要检查大
*小写。case = abap_true需要检查大小写，case = abap_false不需要检查大小写，case = ‘ ’为case = abap_false。

DATA(lv_find_sub)  =  find( val = 'ABA123CAD' sub = 'a' case = ' ')."不区分大小写，查找到第一个值，输出为0
OCC = N：指定字符串在第 N 次出现，当 N 是负数时，从字符串右边开始遍历
DATA(lv_find_sub1) =  find( val = 'ABA123CAD' sub = 'a' case = ' ' occ = 3 ).
"不区分大小写，搜索A第三次出现的位置,输出为7
DATA(lv_find_sub2) =  find( val = 'ABA123CAD' sub = 'a' case = abap_true occ = 3 ).
"区分大小写，搜索A第三次出现的位置。没有结果，输出为1-
DATA(lv_find_sub3) =  find( val = 'ABA123CAD' sub = 'a' case = abap_false occ = 1 ).
"不区分大小写，搜索A第一次出现的位置，输出为0
DATA(lv_find_sub4) =  find( val = 'ABA123CAD' sub = 'A'  occ = -3 ).
"若occ为负数，则从右边开始遍历，比如occ=-3，则从右边数第三个A，为左边数第一个A，偏移量为0 ，输出为0
DATA(lv_find_sub5) =  find( val = 'ABA123CAD' sub = '8' ).
"遍历不到，输出为1-
DATA(lv_find_sub6) =  find( val = 'ABA123CAD' sub = 'a'  occ = 3 ).
" 这里没有写case,默认case区分大小写，输出为1-

"OFF = N  LEN = M：指定搜索区域，从第 N+1 为字符开始长度为 M 的范围
DATA(lv_find_reg)  = find( val = 'ABA123CAD' regex = '\d' off = 0 len = 3 ).
"从第一个字符开始，搜索3个字符，查找数值。未查找到，输出为1-,"\d"在正则表达式中是数字
DATA(lv_find_reg1)  = find( val = 'ABA123CAD' regex = '\d' off = 0 len = 6 ).
"从第一个字符开始，搜索六个字符，查找数字。第一个搜索出来的是1，偏移量为3，输出为3
DATA(lv_find_reg2) = find( val = 'ABA123CAD' regex = 'A' off = 0 len = 3 ).
"从第一个字符开始，搜索3个字符，查找A。偏移量为0，输出为0
DATA(lv_find_reg3) = find( val = 'ABA123CAD' regex = 'a' off = 0 len = 3 ).
"从第一个字符开始，搜索3个字符，查找a。未查找到a，输出为1-
DATA(lv_find_reg4) = find( val = 'ABA123CAD' regex = '123' off = 0 len = 6 ).
"从第一个字符开始，搜索6个字符，查找123。偏移量为3，输出为3

~~~~



#### Find End

~~~ABAP
*与 FIND 用法一致，但是偏移量会计算本身的长度
DATA(lv_find_end_sub) = find_end( val = 'ABA123CAD' sub = 'A' occ = 3 ).
DATA(lv_find_end_reg) = find_end( val = 'ABA123CAD' regex = '\d+' ). "\d+"表示一个或多个数字组成

~~~

#### Find_any_of & Find_any_not_of

~~~ABAP
*以下两种表达式只能使用 SUB 指定文本，且始终区分大小写
*搜索指定字符串中的任一字符并返回最小偏移量
DATA(lv_find_any) = find_any_of( val = 'ABA123CAD' sub = '1B' ). 
*搜索非指定字符串中的任意字符并返回最小偏移量
DATA(lv_find_any_not) = find_any_not_of( val = 'ABA123CAD' sub = '1B' ). 

~~~



#### Match

~~~
根据正则表达式匹配字符,如果在字符前后增加“.”，怎可以将字符前后的字符一起输出。
DATA(lv_match) = match( val = 'S1S2H3H4' regex = 'S.' occ = 2 ).   "S2,第二次出现的S，“.”是S后面的一个字符
DATA(lv_match1) = match( val = 'S1S2H3H4' regex = 'H...' occ = 1 ).  "H3H4
DATA(lv_match2) = match( val = 'S1S2H3H4' regex = 'H.' occ = 6 ).  "不出现值，因为没有第6个H
DATA(lv_match3) = match( val = 'S1S2H3H4' regex = '1.' occ = 1 ).  "1S
DATA(lv_match4) = match( val = 'S1S2H3H4' regex = 'S' occ = 1 ).  "S

~~~



#### Count

**Count, count_any_of, count_any_not_of **返回值是指定字符串出现的次数，因此不能指定 OCC 参数

~~~ABAP
*返回值是指定字符串出现的次数，因此不能指定 OCC 参数
DATA(lv_count) = count( val = 'ABA123CAD' sub = 'a' case = ' ' ). * 3
*计算指定字符串中的任一字符出现的总次数
DATA(lv_count_any) = count_any_of( val = 'ABA123CAD' sub = '1B' ).  * 2
*计算非指定字符串中任意字符出现的总次数
DATA(lv_count_not) = count_any_not_of( val = 'ABA123CAD' sub = '1B' ). * 7

~~~



#### Contains

用于判断包含关系，返回值为布尔型数据。当指定 START/END 时，直接从左侧/右侧比较对应长度的字符，用来判断首尾字符是否是sub中的字符，使用正则表达式时不可指定。

**contains_any_of**判断是否包含指定字符串中的任一字符。**contains_any_not_of**判断是否包含非指定字符串中的任意字符，只有当sub里拥有全部val中的字符时，才会报false。

~~~ABAP
例：
DATA(lv_contains) = xsdbool( contains( val = 'ABA123CAD' sub = 'a' case = ' ' ) ). "true
DATA(lv_contains_start) = xsdbool( contains( val = 'ABA123CAD' start = 'AB' ) ). "true
DATA(lv_contains_end) = xsdbool( contains( val = 'ABA123CAD' end = 'BD' ) ). "false
DATA(lv_contains_any) = xsdbool( contains_any_of( val = 'ABA123CAD' sub = 'E' ) ). "false
DATA(lv_contains_not) = xsdbool( contains_any_not_of( val = 'ABA123CAD' sub = '1B' ) ). "true
DATA(lv_contains_not1) = xsdbool( contains_any_not_of( val = 'ABA123CAD' sub = 'ABA123' ) )."true ？
DATA(lv_contains_not2) = xsdbool( contains_any_not_of( val = 'ABA123CAD' sub = 'ABA123CAD' ) )."false？

~~~



#### Replace

替换字符串，可以指定位置进行替换，也可以查找指定字符串并替换
WITH = new 指定用于替换的字符串
OCC = N 指定字符串第 N 次出现时进行替换，N 为 0 时表示需要全部替换
其他参数可参照 FIND 表达式

~~~ABAP
例：
DATA(lv_replace) = replace( val = 'ABA123CAD' off = 0 len = 4 with = '@12@' ).  "@12@23CAD
DATA(lv_replace_sub) = replace( val = 'ABA123CAD' sub = 'a' with = '@' case = ' ' ). " @BA123CAD
DATA(lv_replace_reg) = replace( val = 'ABA123CAD' regex = '\d' with = '#' occ = 0 ).  "AB###CAD

~~~



#### Insert

~~~ABAP
*插入字符串，可以使用 OFF 指定插入的位置，默认为 0
DATA(lv_insert) = insert( val = 'ABCD' sub = '123' off = 2 ). 

~~~



#### CMax & CMin

~~~ABAP
*返回数个字符串中的最大值/最小值，最多可以有 9 个参数，比较规则：按 0 - 9 ，A - Z，a - z 的顺序从小到大

DATA(lv_cmax) = cmax( val1 = 'aABC' val2 = 'ZABC' val3 = '0123' ).
DATA(lv_cmin) = cmin( val1 = 'aABC' val2 = 'ZABC' val3 = '0123' ). 

~~~



#### Condense

~~~ABAP
*压缩字符串，默认会移除头部/尾部的空格，其他部分的空格都会被压缩至 1 位
*DEL = del 指定需要删除的字符，指定后，从字符串两侧开始遍历并删除字符，直到出现非指定字符
*FROM = from  TO = to  处理完 DEL 后，再遍历字符串，将 from 中出现的字符，替换成 to 的第一位字符
*在遍历过程中，当同一个字符连续出现时，会被当成一个整体进行替换，所有字符均区分大小写
DATA(lv_condense_space) = condense( |  This  is   test | ).

DATA(lv_condense) = condense( val  = '  XXThis ISSS X sTringXX'
                              del  = |X |
                              from = 'TS' 
                              to   = 'to' ). 

~~~



#### Concat_lines_of

~~~ABAP
*将内表中所有的记录连接起来，通过 sep 指定分隔符
DATA: lt_data TYPE TABLE OF char10.
lt_data = VALUE #( ( 'ABC' ) ( '123' ) ( 'DEF' ) ).
DATA(lv_concat_lines) = concat_lines_of( table = lt_data sep = '@' ).   "ABC@123@DEF

~~~



#### Escape

~~~ABAP
*基于规则转义特定字符，FORMAT 用于指定转换规则
DATA(lv_escape_url) = escape( val    = 'http://www.google.com'
                              format = cl_abap_format=>e_url_full ).
*特殊字符的转移显示
DATA(lv_escape_string) = escape( val    = 'Special characters: |, \, {, }'
                                 format = cl_abap_format=>e_string_tpl ). 

~~~

![image-20220927203551132](http://claudpic.oss-cn-hangzhou.aliyuncs.com/img/image-20220927203551132.png)



#### Repeat

~~~ABAP
*循环字符串 N 次
DATA(lv_repeat) = repeat( val = 'ABC' occ = 5 ).  "ABCABCABCABCABC

~~~



#### REVERSE 

~~~~ABAP
*字符串反转
DATA(lv_reverse) = reverse( 'DEMO' ). "OMED

~~~~

#### Translate

~~~ABAP
*按照指定规则替换字符，from 和 to 中的字符一一对应，没有对应关系的字符会被删除
*如下例，Y对应A，Z对应C，B没有对应则被删除
DATA(lv_translate) = translate( val  = 'ABCDAB'
                                from = 'ACB'
                                to   = 'YZ' ). "输出YZDY
*下例中，1、2都没有对应，则val中的1和2都被删除
data(lv_translate2) = translate( val = '1234511223'
                                 from = '12'
                                 to = ''"输出3453

~~~



#### To_mixed

~~~ABAP
*处理大小写格式，首个字符大小写与 CASE 参数中的第一个字符一致，从第二个字符开始转换成小写
*sep 符号（默认为 _ ）后面的一个字符会被转换成大写，min 可以指定前 N 位中的 sep 符号不起作用
DATA(lv_to_mixed) = to_mixed( val = 'THIS is @A STRIN@G' sep = '@' case = 'X' min = 10 ).
"输出This is @a strinG
DATA(lv_to_mixed1) = to_mixed( val = 'THIS is A STRIN@G'  case = 'x' min = 10 ).
"输出this is a strin@g ，这里因为没有sep的值，所以后面的min也没有任何作用
DATA(lv_to_mixed2) = to_mixed( val = 'HEllo worLD' case = 'x').
"输出hello world
DATA(lv_to_mixed3) = to_mixed( val = 'HEllo w_orLD' sep = '_' case = 'x').
"输出hello wOrld
DATA(lv_to_mixed4) = to_mixed( val = 'HE_llo w_orLD' sep = '_' case = 'x' min = 2 ).
"输出heLlo wOrld
DATA(lv_to_mixed5) = to_mixed( val = 'HE_llo w_orLD' sep = '_' case = 'x' min = 3 ).
"输出he_llo wOrld
data(lv_to_mixed6) = to_mixed( val = '@HEllo worLD' sep = '@' case = 'x').
"输出@hello world,首字母当sep和case冲突的时候，以case为准
data(lv_to_mixed7) = to_mixed( val = '@HEllo worLD'  case = 'x' sep = '@').
"输出@hello world
data(lv_to_mixed8) = to_mixed( val = 'H@Ello worLD' sep = '@' case = 'x').
"输出hEllo world，要是sep生效，则生效的sep在输出中消失
data(lv_to_mixed9) = to_mixed( val = 'HE@llo worLD' sep = '@' case = 'x').
"输出heLlo world

~~~



#### From_mixed ??

~~~ABAP
*处理大小写格式，大小写与 CASE 参数中的第一个字符一致（默认大写），在转换前字符是大写的，会在该字符之前添加 sep 符号
DATA(lv_from_mixed) = from_mixed( val = 'This IS a string' )."输出THIS _I_S A STRING
DATA(lv_from_mixed1) = from_mixed( val = 'This IS a string' case = 'x' )."输出this _i_s a string
data(lv_from_mixed2) = from_mixed( val = 'hello worLD' )."输出HELLO WOR_L_D

~~~



#### TO_UPPER/TO_LOWER

~~~
*将字符串转换成大写/小写
DATA(lv_to_upper) = to_upper( val = 'this IS a string' ).
DATA(lv_to_lower) = to_lower( val = 'THIS IS A STRING' ). 

~~~



#### SHIFT_LEFT/SHIFT_RIGHT

~~~
*将字符串左移/右移 N 位
*指定 CIRCULAR 参数时，每次移除的字符需要被添加到另一侧
*指定 SUB 参数时，如果 SUB 与字符串左侧/右侧部分字符完全匹配，则移除这些字符
DATA(lv_left_places) = shift_left( val = 'ABCD’ places = 2 ).  "CD
DATA(lv_left_circular) = shift_left( val  = 'ABCD' circular = 3 ). "DABC
DATA(lv_left_sub) = shift_left( val = 'ABCD' sub = 'A' ). "BCD

DATA(lv_right_places) = shift_right( val = 'ABCD' places = 2 ). "AB
DATA(lv_right_circular) = shift_right( val  = 'ABCD' circular = 3 ). "BCDA
DATA(lv_right_sub) = shift_right( val = 'ABCD' sub = 'D' ).  "ABC

~~~



#### Substring

从第 off + 1 位开始取长度为 len 的字符串，如果截取范围超出原有字符长度，会抛出异常CX_SY_RANGE_OUT_OF_BOUNDS



#### Substring_from

~~~ABAP
*从指定文本 sub 或是正则表达式 regex 匹配到的字符串（包含本身）开始截取，默认截至最后一位
*len 指定长度，occ 指定出现次数，case 指定大小写检查,case = abap_true 检查大小写，case = abap_false不检查大小写
DATA(lv_substring_from) = substring_from( val = 'ABCDEFGH' sub = 'DEF' )."DEFGH
DATA(lv_substring_from1) = substring_from( val = 'ABCACBABC' sub = 'A'  len = 2 )."AB
DATA(lv_substring_from2) = substring_from( val = 'ABCACBABC' sub = 'A' occ = 2 )."ACBABC
DATA(lv_substring_from3) = substring_from( val = 'ABCaCABABC' sub = 'a' case = abap_true )."aCABABC
DATA(lv_substring_from4) = substring_from( val = 'AbBCbabBC' sub = 'B' case = abap_false )."bBCbabBC,当case = abap_false时，不检查大小写
DATA(lv_substring_from5) = substring_from( val = 'ABCbabBC' sub = 'b' case = abap_false )."BCbabBC
DATA(lv_substring_from6) = substring_from( val = 'ABCaCaABABC' sub = 'a' len = 2 occ = 2 case = abap_true )."aA
DATA(lv_substring_from7) = substring_from( val = 'ABCaCaABABC' sub = 'A' len = 3 occ = 3 case = abap_true )."ABC

~~~

#### Substring_after

~~~ABAP
*从指定文本后一位开始截取，不包含本身
DATA(lv_substring_after) = substring_after( val = 'ABCDEFGH' sub = 'DEF' )."GH
DATA(lv_substring_after1) = substring_after( val = 'ABCDEFGH' sub = 'eF' case = abap_false  )."GH
DATA(lv_substring_after2) = substring_after( val = 'ABCDEFGH' sub = 'g' )."g不在字符中，所以没有数据

~~~



#### Substring_before

~~~ABAP
*从第一位字符开始，截取到指定文本前一位，不包含本身
DATA(lv_substring_before) = substring_before( val = 'ABCDEFGH' sub = 'DEF' )."ABC
data(lv_substring_before1) = substring_before( val = '1234' sub = '34' )."12

~~~

#### Substring_to

~~~ABAP
*从第一位字符开始，截取到指定文本结束，包含本身
DATA(lv_substring_to) = substring_to( val = 'ABCDEFGH' sub = 'DEF' )."ABCDEF
data(lv_substring_to1) = substring_to( val = '12345' sub = '34' )."1234

~~~



#### Segment

~~~~ABAP
*根据分隔符获取指定位置的字符串，可以用来拆分字符串，INDEX 用来指定位置，指定位置不存在时，会抛出异常 CX_SY_STRG_PAR_VAL
*通过 SEP 指定的分隔符会被当做一个整体进行操作，当分隔符连续出现时，该位置会返回空字符串；
*而通过 SPACE 指定的分隔符中，每个字符都会被视作单独的分隔符，且在分隔符连续出现时也不会单独返回空串
DO.
  TRY.
      lv_sep = segment( val   = 'AB;CD ;EF ; ;GH'
                        index = sy-index
                        sep = ' ;' ).
    CATCH cx_sy_strg_par_val.
      EXIT.
  ENDTRY.
ENDDO. 

DO.
  TRY.
      lv_space = segment( val   = 'AB  CD - EF_GH'
                          index = sy-index
                          space = ' -_' ).
    CATCH cx_sy_strg_par_val.
      EXIT.
  ENDTRY.
ENDDO.

~~~~

![image-20220928083703986](http://claudpic.oss-cn-hangzhou.aliyuncs.com/img/image-20220928083703986.png)





### Numeric Functions

> **ABS**：取绝对值
> **SIGN( N )**：N>0时返回 1；N<0时返回 -1；N=0时返回 0 CEIL：向上取整
> **FLOOR**：向下取整
> **TRUNC**：取整数位
> **FRAC**：取小数位
> **IPOW**：计算幂值，可以用来代替 ** 使用，避免部分数据丢失精度
> **NMAX/NMIN**：返回参数中的最大值/最小值，参数最多传入 9 个
> **ROUND**：计算舍入值，DEC 指定舍入位置，可以使用 MODE指定舍入规则
> **RESCALE**：与 ROUND 用法一致，但是当需要保留的位数大于实际位数时，RESCALE 会在尾部填充 0，而 ROUND不会



~~~ABAP

DATA(lv_sign) = sign( lv_num ).
DATA(lv_ceil) = ceil( lv_num ).
DATA(lv_floor) = floor( lv_num ).
DATA(lv_trunc) = trunc( lv_num ).
DATA(lv_frac) = frac( lv_num ).
DATA(lv_ipow) = |{ ipow( base = '1.2’ exp = 2 ) } , { ( '1.2' ** 2 ) }|.
DATA(lv_nmax) = nmax( val1 = lv_ceil val2 = lv_floor ).
DATA(lv_nmin) = nmin( val1 = lv_ceil val2 = lv_floor ).
DATA(lv_round) = round( val = lv_num dec = 3 ).
DATA(lv_rescale) = rescale( val = lv_num dec = 8 ). 


~~~

![image-20220928084353071](http://claudpic.oss-cn-hangzhou.aliyuncs.com/img/image-20220928084353071.png)





## 内表操作

### 内表表达式

~~~ABAP
*内表读取不再需要使用 READ TABLE，直接使用类似于数组的方式去读取
*与READ TABLE读表方式类似，可以通过 INDEX 去读取指定位置的行，也可以根据条件去获取行，但无法指定BINARY SEARCH
*默认情况下如果没有读到记录，会抛出异常 CX_SY_ITAB_LINE_NOT_FOUND
SELECT
  carrid, connid, countryfr, cityfrom
  FROM spfli INTO TABLE @DATA(lt_table) UP TO 3 ROWS.
"carrid  connid  countryfr  cityfrom
"AA      0017       US      NEW YORK
"AA      0064       US      SAN FRANCISCO
"AZ      0555       IT       ROME

DATA(lv_line_index) = lt_table[ 1 ]-carrid.       "AA
DATA(lv_line_index1) = lt_table[ 3 ]-countryfr.   "IT

DATA(lwa_line_field) = lt_table[ carrid = 'AZ'
                                 connid = '0555' ].    "AZ 0555 IT ROME
DATA(lwa_line_field1) = lt_table[ carrid = 'AA'         "需要全部的关键词，确认唯一性
                                  connid = '0017' ].     "AA 0017 US NEW YORK

~~~



#### Optional

~~~ABAP
*使用 OPTIONAL 语句时，没有读到记录也不会抛异常，而是返回空的结构
DATA(lwa_line_optional) = VALUE #( lt_table[ 4 ] OPTIONAL ). "没有数值，返回的值都是零：   0000
DATA(lwa_line_optional1) = VALUE #( lt_table[ 3 ] OPTIONAL ). "输出：AZ 0555 IT ROME

~~~



#### Default

~~~ABAP
*使用 DEFAULT 语句，在没有读到记录时，返回一个默认值，如果系统不支持这两种，则需要使用 TRY 语句来捕获异常
DATA(lwa_line_default) = VALUE #( lt_table[ 4 ] DEFAULT VALUE #( carrid = 'ZZ'
                                                                 connid = '0239'
                                                                 countryfr = 'SU'
                                                                 cityfrom = 'CITY_NO' ) ).  
"ZZ 0239 SU CITY_NO
DATA(lwa_line_default1) = VALUE #( lt_table[ 4 ] DEFAULT VALUE #( carrid = 'AA'
                                                                 connid = '0245'
                                                                 countryfr = 'IT'
                                                                 cityfrom = 'CITY_NO' ) ).  
"AA 0245 IT CITY_NO
data(lwa_line_default2) = value #( lt_table[ 4 ]  )."报错 
data(lwa_line_default3) = value #( lt_table[ 3 ]  )."AZ 0555 IT ROME
DATA(lwa_line_default4) = VALUE #( lt_table[ 2 ] DEFAULT VALUE #
                                                                 ( carrid = 'AA' 
                                                                   connid = '0064' 
                                                                countryfr = 'US' 
                                                                 cityfrom = 'SAN FRANCISCO' ) ).
"AA 0064 US SAN FRANCISCO
"第二个value有更改，default的值也不会改变
DATA(lwa_line_default5) = VALUE #( lt_table[ 2 ] DEFAULT VALUE #
                                                                  ( carrid = 'AB' 
                                                                    connid = '0033' 
                                                                 countryfr = 'US' 
                                                                  cityfrom = 'SAN FRANCISCO' ) ).
"AA 0064 US SAN FRANCISCO

~~~



### 内表函数

- **Lines**  计算内表总行数 
- **Line_exists** 判断根据特定条件能否在内表中读取到记录，返回值为布尔型数据
- **Line_index**  获取内表中满足特定条件的记录所在的行数( INDEX ),如果存在多个条件值，则只会返回第一个搜索到的值的行数

~~~ABAP

SELECT * FROM spfli INTO TABLE @DATA(lt_table) UP TO 3 ROWS.
DATA(lv_lines) = lines( lt_table ).
DATA(lv_exist) = xsdbool( line_exists( lt_table[ carrid = 'AZ' ] ) ).
DATA(lv_index) = line_index( lt_table[ carrid = 'AZ' ] ). 

~~~










###  分组  Loop at < table > Group By <  key >

1. 根据field值进行分组

* 在 LOOP 中使用 GROUP BY 后，LS_EMPLOYEE中不会存储相应的数据，同样，如果使用 FIELD-SYMBOL，也不会被分配，如果需要修改内表数据，只能通过每个组进行修改，
* 对内表数据进行分组时，可通过 ASCENDING / DESCENDING 按组排序，否则按前后的顺序依次输出；
* GROUP BY 在需要使用多个字段进行分组时：GROUP BY  (  KEY1 = field1  KEY2 = field2  …  )
* GROUP SIZE  按照条件分组后，该分组的条数
* GROUP index 按照条件分组后，该分组所处的位置

~~~ABAP
TYPES: BEGIN OF TY_EMPLOYEE,
         NAME TYPE CHAR30,
         ROLE TYPE CHAR30,
         AGE  TYPE I,
       END OF TY_EMPLOYEE,
       TY_EMPLOYEE_T TYPE STANDARD TABLE OF TY_EMPLOYEE WITH KEY NAME.
DATA(GT_EMPLOYEE) = VALUE TY_EMPLOYEE_T(
( NAME = 'Mao'         ROLE = 'ABAP guru'       AGE = 29 )
( NAME = 'Zhangjie'    ROLE = 'FI Consultant'   AGE = 33 )
( NAME = 'Hujianchun'  ROLE = 'ABAP guru'       AGE = 37 )
( NAME = 'XiaoLiu'     ROLE = 'FI Consultant'   AGE = 31 )
( NAME = 'Xiuxianhai'  ROLE = 'ABAP guru'       AGE = 30 )
( NAME = 'Huangping'   ROLE = 'SD Consultant'   AGE = 42 ) ).
DATA: GV_TOT_AGE TYPE I,
      GV_AVG_AGE TYPE DECFLOAT34.
"Loop with grouping on Role
"如果需要知道Size 和index,需要使用完整的表达式。
"( ROLE  = LS_EMPLOYEE-ROLE KEY2 = field2
"	SIZE  = GROUP SIZE 
"	INDEX = GROUP INDEX )
*LOOP AT GT_EMPLOYEE INTO DATA(LS_EMPLOYEE)
*  GROUP BY LS_EMPLOYEE-ROLE
*  ASCENDING
*  ASSIGNING FIELD-SYMBOL(<GROUP>).
LOOP AT GT_EMPLOYEE INTO DATA(LS_EMPLOYEE)
  GROUP BY ( ROLE  = LS_EMPLOYEE-ROLE
             SIZE  = GROUP SIZE
             INDEX = GROUP INDEX )
  ASCENDING
  ASSIGNING FIELD-SYMBOL(<GROUP>).
  CLEAR: GV_TOT_AGE.
  "Output info at group level
  WRITE: / |Group:{ <GROUP>-INDEX }    Role: { <GROUP>-ROLE WIDTH = 15 }|
              & |     Number in this role: { <GROUP>-SIZE }|.
  "Loop at members of the group
  LOOP AT GROUP <GROUP> ASSIGNING FIELD-SYMBOL(<LS_MEMBER>).
    GV_TOT_AGE = GV_TOT_AGE + <LS_MEMBER>-AGE.
    WRITE: /13 <LS_MEMBER>-NAME.
  ENDLOOP.
  "Average age
  GV_AVG_AGE = GV_TOT_AGE / <GROUP>-SIZE.
  WRITE: / |Average age: { GV_AVG_AGE }|.
  SKIP.
ENDLOOP.
~~~

**Output**

![](http://claudpic.oss-cn-hangzhou.aliyuncs.com/img/image-20220923204036339.png)

2.  如果需要根据自定义条件进行分组，可以使用 COND 语句将特定条件转换成字符或数字再进行分组

~~~ABAP
TYPES: BEGIN OF TY_DATA,
         MATNR TYPE MARA-MATNR,
         MTART TYPE MARA-MTART,
         MATKL TYPE MARA-MATKL,
         TEXT1 TYPE CHAR50,
       END OF TY_DATA.

DATA LT_DATA TYPE TABLE OF TY_DATA.
DATA LT_TABLE TYPE TABLE OF TY_DATA.

LT_DATA = VALUE #( ( MATNR = 'Material-001'
                     MTART = 'FOOD'
                     MATKL = '1020'
                     TEXT1 = 'FIRST' )
                   ( MATNR = 'Material-002'
                     MTART = 'WATR'
                     MATKL = '1030'
                     TEXT1 = 'SECOND' )
                   ( MATNR = 'Material-003'
                     MTART = 'WATR'
                     MATKL = '1010'
                     TEXT1 = 'THIRD' ) ).

loop at lt_data into data(lwa_data)
                group by ( matkl = cond string( when lwa_data-matkl = '1020' then 'A'  else 'B' )
                           lines = group size
                           index = group index
                          )
                descending into data(g1).
  write : g1-lines .
  lt_table = value #( for lwa_table in group g1 ( lwa_table ) ).
  call method cl_demo_output=>display( lt_table ).
endloop.

~~~

![image-20220924211737360](http://claudpic.oss-cn-hangzhou.aliyuncs.com/img/image-20220924211737360.png)





### 过滤Filter

根据另一个表中的记录筛选表中的记录，可以减少多层循环

 FILTER type( itab [EXCEPT] [IN ftab] [USING KEY keyname] 
      WHERE c1 op f1 [AND c2 op f2 […]] )

* 使用 FILTER 时，待过滤的内表结构至少需要有一个用于访问的 SORTED KEY 或 HASHED KEY，
* 使用关键字“EXCEPT”（参见上面的定义）将返回完全相反的记录，即除上面返回的记录之外的所有记录。

1. 使用内表过滤

~~~ABAP
TYPES: BEGIN OF TY_FILTER,
         CITYFROM TYPE SPFLI-CITYFROM,
         CITYTO   TYPE SPFLI-CITYTO,
         F3       TYPE I,
       END OF TY_FILTER,
       TY_FILTER_TAB TYPE HASHED TABLE OF TY_FILTER
           WITH UNIQUE KEY CITYFROM CITYTO.
DATA: LT_SPLFI TYPE STANDARD TABLE OF SPFLI.
SELECT * FROM SPFLI APPENDING TABLE LT_SPLFI.
DATA(LT_FILTER) = VALUE TY_FILTER_TAB( F3 = 2
                          ( CITYFROM = 'NEW YORK'  CITYTO  ='SAN FRANCISCO' )
                          ( CITYFROM = 'FRANKFURT' CITYTO = 'NEW YORK' )  ).
DATA(LT_MYRECS) = FILTER #( LT_SPLFI IN LT_FILTER
                                  WHERE CITYFROM = CITYFROM
                                    AND CITYTO = CITYTO ).
"Output filtered records
LOOP AT LT_MYRECS ASSIGNING FIELD-SYMBOL(<LS_REC>).
  WRITE: / <LS_REC>-CARRID,8 <LS_REC>-CITYFROM,30
           <LS_REC>-CITYTO,45 <LS_REC>-DEPTIME.
ENDLOOP.
~~~

![img](http://claudpic.oss-cn-hangzhou.aliyuncs.com/img/20200227202130613.png)

2. 使用Where语句过滤

* 否则不能通过语法检查，另外，在 WHERE 条件中运算符两边的字段类型需要完全兼容，否则也不能通过语法检查；
* 根据条件进行过滤的功能可以使用 VALUE 嵌套 FOR 语句实现，而且不用考虑内表的键值问题



~~~
TYPES: BEGIN OF TY_DATA,
         MATNR TYPE MARA-MATNR,
         MTART TYPE MARA-MTART,
         MATKL TYPE MARA-MATKL,
         TEXT1 TYPE CHAR50,
       END OF TY_DATA.
DATA:LT_DATA TYPE TABLE OF TY_DATA WITH KEY MATNR WITH NON-UNIQUE SORTED KEY MATKL COMPONENTS MATKL.

LT_DATA = VALUE #( ( MATNR = 'Material-001'
                     MATKL = '1020'
                     TEXT1 = 100 )
                   ( MATNR = 'Material-002'
                     MATKL = '1030'
                     TEXT1 = 200 )
                   ( MATNR = 'Material-003'
                     MATKL = '1020'
                     TEXT1 = 300 ) ).

DATA(LT_FILTER) = FILTER #( LT_DATA USING KEY MATKL WHERE MATKL = CONV MATKL( '1020' ) )."CONV对值进行类型转换
CALL METHOD CL_DEMO_OUTPUT=>DISPLAY( LT_FILTER ).

DATA(LT_FILTER2) = FILTER #( LT_DATA USING KEY MATKL WHERE MATKL > CONV MATKL( '1020' ) )."可以填入比较运算符
CALL METHOD CL_DEMO_OUTPUT=>DISPLAY( LT_FILTER2 ).

~~~





## Class 操作

### New 实例化

使用 NEW 创建（实例化）引用对象，用来代替CREATE OBJECT

- NEW dtype（ value ) …创建一个类型为dtype的匿名数据对象，然后传值给创建的对象（左操作符）。

![img](https://pic3.zhimg.com/80/v2-ae28d7f5536c60e10538a0af2b371cfa_720w.webp)

![img](https://pic3.zhimg.com/80/v2-31cb409c4e6bac2ce688fbd695bcd7ca_720w.webp)

- NEW class（ p1 = a 1 p2 = a2 … ) …创建一个名为class类的实例，并且传参到实例的构造函数。
- NEW #（ … ) …根据操作数类型创建一个匿名数据对象或者一个类的实例。 如 a = new #（ … ), new #( … )这里创建的匿名类是参考a的类型来的，即和a的类型一致。

~~~ABAP
" zcl_fi_me_check_ob52类对象
  gs_task-task_chk  = new zcl_fi_me_check_ob52( iv_action = zcl_fi_me_check_ob52=>c_open_n ).
  gs_task-task_obj = new zcl_ba_background_job( iv_job_name = |{ gv_batch }_{ gs_task-task_name  } |
                                                io_appl_log = gcl_appl_log  ).
~~~



### Ref

使用 REF 定义引用变量，用来代替 CREATE DATA。

~~~ABAP
*---------------------------------------------------------------------*
*    在使用 REF时，不需要提前声明变量，也不用指定类型，
*      类型默认会与被指向的变量保持一致
*---------------------------------------------------------------------*
TYPES: BEGIN OF TY_MARA,
         MATNR TYPE MARA-MATNR,
         MTART TYPE MARA-MTART,
         MATKL TYPE MARA-MATKL,
         TEXT1 TYPE CHAR50,
       END OF TY_MARA.

DATA(LW_MARA) = VALUE TY_MARA( MATNR = 'MATERIAL-001'
                               MTART = 'FOOD'
                               MATKL = '1010'
                               TEXT1 = 'FIRST' ).

CALL METHOD CL_DEMO_OUTPUT=>DISPLAY( LW_MARA ).

DATA(LV_REF) = REF #( LW_MARA ).

CALL METHOD CL_DEMO_OUTPUT=>DISPLAY( LW_MARA ).

LV_REF->* = VALUE #( MATNR = 'MATERIAL-002'
                     MTART = 'WATR'
                     MATKL = '1020' ).

CALL METHOD CL_DEMO_OUTPUT=>DISPLAY( LW_MARA ).

LV_REF->TEXT1 = 'SECONDE'.

CALL METHOD CL_DEMO_OUTPUT=>DISPLAY( LW_MARA ).

~~~







## Key Words

### Base

1. 结构中使用

~~~
struct2 = VALUE #( BASE struct1  col3 = 33 ).
"First, all components are taken over from struct1, then the columns specified are overwritten.
~~~



~~~
”it works like MOVE-CORRESPONDING, col3 of struct2 keeps its former value.
struct2 = CORRESPONDING #( BASE ( struct2 ) struct1 ).
~~~



2. 内表中使用

~~~
DATA itab TYPE TABLE OF i.

itab = VALUE #( ( 1 ) ( 2 ) ( 3 ) ).
itab = VALUE #( BASE itab ( 4 ) ( 5 ) ( 6 ) ).

"itab中的值等于原始itab中，加上第二次value中新增的 4，5 ，6.
~~~



### CORRESPONDING

类似与MOVE-CORRESPONDING，将源结构或内表的值移动到目标结构或内表的同名字段中。

> CORRESPONDING type( [BASE ( base )] struct|itab [mapping] )

1. 工作区

~~~ABAP
REPORT ZBASE.
DATA:
  BEGIN OF struct1,
    col1 TYPE i VALUE 11,
    col2 TYPE i VALUE 12,
    col3 TYPE i VALUE 13,
  END OF struct1.

DATA:
  BEGIN OF struct2,
    col2 TYPE i VALUE 22,
    col3 TYPE i VALUE 23,
    col4 TYPE i VALUE 24,
  END OF struct2.

*This is not the same as
*MOVE-CORRESPONDING struct2 TO struct1.
*Since the RHS does not know anything of the LHS, component col3 of struct2 does not
*keep its former value but is initialized.
“注意与**MOVE-CORRESPONDING**的区别，struct2中存在但struct1中不存在的字段无法保持原值，会被初始化。
" struct2 = CORRESPONDING #( struct1 ).

*The result of the expression is initialized with struct2 and then the evaluation takes place.
* Now it works like MOVE-CORRESPONDING, col3 of struct2 keeps its former value.
*基于struct2的值进行初始化，然后struct2和struct1公用字段被struct1中的字段所覆盖
struct2 = CORRESPONDING #( BASE ( struct2 ) struct1 ).

”赋值ls_stru2 对应字段到 ls_stru1；并使用Mapping将 ls_stru2-col3 映射到 ls_stru1-col1
ls_stru1 = CORRESPONDING #( ls_stru2 MAPPING col1 = col3 ).

“赋值ls_stru2 对应字段到 ls_stru1；并且使用 EXCEPT 排除字段COL1不赋值（ls_stru1-col2的值会被清空，不清空请看下一个案例）
ls_stru1 = CORRESPONDING ty_stru2( ls_stru2 EXCEPT col2 ).

”赋值ls_stru2 对应字段到 ls_stru1；并且使用 EXCEPT 排除字段COL1不赋值（使用 base ( ls_stru1 ) 保留ls_stru1-col1的值）
ls_stru1 = CORRESPONDING ty_stru2( base ( ls_stru1 ) ls_stru2 EXCEPT col1 ).

"3.类似于mapping的功能，不同名字字段之间传值 (这个语法可以用户和外部接口数据赋值的时候使用）
DATA:BEGIN OF customer_oa,
       id   TYPE char10 VALUE '1000000001',
       name TYPE char35 VALUE 'OA_NAME',
       BEGIN OF information,
         company_id         TYPE char4 VALUE '6100',
         sales_organization TYPE char4 VALUE '6200',
         country            TYPE char3 VALUE 'CN',
       END OF information,
     END OF customer_oa,
     BEGIN OF customer_sap,
       kunnr TYPE kna1-kunnr,
       name1 TYPE kna1-name1,
       BEGIN OF info,
         bukrs TYPE knb1-bukrs,
         vkorg TYPE knvv-vkorg,
         land1 TYPE kna1-land1,
       END OF info,
     END OF customer_sap.

"两个名字不相同的工作区相互赋值

customer_sap = CORRESPONDING #(
                                customer_oa MAPPING
                                kunnr = id 
                                name1 = name
                                ( info = information MAPPING
                                  bukrs = company_id
                                  vkorg = sales_organization
                                  land1 = country
                                  ) ).

WRITE:/ 'sap:' , customer_sap .
WRITE:/ 'oa:' , customer_oa.

~~~

2. 内表

使用语法相同，使用内表类型的变量代替结构类型的变量即可。

The CORRESPONDING operator has two variants, one **base form** and one with a **lookup table** . The latter constructs an internal table by looking up another table and using the lines found there as sources of the corresponding assignment:

~~~ABAP
"CORRESPONDING
LT_T001 = CORRESPONDING #( LT_BSEG MAPPING RBUKRS = BUKRS
                                           DOCLN  = BUZEI ).
"
TYPES: 
  BEGIN OF line, 
    value   TYPE i, 
    comment TYPE string, 
  END OF line, 
  itab1 type STANDARD TABLE OF line WITH EMPTY KEY, 
  itab2 TYPE HASHED TABLE OF line WITH UNIQUE KEY value. 

DATA(itab1) = VALUE itab1( for i = 1 UNTIL i >= 10 ( value = i ) ). 
DATA(itab2) = VALUE itab2( ( value = 2 comment = `...` ) 
                           ( value = 3 comment = `...` ) 
                           ( value = 5 comment = `...` ) 
                           ( value = 8 comment = `...` ) ). 
"如果itab1和itab2中value列的值相同，则itab1中comment的值会被itab2里面的值覆盖
itab1 = CORRESPONDING itab1( itab1 FROM itab2 USING value = value ). 
~~~



### EXACT

EXACT 可以用来检查操作语句返回值是否存在丢失，如果存在丢失则会抛出异常

~~~ABAP
*---------------------------------------------------------------------*
* 抛出异常的范围比CONV更广，例如将CHAR10的数据赋值到CHAR1时，
* 因此在使用时需要注意异常的捕获，如果没有特殊的处理或属性需求，
* 可以直接使用父类异常 CX_SY_CONVERSION_ERROR 进行捕获.
* 判断数值语句是否被精确计算，如下实际抛出的异常是CX_SY_CONVERSION_ROUNDING，
* 获取到该异常类中的属性字段VALUE的数据
*---------------------------------------------------------------------*

TRY .
    DATA(LV_DATA) = EXACT #( 3 * ( 1 / 3 ) ).
  CATCH CX_SY_CONVERSION_ROUNDING INTO DATA(LO_EXCEPT).
    DATA(LV_RESULT) = LO_EXCEPT->VALUE.
ENDTRY.

~~~

![image-20220925151253383](http://claudpic.oss-cn-hangzhou.aliyuncs.com/img/image-20220925151253383.png)





### Let

LET关键字可以使用在VALUE，SWITCH，COND等语句中，与 FOR 语句类似，LET 语句中定义的临时变量同样只能在当前语句中使用.

~~~ABAP
cl_demo_output=>display(
  VALUE string_table(
    FOR i = 1 WHILE i <= 100 (
" COND string: line type is string
" LET: define local variable r3 and r5 - local auxiliary fields. 
      COND string( LET r3 = i MOD 3 
                       r5 = i MOD 5 IN
                   WHEN r3 = 0 AND r5 = 0 THEN |FIZZBUZZ|
                   WHEN r3 = 0            THEN |FIZZ|
                   WHEN r5 = 0            THEN |BUZZ|
                   ELSE i ) ) ) ).

~~~



~~~ABAP
REPORT ZLET.

CLASS demo DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS main.
ENDCLASS.

CLASS demo IMPLEMENTATION.
  METHOD main.
* another example
typeS: BEGIN OF ty_data,
                 index type int4,
                 value type int4,
                 name type string,
           end of ty_Data.
data: lt_data TYPE STANDARD TABLE OF ty_data.

LOOP AT itab ASSIGNING FIELD-SYMBOL(<fs>).
  data(value) = value ty_Data( LET x = <fs> y = x + 1  r = r && x && y in index = sy-index value = x + y  name = r ).
  APPEND value TO lt_data.
ENDLOOP.  

    TYPES text TYPE STANDARD TABLE OF string WITH EMPTY KEY.
* IN后面的才是重点，能产生输出的语句。
    cl_demo_output=>new( )->write(
     VALUE text( LET it = `Jerry` IN
                   ( |To { it } is to do|          )
                   ( |To { it }, or not to { it }| )
                   ( |To do is to { it }|          )
                   ( |Do { it } do { it } do|      ) )
    )->display( ).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  demo=>main( ).


~~~





### Conv

部分数据类型的转换可以用 CONV 实现**CONV dtype|#(… )**

~~~ABAP
"7.40之前表达式 
DATA text   TYPE c LENGTH 255.
DATA helper TYPE string.
DATA xstr   TYPE xstring.
helper = text.
xstr = cl_abap_codepage=>convert_to( source = helper ).
~~~



~~~ABAP
"7.40之后DATA text TYPE c LENGTH 255.
text = 'zhujx'.
DATA(xstr1) = cl_abap_codepage=>convert_to( source = CONV string( text ) ).
*OR
DATA(xstr2) = cl_abap_codepage=>convert_to( source = CONV #( text ) )."转化为16进制
~~~



### Cond

动态赋值语句，可以根据不同条件来动态处理，用法类似于CASE/IF语句

~~~ABAP
*---------------------------------------------------------------------*
* COND语句中允许使用较为复杂的判断条件，因此VALUE语句中动态赋值通常会使用COND
* when 、then、else后都可使用表达式
*---------------------------------------------------------------------*
DATA(time) = COND string(
                   WHEN sy-timlo < '120000' THEN |{ sy-timlo TIME = ISO } AM|
                   WHEN sy-timlo > '120000' THEN |{ CONV t( sy-timlo - 12 * 3600 ) TIME = ISO } PM|
                   WHEN sy-timlo = '120000' THEN |High noon|
*                   ELSE |400 error|
                     ).
WRITE: time.

~~~



### Switch

动态赋值语句，通常根据同一变量的不同数据来动态处理，用法类似于 CASE 语句

~~~ABAP
*---------------------------------------------------------------------*
* SWITCH语句的判断条件相对单一，WHEN关键字后只能使用常量，
*      THEN/ELSE后面可以使用表达式进行赋值
*---------------------------------------------------------------------*
DATA(LV_INDICATOR) = 1.
DATA(LV_DAY) = SWITCH CHAR10( LV_INDICATOR
                              WHEN 1 THEN 'MONDAY'
                              WHEN 2 THEN 'TUESDAY'
                              WHEN 3 THEN 'WEDNESDAY'
                              WHEN 4 THEN 'THURSDAY'
                              WHEN 5 THEN 'FRIDAY'
                              WHEN 6 THEN 'SATURDAY'
                              WHEN 7 THEN 'SUNDAY'
                              ELSE '404' && '-ERROR'
                             ).
CALL METHOD CL_DEMO_OUTPUT=>DISPLAY( LV_DAY ).

*Output : MONDAY.

~~~



### Value

1. 结构Structures:  VALUE dtype|#( comp1 = a1 comp2 = a2 … )

```ABAP
  TYPES: BEGIN OF ty_columnns2,  “Nested structure
                 coln1 TYPE i, 
                 coln2 TYPE ty_columns1, 
              END OF ty_columns2.
 
  DATA: struc_simple TYPE ty_columns1, 
            struc_nest    TYPE ty_columns2.
“Simple structure
struct_Simple   = VALUE t_struct( coln1 = 1  coln2  = 2 ).
“Nested structure
struct_nest   = VALUE t_struct( coln1 = 1 
 								coln2-cols1 = 1 
 								coln2-cols2 = 2 ).
struct_nest   = VALUE t_struct(coln1 = 1 
                               coln2 = VALUE #( cols1 = 1 cols2 = 2 ) ).
```



2. 内表Tables:         VALUE dtype|#( ( … ) ( … ) … ) …

~~~ABAP
TYPES t_itab TYPE TABLE OF i WITH EMPTY KEY.
 
DATA itab TYPE t_itab.
"内表创建
itab = VALUE #( ( ) ( 1 ) ( 2 ) ).
”类似append
itab2 = value #( base itab 
				 (4)
                 (5) ).
 
Structured line type (RANGES table):
DATA itab TYPE RANGE OF i.
"内表创建 可以指定某一行的某一列都是同一个值而不用每一行都要输入该列值。
itab = VALUE #( sign = ‘I’  option = ‘BT’ ( low = 1  high = 10 ) 
										  ( low = 21 high = 30 ) 
										  ( low = 41 high = 50 ) 
							 option = ‘GE’ ( low = 61 )  ).
~~~



### For

1. 内表循环 **FOR wa|<fs> IN itab [INDEX INTO idx] [cond]**

~~~ABAP
DATA(gt_citys） = VALUE ty_citys( FOR ls_ship IN gt_ships  where ( route ='R0001’ )
								( city = ls_ship-city country = ls_ship-country ) ).


~~~



2. 条件循环 **FOR with THEN and UNTIL|WHILE**

~~~ABAP
*旧语法：
TYPES:BEGIN OF ty_line,
        a TYPE i,
        b TYPE i,
        c TYPE i,
      END OF ty_line,
      ty_t_line TYPE TABLE OF ty_line WITH EMPTY KEY.
DATA j TYPE i.
DATA gt_lines1 TYPE ty_t_line.
FIELD-SYMBOLS <ls_line1> TYPE ty_line.
j = 0.

DO.
  j = j + 1.
  IF j > 10. EXIT. ENDIF.
  APPEND INITIAL LINE TO gt_lines1 ASSIGNING <ls_line1>.
  <ls_line1>-a = j.
  <ls_line1>-b = j + 1.
  <ls_line1>-c = j + 2.
ENDDO.

*新语法 x未表达式中隐式定义的变量
DATA(gt_lines2) = VALUE ty_t_line( FOR x = 1 THEN x + 1 UNTIL x > 10
								   ( a = x 
								   	 b = x + 1 
								   	 c = x + 2 ) ).

~~~



### Reduce

REDUCE循环迭代构造数据，这个关键字的作用和在大规模数据集并行计算领域里广泛使用的"Map-Reduce"编程模型中的Reduce操作类似，可以按照字面意思理解为“归约”。可以计算下列几种数据

1. 统计表中符合要求的数据有多少行
2. 循环累加计算总金额
3. 循环拼接字符串

虽然VALUE和NEW 表达式可以包含FOR表达式，但REDUCE必须至少包含一个FOR表达式。您可以在REDUCE中使用各种FOR表达式：IN用于迭代内表或有UNTIL 或者 WHILE的条件迭代。

~~~ABAP
"循环累加计算总金额
DATA(lv_sum) = REDUCE wertv12( INIT x = 0 FOR wa2 IN lt_fag NEXT x = x + wa2-tslvt + wa2-tsl01 + wa2-tsl02  ).
"循环拼接字符串
DATA(lv_string) = REDUCE string( INIT  tex = `递增数据：` FOR n = 1 THEN N + 1 UNTIL n > 9 NEXT tex = tex && n   ).
"循环拼接字符串
    v_req_parameters = reduce string( init v_param = ``
                        for s_parameters in it_parameters
                        next v_param = |{ v_param }&{ s_parameters-name }='{ s_parameters-value }'| ).
    shift v_req_parameters left deleting leading '&'.
~~~

~~~ABAP
*LOOP AT itab result [cond] GROUP BY key ( key1 = dobj1 key2 = dobj2 …
*      [gs = GROUP SIZE] [gi = GROUP INDEX] )
*      [ASCENDING|DESCENDING [AS TEXT]]
*      [WITHOUT MEMBERS]
*      [{INTO group}|{ASSIGNING <group>}]
*      …
*      [LOOP AT GROUP group|<group>
*      …
*      ENDLOOP.]
*      …
*ENDLOOP.
*
*… REDUCE type(
*INIT result = start_value
*           …
*FOR for_exp1
*FOR for_exp2
*…
*NEXT …
*           result = iterated_value
*… )


"首先创建一个内表
TYPES:BEGIN OF ty_data,
        id       TYPE i, "人员ID
        name     TYPE char10, "人员名称
        country  TYPE char10, "国家
        language TYPE char2, "语言
        age      TYPE i,
      END OF ty_data,
      ty_t_data TYPE STANDARD TABLE OF ty_data WITH EMPTY KEY.

"数据
DATA(gt_data) = VALUE ty_t_data(
( id = 1 name = 'Jerry' country = 'China' language = 'ZH' age = 18 )
( id = 2 name = 'Jack' country = 'China' language = 'ZH' age = 19 )
( id = 3 name = 'Nick' country = 'Korea' language = 'EN' age = 20 )
( id = 4 name = 'Rossi' country = 'Korea' language = 'EN' age = 25 )
( id = 5 name = 'Randy' country = 'Korea' language = 'EN' age = 23 )
( id = 6 name = 'Tab' country = 'China' language = 'ZH' age = 22 )
( id = 7 name = 'Lily' country = 'Korea' language = 'EN' age = 21 )
( id = 8 name = 'Lucy' country = 'China' language = 'EN' age = 24 )
( id = 9 name = 'Zera' country = 'China' language = 'EN' age = 28 )
( id = 10 name = 'Grace' country = 'China' language = 'EN' age = 19 )
).

"REDUCE
"1计算年龄最大
DATA(lv_age_max_zh) = REDUCE i( INIT x = 0 FOR lw_data IN gt_data
                      WHERE ( language = 'ZH' ) NEXT x = nmax( val1 = x
                                                               val2 = lw_data-age )
                                                               ) .
WRITE:/ |说中文的人中年龄最大的是：{ lv_age_max_zh  } |.

"2.输出的reduce
TYPES:outref TYPE REF TO if_demo_output.
DATA(output) = REDUCE outref( INIT out = cl_demo_output=>new( )
                              text = 'Count up：'
                              FOR n = 1 UNTIL n > 11
                              NEXT out = out->write( text )
                              text = | { n } | ).
output->display( ).

"分组循环
"1.ls_data这个工作区里面是没有内容的
"2.<group>里面只有size index 和分组参数
LOOP AT gt_data INTO DATA(ls_data) GROUP BY ( country = ls_data-country language = ls_data-language
  size = GROUP SIZE index = GROUP INDEX ) ASCENDING ASSIGNING FIELD-SYMBOL(<group>).

  WRITE:/ |Group:{ <group>-index } Country :{ <group>-country } language : { <group>-language }| &
  | Number lines :{ <group>-size } |.

  "3.按照<group>中的分组参数循环 GT_data中的数据
  LOOP AT GROUP <group> ASSIGNING FIELD-SYMBOL(<ls_member>).
    WRITE:/ | Name:{ <ls_member>-name } |.
  ENDLOOP.
  DATA(lv_age_max) = REDUCE i( INIT max = 0 FOR lw_member IN GROUP <group>
                             NEXT max = nmax( val1 = max val2 = lw_member-age ) ).
  DATA(lv_age_min) = REDUCE i( INIT min = 100 FOR lw_member IN GROUP <group>
                               NEXT min = nmin( val1 = min val2 = lw_member-age ) ).
  DATA(lv_age_sum) = REDUCE i( INIT sum = 0 FOR lw_member IN GROUP <group>
                               NEXT sum = sum + lw_member-age ).
  DATA(lv_age_avg) = lv_age_sum / <group>-size.

  WRITE:/ | 该组最大年龄，最小年龄和平均年龄分别为:{ lv_age_max } { lv_age_min } { lv_age_avg } |.
ENDLOOP.



~~~

~~~ABAP
*---------------------------------------------------------------------*
*  REDUCE语句返回值为结构时，常用来汇总字段或作其他处理（例如取最大值/最小值/平均值等）
*---------------------------------------------------------------------*
TYPES:BEGIN OF LTY_RESULT,
        SUM   TYPE S_DISTANCE,
        COUNT TYPE I,
        MAX   TYPE S_DISTANCE,
        MIN   TYPE S_DISTANCE,
      END OF LTY_RESULT.
TYPES: LTY_TABLE TYPE TABLE OF LTY_RESULT WITH DEFAULT KEY.

SELECT DISTANCE FROM SPFLI INTO TABLE @DATA(LT_TABLE) UP TO 5 ROWS WHERE DISTANCE > 0.

DATA(LWA_RESULT) = REDUCE #( INIT LWA_TMP = VALUE LTY_RESULT( )
                             FOR LWA_TABLE IN LT_TABLE
                             INDEX INTO LV_INDEX
                             NEXT LWA_TMP = VALUE #( BASE LWA_TMP
                                                     SUM   = LWA_TMP-SUM + LWA_TABLE-DISTANCE
                                                     COUNT = LWA_TMP-COUNT + 1
                                                     MAX   = NMAX( VAL1 = LWA_TMP-MAX VAL2 = LWA_TABLE-DISTANCE )
                                                     MIN   = COND #( WHEN LV_INDEX = 1 THEN LWA_TABLE-DISTANCE
                                                                     ELSE NMIN( VAL1 = LWA_TMP-MIN VAL2 = LWA_TABLE-DISTANCE ) ) ) ).

CALL METHOD CL_DEMO_OUTPUT=>DISPLAY( LWA_RESULT ).
~~~

![image-20220928152556028](http://claudpic.oss-cn-hangzhou.aliyuncs.com/img/image-20220928152556028.png)
