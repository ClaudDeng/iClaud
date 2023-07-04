---
autoGroup-2: ABAPSQL
title: ABAP CDS 2
---



# ABAP CDS

## Group

完成聚集运算，例如MIN，MAX，COUNT，SUM等。使用聚集运算时，要使用group by指定聚集的件，也即按哪些字段进行分组统计。

~~~ABAP
@AbapCatalog.sqlViewName: 'ZDEMO_CDS_SQL'
@AbapCatalog.preserveKey: true
define view ZDEMO_CDS_DDL
  as select from sflight
{
  planetype,
  min(price) as min_price,
  max(price) as max_price,
  sum(price) as sum_price,
  count(*)   as count_planes

}
group by
  planetype


~~~

![在这里插入图片描述](http://claudpic.oss-cn-hangzhou.aliyuncs.com/img/20200501105228972.png)



## Having

指定聚集运算过程中的筛选条件.Having中指定的条件字段，只能是group by中的字段的子集；在Having中也可以使用聚集运算的中间结果集作为删选条件

~~~ABAP
@AbapCatalog.sqlViewName: 'ZDEMO_CDS_SQL'
@AbapCatalog.preserveKey: true
define view ZDEMO_CDS_DDL
  as select from sflight
{
  planetype,
  min(price) as min_price,
  max(price) as max_price,
  sum(price) as sum_price,
  count(*)   as count_planes

}
group by
  planetype
having
  planetype = '747-400'
  or count(*)  > 60

~~~

![在这里插入图片描述](http://claudpic.oss-cn-hangzhou.aliyuncs.com/img/20200501110245891.png)



## Join

CDS支持Inner Join， Left Outer Join，Right Outer Join。Inner Join， Left Outer Join， Right Outer Join的用法同ABAP OPEN SQL的用法一致

~~~ABAP
@AbapCatalog.sqlViewName: 'ZDEMO_CDS_SQL'
@AbapCatalog.preserveKey: true
define view ZDEMO_CDS_DDL
  as select from sbook   as a
    inner join   sflight as b on  a.carrid = b.carrid
                              and a.connid = b.connid
                              and a.fldate = b.fldate
{
  a.carrid    as airline_code,
  a.connid    as connection_number,
  a.fldate    as flight_date,
  a.customid  as customer_id,
  b.planetype as planetype
}
where
  a.carrid = 'DL'

~~~

## Union

UNION两个SELECT的结果集

- UNION可以合并两个SELECT的结果集，并自动去除重复的条目
- UNION ALL 合并结果集，保留重复的条目
- 合并的两个结果集要有相同字段数
- 合并结果集的列类型要兼容
- 字段名称要相同

~~~ABAP
@AbapCatalog.sqlViewName: 'ZDEMO_CDS_SQL'
@AbapCatalog.preserveKey: true
define view ZDEMO_CDS_DDL
  as select distinct from sbook
{
  carrid as airline_code,
  connid as connection_number,
  fldate as flight_date
}
where
  carrid = 'DL'

union

select distinct from sflight
{
  carrid as airline_code,
  connid as connection_number,
  fldate as flight_date
}
where
  planetype = '747-400'

~~~

![运行结果](http://claudpic.oss-cn-hangzhou.aliyuncs.com/img/20200501112622620.png)





## CDS with Parameters

在查询语句中，参数必须紧接在对应的数据源后边，作为一个整体来使用，否则不能通过语法检查 测试用CDS
View如下图所示，该视图需要传入两个参数 ip_matnr 以及 ip_langu

![在这里插入图片描述](http://claudpic.oss-cn-hangzhou.aliyuncs.com/img/20210715115325992.png)

~~~
SELECT *
  FROM zcds_demo( ip_matnr = 'DHA_DEMO_1',
                  ip_langu = @sy-langu ) AS a 
  INTO TABLE @DATA(lt_data). 

~~~





## CTE ??? 

[WITH - ABAP Keyword Documentation (sap.com)](https://help.sap.com/doc/abapdocu_753_index_htm/7.53/en-US/abapwith.htm)

可以通过[公用表表达式（common table expression，以下简称CTE）](https://help.sap.com/doc/abapdocu_753_index_htm/7.53/en-US/abapwith.htm)访问CDS视图，现在可以使用语句WITH的**WITH ASSOCIATIONS**附加项来发布这些视图的association，以便在当前WITH语句的路径表达式中使用。附加项**REDIRECT TO**也可以用于替换前CTE或当前CTE发布的association的目标数据源。







## 5，访问控制

新的关键字[**WITH PRIVILEGED ACCESS**](https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/abapselect_data_source.htm)可以关闭[CDS的访问控制](https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/abencds_access_control_glosry.htm)。
