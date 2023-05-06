---
layout: post
title: Practical Business Data Analysis SQL Query (MySQL)
subtitle: SQL Data Analysis
gh-repo: daattali/beautiful-jekyll
gh-badge: [star, fork, follow]
tags: [SQL]
comments: true
---

#Daily Registrations

```sql
SELECT date(registtime),count(id)
FROM da_account WHERE date(registtime)>'2019-10-31'
GROUP BY date(registtime);

#Daily Registrions and the transaction made on the same day

SELECT date(tt),count(DISTINCT(aa))
FROM 
		(SELECT a.accountid as aa,a.createtime as tt
		from 
		(SELECT accountid,createtime FROM da_account_trade WHERE biztype in (1,3) AND inouttype=1 AND `status`=20) a
JOIN 
		(SELECT id,registtime  FROM da_account) b
ON a.accountid=b.id AND date(a.createtime)=date(b.registtime)
HAVING a.createtime>'2019-10-31') mm
GROUP BY 1;
```

#Transaction Count 

```sql
FROM
(SELECT CASE
	WHEN count(accountid)=1 THEN 'count 1 '
	when count(accountid)=2 THEN 'count 2'
	when count(accountid)=3 THEN 'count 3'
	WHEN count(accountid)>3 THEN 'count 3+' END as times,sum(amount+assetamount+bonusamount+couponamount) as sum 
FROM da_account_trade
WHERE biztype in (1,3) AND inouttype=1 AND `status`=20 AND date(createtime)='2020-10-31' 
AND accountid in (SELECT accountid FROM da_account_trade
WHERE date(createtime)='2020-10-31'
and `status` =20 and biztype in (1,3) and inouttype = 1)
GROUP BY accountid) aaa
GROUP BY 1;
```

#Transanction made on the day user registered

```sql
SELECT date(tt),count(DISTINCT(aa))
FROM 
		(SELECT a.accountid as aa,a.createtime as tt
		from 
		(SELECT accountid,createtime FROM da_account_trade WHERE biztype in (1,3) AND inouttype=1 AND `status`=20) a
JOIN 
		(SELECT id,registtime  FROM da_account) b
ON a.accountid=b.id AND date(a.createtime)=date(b.registtime)
HAVING a.createtime>'2018-12-25') mm
GROUP BY 1;
```
#Identifying the day users made their first transaction

```sql
SELECT
	accountid,
	createtime,
	sum,
	itemtitle,
	totalperiods
FROM
	(
		SELECT
			accountid,
			itemid,
			createtime,
			sum(
				amount + assetamount + bonusamount + couponamount
			) AS sum
		FROM
			account_trade
		WHERE
			biztype IN (1, 3)
		AND inouttype = 1
		AND `status` = 20
		AND date(createtime) BETWEEN '2019-01-06'
		AND '2016-08-10'
		AND accountid NOT IN (
			SELECT
				accountid
			FROM
				account_trade
			WHERE
				biztype IN (1, 3)
			AND inouttype = 1
			AND `status` = 20
			AND date(createtime) < '2019-01-06'
			GROUP BY
				1
		)
		AND accountid IN (
			SELECT
				id
			FROM
				account
			WHERE
				channel = 'edwa'
		)
		GROUP BY
			id
	) a
JOIN (
	SELECT
		id,
		itemtitle,
		totalperiods
	FROM
		selfitem
) b ON a.itemid = b.id;
```

#Average time spent on surfing

```sql
SELECT count(accountid),sum(diff),sum(diff)/count(accountid)
FROM
(SELECT a.accountid,a.createtime,b.createtime,timestampdiff(day,b.createtime,a.createtime) as diff
FROM
(SELECT accountid,createtime
FROM da_account_trade
where biztype in (1,3) AND inouttype=1 AND `status`=20 
AND accountid in 
		(SELECT id
		FROM da_account
		WHERE date(registtime)>'2015-5-31' 
		AND channel NOT in ('dands','kumi','ssow','xaaao','8868','d77u'))
GROUP BY  1) a
JOIN
(SELECT accountid,createtime
FROM da_account_trade
WHERE biztype=4 AND inouttype=1 AND `status`=20 
AND accountid in (SELECT id
  FROM ds_account
  WHERE date(registtime)>'2015-05-31' 
	AND iDdate is not null 
	AND channel NOT in ('dands','kumi','ssow','xaaao','8868','d77u')) 
GROUP BY 1) b
ON a.accountid=b.accountid
ORDER BY 4 DESC) seanw;



SELECT count(accountid),sum(diff),sum(diff)/count(accountid)
FROM
	(SELECT a.accountid,a.createtime as atime,b.createtime,timestampdiff(day,b.createtime,a.createtime) as diff
	FROM
	(SELECT accountid,createtime
	FROM da_account_trade
	where biztype in (1,3) AND inouttype=1 AND `status`=20 
	AND accountid in 
			(SELECT id
			FROM da_account
			WHERE date(registtime)>'2015-5-31' 
      AND commandid is null 
			AND channel NOT in ('dands','kumi','ssow','xaaao','8868','d77u'))
	GROUP BY  1) a
	LEFT JOIN
	(SELECT accountid,createtime
	FROM da_account_trade
	WHERE biztype=4 AND inouttype=1 AND `status`=20
	AND accountid in (SELECT accountid
		FROM da_account_trade
		where biztype in (1,3) AND inouttype=1 AND `status`=20 
		AND accountid in 
				(SELECT id
				FROM da_account
				WHERE date(registtime)>'2015-5-31' AND commandid is null 
				AND channel NOT in ('dands','kumi','ssow','xaaao','8868','d77u'))
		GROUP BY 1)) b
	ON a.accountid=b.accountid
  GROUP BY 1
	ORDER BY 4 DESC) seanw;
```    
    
    
