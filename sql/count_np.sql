WITH
/*
Select each case, indicating 'true' if all charges are NP else 'false'
*/
case_table AS (
SELECT 
	d1.case_number as case_number,
	CASE WHEN true = ALL(
		SELECT CASE
		WHEN disposition ILIKE '%NOLLE PROSEQUI%' THEN true
		ELSE false END
		FROM dscr_charges
		WHERE case_number = d1.case_number
	) THEN 'true' ELSE 'false' END AS np
FROM dscr d1
JOIN dscr_related_persons
ON d1.case_number = dscr_related_persons.case_number
WHERE dscr_related_persons.name ILIKE 'leepa, matthew'
GROUP BY d1.case_number
),

/*
Count the total cases and the number of cases where all charges are NP
*/
count_table AS(
SELECT
	COUNT(CASE WHEN 'true' = np THEN 1 ELSE null END) as np,
	COUNT(case_number) as total
FROM case_table
)

/*
Select the count where all charges are NP and the total count and divide to get rate
*/
SELECT
	np,
	total,
	np*100/total AS rate
FROM count_table
