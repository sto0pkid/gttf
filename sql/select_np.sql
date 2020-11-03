SELECT 
	d1.case_number as case_number
FROM dscr d1
JOIN dscr_related_persons
ON d1.case_number = dscr_related_persons.case_number
WHERE dscr_related_persons.name ILIKE 'leepa, matthew'
AND true = ALL(
	SELECT CASE
	WHEN disposition ILIKE '%NOLLE PROSEQUI%' THEN true
	ELSE false END
	FROM dscr_charges
	WHERE case_number = d1.case_number
)
GROUP BY d1.case_number
