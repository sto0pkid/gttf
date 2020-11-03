SELECT 
	dscr.case_number as case_number,
	dscr_charges.disposition as disposition
FROM dscr
JOIN dscr_charges
ON dscr.case_number = dscr_charges.case_number
WHERE dscr.case_number = '0B02314844'
