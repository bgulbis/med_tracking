SELECT DISTINCT
	CODE_VALUE.DISPLAY AS MEDICATION
FROM
	CODE_VALUE
WHERE
	CODE_VALUE.CODE_VALUE IN (
		37556009, -- acetaminophen
		37556051, -- albumin human
		37556077, -- alteplase
		627676649, -- ceftaroline
		37556551, -- cisatracurium
		37556587, -- coagulation factor VIIa
		37556681, -- daptomycin
		37556709, -- dexmedetomidine
		117038716, -- ertapenem
		37556889, -- esmolol
		37557425, -- levothyroxine
		37557675, -- niCARdipine
		926562948, -- prothrombin complex
		1895018730, -- sugammadex
		37558323 -- vasopressin
	)