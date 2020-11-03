Example queries to find/count cases where all charges are nolle prosequi (NP)

* `case_charges.sql` gets the charges for a particular case and their disposition
* `list_np.sql` lists cases and indicates true/false whether or not all charges have disposition NP
* `select_np.sql` selects only cases where all charges have disposition NP
* `count_np.sql` counts # of cases where all charges have disposition NP

Only handles one officer name at a time and doesn't account for the officer names not being normalized.
