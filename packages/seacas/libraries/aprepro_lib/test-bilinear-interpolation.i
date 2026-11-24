{a = make_array(2,2)}
{NOECHO}
{a[0,0] = 1}
{a[0,1] = 2}
{a[1,0] = 3}
{a[1,1] = 4}
{ECHO}
row-only = {a[0.5,0]}
column-only = {a[0,0.5]}
center = {a[0.5,0.5]}
