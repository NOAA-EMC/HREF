function formula (args)

'numargs  'args
 numargs = result

       n  = 1
while( n <= numargs )
       qname.n = subwrd(args,n)
       n  = n + 1
endwhile

* Formula:  DQDTTOT = DQDT_MOIST + DQDT_TURB + DQDT_DYN
* ----------------------------------------------------------------
say 'define 'qname.1' = 'qname.2' + 'qname.3' + 'qname.4

'seasonalf -FUNCTION 'qname.2'+'qname.3'+'qname.4' -NAME 'qname.1
 newfile = result

return newfile
