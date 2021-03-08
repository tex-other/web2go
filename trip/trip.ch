@x 309
@d stat==@{ {change this to `$\\{stat}\equiv\null$' when gathering
  usage statistics}
@d tats==@t@>@} {change this to `$\\{tats}\equiv\null$' when gathering
  usage statistics}
@y
@d stat== 
@d tats== 
@z

@x 390
@!mem_max=30000; {greatest index in \TeX's internal |mem| array;
@y
@!mem_max=3000; {greatest index in \TeX's internal |mem| array;
@z

@x 393
@!mem_min=0; {smallest index in \TeX's internal |mem| array;
@y
@!mem_min=1; {smallest index in \TeX's internal |mem| array;
@z

@x 399
@!error_line=72; {width of context lines on terminal error messages}
@y
@!error_line=64; {width of context lines on terminal error messages}
@z

@x 400
@!half_error_line=42; {width of first lines of contexts in terminal
@y
@!half_error_line=32; {width of first lines of contexts in terminal
@z

@x 402
@!max_print_line=79; {width of longest text lines output; should be at least 60}
@y
@!max_print_line=72; {width of longest text lines output; should be at least 60}
@z

@x 441
@d mem_bot=0 {smallest index in the |mem| array dumped by \.{INITEX};
@y
@d mem_bot=1 {smallest index in the |mem| array dumped by \.{INITEX};
@z

@x 443
@d mem_top==30000 {largest index in the |mem| array dumped by \.{INITEX};
@y
@d mem_top==3000 {largest index in the |mem| array dumped by \.{INITEX};
@z
