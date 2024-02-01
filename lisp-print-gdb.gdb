define lprint
  call lisp_print(lisp, $arg0, stdout)
  call (int)fputc('\n', stdout)
end

