

int f(void* x)
{
   return ((int) x) & 3;
}

void main()
{
  void* a1 = &&hello;
  void* a2 = &&neil;
  int i = 0;
  printf("hello neil %x %x\n", ((i += 4) - 4), f(a2));
  
hello:
  i += 3;
neil:
  i ++;

  if (i == 12) goto finish;
  if (i == 13) goto hello;
  if (i == 14) goto neil;
finish:
}
