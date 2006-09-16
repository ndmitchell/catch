#include <stdio.h>


#define Func 0
#define Ctor 1
#define Ind  2


// make sure all the constructors are available
#ifdef ctor_IO
# define CtorIO ctor_IO
#else
# define CtorIO 0
#endif

#ifdef ctor__40_41
# define CtorTup0 ctor__40_41
#else
# define CtorTup0 0
#endif

#ifdef ctor__91_93
# define CtorNil ctor__91_93
# define CtorCons ctor__58
#else
# define CtorNil 0
# define CtorCons 1
#endif

#ifdef ctor__40_44_41
# define CtorTup2 ctor__40_44_41
#else
# define CtorTup2 0
#endif

#ifdef ctor_False
# define CtorFalse ctor_False
# define CtorTrue ctor_True
#else
# define CtorFalse 0
# define CtorTrue 1
#endif

#ifdef ctor_LT
# define CtorLT ctor_LT
# define CtorEQ ctor_EQ
# define CtorGT ctor_GT
#else
# define CtorLT 0
# define CtorEQ 1
# define CtorGT 2
#endif



typedef int*(*func)();

int* stack[1000];
int stack_pos = -1;

int* stack_top()
{
	return stack[stack_pos];
}

void stack_push(int* value)
{
	stack[++stack_pos] = value;
}

void stack_pop()
{
	stack_pos--;
}

// get the root ctor
int eval(int* x)
{
	int* res;

	switch (x[0])
	{
	case Ctor:
		return x[1];
		break;

	case Ind:
		return eval((int*) x[1]);
		break;

	case Func:
		stack_push(x);
		res = ((func) x[1])();
		stack_pop();
		x[0] = Ind;
		x[1] = (int) res;
		return eval(res);
		break;
	}
}

int* follow_ind(int* x)
{
	switch (x[0])
	{
	case Ind:
		return follow_ind((int*) x[1]);
		break;

	default:
		return x;
	}
}

#define INITIAL_TOTAL 62500
int* block;
int total = INITIAL_TOTAL;
int taken = INITIAL_TOTAL;

int* myalloc(int n)
{
	if (taken + n >= total)
	{
		total *= 2;
		block = malloc(total * sizeof(int));
		taken = n;
		return block;
	}
	else
	{
		int oldtaken = taken;
		taken += n;
		return &block[oldtaken];
	}
}



int* alloc0(int typ, int x)
{
	int* res = myalloc(2);
	res[0] = typ;
	res[1] = x;
	return res;
}

int* alloc1(int typ, int x, int* arg0)
{
	int* res = myalloc(3);
	res[0] = typ;
	res[1] = x;
	res[2] = (int) arg0;
	return res;
}

int* alloc2(int typ, int x, int* arg0, int* arg1)
{
	int* res = myalloc(4);
	res[0] = typ;
	res[1] = x;
	res[2] = (int) arg0;
	res[3] = (int) arg1;
	return res;
}

int* alloc3(int typ, int x, int* arg0, int* arg1, int* arg2)
{
	int* res = myalloc(5);
	res[0] = typ;
	res[1] = x;
	res[2] = (int) arg0;
	res[3] = (int) arg1;
	res[4] = (int) arg2;
	return res;
}

int* alloc4(int typ, int x, int* arg0, int* arg1, int* arg2, int* arg3)
{
	int* res = myalloc(6);
	res[0] = typ;
	res[1] = x;
	res[2] = (int) arg0;
	res[3] = (int) arg1;
	res[4] = (int) arg2;
	res[5] = (int) arg3;
	return res;
}

int* alloc5(int typ, int x, int* arg0, int* arg1, int* arg2, int* arg3, int* arg4)
{
	int* res = myalloc(7);
	res[0] = typ;
	res[1] = x;
	res[2] = (int) arg0;
	res[3] = (int) arg1;
	res[4] = (int) arg2;
	res[5] = (int) arg3;
	res[6] = (int) arg4;
	return res;
}


int* allocCtor0(int ctor)
{
	return alloc0(Ctor, ctor);
}

int* allocCtor1(int ctor, int* arg0)
{
	return alloc1(Ctor, ctor, arg0);
}

int* allocCtor2(int ctor, int* arg0, int* arg1)
{
	return alloc2(Ctor, ctor, arg0, arg1);
}

int* allocCall0(func call)
{
	return alloc0(Func, (int) call);
}

int* allocCall1(func call, int* arg0)
{
	return alloc1(Func, (int) call, arg0);
}

int* allocCall2(func call, int* arg0, int* arg1)
{
	return alloc2(Func, (int) call, arg0, arg1);
}

int* allocCall3(func call, int* arg0, int* arg1, int* arg2)
{
	return alloc3(Func, (int) call, arg0, arg1, arg2);
}

int* allocCall4(func call, int* arg0, int* arg1, int* arg2, int* arg3)
{
	return alloc4(Func, (int) call, arg0, arg1, arg2, arg3);
}

int* allocCall5(func call, int* arg0, int* arg1, int* arg2, int* arg3, int* arg4)
{
	return alloc5(Func, (int) call, arg0, arg1, arg2, arg3, arg4);
}

int* followVar(int* var, int n)
{
	eval(var);
	var = follow_ind(var);
	return (int*) var[n+1];
}

int* follow(int n)
{
	followVar((int*) (stack_top()[2]), n);
}

int* follow1()
{
	follow(1);
}

int* follow2()
{
	follow(2);
}

int* follow3()
{
	follow(3);
}

void error(char* msg)
{
	printf(msg);
	exit(1);
}

int* func_prim_95Prelude_46Prelude_46Integral_46Prelude_46Int_46divMod()
{
	int* vars = stack_top();
	int a = eval((int*) vars[2]);
	int b = eval((int*) vars[3]);
	return allocCtor2(CtorTup2, allocCtor0(a/b), allocCtor0(a%b));
}


int* func_prim_95System_46IO_46hPutChar()
{
	int* vars = stack_top();

	FILE* f = (FILE*) eval( (int*) vars[2] );
	int   c =         eval( (int*) vars[3] );

	fputc(c, f);
	return allocCtor1(CtorIO, allocCtor0(CtorTup0));
}

int* func_prim_95System_46IO_46hGetChar()
{
	int* vars = stack_top();

	FILE* f = (FILE*) eval( (int*) vars[2] );
	int c = fgetc(f);
	
	return allocCtor1(CtorIO, allocCtor0(c == EOF ? 0 : c));
}

void prim_string(char* Buffer, int* Closure)
{
	int i;
	for (i = 0; ; i++)
	{
		if (eval(Closure) == CtorNil)
		{
			Buffer[i] = 0;
			return;
		}
		Buffer[i] = eval(followVar(Closure, 1));
		Closure = followVar(Closure, 2);
	}
}

int* func_prim_95System_46IO_46openFile()
{
	int* vars = stack_top();
	FILE* f;
	char Buffer[1000];
	prim_string(Buffer, (int*) vars[2]);
	f = fopen(Buffer, "rt");
	if (f == NULL)
	{
		error("Failed to open file");
		return NULL;
	}
	else
		return allocCtor1(CtorIO, allocCtor0((int) f));
}


int* func_prim_95System_46IO_46stdout()
{
	return allocCtor0((int) stdout);
}

int* func_prim_95System_46IO_46stdin()
{
	return allocCtor0((int) stdin);
}

int* func_prim_95prim_95ORD()
{
    int* vars = stack_top();
    return allocCtor0(eval((int*) vars[2]));
}

int* func_prim_95prim_95SEQ()
{
	int* vars = stack_top();
	eval((int*) vars[2]);
	eval((int*) vars[3]);
	return follow_ind((int*) vars[3]);
}

int* func_prim_95Ord_46Char_46compare()
{
    int* vars = stack_top();
    int a = eval((int*) vars[2]);
    int b = eval((int*) vars[3]);
    return allocCtor0(a == b ? CtorEQ : (a > b ? CtorGT : CtorLT));
}


#define BinOp(name, code) int* name(){ \
						  int* vars = stack_top(); \
						  int a = eval((int*) vars[2]); \
						  int b = eval((int*) vars[3]); \
						  return allocCtor0(code); \
					      }

#define Bool(cond) (cond ? CtorTrue : CtorFalse)

BinOp(func_prim_95prim_95MUL_95W, a * b)
BinOp(func_prim_95prim_95SUB_95W, a - b)
BinOp(func_prim_95prim_95ADD_95W, a + b)
BinOp(func_prim_95prim_95EQ_95W, Bool(a == b))
BinOp(func_prim_95prim_95NE_95W, Bool(a != b))
BinOp(func_prim_95prim_95GE_95W, Bool(a >= b))
BinOp(func_prim_95prim_95LT_95W, Bool(a <  b))
BinOp(func_prim_95prim_95LE_95W, Bool(a <= b))
