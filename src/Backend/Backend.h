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
# define CtorCons ctor__91_93
# define CtorNil ctor__58
#else
# define CtorCons 0
# define CtorNil 1
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


int* alloc0(int typ, int x)
{
	int* res = malloc(sizeof(int) * 2);
	res[0] = typ;
	res[1] = x;
	return res;
}

int* alloc1(int typ, int x, int* arg0)
{
	int* res = malloc(sizeof(int) * 3);
	res[0] = typ;
	res[1] = x;
	res[2] = (int) arg0;
	return res;
}

int* alloc2(int typ, int x, int* arg0, int* arg1)
{
	int* res = malloc(sizeof(int) * 4);
	res[0] = typ;
	res[1] = x;
	res[2] = (int) arg0;
	res[3] = (int) arg1;
	return res;
}

int* alloc3(int typ, int x, int* arg0, int* arg1, int* arg2)
{
	int* res = malloc(sizeof(int) * 5);
	res[0] = typ;
	res[1] = x;
	res[2] = (int) arg0;
	res[3] = (int) arg1;
	res[4] = (int) arg2;
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

int* follow(int n)
{
	int* var = (int*) (stack_top()[2]);
	eval(var);
	var = follow_ind(var);
	return (int*) var[n+1];
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

int* func_prim_95System_46IO_46hGetContents()
{
	int* vars = stack_top();

	FILE* f = (FILE*) eval( (int*) vars[2] );
	int c = fgetc(f);
	if (c == EOF)
		return allocCtor0(CtorNil);
	else
		return allocCtor2(CtorCons, allocCtor0(c),
			allocCall1(func_prim_95System_46IO_46hGetContents, (int*) vars[2]));
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

int* func_prim_95prim_95EQ_95W()
{
    int* vars = stack_top();
    int a = eval((int*) vars[2]);
    int b = eval((int*) vars[3]);
    return allocCtor0(a == b ? CtorTrue : CtorFalse);
}

int* func_prim_95Ord_46Char_46compare()
{
    int* vars = stack_top();
    int a = eval((int*) vars[2]);
    int b = eval((int*) vars[3]);
    return allocCtor0(a == b ? CtorEQ : (a > b ? CtorGT : CtorLT));
}
