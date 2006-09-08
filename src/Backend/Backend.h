#include <stdio.h>


#define Func 0
#define Ctor 1
#define Ind  2


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
	return alloc0(Func, call);
}

int* allocCall1(func call, int* arg0)
{
	return alloc1(Func, call, arg0);
}

int* allocCall2(func call, int* arg0, int* arg1)
{
	return alloc2(Func, call, arg0, arg1);
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


int* func_prim_95putChar()
{
	int* vars = stack_top();
	putchar(eval( (int*) vars[2] ));
	return allocCtor1(ctor_OI, allocCtor0(ctor_Tup0));
}
