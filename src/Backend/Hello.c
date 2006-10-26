#include "Backend.h"

const int ctor_Tup0 = 0;
const int ctor__91_93 = 0;
const int ctor__58 = 1;
const int ctor_OI = 0;


void main()
{
	func_main();
}

int* func_main()
{
	int* vars = stack_top();
	stack_push(allocCall0(func_Hello_46main));
	int res = func_Hello_46main();
	stack_pop();
	return res;
}

int* func_Hello_46main()
{
	int* vars = stack_top();
	stack_push(allocCall1(func_putStrLn,allocCall0(func__95LAMBDA191)));
	int res = func_putStrLn();
	stack_pop();
	return res;
}

int* func__95LAMBDA191()
{
	int* vars = stack_top();
	return eval(allocCtor2(ctor__58,allocCtor0('H'),allocCtor2(ctor__58,allocCtor0('e'),allocCtor2(ctor__58,allocCtor0('l'),allocCtor2(ctor__58,allocCtor0('l'),allocCtor2(ctor__58,allocCtor0('o'),allocCtor2(ctor__58,allocCtor0(','),allocCtor2(ctor__58,allocCtor0(' '),allocCtor2(ctor__58,allocCtor0('N'),allocCtor2(ctor__58,allocCtor0('e'),allocCtor2(ctor__58,allocCtor0('i'),allocCtor2(ctor__58,allocCtor0('l'),allocCtor2(ctor__58,allocCtor0('!'),allocCtor0(ctor__91_93))))))))))))));
}

int* func_putChar()
{
	int* vars = stack_top();
	stack_push(allocCall1(func_prim_95putChar,vars[1]));
	int res = func_prim_95putChar();
	stack_pop();
	return res;
}

int* func_putStr()
{
	int* vars = stack_top();
	switch (eval(vars[1]))
	{
		case ctor__91_93:
			return eval(allocCtor1(ctor_OI,allocCtor0(ctor_Tup0)));
		case ctor__58:
			stack_push(allocCall2(func__62_62,allocCall1(func_putChar,allocCall1(follow1(vars[1]))),allocCall1(func_putStr,allocCall1(follow2(vars[1])))));
			int res = func__62_62();
			stack_pop();
			return res;
	};
}

int* func__43_43()
{
	int* vars = stack_top();
	switch (eval(vars[1]))
	{
		case ctor__91_93:
			return eval(vars[2]);
		case ctor__58:
			return eval(allocCtor2(ctor__58,allocCall1(follow1(vars[1])),allocCall2(func__43_43,allocCall1(follow2(vars[1])),vars[2])));
	};
}

int* func_putStrLn()
{
	int* vars = stack_top();
	stack_push(allocCall1(func_putStr,allocCall2(func__43_43,vars[1],allocCall0(func__95LAMBDA13007))));
	int res = func_putStr();
	stack_pop();
	return res;
}

int* func__95LAMBDA13007()
{
	int* vars = stack_top();
	return eval(allocCtor2(ctor__58,allocCtor0('\n'),allocCtor0(ctor__91_93)));
}

int* func__62_62()
{
	int* vars = stack_top();
	switch (eval(vars[1]))
	{
		case ctor_OI:
			return eval(vars[2]);
	};
}

