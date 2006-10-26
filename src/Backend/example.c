// To align a label on a 4 byte boundary
// MUST have a goto Label, directly
// -falign-labels=3 

#include "header.h"

char StringTable[] = "Neil";

PROGRAM_BEGIN()

FUNCTION_UNPACK(main);
    // unpack the arguments in a cell
    // onto the stack
    // since main takes no arguments, just delete the
    // cell on the stack
    STACK_POP(1);

FUNCTION_BEGIN(main);
    // main = putStrLn (!unpack_str "neil")
    UNPACK_STR(&StringTable[0],4);
    STACK_PUSH(ret);
    TAIL_CALL(putStrLn);

FUNCTION_END(main)


FUNCTION_UNPACK(putStrLn);
	i = STACK_GET();
	STACK_SET(i[1]);

FUNCTION_BEGIN(putStrLn);
   r0 = STACK_GET()[0];
   EVAL(r0);
   if (IS_CTR0(r0)) {
       res = CTR0(TUP0); // pass back the tuple (elim IO)
       RETSTACK_RETURN();
   } else {
       r0 = PTR_CELLS(0);
       PRIM_CHAR_OUT(r0[1]);
       STACK_SET(r0[2]);
       TAIL_CALL(putStrLn);
   }

FUNCTION_END(putStrLn);




PROGRAM_END()
