// Yhc -O back end
// Part of the Catch Project
// Impelements the +S+T machine
// Super-spine-less, tagged, machine

// Cells is a heap item
typedef int Cell;
typedef Cell* Cells;

#define NULL ((void*) 0)

// The types of tag
#define TAG_HEAD 0
#define TAG_CTR0 1
#define TAG_CTRN 2
#define TAG_FUNN 3

// Each cell starts with a TAG_HEAD
// every argument in a cell is not a TAG_HEAD
// TAG_FUNN is a pointer to a function cell
//          may be replaced with an indirection
// TAG_CTR0 is a constructor value
// TAG_CTRN is a pointer to a ctor cell

#define PTR(tag,value) (tag | ((value) << 2))

#define PTR_CTOR(x) (x >> 2)
#define PTR_CELLS(x) ((Cells) (x & ~3))
#define PTR_TAG(x) (x & 3)

#define CTR0(x) PTR(x,TAG_CTR0)

#define IS_FUNN(x) (x & TAG_FUNN)

#define IND TAG_FUNN
#define IS_IND(c) (c[0] == IND)
#define FOLLOW_IND(c) (c[1])


// The heap
struct Heap
{
    int Size;
    int Used;
    Cells Mem;
};

struct Heap Heap;
struct Heap HeapAlt;

#define HEAP_RESERVE(n) {if (n > Heap.Size - Heap.Used) GC();}
#define HEAP_INITIAL 5000

#define HEAP_ALLOC(n) (&Heap.Mem[(Heap.Used += n) - n]);

void HeapInit()
{
    Heap.Mem = (Cells) malloc(sizeof(Cell) * HEAP_INITIAL);
    Heap.Size = HEAP_INITIAL;
    Heap.Used = 0;
    
    HeapAlt.Mem = NULL;
    HeapAlt.Size = 0;
    HeapAlt.Used = 0;
}

void GC()
{
    printf("Garbage collection, todo\n");
    assert(0);
}


// The stack
#define STACK_SIZE 400
int StackPos;
Cell Stack[STACK_SIZE];

void StackInit()
{
    StackPos = 0;
}

#define STACK_PUSH(i) (Stack[StackPos++] = x)
#define STACK_POP(n) (StackPos -= n)
#define STACK_GET (&Stack[StackPos])

// The Ret Stack
#define RETSTACK_SIZE 50
int RetStackPos;
void* RetStack[RETSTACK_SIZE];

void RetStackInit()
{
    RetStackPos = 0;
}

#define RETSTACK_PUSH(i) (RetStack[RetStackPos++] = x)
#define RETSTACK_RETURN(i) (goto *(&RetStack[RetStack--]))


// All Init's
void Init()
{
    HeapInit();
    StackInit();
    RetStackInit();
}

#define PROGRAM_BEGIN()                    \
    void main(){                           \
        Cell ret;                          \
        Init();                            \
        RETSTACK_PUSH(&        &ProgramEnd);       \
        goto begin_main;

#define PROGRAM_END()                      \
    ProgramEnd:                            \
    }
        


// Evaluation

// x MUST be a variable, since may be assigned to
// result is a point to a Ptr of CTR0 or CTRN
// uid is required, must be unique amongst entire program
#define EVAL_VAR(x, uid) {              \
    if (IS_FUNN(x)) {                   \
        x = FollowInds(x);              \
        if (IS_FUNN(x)) {               \
            Cells t0 = PTR_CELLS(x);    \
            STACK_PUSH(t0);             \
            RETSTACK_PUSH(&&uid);       \
            goto *(PTR_FUN t0);         \
          uid:                          \
        }                               \
    }


Cell FollowInds(Cell x)
{
    Cells i;
    assert(IS_FUNN(x));
    i = PTR_CELLS(x);
    while (IS_IND(i))
    {
        x = FOLLOW_IND(i);
        if (!IS_FUNN(x)) return x;
        i = PTR_CELLS(x);
    }
    return x;
}

// Common Values
#define CTOR_NIL  0
#define CTOR_CONS 1

// Utilities

#define UNPACK_STR(x, n) (ret = UnpackStr(x, n))

Cell UnpackStr(char* s, int n)
{
    Cell ret, *now, *last;
    int i;
    HEAP_RESERVE(3 * n);
    last = &ret;
    for (i = 0; i < n; i++) {
        now = HEAP_ALLOC(3);
        *last = (Cell) ((int) now & TAG_CTRN); 
        now[0] = PTR(TAG_HEAD, CTOR_CONS);
        now[1] = CTR0(s[i]);
        last = &(now[2]);
    }
    *last = CTR0(CTOR_NIL);
}
