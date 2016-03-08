%{
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "llvm-c/Core.h"
#include "llvm-c/BitReader.h"
#include "llvm-c/BitWriter.h"

#include "list.h"
#include "symbol.h"

int num_errors;

extern int yylex();   /* lexical analyzer generated from lex.l */

int yyerror();
int parser_error(const char*);

void minic_abort();
char *get_filename();
int get_lineno();

int loops_found=0;

extern LLVMModuleRef Module;
extern LLVMContextRef Context;
 LLVMBuilderRef Builder;

LLVMValueRef Function=NULL;
LLVMValueRef BuildFunction(LLVMTypeRef RetType, const char *name, 
			   paramlist_t *params);

%}

/* Data structure for tree nodes*/

%union {
  int num;
  char * id;
  LLVMTypeRef  type;
  LLVMValueRef value;
  LLVMBasicBlockRef bb;
  paramlist_t *params;
}

/* these tokens are simply their corresponding int values, more terminals*/

%token SEMICOLON COMMA COLON
%token LBRACE RBRACE LPAREN RPAREN LBRACKET RBRACKET
%token ASSIGN PLUS MINUS STAR DIV MOD 
%token LT GT LTE GTE EQ NEQ NOT
%token LOGICAL_AND LOGICAL_OR
%token BITWISE_OR BITWISE_XOR LSHIFT RSHIFT BITWISE_INVERT

%token DOT ARROW AMPERSAND QUESTION_MARK

%token FOR WHILE IF ELSE DO STRUCT SIZEOF RETURN 
%token BREAK CONTINUE
%token INT VOID

/* no meaning, just placeholders */
%token STATIC AUTO EXTERN TYPEDEF CONST VOLATILE ENUM UNION REGISTER
/* NUMBER and ID have values associated with them returned from lex*/

%token <num> NUMBER /*data type of NUMBER is num union*/
%token <id>  ID

%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE

/* values created by parser*/

%type <id> declarator
%type <params> param_list param_list_opt
%type <value> expression
%type <value> assignment_expression
%type <value> conditional_expression
%type <value> constant_expression
%type <value> logical_OR_expression
%type <value> logical_AND_expression
%type <value> inclusive_OR_expression
%type <value> exclusive_OR_expression
%type <value> AND_expression
%type <value> equality_expression
%type <value> relational_expression
%type <value> shift_expression
%type <value> additive_expression
%type <value> multiplicative_expression
%type <value> cast_expression
%type <value> unary_expression
%type <value> lhs_expression
%type <value> postfix_expression
%type <value> primary_expression
%type <value> constant
%type <value> expr_opt
%type <type>  type_specifier
/* 
   The grammar used here is largely borrowed from Kernighan and Ritchie's "The C
   Programming Language," 2nd Edition, Prentice Hall, 1988. 

   But, some modifications have been made specifically for MiniC!
 */

%%

/* 
   Beginning of grammar: Rules
*/

translation_unit:	  external_declaration
			| translation_unit external_declaration
;

external_declaration:	  function_definition
{
  /* finish compiling function */
  if(num_errors>100)
    {
      minic_abort();
    }
  else if(num_errors==0)
    {
      
    }
}
                        | declaration 
{ 
  /* nothing to be done here */
}
;

function_definition:	  type_specifier ID LPAREN param_list_opt RPAREN 
{
  symbol_push_scope();
  /* This is a mid-rule action */
  BuildFunction($1,$2,$4);  
} 
                          compound_stmt 
{ 
  /* This is the rule completion */
  LLVMBasicBlockRef BB = LLVMGetInsertBlock(Builder);
  if(!LLVMGetBasicBlockTerminator(BB))
    {
      LLVMBuildRet(Builder,LLVMConstInt(LLVMInt32TypeInContext(Context),
					0,(LLVMBool)1));
    }

  symbol_pop_scope();
  /* make sure basic block has a terminator (a return statement) */
}
                        | type_specifier STAR ID LPAREN param_list_opt RPAREN 
{
  symbol_push_scope();
  BuildFunction(LLVMPointerType($1,0),$3,$5);
} 
                          compound_stmt 
{ 
  /* This is the rule completion */


  /* make sure basic block has a terminator (a return statement) */

  LLVMBasicBlockRef BB = LLVMGetInsertBlock(Builder);
  if(!LLVMGetBasicBlockTerminator(BB))
    {
      LLVMBuildRet(Builder,LLVMConstPointerNull(LLVMPointerType(LLVMInt32TypeInContext(Context),0)));
    }

  symbol_pop_scope();
}
;

declaration:    type_specifier STAR declarator SEMICOLON
{
  if (is_global_scope())
    {
      LLVMAddGlobal(Module,LLVMPointerType($1,0),$3);
    } 
  else
    {
      symbol_insert($3,  /* map name to alloca */
		    LLVMBuildAlloca(Builder,LLVMPointerType($1,0),$3), /* build alloca */
		    0);  /* not an arg */
    }

} 
              | type_specifier declarator SEMICOLON
{
  if (is_global_scope())
    {
      LLVMAddGlobal(Module,$1,$2);
    }
  else
    {
      symbol_insert($2,  /* map name to alloca */
		    LLVMBuildAlloca(Builder,$1,$2), /* build alloca */
		    0);  /* not an arg */
    }
} 
;

declaration_list:	   declaration
{

}
                         | declaration_list declaration  
{

}
;


type_specifier:		  INT 
{
  $$ = LLVMInt32TypeInContext(Context);
}
;


declarator:		  ID
{
  $$ = $1;
}
;

param_list_opt:           
{ 
  $$ = NULL;
}
                        | param_list
{ 
  $$ = $1;
}
;

param_list:	
			  param_list COMMA type_specifier declarator
{
  $$ = push_param($1,$4,$3);
}
			| param_list COMMA type_specifier STAR declarator
{
  $$ = push_param($1,$5,LLVMPointerType($3,0));
}
                        | param_list COMMA type_specifier
{
  $$ = push_param($1,NULL,$3);
}
			|  type_specifier declarator
{
  /* create a parameter list with this as the first entry */
  $$ = push_param(NULL, $2, $1);
}
			| type_specifier STAR declarator
{
  /* create a parameter list with this as the first entry */
  $$ = push_param(NULL, $3, LLVMPointerType($1,0));
}
                        | type_specifier
{
  /* create a parameter list with this as the first entry */
  $$ = push_param(NULL, NULL, $1);
}
;


statement:		  expr_stmt            
			| compound_stmt        
			| selection_stmt       
			| iteration_stmt       
			| jump_stmt            
                        | break_stmt
                        | continue_stmt
;

expr_stmt:	           SEMICOLON            
{ 

}
			|  expression SEMICOLON       
{ 

}
;

compound_stmt:		  LBRACE declaration_list_opt statement_list_opt RBRACE 
{

}
;

declaration_list_opt:	
{

}
			| declaration_list
{

}
;

statement_list_opt:	
{

}
			| statement_list
{

}
;

statement_list:		statement
{

}
			| statement_list statement
{

}
;

break_stmt:               BREAK SEMICOLON
{
	loop_info_t info =  get_loop();
	if(info.exit == NULL){
		printf("Error\n");
		exit(-1);
	}
	else{
 		LLVMBuildBr(Builder,info.exit);
		LLVMBasicBlockRef cond = LLVMAppendBasicBlock(Function,"break.cond");
   		LLVMPositionBuilderAtEnd(Builder, cond);
	}
};

continue_stmt:            CONTINUE SEMICOLON
{
	loop_info_t info =  get_loop();
	if(info.reinit == NULL){
		printf("Error\n");
		exit(-1);
	}
	else{
 		LLVMBuildBr(Builder,info.reinit);
		LLVMBasicBlockRef init = LLVMAppendBasicBlock(Function,"break.init");
   		LLVMPositionBuilderAtEnd(Builder, init);
	}


};

selection_stmt:		  
		          IF LPAREN expression{
	LLVMBasicBlockRef then = LLVMAppendBasicBlock(Function,"then.block");
  	LLVMBasicBlockRef ELSE = LLVMAppendBasicBlock(Function,"else.block");
	LLVMValueRef zero = LLVMConstInt(LLVMTypeOf($3),0,1); 
 	LLVMValueRef cond = LLVMBuildICmp(Builder, LLVMIntNE, $3,
                                  zero,"cond");
         LLVMBuildCondBr(Builder,cond,then,ELSE);
        LLVMPositionBuilderAtEnd(Builder,then);
        $<bb>$ = ELSE;
}
 RPAREN statement{
         LLVMBasicBlockRef join = LLVMAppendBasicBlock(Function,"join.block");
  	 LLVMBuildBr(Builder,join);
	 LLVMBasicBlockRef ELSE = $<bb>4;
  	 LLVMPositionBuilderAtEnd(Builder,ELSE);	
         $<bb>$ = join;
} ELSE statement 
{ 
        LLVMBasicBlockRef join = $<bb>7;
  	LLVMBuildBr(Builder,join);
	LLVMPositionBuilderAtEnd(Builder,join);  
}
;

iteration_stmt:		  WHILE LPAREN { 
  /* set up header basic block
     make it the new insertion point */
    LLVMBasicBlockRef cond = LLVMAppendBasicBlock(Function,"while.cond");
    LLVMBuildBr(Builder,cond);
    LLVMPositionBuilderAtEnd(Builder, cond);
    $<bb>$ = cond;



} expression RPAREN { 
  /* set up loop body */

  /* create new body and exit blocks */

  /* to support nesting: */
  /*push_loop(expr,body,body,after);*/


  LLVMBasicBlockRef body = LLVMAppendBasicBlock(Function,"while.body");
  // 2. Make the join block
  LLVMBasicBlockRef join = LLVMAppendBasicBlock(Function,"while.join");

  // 3. Build a conditional branch to body and join block  
  LLVMValueRef zero = LLVMConstInt(LLVMTypeOf($4),0,1); 
  LLVMValueRef cond = LLVMBuildICmp(Builder, LLVMIntNE, $4,
                                  zero,"cond");
  LLVMValueRef br = LLVMBuildCondBr(Builder,cond,body,join);
  
  // 4. Position builder in the body
  LLVMPositionBuilderAtEnd(Builder,body);
  
  push_loop($<bb>6,body,body,join);
  // 5. Remember the join block ($<bb>$)
  $<bb>$ = join;

} 
  statement
{
  /* finish loop */
  /*loop_info_t info = get_loop();*/

  /*pop_loop();*/

 // 1. Make an unconditional branch back to $<bb>2
  LLVMBuildBr(Builder,$<bb>3);
  // 2. Move builder to $<bb>6
  LLVMPositionBuilderAtEnd(Builder, $<bb>6);
  pop_loop();
			   
}
| FOR LPAREN expr_opt 
{
  // build a new basic block for the cond expression (LLVMAppendBasicBlock)
  // remember the cond block for this for loop (stack of nested loops)
  // insert a branch from the current basic to the cond basic block (LLVMBuildBr)
  // set builder to insert inside the cond block LLVMPositionBuilderAtEnd(Builder,cond block)
  //$$ = initblock;
    LLVMBasicBlockRef cond = LLVMAppendBasicBlock(Function,"for.cond"); // Building a conditional block directly 
    LLVMBuildBr(Builder,cond); // Build a branch to condition block from init block
    LLVMPositionBuilderAtEnd(Builder, cond); // Putting instruction builder
    $<bb>$ = cond; // Storing the mid rule output of cond block

 } 
SEMICOLON
{
   LLVMBasicBlockRef exit = LLVMAppendBasicBlock(Function,"for.join"); // Making the Join(Exit) block 
     $<bb>$ = exit;
} expr_opt 
{
  // build a new block
  // position builder in this block
  // add the branch back to the cond block
  //$$ = condblock;
  
   LLVMBasicBlockRef body = LLVMAppendBasicBlock(Function,"for.body"); //Making the main for body block 
 LLVMBasicBlockRef exit = $<bb>6; // Getting out the exit block
   //LLVMValueRef rhs = ;
   LLVMValueRef CHECK = LLVMBuildICmp(Builder,LLVMIntNE, $7,LLVMConstInt(LLVMTypeOf($7),0,1),""); // Checking the condition expression
  LLVMValueRef br = LLVMBuildCondBr(Builder,CHECK,body,exit); // Branching conditionally to exit or body
   $<bb>$ = body; // Storing the output of midrule action
} 
SEMICOLON 
{
  // build a new block for the beginning of the statement
  // add a branch from the cond block to the statement block
  //LLVMPositionBuilder(Builder,$6);
  //stateblock = LLVMAppendBasicBlock(Function,"for-statement");
  //LLVMBuildBr(Builder,statementblock);
  // set insert piont int the new statement block  
 LLVMBasicBlockRef INCREMENT = LLVMAppendBasicBlock(Function,"for.inc"); // Incrementer block of for loop
 LLVMPositionBuilderAtEnd(Builder, INCREMENT); // Putting instruciton  builder at incrementer block
 $<bb>$ = INCREMENT; // Storing the output of midrule action
}
expr_opt {
 LLVMBasicBlockRef cond = $<bb>4; // Getting the cond block
 LLVMBuildBr(Builder,cond); // Branch unconditionally to Cond block
 LLVMBasicBlockRef body = $<bb>8; // Getting the body block
 LLVMPositionBuilderAtEnd(Builder, body); // Putting the instruction builder at body
 LLVMBasicBlockRef join = $<bb>6; // getting the join block
 push_loop(cond,body,$<bb>10,join); // Pushin loop info now 
}
RPAREN statement
{
  /* 566: add mid-rule actions to support for loop */
  // connect current block ($12) the re-init block 
  LLVMBasicBlockRef INCREMENT = $<bb>10; // Getting increment block to goto after body of for loop
  LLVMBuildBr(Builder,INCREMENT); 
 LLVMBasicBlockRef exit = $<bb>6; // Getting the exit block
  LLVMPositionBuilderAtEnd(Builder,exit); // Positioning the instruction block at Exit block
  pop_loop(); // Popping loop info
}
;

expr_opt:		
{ 

}
			| expression
{ 

}
;

jump_stmt:		  RETURN SEMICOLON
{ 
      LLVMBuildRetVoid(Builder);

}
			| RETURN expression SEMICOLON
{
      LLVMBuildRet(Builder,$2);
}
;

expression:               assignment_expression
{ 
  $$=$1;
}
;

assignment_expression:    conditional_expression
{
  $$=$1;
}
                        | lhs_expression ASSIGN assignment_expression
{
  /* Implement */
	
 
  	/*if((LLVMTypeOf($1) ==  LLVMInt32Type()) && (LLVMTypeOf($3) ==  LLVMInt32Type())){
		$$ = LLVMBuildStore(Builder,$3,$1);
                //printf("This came\n");
	}
	if((LLVMTypeOf($1) ==  LLVMInt32Type()) && (LLVMTypeOf($3) ==  LLVMPointerType(LLVMInt32Type(),0))){
			$$ =  LLVMBuildStore(Builder,LLVMBuildPtrToInt(Builder,$3,LLVMInt32Type(),""),$1);
        }*/
     if(LLVMGetElementType(LLVMTypeOf($1)) == LLVMTypeOf($3))
    {
    	LLVMBuildStore(Builder,$3,$1);
   	 $$=$3;
    }  
   else if(LLVMGetElementType(LLVMTypeOf($1)) == LLVMPointerType(LLVMInt32Type(),0) && LLVMTypeOf($3) == LLVMInt32Type())
   {
        LLVMBuildStore(Builder,LLVMBuildIntToPtr(Builder,$3,LLVMPointerType(LLVMInt32Type(),0),""),$1);
        $$=$3;
   }
   else if(LLVMGetElementType(LLVMTypeOf($1)) == LLVMInt32Type() && LLVMTypeOf($3) == LLVMPointerType(LLVMInt32Type(),0))
   {
         LLVMBuildStore(Builder,LLVMBuildPtrToInt(Builder,$3,LLVMInt32Type(),""),$1);
         $$ = $3; 
   }
   else 
   {
    printf("Invalid Assign statement\n");
    exit(0);
  }
}
;


conditional_expression:   logical_OR_expression
{
  $$=$1;
}
                        | logical_OR_expression QUESTION_MARK expression COLON conditional_expression
{
  /* Implement */
   LLVMValueRef one = LLVMConstInt(LLVMInt32Type(),1,0); 
   LLVMValueRef icmp = LLVMBuildICmp(Builder, LLVMIntEQ, $1,one,"");
   if(icmp){
	$$ = $3;
   }
   else{
	$$ = $5;
   }
}
;

constant_expression:       conditional_expression
{ $$ = $1; }
;

logical_OR_expression:    logical_AND_expression
{
  $$ = $1;
}
                        | logical_OR_expression LOGICAL_OR logical_AND_expression
{
  // Implement

LLVMValueRef zero = LLVMConstInt(LLVMInt32Type(),0,0);
LLVMValueRef one = LLVMConstInt(LLVMInt32Type(),1,0);
LLVMValueRef n1 = LLVMBuildZExt(Builder,LLVMBuildICmp(Builder,LLVMIntUGT,$1,zero,""),LLVMInt32Type(),"");
  LLVMValueRef n2 = LLVMBuildZExt(Builder,LLVMBuildICmp(Builder,LLVMIntUGT,$3,zero,""),LLVMInt32Type(),"");
  
  if(LLVMTypeOf($1) == LLVMTypeOf($3))
    {
 	 $$ = LLVMBuildOr(Builder,n1,n2,"");
    }  
   else{
	printf("Error\n");
	exit(-1);
   }

/*LLVMValueRef zero = LLVMConstInt(LLVMInt32Type(),0,0);
LLVMValueRef one = LLVMConstInt(LLVMInt32Type(),1,0);
LLVMValueRef n1 = LLVMBuildZExt(Builder,LLVMBuildICmp(Builder,LLVMIntUGT,$1,zero,""),LLVMInt32Type(),"");
  LLVMValueRef n2 = LLVMBuildZExt(Builder,LLVMBuildICmp(Builder,LLVMIntUGT,$3,zero,""),LLVMInt32Type(),"");
  
 //printf("%d\n",n);
  $$ = LLVMBuildOr(Builder,n1,n2,"");*/
};

logical_AND_expression:   inclusive_OR_expression
{
  $$ = $1;
}
                        | logical_AND_expression LOGICAL_AND inclusive_OR_expression
{
  /* Implement */
  LLVMValueRef zero = LLVMConstInt(LLVMInt32Type(),0,0);
  LLVMValueRef n1 = LLVMBuildZExt(Builder,LLVMBuildICmp(Builder,LLVMIntUGT,$1,zero,""),LLVMInt32Type(),"");
  LLVMValueRef n2 = LLVMBuildZExt(Builder,LLVMBuildICmp(Builder,LLVMIntUGT,$3,zero,""),LLVMInt32Type(),"");
 //printf("%d\n",n);
  //$$ = LLVMBuildAnd(Builder,n1,n2,"");
  if(LLVMTypeOf($1) == LLVMTypeOf($3))
    {
 	 $$ = LLVMBuildAnd(Builder,n1,n2,"");
    }  
   else{
	printf("Error\n");
	exit(-1);
   }
  
}
;

inclusive_OR_expression:  exclusive_OR_expression
{
    $$=$1;
}
                        | inclusive_OR_expression BITWISE_OR exclusive_OR_expression
{
  /* Implement */
  if(LLVMTypeOf($1) == LLVMTypeOf($3))
    {
 	 $$ = LLVMBuildOr(Builder,$1,$3,"");
    }  
   else{
	printf("Error\n");
	exit(-1);
   }
}
;

exclusive_OR_expression:  AND_expression
{
  $$ = $1;
}
                        | exclusive_OR_expression BITWISE_XOR AND_expression
{
  /* Implement */
   if(LLVMTypeOf($1) == LLVMTypeOf($3))
    {
 	 $$ = LLVMBuildXor(Builder,$1,$3,"");
    }  
   else{
	printf("Error\n");
	exit(-1);
   }
  //$$ = LLVMBuildXor(Builder,$1,$3,"");
}
;

AND_expression:           equality_expression
{
  $$ = $1;
}
                        | AND_expression AMPERSAND equality_expression
{
  /* Implement */
   if(LLVMTypeOf($1) == LLVMTypeOf($3))
    {
 	 $$ = LLVMBuildAnd(Builder,$1,$3,"");
    }  
   else{
	printf("Error\n");
	exit(-1);
   }
}
;

equality_expression:      relational_expression
{
  $$ = $1;
}
                        | equality_expression EQ relational_expression
{
  /* Implement: use icmp */
    if(LLVMTypeOf($1) == LLVMTypeOf($3))
    {
 	 $$ = LLVMBuildZExt(Builder,LLVMBuildICmp(Builder,LLVMIntEQ,$1,$3,""),LLVMInt32Type(),"");
    }  
   else{
	printf("Error\n");
	exit(-1);
   }
  
}
                        | equality_expression NEQ relational_expression
{
  /* Implement: use icmp */
	if(LLVMTypeOf($1) == LLVMTypeOf($3))
    {
 	 $$ = LLVMBuildZExt(Builder,LLVMBuildICmp(Builder,LLVMIntNE,$1,$3,""),LLVMInt32Type(),"");
    }  
   else{
	printf("Error\n");
	exit(-1);
   }
  
}
;

relational_expression:    shift_expression
{
    $$=$1;
}
                        | relational_expression LT shift_expression
{
  /* Implement: use icmp */
    if(LLVMTypeOf($1) == LLVMTypeOf($3))
    {
 	 $$ = LLVMBuildZExt(Builder,LLVMBuildICmp(Builder,LLVMIntSLT,$1,$3,""),LLVMInt32Type(),"");
    }  
   else{
	printf("Error\n");
	exit(-1);
   }
}
                        | relational_expression GT shift_expression
{
  /* Implement: use icmp */
  if(LLVMTypeOf($1) == LLVMTypeOf($3))
    {
 	$$ = LLVMBuildZExt(Builder,LLVMBuildICmp(Builder,LLVMIntSGT,$1,$3,""),LLVMInt32Type(),"");
    }  
   else{
	printf("Error\n");
	exit(-1);
   }
}
                        | relational_expression LTE shift_expression
{
  /* Implement: use icmp */
    if(LLVMTypeOf($1) == LLVMTypeOf($3))
    {
 	$$ = LLVMBuildZExt(Builder,LLVMBuildICmp(Builder,LLVMIntSLE,$1,$3,""),LLVMInt32Type(),"");
    }  
   else{
	printf("Error\n");
	exit(-1);
   }
  
}
                        | relational_expression GTE shift_expression
{
  /* Implement: use icmp */
	if(LLVMTypeOf($1) == LLVMTypeOf($3))
    {
 	$$ = LLVMBuildZExt(Builder,LLVMBuildICmp(Builder,LLVMIntSGE,$1,$3,""),LLVMInt32Type(),"");
    }  
   else{
	printf("Error\n");
	exit(-1);
   }
  
}
;

shift_expression:         additive_expression
{
    $$=$1;
}
                        | shift_expression LSHIFT additive_expression
{
  /* Implement */
	if(LLVMTypeOf($1) == LLVMTypeOf($3))
    {
 	 $$ = LLVMBuildShl(Builder,$1,$3,"");
    }  
   else{
	printf("Error\n");
	exit(-1);
   }
   
}
                        | shift_expression RSHIFT additive_expression
{
  /* Implement */
if(LLVMTypeOf($1) == LLVMTypeOf($3))
    {
 	 $$ = LLVMBuildLShr(Builder,$1,$3,"");
    }  
   else{
	printf("Error\n");
	exit(-1);
   }
  
}
;

additive_expression:      multiplicative_expression
{
  $$ = $1;
}
                        | additive_expression PLUS multiplicative_expression
{
  /* Implement */
        if((LLVMTypeOf($1) ==  LLVMInt32Type()) && (LLVMTypeOf($3) ==  LLVMInt32Type())){
		$$ = LLVMBuildAdd(Builder,$1,$3,"");
              //  printf("This came\n");
	}
	if((LLVMTypeOf($1) ==  LLVMInt32Type()) && (LLVMTypeOf($3) ==  LLVMPointerType(LLVMInt32Type(),0))){
			LLVMValueRef indices[1] = {$1};
			LLVMValueRef gep = LLVMBuildGEP(Builder,$3,indices,1,"");
			$$ = gep;
		//printf("This came2\n");
        }
        if((LLVMTypeOf($1) == LLVMPointerType(LLVMInt32Type(),0)) && (LLVMTypeOf($3) ==  LLVMInt32Type())){
			LLVMValueRef indices[1] = {$3};
			LLVMValueRef gep = LLVMBuildGEP(Builder,$1,indices,1,"");
			$$ =  gep;
	//		printf("This came3\n");			
        }
        if((LLVMTypeOf($1) == LLVMPointerType(LLVMInt32Type(),0)) && (LLVMTypeOf($3) ==  LLVMPointerType(LLVMInt32Type(),0))){
		printf("ERROR\n");
		exit(0);
        } 

 }
                        | additive_expression MINUS multiplicative_expression
{
  /* Implement */
    
 	if((LLVMTypeOf($1) ==  LLVMInt32Type()) && (LLVMTypeOf($3) ==  LLVMInt32Type())){
		$$ = LLVMBuildSub(Builder,$1,$3,""); // Normal subtraction
              //  printf("This came\n");
	}
	if((LLVMTypeOf($1) ==  LLVMInt32Type()) && (LLVMTypeOf($3) ==  LLVMPointerType(LLVMInt32Type(),0))){ // ptr subtraction
			LLVMValueRef inter = LLVMBuildNeg(Builder, $1, ""); 

			LLVMValueRef indices[1] = {inter};
			LLVMValueRef gep = LLVMBuildGEP(Builder,$3,indices,1,"");
			$$ = gep;
		//printf("This came2\n");
        }
        if((LLVMTypeOf($1) == LLVMPointerType(LLVMInt32Type(),0)) && (LLVMTypeOf($3) ==  LLVMInt32Type())){
			LLVMValueRef inter = LLVMBuildNeg(Builder, $3, "");
			LLVMValueRef indices[1] = {inter};
			LLVMValueRef gep = LLVMBuildGEP(Builder,$1,indices,1,"");
			$$ =  gep;
	//		printf("This came3\n");			
        }
        if((LLVMTypeOf($1) == LLVMPointerType(LLVMInt32Type(),0)) && (LLVMTypeOf($3) ==  LLVMPointerType(LLVMInt32Type(),0))){
		printf("ERROR\n");
		exit(0);
        } 
}
;

multiplicative_expression:  cast_expression
{
  $$ = $1;
}
                        | multiplicative_expression STAR cast_expression
{
  /* Implement */ 
 
    if(LLVMTypeOf($1) == LLVMTypeOf($3))
    {
 	  $$ = LLVMBuildMul(Builder,$1,$3,""); //Building Multiplication
    }   
   else{
	printf("Error\n");
	exit(-1);
   } 
}
                        | multiplicative_expression DIV cast_expression
{
  /* Implement */
  LLVMValueRef n = LLVMBuildZExt(Builder,$3,LLVMInt32Type(),"");
 // printf("%d\n",n);
  if($3 == 0){
 	printf("Error \n");
	exit(0);
  }
  else if(LLVMTypeOf($1) == LLVMTypeOf($3))
    {
 	  $$   = LLVMBuildSDiv(Builder,$1,$3,""); // Building Division
    }  
   else{
	printf("Error\n");
	exit(-1);
   } 
  
}
                        | multiplicative_expression MOD cast_expression
{
  /* Implement */
   if(LLVMTypeOf($1) == LLVMTypeOf($3))
    {
 	  $$ = LLVMBuildSRem(Builder,$1,$3,""); // Building Modulus
    }  
   else{
	printf("Error\n");
	exit(-1);
   } 
  
}
;

cast_expression:          unary_expression
{ $$ = $1; }
;

lhs_expression:           ID 
{
  int isArg=0;
  LLVMValueRef val = symbol_find($1,&isArg);
  if (isArg)
    {
      // error
    }
  else
    $$ = val;
}
                        | STAR ID
{
  int isArg=0;
  LLVMValueRef val = symbol_find($2,&isArg);
  if (isArg)
    {
      // error
    }
  else
    $$ = LLVMBuildLoad(Builder,val,""); 
}
;

unary_expression:         postfix_expression
{
  $$ = $1;
}
                        | AMPERSAND primary_expression
{
  /* Implement */
   LLVMValueRef load = $2;
   LLVMValueRef address = LLVMGetOperand($2,0); //Getting the address of the integer
   $$ = address;
}
                        | STAR primary_expression
{
  /* FIXME */
  $$ = LLVMBuildLoad(Builder,$2,""); // Building a load from the address
}
                        | MINUS unary_expression
{
  /* Implement */
    $$ = LLVMBuildNeg(Builder, $2, ""); // Negation of the statement
}
                        | PLUS unary_expression
{
  $$ = $2;
}
                        | BITWISE_INVERT unary_expression
{
  /* Implement */
    $$ = LLVMBuildNot(Builder, $2, ""); // Bitwise not
}
                        | NOT unary_expression
{
  /* Implement */
  LLVMValueRef zero = LLVMConstInt(LLVMTypeOf($2),0,1);   // Making a constant zero
   LLVMValueRef icmp = LLVMBuildICmp(Builder, LLVMIntEQ, $2,zero,""); // Comparing the expression
  $$ = LLVMBuildZExt(Builder, icmp, LLVMInt32Type(),""); // Making the value out of comparison
  
}
;


postfix_expression:       primary_expression
{
  $$ = $1;
}
;

primary_expression:       ID 
{ 
  int isArg=0;
  LLVMValueRef val = symbol_find($1,&isArg);
  if (isArg)
    $$ = val;
  else
    $$ = LLVMBuildLoad(Builder,val,"");
}
                        | constant
{
  $$ = $1;
}
                        | LPAREN expression RPAREN
{
  $$ = $2;
}
;

constant:	          NUMBER  
{ 
  /* Implement */
  $$ = LLVMConstInt(LLVMInt32Type(),$1,0);   // Making a Constant of 32 bit type
} 
;

%%

LLVMValueRef BuildFunction(LLVMTypeRef RetType, const char *name, 
			   paramlist_t *params)
{
  int i;
  int size = paramlist_size(params);
  LLVMTypeRef *ParamArray = malloc(sizeof(LLVMTypeRef)*size);
  LLVMTypeRef FunType;
  LLVMBasicBlockRef BasicBlock;

  paramlist_t *tmp = params;
  /* Build type for function */
  for(i=size-1; i>=0; i--) 
    {
      ParamArray[i] = tmp->type;
      tmp = next_param(tmp);
    }
  
  FunType = LLVMFunctionType(RetType,ParamArray,size,0);

  Function = LLVMAddFunction(Module,name,FunType);
  
  /* Add a new entry basic block to the function */
  BasicBlock = LLVMAppendBasicBlock(Function,"entry");

  /* Create an instruction builder class */
  Builder = LLVMCreateBuilder();

  /* Insert new instruction at the end of entry block */
  LLVMPositionBuilderAtEnd(Builder,BasicBlock);

  tmp = params;
  for(i=size-1; i>=0; i--)
    {
      LLVMValueRef alloca = LLVMBuildAlloca(Builder,tmp->type,tmp->name);
      LLVMBuildStore(Builder,LLVMGetParam(Function,i),alloca);
      symbol_insert(tmp->name,alloca,0);
      tmp=next_param(tmp);
    }

  return Function;
}

extern int line_num;
extern char *infile[];
static int   infile_cnt=0;
extern FILE * yyin;

int parser_error(const char *msg)
{
  printf("%s (%d): Error -- %s\n",infile[infile_cnt-1],line_num,msg);
  return 1;
}

int internal_error(const char *msg)
{
  printf("%s (%d): Internal Error -- %s\n",infile[infile_cnt-1],line_num,msg);
  return 1;
}

int yywrap() {
  static FILE * currentFile = NULL;

  if ( (currentFile != 0) ) {
    fclose(yyin);
  }
  
  if(infile[infile_cnt]==NULL)
    return 1;

  currentFile = fopen(infile[infile_cnt],"r");
  if(currentFile!=NULL)
    yyin = currentFile;
  else
    printf("Could not open file: %s",infile[infile_cnt]);

  infile_cnt++;
  
  return (currentFile)?0:1;
}

int yyerror()
{
  parser_error("Un-resolved syntax error.");
  return 1;
}

char * get_filename()
{
  return infile[infile_cnt-1];
}

int get_lineno()
{
  return line_num;
}


void minic_abort()
{
  parser_error("Too many errors to continue.");
  exit(1);
}
