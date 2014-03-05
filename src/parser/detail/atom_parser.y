%include {
	#include <string>
	#include <cassert>
	#include <cstring>

	#include "Referenced.h"
	#include "AtomParser.h"
	#include "parser/BaseElement.h"
	#include "parser/BaseType.h"

	#define UNUSED void*
}

%name 					AtomParser_
%token_prefix 			T_
%extra_argument 		{ AtomParser* parser						}


%nonassoc 				ID STRING_LITERAL.
%nonassoc				L_PAREN R_PAREN DOT.
%left					COMMA COLON.

%nonassoc 				NUMBER FP_NUMBER.
%nonassoc 				ANSWER SATISFIABLE UNSATISFIABLE.
%nonassoc 				MODELS TIME PREPARE PREPRO_DOT SOLVING.


%nonassoc 				EOF.
%nonassoc 				ERR_UNTERMINATED_STRING ERR_UNKNOWN_SYMBOL.

%token_type 			{ ref_ptr<Token>* 							}
%token_destructor 		{ delete $$;								}

%type start				{ UNUSED									}
%type solutionlist		{ UNUSED									}
%type epilogue			{ UNUSED 									}

%type pa_solution		{ Solution* 								}
%type reg_solution		{ Solution* 								}
%type preamble			{ int	 									}
%type atomlist			{ Solution::AtomList* 						}
%type nonempty_atomlist	{ Solution::AtomList*						}
%type atom 				{ BaseElement*								}
%type function			{ BaseElement* 								}
%type arglist			{ BaseElement::ArgumentList* 				}

%destructor pa_solution	{ delete $$;								}
%destructor reg_solution{ delete $$;								}
%destructor atomlist	{ delete $$;								}
%destructor atom 		{ delete $$;								}
%destructor function	{ delete $$;								}
%destructor arglist 	{ delete $$;								}

%syntax_error 			{ parser->_parse_error("unexpected token");	}


// ---------------------------------------------------------------------------------
start ::= solutionlist epilogue EOF.

// ------- solutionlist

// A solution list is either a series of preambled solutions (i.e. solver output)
// or a single non-preambled solution (i.e. user input)

solutionlist ::= error.
solutionlist ::= solutionlist error.

solutionlist ::= pa_solution(solution).
		{
			parser->_addSolution(solution);
		}
solutionlist ::= solutionlist pa_solution(solution).
		{
			parser->_addSolution(solution);
		}

solutionlist ::= reg_solution(solution).
		{
			parser->_addSolution(solution);
		}

// ------- solutions

// preambled solution
pa_solution(solution) ::= preamble(n) atomlist(atomlist).
		{
			solution = new Solution(atomlist, n);
		}

// non-preambled solution
reg_solution(solution) ::= nonempty_atomlist(atomlist).
		{
			solution = new Solution(atomlist);
		}


// ------- preamble

preamble(p) ::= ANSWER COLON NUMBER(n).
		{
			p = atoi((*n)->str()->c_str());
			delete n;
		}

// ------- atomlist

atomlist(list) ::= .
		{
			list = new Solution::AtomList();
		}
atomlist(l2) ::= nonempty_atomlist(l1).
		{
			l2 = l1;
		}

nonempty_atomlist(list) ::= atom(atom).
		{
			list = new Solution::AtomList();
			list->insert(atom);
		}
nonempty_atomlist(l2) ::= nonempty_atomlist(l1) atom(atom).
		{
			l2 = l1;
			l2->insert(atom);
		}

nonempty_atomlist ::= nonempty_atomlist error.

atom(atom) ::= ID(id).
		{ 
			BaseType const* type = parser->_resolveType(BaseType::PREDICATE, (*id)->str(), 0);
			atom = new BaseElement(type, (*id)->loc());
			delete id;
		}
atom(atom) ::= ID(id) DOT.
		{ 
			BaseType const* type = parser->_resolveType(BaseType::PREDICATE, (*id)->str(), 0);
			atom = new BaseElement(type, (*id)->loc());
			delete id;
		}

atom(atom) ::= ID(id) L_PAREN arglist(args) R_PAREN.
		{
			BaseType const* type = parser->_resolveType(BaseType::PREDICATE, (*id)->str(), args->size());
			atom = new BaseElement(type, args, (*id)->loc());
			delete id;
		} 
atom(atom) ::= ID(id) L_PAREN arglist(args) R_PAREN DOT.
		{
			BaseType const* type = parser->_resolveType(BaseType::PREDICATE, (*id)->str(), args->size());
			atom = new BaseElement(type, args, (*id)->loc());
			delete id;
		} 

arglist(args) ::= function(elem).
		{
			args = new BaseElement::ArgumentList();
			args->push_back(elem);
		}

arglist(new_args) ::= arglist(old_args) COMMA function(elem).
		{
			new_args = old_args;
			old_args->push_back(elem);
		}

function(f) ::= ID(id).
		{ 
			BaseType const* type = parser->_resolveType(BaseType::FUNCTION, (*id)->str(), 0);
			f = new BaseElement(type, (*id)->loc());
			delete id;
		}
function(f) ::= STRING_LITERAL(string).
		{
			BaseType const* type = parser->_resolveType(BaseType::FUNCTION, (*string)->str(), 0);
			f = new BaseElement(type, (*string)->loc());
			delete string;
		}

function(f) ::= NUMBER(n).
		{
			BaseType const* type = parser->_resolveType(BaseType::FUNCTION, (*n)->str(), 0);
			f = new BaseElement(type, (*n)->loc());
			delete n;
		}

function(f) ::= ID(id) L_PAREN arglist(args) R_PAREN. 
		{
			BaseType const* type = parser->_resolveType(BaseType::FUNCTION, (*id)->str(), args->size());
			f = new BaseElement(type, args, (*id)->loc());
			delete id;
		} 

// ---------- epilogue

epilogue ::= .
epilogue ::= epilogue UNSATISFIABLE.
epilogue ::= epilogue SATISFIABLE.
epilogue ::= epilogue
	MODELS COLON NUMBER
	TIME COLON FP_NUMBER
	PREPARE COLON FP_NUMBER
	PREPRO_DOT COLON FP_NUMBER
	SOLVING COLON FP_NUMBER.









