/* Driver template for the LEMON parser generator.
** The author disclaims copyright to this source code.
*/
/* First off, code is included that follows the "include" declaration
** in the input grammar file. */
#include <stdio.h>
#line 1 "bcplus/parser/detail/lemon_parser.y"

			#include <cassert>
			#include <cstring>

			#include <boost/foreach.hpp>

			#include "babb/utils/memory.h"

			#include "bcplus/Location.h"
			#include "bcplus/parser/BCParser.h"
			#include "bcplus/parser/Token.h"
			#include "bcplus/parser/detail/lemon_parser.h"
			#include "bcplus/parser/detail/Number.h"
			#include "bcplus/parser/detail/NumberRange.h"
			#include "bcplus/statements/Statement.h"
			#include "bcplus/statements/declarations.h"
			#include "bcplus/statements/QueryStatement.h"
			#include "bcplus/statements/blocks.h"
			#include "bcplus/statements/laws.h"
			#include "bcplus/elements/Element.h"
			#include "bcplus/elements/terms.h"
			#include "bcplus/elements/formulas.h"
			#include "bcplus/symbols/Symbol.h"
			#include "bcplus/symbols/MacroSymbol.h"
			#include "bcplus/symbols/ConstantSymbol.h"
			#include "bcplus/symbols/AttributeSymbol.h"

			#define UNUSED void*

			using namespace bcplus;
			using namespace babb::utils;
			using namespace bcplus::parser;
			using namespace bcplus::statements;
			using namespace bcplus::elements;
			using namespace bcplus::languages;
			using namespace bcplus::symbols;
			using namespace bcplus::parser::detail;
			

			/// A list of terms
			typedef ReferencedVector<ref_ptr<const Term> >::type TermList;

			/// A list of sorts
			typedef ReferencedVector<ref_ptr<const SortSymbol> >::type SortList;


			typedef ReferencedVector<ref_ptr<const Token> >::type TokenList;

			/// Helper for deallocating things
			#define DEALLOC(x)	{ if (x) delete x; x = NULL; }

			/// A list of name sortlist pairs for declaring identifiers
			typedef std::pair<ref_ptr<const Token>, ref_ptr<SortList> > IdentifierDecl;
			typedef ReferencedVector<IdentifierDecl>::type IdentifierDeclList;

		
#line 327 "bcplus/parser/detail/lemon_parser.y"

	#define BASE_ELEM_DEF(elem, id, lparen, args, rparen, symtype, class, symclass)											\
		BASE_ELEM_DEF9(elem, id, lparen, args, rparen, symtype, class, symclass, false)

	#define BASE_ELEM_DEF9(elem, id, lparen, args, rparen, symtype, class, symclass, dynamic)								\
		elem = NULL;																										\
		ref_ptr<const Token> id_ptr = id;																					\
		ref_ptr<const Token> lparen_ptr = lparen;																			\
		ref_ptr<TermList> args_ptr = args;																					\
		ref_ptr<const Token> rparen_ptr = rparen;																			\
		size_t arity = (args_ptr ? args_ptr->size() : 0);																	\
																															\
		/* Check to see if we have constants in any of the terms */															\
		bool good = true;																									\
		if (!parser->lang()->support(Language::Feature::FORMULA_CONSTANT_ARGS) && arity) {									\
			int cargs = 0;																									\
			/*BOOST_FOREACH(Element const* elem, *args_ptr) { */															\
			for (TermList::const_iterator it = args_ptr->begin(); it != args_ptr->end(); it++) {							\
				cargs |=(*it)->cmask();																						\
			}																												\
			if (cargs) {																									\
				parser->_feature_error(Language::Feature::FORMULA_CONSTANT_ARGS, &id->beginLoc());							\
				good = false;																								\
			}																												\
		}																													\
																															\
		if (good) {																											\
			Symbol const* sym = parser->symtab()->resolve(symtype, *id_ptr->str(), arity);									\
																															\
			if (!sym && dynamic) {																							\
				/* Dynamic declarations are allowed. Attempt to create a new symbol */										\
				ref_ptr<ConstantSymbol::SortList> sorts = new ConstantSymbol::SortList();									\
				int argnum = 0;																								\
				if (args) {																									\
					BOOST_FOREACH(Term const* t, *args_ptr) {																\
							argnum++;																						\
						switch (t->subType()) {																				\
						case Term::Type::VARIABLE:																			\
							sorts->push_back(((Variable const*)t)->symbol()->sort());										\
							break;																							\
						case Term::Type::CONSTANT:																			\
							sorts->push_back(((Constant const*)t)->symbol()->sort());										\
							break;																							\
						case Term::Type::ANON_VAR:																			\
						case Term::Type::NULLARY:																			\
						case Term::Type::LUA:																				\
						case Term::Type::OBJECT:																			\
						case Term::Type::BINARY:																			\
						case Term::Type::UNARY:																				\
							parser->_parse_error("Unable to dynamically declare abAction \"" + Symbol::genName(*id_ptr->str(), (args_ptr ? args_ptr->size() : 0))\
							+ "\". Could not deduce the sort of argument #" + boost::lexical_cast<std::string>(argnum)		\
							+ " as it isn't a constant or variable. This problem can be fixed by explicitly declaring the abAction" \
							" prior to this rule.", &id_ptr->beginLoc());													\
							good = false;																					\
							break;																							\
						}																									\
					}																										\
				}																											\
				if  (good) {																								\
					ref_ptr<ConstantSymbol> cs = new ConstantSymbol(ConstantSymbol::Type::ABACTION, id_ptr->str(), parser->symtab()->bsort(SymbolTable::BuiltinSort::BOOLEAN), sorts);\
					/* add the sort to the symbol table */																	\
					if (!parser->symtab()->create(cs)) {																	\
						/* It seems there was a problem. */																	\
						parser->_parse_error("An error occurred while declaring \"" + Symbol::genName(*id_ptr->str(), (args_ptr ? args_ptr->size() : 0)) + "\".");\
						good = false;																						\
						break;																								\
					} else sym = cs;																						\
				}																											\
			}																												\
																															\
			if (!good) {																									\
				YYERROR;																									\
			} else if (!sym || sym->type() != symtype) {																	\
				/* The preprocessor indicated this was a constant and it's not... oops. */									\
				parser->_parse_error(std::string("INTERNAL ERROR: Could not locate symbol table entry for ")				\
					+ Symbol::Type::cstr(symtype) + " \"" + Symbol::genName(*id_ptr->str(), arity)		 					\
					+ "\".", &id_ptr->beginLoc());																			\
				YYERROR;																									\
			} else {																										\
				elem = new class((symclass*)sym, args, id_ptr->beginLoc(), (arity ? rparen_ptr->endLoc() : id_ptr->endLoc()));	\
			} 																												\
		}																													\
	
	#define BASE_ELEM_BARE_DEF(elem, id, symtype, class, symclass)															\
		elem = NULL;																										\
		ref_ptr<const Token> id_ptr = id;																					\
																															\
		Symbol const* sym = parser->symtab()->resolve(symtype, *id_ptr->str());												\
		if (sym && sym->type() == symtype) {																				\
			elem = new class((symclass*)sym, id_ptr->beginLoc(), id_ptr->endLoc());											\
		} else {																											\
			/* The preprocessor indicated this was a constant and it's not... oops. */										\
			parser->_parse_error(std::string("INTERNAL ERROR: Could not locate symbol table entry for ")					\
				+ Symbol::Type::cstr(symtype) + " \"" + Symbol::genName(*id_ptr->str(), 0)			 						\
				+ "\".", &id_ptr->beginLoc());																				\
			YYERROR;																										\
		}	
		
	#define BASE_LUA_ELEM(elem, id, lparen, args, rparen)																	\
		ref_ptr<const Token> id_ptr = id;																					\
		ref_ptr<const Token> lparen_ptr = lparen;																			\
		ref_ptr<TermList> args_ptr = args;																					\
		ref_ptr<const Token> rparen_ptr = rparen;																			\
		elem = new LuaTerm(id_ptr->str(), args, id_ptr->beginLoc(), (args ? rparen_ptr->endLoc() : id_ptr->endLoc()));

	#define UNDECLARED(elem, id, args)																						\
		elem = NULL;																										\
		ref_ptr<const Token> id_ptr = id;																					\
		ref_ptr<TermList> args_ptr = args;																					\
		parser->_parse_error("Encountered undeclared identifier \"" 														\
			+ Symbol::genName(*id->str(), (args_ptr ? args_ptr->size() : 0)) + "\".", &id_ptr->beginLoc());					\
		YYERROR;		

#line 515 "bcplus/parser/detail/lemon_parser.y"

	#define BASIC_TERM(term, id)																							\
		term = NULL;																										\
		ref_ptr<const Token> id_ptr = id;																					\
		ObjectSymbol const* sym = parser->symtab()->resolveOrCreate(new ObjectSymbol(id->str()));							\
		if (!sym) {																											\
			parser->_parse_error("An error occurred creating symbol \"" + *(id->str()) + "/0\".", &id->beginLoc());			\
			YYERROR;																										\
		} else term = new Object(sym, NULL, id->beginLoc(), id->endLoc());

	#define TERM_PARENS(term, lparen, sub, rparen)																			\
		ref_ptr<const Token> lparen_ptr = lparen;																			\
		ref_ptr<const Token> rparen_ptr = rparen;																			\
		term = sub;																											\
		term->parens(true);																									\
		term->beginLoc(lparen->beginLoc());																					\
		term->endLoc(rparen->endLoc());


	#define UNARY_ARITH(term, op, sub, operator)																			\
		term = NULL;																										\
		ref_ptr<const Token> op_ptr = op;																					\
		ref_ptr<Term> sub_ptr = sub;																						\
																															\
		if (sub->domainType() != DomainType::INTEGRAL && sub->domainType() != DomainType::UNKNOWN) {						\
			parser->_parse_error("Malformed arithmetic operation. Operand is not numeric.", &sub->beginLoc());				\
			YYERROR;																										\
		} else term = new UnaryTerm(operator, sub, sub->beginLoc(), sub->endLoc());

	#define BINARY_ARITH(term, lhs, op, rhs, operator)																		\
		term = NULL;																										\
		ref_ptr<Term> lhs_ptr = lhs;																						\
		ref_ptr<Term> rhs_ptr = rhs;																						\
		ref_ptr<const Token> op_ptr = op;																					\
																															\
		bool good = true;																									\
		if (lhs->domainType() != DomainType::INTEGRAL && lhs->domainType() != DomainType::UNKNOWN) {						\
			good = false;																									\
			parser->_parse_error("Malformed arithmetic operation. Left Operand is not numeric.", &op->beginLoc());			\
			YYERROR;																										\
		}																													\
		if (rhs->domainType() != DomainType::INTEGRAL && rhs->domainType() != DomainType::UNKNOWN) {						\
			good = false;																									\
			parser->_parse_error("Malformed arithmetic operation. Right Operand is not numeric.", &rhs->beginLoc());		\
			YYERROR;																										\
		}																													\
		if (good) term = new BinaryTerm(operator, lhs, rhs, lhs->beginLoc(), rhs->endLoc());

	#define NULLARY_TERM(term, op, feature, operator)																		\
		term = NULL;																										\
		ref_ptr<const Token> op_ptr = op;																					\
																															\
		if (!parser->lang()->support(feature)) {																			\
			parser->_feature_error(feature, &op->beginLoc());																\
		} else {																											\
			term = new NullaryTerm(operator, op->beginLoc(), op->endLoc());													\
		}

#line 739 "bcplus/parser/detail/lemon_parser.y"

	#define NUM_UOP(t_new, t, val)																				\
		ref_ptr<const Referenced> t_ptr = t;																			\
		t_new = new Number(val, t->beginLoc(), t->endLoc());

	
	#define NUM_BOP(t_new, l, r, val)																			\
		ref_ptr<const Referenced> l_ptr = l, r_ptr = r;																\
		t_new = new Number(val, l->beginLoc(), r->endLoc());

#line 783 "bcplus/parser/detail/lemon_parser.y"

	#define NESTED_BOP(new_f, lhs, op, rhs, operator)															\
		new_f = NULL;																							\
		ref_ptr<Formula> lhs_ptr = lhs;																			\
		ref_ptr<const Token> op_ptr = op;																		\
		ref_ptr<Formula> rhs_ptr = rhs;																			\
																												\
		if (!parser->lang()->support(Language::Feature::FORMULA_NESTED)) {										\
			parser->_feature_error(Language::Feature::FORMULA_NESTED, &op->beginLoc());							\
			YYERROR;																							\
		}																										\
		new_f = new BinaryFormula(operator, lhs, rhs, lhs->beginLoc(), rhs->endLoc());  						\

	#define NESTED_UOP(new_f, op, rhs, operator, feature)														\
		new_f = NULL;																							\
		ref_ptr<const Token> op_ptr = op;																		\
		ref_ptr<Formula> rhs_ptr = rhs;																			\
																												\
		/* Check to ensure that the operator is supported */													\
		if (!parser->lang()->support(feature)) {																\
			parser->_feature_error(feature);																	\
			YYERROR;																							\
		} else if (!parser->lang()->support(Language::Feature::FORMULA_NESTED)) {								\
			/* Check to ensure that this isn't nested */														\
			if (rhs->subType() == Formula::Type::BINARY || rhs->subType() == Formula::Type::UNARY) {			\
				parser->_feature_error(Language::Feature::FORMULA_NESTED, &rhs->beginLoc());					\
				YYERROR;																						\
			}																									\
		}																										\
		else new_f = new UnaryFormula (UnaryFormula:: Operator::NOT, rhs, op->beginLoc(), rhs->endLoc());

#line 863 "bcplus/parser/detail/lemon_parser.y"


	#define ATOMIC_FORMULA(af, constant, value) 														\
		af = NULL;																						\
		ref_ptr<Constant> c_ptr = constant;																\
																										\
		/* If this is a boolean constant we can interpret it as a shortcut for c=true */				\
		if (constant->domainType() != DomainType::BOOLEAN) {											\
			parser->_parse_error("\"" + *constant->symbol()->name() 									\
				+ "\" is not a boolean valued constant and therefore "									\
				"cannot be used in a bare atomic formula.", &constant->beginLoc());						\
			YYERROR;																					\
		} else {																						\
			ref_ptr<const ObjectSymbol> t =																\
				(value ? parser->symtab()->bobj(SymbolTable::BuiltinObject::TRUE) 						\
					: parser->symtab()->bobj(SymbolTable::BuiltinObject::FALSE));						\
			af = new AtomicFormula(constant,															\
					new Object(t, NULL, constant->beginLoc(), constant->endLoc()), 						\
					constant->beginLoc(), constant->endLoc());											\
		}
	


#line 942 "bcplus/parser/detail/lemon_parser.y"

	#define BINDING(new_f, lhs, op, rhs)																		\
		new_f = NULL;																							\
		ref_ptr<Term> lhs_ptr = lhs;																			\
		ref_ptr<const Token> op_ptr = op;																		\
		ref_ptr<Formula> rhs_ptr = rhs;																			\
																												\
		if (!parser->lang()->support(Language::Feature::QUERY_BIND_STEP)) {										\
			parser->_feature_error(Language::Feature::QUERY_BIND_STEP, &op->beginLoc());						\
			YYERROR;																							\
		} else {																								\
			new_f = new BindingFormula(lhs, rhs, lhs->beginLoc(), rhs->endLoc());  								\
		}

#line 1022 "bcplus/parser/detail/lemon_parser.y"

	#define CARD_FORMULA(card, min, lbrack, vars, af, rbrack, max)																	\
		card = NULL;																												\
		ref_ptr<const Referenced> vars_ptr = vars, af_ptr = af;																	\
		ref_ptr<const Term> min_ptr = min, max_ptr = max;																		\
		ref_ptr<const Token> lbrack_ptr = lbrack, rbrack_ptr = rbrack;															\
																																	\
		bool good = true;																										\
		if (min && min_ptr->domainType() != DomainType::INTEGRAL && min_ptr->domainType() != DomainType::UNKNOWN) {				\
			parser->_parse_error("Invalid lower cardinality bound. Expected an integral expression.", &min_ptr->beginLoc());		\
			good = false;																										\
			YYERROR;																											\
		}																														\
																																	\
		if (max && max_ptr->domainType() != DomainType::INTEGRAL && max_ptr->domainType() != DomainType::UNKNOWN) {				\
			parser->_parse_error("Invalid upper cardinality bound. Expected an integral expression.", &max_ptr->beginLoc());		\
			good = false;																										\
			YYERROR;																											\
		}																														\
																																\
		if (good) {																												\
			/* hopefully good to go. */																							\
			card = new CardinalityFormula(vars, af, min, max, 																	\
				(min ? min_ptr->beginLoc() : lbrack_ptr->beginLoc()), 															\
				(max ? max_ptr->endLoc() : rbrack_ptr->endLoc()));																\
		}																														\



#line 1260 "bcplus/parser/detail/lemon_parser.y"

		
	#define DYNAMIC_SORT_PLUS(new_s, s, op, feature, o)																												\
		new_s = NULL;																																				\
		ref_ptr<const Referenced> s_ptr = s, op_ptr = op;																											\
																																									\
																																									\
		if (!parser->lang()->support(feature)) {																													\
			parser->_feature_error(feature, &op->beginLoc());																										\
			YYERROR;																																				\
		} else {																																					\
			new_s = parser->symtab()->plus(s, o);																													\
		}
#line 1407 "bcplus/parser/detail/lemon_parser.y"

	#define CONSTANT_DECL(c, loc)																							\
		if (!parser->symtab()->create(c)) {																					\
			/* Check if it's a duplicate */																					\
			ConstantSymbol* c2 = (ConstantSymbol*)parser->symtab()->resolve(Symbol::Type::CONSTANT, *c->base(), c->arity());\
			if (!c2 || c2 != c) {																							\
				parser->_parse_error("Detected conflicting definition of symbol \"" + *c->name() + "\".", &loc);			\
			} else {																										\
				parser->_parse_error("Detected a duplicate definition of symbol \"" + *c->name() + "\".", &loc);			\
			}																												\
		}
#line 2139 "bcplus/parser/detail/lemon_parser.y"

	#define NC_STATEMENT(stmt, kw, period, feature, class)											\
		stmt = NULL;																				\
		ref_ptr<const Token> kw_ptr = kw, p_ptr = period;											\
																									\
		if (!parser->lang()->support(feature)) {													\
			parser->_feature_error(feature, &kw->beginLoc());										\
			YYERROR;																				\
		} else {																					\
			stmt = new class(kw->beginLoc(), period->endLoc());										\
		}																							

#line 2162 "bcplus/parser/detail/lemon_parser.y"

	#define VALUE_DECL(stmt, cd, kw, val_obj, p, feature, class)									\
		stmt = NULL;																				\
		ref_ptr<const Referenced> cd_ptr = cd, kw_ptr = kw, val_ptr = val_obj, p_ptr = p;			\
																									\
		if (!parser->lang()->support(feature)) {													\
			parser->_feature_error(feature, &kw->beginLoc());										\
			YYERROR;																				\
		} else { 																					\
			int value = val_obj->val();																\
			if (value < 0) {																		\
				parser->_parse_error("ERROR: Expected a positive integer.", &val_obj->beginLoc());	\
			} else {																				\
				stmt = new class(value, cd->beginLoc(), p->endLoc());								\
			}																						\
		}
#line 2189 "bcplus/parser/detail/lemon_parser.y"

	struct QueryData {
		QueryStatement::FormulaList* l;
		NumberRange const* maxstep;
		Token const* label;
	};

#line 2296 "bcplus/parser/detail/lemon_parser.y"

	#define QUERY_DECL(decl, kw, val, feature)																\
		decl = NULL;																						\
		ref_ptr<const Token> kw_ptr = kw, val_ptr = val;													\
																											\
		if (!parser->lang()->support(feature)) {															\
			parser->_feature_error(feature, &kw->beginLoc());												\
			YYERROR;																						\
		} else {																							\
			decl = val_ptr.release();																		\
		}

#line 2368 "bcplus/parser/detail/lemon_parser.y"

	#define CLAUSE(elem, kw, f, feature) 														\
		ref_ptr<const Token> kw_ptr = kw;														\
		elem = f;																				\
		if (!parser->lang()->support(feature)) {												\
			parser->_feature_error(feature, &kw->beginLoc());									\
			YYERROR;																			\
		}
#line 2454 "bcplus/parser/detail/lemon_parser.y"

	#define LAW_BASIC_FORM(law, kw, head, ifbody, ifcons, after, unless, where, p, static, dynamic, class)											\
		law = NULL;																																	\
		ref_ptr<Element> head_ptr = head, if_ptr = ifbody, ifcons_ptr = ifcons, after_ptr = after, unless_ptr = unless, where_ptr = where;		\
		ref_ptr<const Token> kw_ptr = kw, p_ptr = p;																								\
		Language::Feature::type feature = ((after) ? (dynamic) : (static));																			\
																																					\
		if (!parser->lang()->support(feature)) {																									\
			parser->_feature_error(feature, ((kw) ? &(kw_ptr)->beginLoc() : &(head_ptr)->beginLoc()));												\
			YYERROR;																																\
		} else {																																	\
			law = new class(head, ifbody, ifcons, after, unless, where, ((kw) ? (kw_ptr)->beginLoc() : (head_ptr)->beginLoc()), (p_ptr)->endLoc());	\
		}																																			

	#define LAW_IMPL_FORM(law, head, kw, body, where, p, feature, class)																			\
		law = NULL;																																	\
		ref_ptr<Element> head_ptr = head, body_ptr = body, where_ptr = where;																	\
		ref_ptr<const Token> kw_ptr = kw, p_ptr = p;																								\
																																					\
		if (!parser->lang()->support(feature)) {																									\
			parser->_feature_error(feature, &kw->beginLoc());																						\
			YYERROR;																																\
		} else {																																	\
			law = new class(head, body, where, head->beginLoc(), p->endLoc());																		\
		}


	#define LAW_DYNAMIC_FORM(law, body, kw, head, ifbody, unless, where, p, feature, class)															\
		law = NULL;																																	\
		ref_ptr<Element> body_ptr = body, head_ptr = head, if_ptr = ifbody, unless_ptr = unless, where_ptr = where;								\
		ref_ptr<const Token> kw_ptr = kw, p_ptr = p;																								\
																																					\
		if (!parser->lang()->support(feature)) {																									\
			parser->_feature_error(feature, &kw->beginLoc());																						\
			YYERROR;																																\
		} else {																																	\
			law = new class(body, head, ifbody, unless, where, body->beginLoc(), p->endLoc());														\
		}
	
	#define LAW_INCREMENTAL_FORM(law, body, kw, head, by, ifbody, unless, where, p, feature, class)													\
		law = NULL;																																	\
		ref_ptr<Element> body_ptr = body, head_ptr = head, if_ptr = ifbody, unless_ptr = unless, where_ptr = where;								\
		ref_ptr<Element> by_ptr = by;																											\
		ref_ptr<const Token> kw_ptr = kw, p_ptr = p;																								\
																																					\
		if (!parser->lang()->support(feature)) {																									\
			parser->_feature_error(feature, &kw->beginLoc());																						\
			YYERROR;																																\
		} else {																																	\
			law = new class(body, head, by, ifbody, unless, where, body->beginLoc(), p->endLoc());													\
		}

		
	#define LAW_CONSTRAINT_FORM(law, kw, body, after, unless, where, p, static, dynamic, class)														\
		law = NULL;																																	\
		ref_ptr<Element> body_ptr = body, after_ptr = after, unless_ptr = unless, where_ptr = where;												\
		ref_ptr<const Token> kw_ptr = kw, p_ptr = p;																								\
																																					\
		Language::Feature::type feature = (after ? dynamic : static);																				\
																																					\
		if (!parser->lang()->support(feature)) {																									\
			parser->_feature_error(feature, (kw ? &kw_ptr->beginLoc() : &body_ptr->beginLoc()));													\
			YYERROR;																																\
		} else {																																	\
			law = new class(body, after, unless, where, (kw ? kw_ptr->beginLoc() : body_ptr->beginLoc()), p_ptr->endLoc());							\
		}

	#define LAW_DYNAMIC_CONSTRAINT_FORM(law, kw, body, ifbody, unless, where, p, feature, class)													\
		law = NULL;																																	\
		ref_ptr<Element> body_ptr = body, if_ptr = ifbody, unless_ptr = unless, where_ptr = where;													\
		ref_ptr<const Token> kw_ptr = kw, p_ptr = p;																								\
																																					\
		if (!parser->lang()->support(feature)) {																									\
			parser->_feature_error(feature, (kw ? &kw_ptr->beginLoc() : &body_ptr->beginLoc()));													\
			YYERROR;																																\
		} else {																																	\
			law = new class(body, ifbody, unless, where, (kw ? kw_ptr->beginLoc() : body_ptr->beginLoc()), p_ptr->endLoc());						\
		}
	
	#define LAW_SIMPLE_FORM(law, kw, head, where, p, feature, class)																				\
		law = NULL;																																	\
		ref_ptr<Element> head_ptr = head, where_ptr = where;																						\
		ref_ptr<const Token> kw_ptr = kw, p_ptr = p;																								\
																																					\
		if (!parser->lang()->support(feature)) {																									\
			parser->_feature_error(feature, (kw ? &kw_ptr->beginLoc() : &head_ptr->beginLoc()));													\
			YYERROR;																																\
		} else {																																	\
			law = new class(head, where, (kw ? kw_ptr->beginLoc() : head_ptr->beginLoc()), p_ptr->endLoc());										\
		}
		

#line 2647 "bcplus/parser/detail/lemon_parser.y"

	#define CODE_BLK(stmt, code, feature, type) 												\
		ref_ptr<const Token> code_ptr = code;													\
		if (!parser->lang()->support(feature)) {												\
			stmt = NULL;																		\
			parser->_feature_error(feature, &code->beginLoc());									\
			YYERROR;																			\
		} else {																				\
			stmt = new type(code, code->beginLoc(), code->endLoc());							\
		}
#line 540 "bcplus/parser/detail/lemon_parser.c"
/* Next is all token values, in a form suitable for use by makeheaders.
** This section will be null unless lemon is run with the -m switch.
*/
/* 
** These constants (all generated automatically by the parser generator)
** specify the various kinds of tokens (terminals) that the parser
** understands. 
**
** Each symbol here is a terminal symbol in the grammar.
*/
/* Make sure the INTERFACE macro is defined.
*/
#ifndef INTERFACE
# define INTERFACE 1
#endif
/* The next thing included is series of defines which control
** various aspects of the generated parser.
**    YYCODETYPE         is the data type used for storing terminal
**                       and nonterminal numbers.  "unsigned char" is
**                       used if there are fewer than 250 terminals
**                       and nonterminals.  "int" is used otherwise.
**    YYNOCODE           is a number of type YYCODETYPE which corresponds
**                       to no legal terminal or nonterminal number.  This
**                       number is used to fill in empty slots of the hash 
**                       table.
**    YYFALLBACK         If defined, this indicates that one or more tokens
**                       have fall-back values which should be used if the
**                       original value of the token will not parse.
**    YYACTIONTYPE       is the data type used for storing terminal
**                       and nonterminal numbers.  "unsigned char" is
**                       used if there are fewer than 250 rules and
**                       states combined.  "int" is used otherwise.
**    lemon_parserTOKENTYPE     is the data type used for minor tokens given 
**                       directly to the parser from the tokenizer.
**    YYMINORTYPE        is the data type used for all minor tokens.
**                       This is typically a union of many types, one of
**                       which is lemon_parserTOKENTYPE.  The entry in the union
**                       for base tokens is called "yy0".
**    YYSTACKDEPTH       is the maximum depth of the parser's stack.  If
**                       zero the stack is dynamically sized using realloc()
**    lemon_parserARG_SDECL     A static variable declaration for the %extra_argument
**    lemon_parserARG_PDECL     A parameter declaration for the %extra_argument
**    lemon_parserARG_STORE     Code to store %extra_argument into yypParser
**    lemon_parserARG_FETCH     Code to extract %extra_argument from yypParser
**    YYNSTATE           the combined number of states.
**    YYNRULE            the number of rules in the grammar
**    YYERRORSYMBOL      is the code number of the error symbol.  If not
**                       defined, then do no error processing.
*/
#define YYCODETYPE unsigned char
#define YYNOCODE 245
#define YYACTIONTYPE unsigned short int
#define lemon_parserTOKENTYPE  Token const* 								
typedef union {
  int yyinit;
  lemon_parserTOKENTYPE yy0;
  IdentifierDeclList* yy42;
  ConstantDeclaration* yy55;
  ObjectDeclaration::Element* yy70;
  ConstantDeclaration::ElementList* yy97;
  MacroDeclaration::ElementList* yy105;
  VariableDeclaration::ElementList* yy117;
  AtomicFormula* yy138;
  QueryStatement* yy146;
  TokenList* yy152;
  Term* yy163;
  VariableDeclaration* yy171;
  TermList* yy187;
  ObjectDeclaration::ElementList* yy198;
  UNUSED yy209;
  NCStatement* yy210;
  Statement* yy224;
  SortSymbol const* yy228;
  Object* yy238;
  QuantifierFormula::Operator::type yy249;
  Token const* yy251;
  QuantifierFormula::QuantifierList* yy261;
  QuantifierFormula* yy285;
  ObjectDeclaration* yy296;
  QueryData yy301;
  NumberRange* yy309;
  ConstantSymbol::Type::type yy310;
  LuaTerm* yy313;
  MacroSymbol* yy315;
  CardinalityFormula::VariableList* yy319;
  SortDeclaration::ElementList* yy320;
  ObjectDeclaration::Element::ObjectList* yy341;
  Constant* yy345;
  Formula* yy353;
  MacroSymbol::ArgumentList* yy354;
  CardinalityFormula* yy369;
  NumberRange const* yy392;
  SortSymbol* yy393;
  ConstantSymbol::SortList* yy411;
  Number* yy416;
  ShowStatement::ElementList* yy451;
  Variable* yy453;
  MacroDeclaration* yy455;
  SortDeclaration* yy469;
  StrongNCStatement* yy482;
  int yy489;
} YYMINORTYPE;
#ifndef YYSTACKDEPTH
#define YYSTACKDEPTH 100
#endif
#define lemon_parserARG_SDECL  BCParser* parser						;
#define lemon_parserARG_PDECL , BCParser* parser						
#define lemon_parserARG_FETCH  BCParser* parser						 = yypParser->parser						
#define lemon_parserARG_STORE yypParser->parser						 = parser						
#define YYNSTATE 727
#define YYNRULE 375
#define YYERRORSYMBOL 135
#define YYERRSYMDT yy489
#define YYFALLBACK 1
#define YY_NO_ACTION      (YYNSTATE+YYNRULE+2)
#define YY_ACCEPT_ACTION  (YYNSTATE+YYNRULE+1)
#define YY_ERROR_ACTION   (YYNSTATE+YYNRULE)

/* The yyzerominor constant is used to initialize instances of
** YYMINORTYPE objects to zero. */
static const YYMINORTYPE yyzerominor = { 0 };

/* Define the yytestcase() macro to be a no-op if is not already defined
** otherwise.
**
** Applications can choose to define yytestcase() in the %include section
** to a macro that can assist in verifying code coverage.  For production
** code the yytestcase() macro should be turned off.  But it is useful
** for testing.
*/
#ifndef yytestcase
# define yytestcase(X)
#endif


/* Next are the tables used to determine what action to take based on the
** current state and lookahead token.  These tables are used to implement
** functions that take a state number and lookahead value and return an
** action integer.  
**
** Suppose the action integer is N.  Then the action is determined as
** follows
**
**   0 <= N < YYNSTATE                  Shift N.  That is, push the lookahead
**                                      token onto the stack and goto state N.
**
**   YYNSTATE <= N < YYNSTATE+YYNRULE   Reduce by rule N-YYNSTATE.
**
**   N == YYNSTATE+YYNRULE              A syntax error has occurred.
**
**   N == YYNSTATE+YYNRULE+1            The parser accepts its input.
**
**   N == YYNSTATE+YYNRULE+2            No such action.  Denotes unused
**                                      slots in the yy_action[] table.
**
** The action table is constructed as a single large table named yy_action[].
** Given state S and lookahead X, the action is computed as
**
**      yy_action[ yy_shift_ofst[S] + X ]
**
** If the index value yy_shift_ofst[S]+X is out of range or if the value
** yy_lookahead[yy_shift_ofst[S]+X] is not equal to X or if yy_shift_ofst[S]
** is equal to YY_SHIFT_USE_DFLT, it means that the action is not in the table
** and that yy_default[S] should be used instead.  
**
** The formula above is for computing the action when the lookahead is
** a terminal symbol.  If the lookahead is a non-terminal (as occurs after
** a reduce action) then the yy_reduce_ofst[] array is used in place of
** the yy_shift_ofst[] array and YY_REDUCE_USE_DFLT is used in place of
** YY_SHIFT_USE_DFLT.
**
** The following are the tables generated in this section:
**
**  yy_action[]        A single table containing all actions.
**  yy_lookahead[]     A table containing the lookahead for each entry in
**                     yy_action.  Used to detect hash collisions.
**  yy_shift_ofst[]    For each state, the offset into yy_action for
**                     shifting terminals.
**  yy_reduce_ofst[]   For each state, the offset into yy_action for
**                     shifting non-terminals after a reduce.
**  yy_default[]       Default action for each state.
*/
#define YY_ACTTAB_COUNT (2803)
static const YYACTIONTYPE yy_action[] = {
 /*     0 */   726,  695,  727,  725,  724,  723,  722,  721,  720,  719,
 /*    10 */   718,  717,  716,  715,  714,  713,  712,  711,  648,  684,
 /*    20 */   271,  710,  703,  709,  708,  702,  107,  105,  104,  623,
 /*    30 */     5,   55,  268,  272,  111,  109,  107,  105,  104,  647,
 /*    40 */   346,   22,   73,   72,   71,  595,   70,   69,   68,   67,
 /*    50 */   136,  134,  133,  197,  646,  645,  569,  292,  684,  271,
 /*    60 */   710,  703,  709,  708,  702,  245,  103,  102,  101,  100,
 /*    70 */    99,  264,  272,  189,  187,  186,   54,  522,  637,  636,
 /*    80 */   635,  634,  441,  389,  570,  571,  117,  287,   53,  440,
 /*    90 */   439,  507,  506,  505,  504,  503,  502,  501,  500,  499,
 /*   100 */   498,  497,  496,  495,  494,  493,  492,  491,  490,  683,
 /*   110 */   464,  705,  433,  465,  682,  474,  473,  470,  469,  472,
 /*   120 */   471,  369,  511,  510,  698,  706,  707,  710,  703,  709,
 /*   130 */   708,  702,  402,  345,   40,   52,   10,  251,  168,  631,
 /*   140 */    38,  112,    8,    9,  254,  158,  132,  214,   39,  683,
 /*   150 */   464,  651,  208,  465,  682,  261,  676,  675,  677,    7,
 /*   160 */   418,  417,    6,  442,   37,  200,   29,   28,   27,   31,
 /*   170 */    30,  643,  644,   32,  940,  534,  273,  151,  940,  115,
 /*   180 */   577,  683,  464,  260,  576,  465,  682,  684,  271,  710,
 /*   190 */   703,  709,  708,  702,  419,   66,  676,  675,  420,  681,
 /*   200 */   267,  272,  165,  166,  454,  294,  640,  637,  636,  635,
 /*   210 */   634,  632,  633,  113,  170,  453,  567,    4,   98,   23,
 /*   220 */   116,   31,   30,  260,  622,   32,  468,  621,  676,  675,
 /*   230 */   677,  455,  103,  102,  101,  100,   99,   65,  467,  701,
 /*   240 */   466,   64,  157,  632,  633,  455,  170,  513,  512,    4,
 /*   250 */   195,   35,   20,   21,  532,  260,  545,  292,   98,   63,
 /*   260 */   683,  464,   45,   44,  465,  682,   46,  462,  684,  271,
 /*   270 */   710,  703,  709,  708,  702,  391,  551,  390,  467,  701,
 /*   280 */   466,  268,  272,  159,   33,   34,  530,  704,  647,  642,
 /*   290 */    98,   32,  584,  568,  547,  664,  658,  710,  703,  709,
 /*   300 */   708,  702,  259,  646,  645,  398,  319,  676,  675,  677,
 /*   310 */   467,  701,  466,  565,  564,  563,  562,  112,  169,  372,
 /*   320 */   528,  434,  632,  633,   51,  170,  700,   46,    4,  561,
 /*   330 */    23,  559,  572,  573,  260,  699,  558,  379,  537,  378,
 /*   340 */   684,  271,  710,  703,  709,  708,  702,  533,  557,  555,
 /*   350 */   556,  560,   74,  268,  272,   58,  112,  545,  292,  467,
 /*   360 */   647,  642,  192,   20,   21,  112,  683,  464,  462,   98,
 /*   370 */   465,  682,  665,  467,  248,  646,  645,  698,  706,  707,
 /*   380 */   710,  703,  709,  708,  702,  401,  345,  530,  554,  467,
 /*   390 */   701,  466,  382,  543,  381,  544,  190,   28,   27,   31,
 /*   400 */    30,  188,  141,   32,  684,  271,  710,  703,  709,  708,
 /*   410 */   702,  629,  628,  676,  675,  677,  164,  264,  272,   26,
 /*   420 */   371,  528,  195,  522,  637,  636,  635,  634,  643,  644,
 /*   430 */   377,  373,  428,  286,  151,  462,  115,  599, 1011,   57,
 /*   440 */   260,  624,  698,  706,  707,  710,  703,  709,  708,  702,
 /*   450 */   400,  345,   18,   17,  530,  162,   19,  175,  146,    2,
 /*   460 */   154,  160,  161,  575, 1011,   36,  165,  166,  515,  514,
 /*   470 */   113,  112,  619,  464,  598,   98,  465,  618,  394,  587,
 /*   480 */   684,  271,  710,  703,  396,  708,  702,  649,  531,  423,
 /*   490 */   424,   56,  462,  267,  272,  467,  701,  466,  278,  640,
 /*   500 */   637,  636,  635,  634,  684,  271,  710,  703,  396,  708,
 /*   510 */   702,  460,   24,  397,  443,  172,  397,  267,  272,  612,
 /*   520 */   611,  613,  280,  640,  637,  636,  635,  634,  586,  111,
 /*   530 */   109,  107,  105,  104,  602,  603,   25,  397,   42,   41,
 /*   540 */    45,   44,   49,  597,   46,  596,    5,  593,  174,  681,
 /*   550 */   444,  697,  464,  588,  152,  465,  696,   22,   73,   72,
 /*   560 */    71,  149,   70,   69,   68,   67,   73,   72,   71,  462,
 /*   570 */    70,   69,   68,   67,  153,   47,   48,  454,   36,  438,
 /*   580 */   198,  119,  103,  102,  101,  100,   99,  527,  461,  566,
 /*   590 */   103,  102,  101,  100,   99,  550,  244,  642,  691,  690,
 /*   600 */   692,  467,  701,  466,  684,  342,  710,  703,  709,  708,
 /*   610 */   702,  219,  195,  693,  694,  150,  159,  296,  343,  148,
 /*   620 */   542,  110,  156,  684,  271,  710,  703,  709,  708,  702,
 /*   630 */   437,   29,   28,   27,   31,   30,  265,  272,   32,  243,
 /*   640 */   173,  276,  640,  637,  636,  635,  634,  432,  684,  270,
 /*   650 */   710,  703,  709,  708,  702,  108,  539,  553,  260,  536,
 /*   660 */   106,  641,  272,  145, 1046,  429,  626,  640,  637,  636,
 /*   670 */   635,  634,  684,  271,  710,  703,  709,  708,  702,  167,
 /*   680 */   467,  701,  466,  625, 1046,  267,  272,  148,  220,   50,
 /*   690 */   639,  640,  637,  636,  635,  634,  147,  684,  271,  710,
 /*   700 */   703,  709,  708,  702,  144,   29,   28,   27,   31,   30,
 /*   710 */   267,  272,   32,  467,   62,  638,  640,  637,  636,  635,
 /*   720 */   634,   29,   28,   27,   31,   30,  516,  454,   32,  143,
 /*   730 */   684,  271,  710,  703,  709,  708,  702,  142,   29,   28,
 /*   740 */    27,   31,   30,  267,  272,   32,    3,  642,  457,  640,
 /*   750 */   637,  636,  635,  634,  684,  271,  710,  703,  709,  708,
 /*   760 */   702,  403,   61,   15,   14,   18,   17,  267,  272,   19,
 /*   770 */   590,  591,  456,  640,  637,  636,  635,  634,  684,  271,
 /*   780 */   710,  703,  709,  708,  702,  552,   29,   28,   27,   31,
 /*   790 */    30,  267,  272,   32,  585,  390,  313,  640,  637,  636,
 /*   800 */   635,  634,  427,  684,  271,  710,  703,  709,  708,  702,
 /*   810 */   535,   60,  262,   36,  546,  148,  267,  272,  541,  378,
 /*   820 */   529,  357,  640,  637,  636,  635,  634,  684,  271,  710,
 /*   830 */   703,  709,  708,  702,  526,   29,   28,   27,   31,   30,
 /*   840 */   267,  272,   32,  194,  148,  356,  640,  637,  636,  635,
 /*   850 */   634,   11,  684,  271,  710,  703,  709,  708,  702,  193,
 /*   860 */   191,  189,  187,  186,  185,  267,  272,  463,  701,   59,
 /*   870 */   281,  640,  637,  636,  635,  634,   29,   28,   27,   31,
 /*   880 */    30,  184,  462,   32,  661,  684,  271,  710,  703,  709,
 /*   890 */   708,  702,   19,   29,   28,   27,   31,   30,  267,  272,
 /*   900 */    32,  459,  262,  279,  640,  637,  636,  635,  634,  684,
 /*   910 */   271,  710,  703,  709,  708,  702,  163,  140,  138,  136,
 /*   920 */   134,  133,  267,  272,  368,  948,  509,  277,  640,  637,
 /*   930 */   636,  635,  634,  684,  271,  710,  703,  709,  708,  702,
 /*   940 */   948,  948,  196,  257,  629,  628,  263,  272,  508,  392,
 /*   950 */   589,  489,  522,  637,  636,  635,  634,  463,  376,  428,
 /*   960 */   948,  948,  274,  684,  271,  710,  703,  709,  708,  702,
 /*   970 */   375,  428,  948,  374,  428,  488,  264,  272,  948,  349,
 /*   980 */   428,  600,  522,  637,  636,  635,  634,  525,  517,  292,
 /*   990 */   487,  486,  288, 1103,    1,  684,  270,  710,  703,  709,
 /*  1000 */   708,  702,  485,   43,   42,   41,   45,   44,  523,  272,
 /*  1010 */    46,  518,  118,  484,  522,  637,  636,  635,  634,  193,
 /*  1020 */   191,  189,  187,  186,  519,  684,  271,  710,  703,  709,
 /*  1030 */   708,  702,  483,   16,   15,   14,   18,   17,  264,  272,
 /*  1040 */    19,  482,  480,  479,  522,  637,  636,  635,  634,   43,
 /*  1050 */    42,   41,   45,   44,  521,  478,   46,  477,  684,  271,
 /*  1060 */   710,  703,  709,  708,  702,  476,  111,  109,  107,  105,
 /*  1070 */   104,  264,  272,  652,   12,  463,  648,  522,  637,  636,
 /*  1080 */   635,  634,   92,   91,   90,   89,   88,  520,  684,  271,
 /*  1090 */   710,  703,  709,  708,  702,  650,   16,   15,   14,   18,
 /*  1100 */    17,  264,  272,   19,  701,  467,  256,  522,  637,  636,
 /*  1110 */   635,  634,   97,   96,   95,   94,   93,  422,  684,  271,
 /*  1120 */   710,  703,  709,  708,  702,  140,  138,  136,  134,  133,
 /*  1130 */   630,  264,  272,  569,  292,   36,  627,  522,  637,  636,
 /*  1140 */   635,  634,   29,   28,   27,   31,   30,  421,  255,   32,
 /*  1150 */   684,  271,  710,  703,  709,  708,  702,  426,  387,  436,
 /*  1160 */   389,  570,  571,  264,  272,  155,  450,   50,  449,  522,
 /*  1170 */   637,  636,  635,  634,  193,  191,  189,  187,  186,  298,
 /*  1180 */   684,  271,  710,  703,  709,  708,  702,  448,  447,  252,
 /*  1190 */   249,  594,  446,  264,  272,  569,  292,  247,  246,  522,
 /*  1200 */   637,  636,  635,  634,  583,  262,  445,  393,  548,  348,
 /*  1210 */   425,  568,  241,  684,  271,  710,  703,  709,  708,  702,
 /*  1220 */   385,  436,  389,  570,  571,  240,  264,  272,  540,   13,
 /*  1230 */    12,  416,  522,  637,  636,  635,  634,  193,  191,  189,
 /*  1240 */   187,  186,  347,  684,  271,  710,  703,  709,  708,  702,
 /*  1250 */   236,  545,  292,  237,  239,  234,  267,  272,  262,  235,
 /*  1260 */   463,  285,  640,  637,  636,  635,  634,  414,  232,  231,
 /*  1270 */   413,  230,  684,  271,  710,  703,  709,  708,  702,  569,
 /*  1280 */   292,  229,  227,  481,  225,  267,  272,  549,  381,  544,
 /*  1290 */   284,  640,  637,  636,  635,  634,  684,  271,  710,  703,
 /*  1300 */   709,  708,  702,  412,  384,  436,  389,  570,  571,  267,
 /*  1310 */   272,  411,  223,  463,  181,  640,  637,  636,  635,  634,
 /*  1320 */   410,  221,  218,  409,  684,  271,  710,  703,  709,  708,
 /*  1330 */   702,  569,  292,  217,  216,  215,  213,  267,  272,  408,
 /*  1340 */   212,  209,  180,  640,  637,  636,  635,  634,  684,  271,
 /*  1350 */   710,  703,  709,  708,  702,  210,  380,  436,  389,  570,
 /*  1360 */   571,  267,  272,  211,  407,  206,  179,  640,  637,  636,
 /*  1370 */   635,  634,  684,  271,  710,  703,  709,  708,  702,  207,
 /*  1380 */   569,  292,  205,  406,  204,  267,  272,  203,  201,  592,
 /*  1390 */   178,  640,  637,  636,  635,  634,  405,  684,  271,  710,
 /*  1400 */   703,  709,  708,  702,  569,  292,  388,  389,  570,  571,
 /*  1410 */   267,  272,  199,  404,  242,  177,  640,  637,  636,  635,
 /*  1420 */   634,  684,  271,  710,  703,  709,  708,  702,  574,  258,
 /*  1430 */   435,  389,  570,  571,  267,  272,  299,  350,  253,  176,
 /*  1440 */   640,  637,  636,  635,  634,    5,  291,  351,  681,  664,
 /*  1450 */   658,  710,  703,  709,  708,  702,  582,   73,   72,   71,
 /*  1460 */   320,   70,   69,   68,   67,  620,  610,  710,  703,  709,
 /*  1470 */   708,  702,  578,  684,  271,  710,  703,  709,  708,  702,
 /*  1480 */   269,  103,  102,  101,  100,   99,  268,  272,  581,  524,
 /*  1490 */   293,  607,  604,  647,  642,  580,  579,  684,  271,  710,
 /*  1500 */   703,  709,  708,  702,  290,  289,  415,  238,  646,  645,
 /*  1510 */   268,  272,  250,  431,  228,  226,  224,  647,  642,  222,
 /*  1520 */   202,  193,  191,  189,  187,  186,  538,    5, 1104, 1104,
 /*  1530 */  1104,  233,  646,  645, 1104, 1104, 1104, 1104, 1104,   73,
 /*  1540 */    72,   71, 1104,   70,   69,   68,   67,  698,  706,  707,
 /*  1550 */   710,  703,  709,  708,  702,  399,  345, 1104, 1104,  617,
 /*  1560 */  1104, 1104, 1104,  103,  102,  101,  100,   99,  126,  125,
 /*  1570 */   124, 1104,  123,  122,  121,  120,  683,  464, 1104, 1104,
 /*  1580 */   465,  682,   87,   86,   85, 1104,   84,   83,   82,   81,
 /*  1590 */  1104, 1104,  131,  130,  129,  128,  127, 1104,  684,  672,
 /*  1600 */   710,  703,  709,  708,  702, 1104,   92,   91,   90,   89,
 /*  1610 */    88,  674,  343,   74,   80,   79, 1104,   78,   77,   76,
 /*  1620 */    75, 1104, 1104,  676,  675,  677,  794,  794,  794, 1104,
 /*  1630 */   794,  794,  794,  794, 1104, 1104, 1104,   97,   96,   95,
 /*  1640 */    94,   93, 1104, 1104, 1104, 1104,  115, 1104, 1104, 1104,
 /*  1650 */   794,  794,  794,  794,  794,  663,  464, 1104, 1104,  465,
 /*  1660 */   662, 1104,  126,  125,  124, 1104,  123,  122,  121,  120,
 /*  1670 */  1104, 1104,  698,  706,  707,  710,  703,  709,  708,  702,
 /*  1680 */   114,  344, 1104, 1104, 1104,   98,  131,  130,  129,  128,
 /*  1690 */   127,  383,  430, 1104,  620,  610,  710,  703,  709,  708,
 /*  1700 */   702, 1104,  475, 1104, 1104,  467,  701,  466, 1104,  266,
 /*  1710 */   684,  282,  710,  703,  709,  708,  702,  659,  660,  275,
 /*  1720 */   607,  604, 1104,  674,  343,  139, 1104,  620,  610,  710,
 /*  1730 */   703,  709,  708,  702,  140,  138,  136,  134,  133, 1104,
 /*  1740 */  1104, 1104,  609,  620,  610,  710,  703,  709,  708,  702,
 /*  1750 */  1104, 1104,  601,  607,  604, 1104, 1104, 1104,  269,  137,
 /*  1760 */  1104,  192, 1104, 1104,  135, 1104, 1104, 1104,  606,  607,
 /*  1770 */   604, 1104,  620,  610,  710,  703,  709,  708,  702, 1104,
 /*  1780 */  1104, 1104, 1104, 1104,  467,  701,  466,  269,  620,  610,
 /*  1790 */   710,  703,  709,  708,  702,  190, 1104,  605,  607,  604,
 /*  1800 */   188, 1104, 1104,  269, 1104,  620,  610,  710,  703,  709,
 /*  1810 */   708,  702, 1104,  452,  607,  604, 1104, 1104, 1104, 1104,
 /*  1820 */   269,  620,  610,  710,  703,  709,  708,  702, 1104, 1104,
 /*  1830 */   451,  607,  604,  569,  292, 1104,  269, 1104, 1104, 1104,
 /*  1840 */  1104, 1104, 1104, 1104, 1104, 1104,  300,  607,  604,  620,
 /*  1850 */   610,  710,  703,  709,  708,  702, 1104, 1104, 1104,  386,
 /*  1860 */   389,  570,  571, 1104,  269, 1104,  664,  658,  710,  703,
 /*  1870 */   709,  708,  702,  649,  353,  607,  604,  358,  462, 1104,
 /*  1880 */   620,  610,  710,  703,  709,  708,  702,  698,  706,  707,
 /*  1890 */   710,  703,  709,  708,  702,  269,  366,  458, 1104, 1104,
 /*  1900 */  1104,  171,  397, 1104, 1104,  352,  607,  604,  698,  706,
 /*  1910 */   707,  710,  703,  709,  708,  702,  395,  345,  698,  706,
 /*  1920 */   707,  710,  703,  709,  708,  702, 1104,  297,  698,  706,
 /*  1930 */   707,  710,  703,  709,  708,  702, 1104,  367,  698,  706,
 /*  1940 */   707,  710,  703,  709,  708,  702, 1104,  689,  698,  706,
 /*  1950 */   707,  710,  703,  709,  708,  702, 1104,  685, 1104,  698,
 /*  1960 */   706,  707,  710,  703,  709,  708,  702, 1104,  688,  698,
 /*  1970 */   706,  707,  710,  703,  709,  708,  702, 1104,  687,  698,
 /*  1980 */   706,  707,  710,  703,  709,  708,  702, 1104,  686, 1104,
 /*  1990 */   698,  706,  707,  710,  703,  709,  708,  702, 1104,  365,
 /*  2000 */   698,  706,  707,  710,  703,  709,  708,  702, 1104,  364,
 /*  2010 */  1104, 1104, 1104, 1104, 1104, 1104, 1104, 1104,  698,  706,
 /*  2020 */   707,  710,  703,  709,  708,  702, 1104,  680,  698,  706,
 /*  2030 */   707,  710,  703,  709,  708,  702, 1104,  679,  698,  706,
 /*  2040 */   707,  710,  703,  709,  708,  702, 1104,  678,  698,  706,
 /*  2050 */   707,  710,  703,  709,  708,  702, 1104,  673, 1104,  698,
 /*  2060 */   706,  707,  710,  703,  709,  708,  702, 1104,  363,  698,
 /*  2070 */   706,  707,  710,  703,  709,  708,  702, 1104,  362,  698,
 /*  2080 */   706,  707,  710,  703,  709,  708,  702, 1104,  671,  698,
 /*  2090 */   706,  707,  710,  703,  709,  708,  702, 1104,  670,  698,
 /*  2100 */   706,  707,  710,  703,  709,  708,  702, 1104,  669,  698,
 /*  2110 */   706,  707,  710,  703,  709,  708,  702, 1104,  361,  698,
 /*  2120 */   706,  707,  710,  703,  709,  708,  702, 1104,  360,  698,
 /*  2130 */   706,  707,  710,  703,  709,  708,  702, 1104,  668,  698,
 /*  2140 */   706,  707,  710,  703,  709,  708,  702, 1104,  667,  698,
 /*  2150 */   706,  707,  710,  703,  709,  708,  702, 1104,  666,  698,
 /*  2160 */   706,  707,  710,  703,  709,  708,  702, 1104,  341, 1104,
 /*  2170 */  1104, 1104,  698,  706,  707,  710,  703,  709,  708,  702,
 /*  2180 */  1104,  340,  698,  706,  707,  710,  703,  709,  708,  702,
 /*  2190 */  1104,  339,  698,  706,  707,  710,  703,  709,  708,  702,
 /*  2200 */  1104,  338,  698,  706,  707,  710,  703,  709,  708,  702,
 /*  2210 */  1104,  337, 1104,  698,  706,  707,  710,  703,  709,  708,
 /*  2220 */   702, 1104,  336,  698,  706,  707,  710,  703,  709,  708,
 /*  2230 */   702, 1104,  335,  698,  706,  707,  710,  703,  709,  708,
 /*  2240 */   702, 1104,  334,  698,  706,  707,  710,  703,  709,  708,
 /*  2250 */   702, 1104,  333,  698,  706,  707,  710,  703,  709,  708,
 /*  2260 */   702, 1104,  332,  698,  706,  707,  710,  703,  709,  708,
 /*  2270 */   702, 1104,  331,  698,  706,  707,  710,  703,  709,  708,
 /*  2280 */   702, 1104,  330,  698,  706,  707,  710,  703,  709,  708,
 /*  2290 */   702, 1104,  329,  698,  706,  707,  710,  703,  709,  708,
 /*  2300 */   702, 1104,  328,  698,  706,  707,  710,  703,  709,  708,
 /*  2310 */   702, 1104,  327,  698,  706,  707,  710,  703,  709,  708,
 /*  2320 */   702, 1104,  326, 1104, 1104, 1104,  698,  706,  707,  710,
 /*  2330 */   703,  709,  708,  702, 1104,  325,  698,  706,  707,  710,
 /*  2340 */   703,  709,  708,  702, 1104,  324,  698,  706,  707,  710,
 /*  2350 */   703,  709,  708,  702, 1104,  323,  698,  706,  707,  710,
 /*  2360 */   703,  709,  708,  702, 1104,  322, 1104,  698,  706,  707,
 /*  2370 */   710,  703,  709,  708,  702, 1104,  321,  698,  706,  707,
 /*  2380 */   710,  703,  709,  708,  702, 1104,  317,  698,  706,  707,
 /*  2390 */   710,  703,  709,  708,  702, 1104,  316,  698,  706,  707,
 /*  2400 */   710,  703,  709,  708,  702, 1104,  315,  698,  706,  707,
 /*  2410 */   710,  703,  709,  708,  702, 1104,  314,  698,  706,  707,
 /*  2420 */   710,  703,  709,  708,  702, 1104,  312,  698,  706,  707,
 /*  2430 */   710,  703,  709,  708,  702, 1104,  311,  698,  706,  707,
 /*  2440 */   710,  703,  709,  708,  702, 1104,  310,  698,  706,  707,
 /*  2450 */   710,  703,  709,  708,  702, 1104,  309,  698,  706,  707,
 /*  2460 */   710,  703,  709,  708,  702, 1104,  308,  698,  706,  707,
 /*  2470 */   710,  703,  709,  708,  702, 1104,  183, 1104, 1104, 1104,
 /*  2480 */   698,  706,  707,  710,  703,  709,  708,  702, 1104,  182,
 /*  2490 */   664,  658,  710,  703,  709,  708,  702, 1104, 1104, 1104,
 /*  2500 */  1104,  295,  664,  658,  710,  703,  709,  708,  702, 1104,
 /*  2510 */  1104, 1104, 1104,  359,  664,  658,  710,  703,  709,  708,
 /*  2520 */   702, 1104, 1104, 1104, 1104,  657,  664,  658,  710,  703,
 /*  2530 */   709,  708,  702, 1104, 1104, 1104, 1104,  653,  664,  658,
 /*  2540 */   710,  703,  709,  708,  702, 1104, 1104, 1104, 1104,  656,
 /*  2550 */  1104,  664,  658,  710,  703,  709,  708,  702, 1104, 1104,
 /*  2560 */  1104, 1104,  655,  664,  658,  710,  703,  709,  708,  702,
 /*  2570 */  1104, 1104, 1104, 1104,  654,  664,  658,  710,  703,  709,
 /*  2580 */   708,  702, 1104, 1104, 1104, 1104,  318,  664,  658,  710,
 /*  2590 */   703,  709,  708,  702, 1104, 1104, 1104, 1104,  355,  664,
 /*  2600 */   658,  710,  703,  709,  708,  702, 1104, 1104, 1104, 1104,
 /*  2610 */   354,  664,  658,  710,  703,  709,  708,  702, 1104, 1104,
 /*  2620 */  1104, 1104,  616,  664,  658,  710,  703,  709,  708,  702,
 /*  2630 */  1104, 1104, 1104, 1104,  615,  664,  658,  710,  703,  709,
 /*  2640 */   708,  702, 1104, 1104, 1104, 1104,  614,  664,  658,  710,
 /*  2650 */   703,  709,  708,  702, 1104, 1104, 1104, 1104,  307,  664,
 /*  2660 */   658,  710,  703,  709,  708,  702, 1104, 1104, 1104, 1104,
 /*  2670 */   306,  664,  658,  710,  703,  709,  708,  702, 1104, 1104,
 /*  2680 */  1104, 1104,  305,  664,  658,  710,  703,  709,  708,  702,
 /*  2690 */   584,  568,  584,  370,  304,  664,  658,  710,  703,  709,
 /*  2700 */   708,  702, 1104, 1104, 1104, 1104,  303,  664,  658,  710,
 /*  2710 */   703,  709,  708,  702, 1104, 1104, 1104, 1104,  302,  664,
 /*  2720 */   658,  710,  703,  709,  708,  702, 1104, 1104, 1104, 1104,
 /*  2730 */   301,  664,  658,  710,  703,  709,  708,  702, 1104, 1104,
 /*  2740 */  1104, 1104,  608,  664,  658,  710,  703,  709,  708,  702,
 /*  2750 */  1104, 1104, 1104, 1104,  283, 1104, 1104, 1104, 1104, 1104,
 /*  2760 */   192, 1104,  192,  192, 1104, 1104, 1104, 1104, 1104, 1104,
 /*  2770 */  1104, 1104, 1104, 1104, 1104, 1104, 1104, 1104, 1104, 1104,
 /*  2780 */  1104, 1104, 1104, 1104, 1104, 1104, 1104, 1104, 1104, 1104,
 /*  2790 */  1104, 1104, 1104, 1104,  190, 1104,  190,  190, 1104,  188,
 /*  2800 */  1104,  188,  188,
};
static const YYCODETYPE yy_lookahead[] = {
 /*     0 */   135,   72,    0,  138,  139,  140,  141,  142,  143,  144,
 /*    10 */   145,  146,  147,  148,  149,  150,  151,  152,  155,  154,
 /*    20 */   155,  156,  157,  158,  159,  160,  107,  108,  109,  166,
 /*    30 */    69,   71,  167,  168,  105,  106,  107,  108,  109,  174,
 /*    40 */   175,   80,   81,   82,   83,   73,   85,   86,   87,   88,
 /*    50 */   107,  108,  109,  188,  189,  190,  170,  171,  154,  155,
 /*    60 */   156,  157,  158,  159,  160,   93,  105,  106,  107,  108,
 /*    70 */   109,  167,  168,  107,  108,  109,   71,  173,  174,  175,
 /*    80 */   176,  177,  196,  197,  198,  199,   76,  183,   71,  203,
 /*    90 */   204,  226,  227,  228,  229,  230,  231,  232,  233,  234,
 /*   100 */   235,  236,  237,  238,  239,  240,  241,  242,  243,    1,
 /*   110 */     2,   72,  102,    5,    6,    7,    8,    9,   10,   11,
 /*   120 */    12,  217,  218,  219,  153,  154,  155,  156,  157,  158,
 /*   130 */   159,  160,  161,  162,   33,   71,   28,   36,   71,   67,
 /*   140 */    32,  102,   34,   35,   43,   37,   81,   39,   47,    1,
 /*   150 */     2,   98,   44,    5,    6,  102,   48,   49,   50,   51,
 /*   160 */    52,   53,   54,   96,   56,   57,   94,   95,   96,   97,
 /*   170 */    98,   63,   64,  101,   98,   72,   68,   69,  102,   71,
 /*   180 */   107,    1,    2,   75,  111,    5,    6,  154,  155,  156,
 /*   190 */   157,  158,  159,  160,   46,   70,   48,   49,   50,   72,
 /*   200 */   167,  168,   99,  100,  155,  172,  173,  174,  175,  176,
 /*   210 */   177,   63,   64,  105,   66,  166,   72,   69,  110,   71,
 /*   220 */    71,   97,   98,   75,  175,  101,  118,  178,   48,   49,
 /*   230 */    50,    2,  105,  106,  107,  108,  109,   70,  130,  131,
 /*   240 */   132,   70,  134,   63,   64,    2,   66,    1,    2,   69,
 /*   250 */   106,   71,  104,  105,   73,   75,  170,  171,  110,   70,
 /*   260 */     1,    2,   97,   98,    5,    6,  101,  163,  154,  155,
 /*   270 */   156,  157,  158,  159,  160,  200,  201,  202,  130,  131,
 /*   280 */   132,  167,  168,  102,  104,  105,  182,   72,  174,  175,
 /*   290 */   110,  101,    1,    2,  208,  154,  155,  156,  157,  158,
 /*   300 */   159,  160,  188,  189,  190,  164,  165,   48,   49,   50,
 /*   310 */   130,  131,  132,   22,   23,   24,   25,  102,   75,  215,
 /*   320 */   216,   30,   63,   64,   71,   66,   72,  101,   69,   38,
 /*   330 */    71,   40,    1,    2,   75,   72,   45,  209,  210,  211,
 /*   340 */   154,  155,  156,  157,  158,  159,  160,    2,   57,   58,
 /*   350 */    59,   60,   81,  167,  168,   81,  102,  170,  171,  130,
 /*   360 */   174,  175,   71,  104,  105,  102,    1,    2,  163,  110,
 /*   370 */     5,    6,   72,  130,  188,  189,  190,  153,  154,  155,
 /*   380 */   156,  157,  158,  159,  160,  161,  162,  182,   72,  130,
 /*   390 */   131,  132,  205,  206,  207,  208,  105,   95,   96,   97,
 /*   400 */    98,  110,  102,  101,  154,  155,  156,  157,  158,  159,
 /*   410 */   160,   90,   91,   48,   49,   50,   71,  167,  168,   98,
 /*   420 */   215,  216,  106,  173,  174,  175,  176,  177,   63,   64,
 /*   430 */   212,  213,  214,  183,   69,  163,   71,   73,   76,   31,
 /*   440 */    75,   72,  153,  154,  155,  156,  157,  158,  159,  160,
 /*   450 */   161,  162,   97,   98,  182,   14,  101,   16,   17,   18,
 /*   460 */    19,   20,   21,  132,  102,   41,   99,  100,  218,  219,
 /*   470 */   105,  102,    1,    2,   73,  110,    5,    6,  191,  192,
 /*   480 */   154,  155,  156,  157,  158,  159,  160,  158,  216,   48,
 /*   490 */    49,   31,  163,  167,  168,  130,  131,  132,  172,  173,
 /*   500 */   174,  175,  176,  177,  154,  155,  156,  157,  158,  159,
 /*   510 */   160,  182,  186,  187,   96,  186,  187,  167,  168,   48,
 /*   520 */    49,   50,  172,  173,  174,  175,  176,  177,   73,  105,
 /*   530 */   106,  107,  108,  109,   63,   64,  186,  187,   95,   96,
 /*   540 */    97,   98,   71,   73,  101,   73,   69,   74,   93,   72,
 /*   550 */    72,    1,    2,   74,   69,    5,    6,   80,   81,   82,
 /*   560 */    83,   71,   85,   86,   87,   88,   81,   82,   83,  163,
 /*   570 */    85,   86,   87,   88,  133,  104,  105,  155,   41,   55,
 /*   580 */   102,  110,  105,  106,  107,  108,  109,   73,  182,   72,
 /*   590 */   105,  106,  107,  108,  109,   73,   76,  175,   48,   49,
 /*   600 */    50,  130,  131,  132,  154,  155,  156,  157,  158,  159,
 /*   610 */   160,  189,  106,   63,   64,   93,  102,  167,  168,  102,
 /*   620 */    73,   71,  102,  154,  155,  156,  157,  158,  159,  160,
 /*   630 */     2,   94,   95,   96,   97,   98,  167,  168,  101,   76,
 /*   640 */    93,  172,  173,  174,  175,  176,  177,    2,  154,  155,
 /*   650 */   156,  157,  158,  159,  160,  105,    2,   72,   75,   73,
 /*   660 */   110,  167,  168,   71,   73,  102,  172,  173,  174,  175,
 /*   670 */   176,  177,  154,  155,  156,  157,  158,  159,  160,   93,
 /*   680 */   130,  131,  132,   72,   93,  167,  168,  102,  105,   62,
 /*   690 */   172,  173,  174,  175,  176,  177,   71,  154,  155,  156,
 /*   700 */   157,  158,  159,  160,   71,   94,   95,   96,   97,   98,
 /*   710 */   167,  168,  101,  130,   70,  172,  173,  174,  175,  176,
 /*   720 */   177,   94,   95,   96,   97,   98,   73,  155,  101,   71,
 /*   730 */   154,  155,  156,  157,  158,  159,  160,   71,   94,   95,
 /*   740 */    96,   97,   98,  167,  168,  101,   93,  175,  172,  173,
 /*   750 */   174,  175,  176,  177,  154,  155,  156,  157,  158,  159,
 /*   760 */   160,  189,   70,   95,   96,   97,   98,  167,  168,  101,
 /*   770 */     3,    4,  172,  173,  174,  175,  176,  177,  154,  155,
 /*   780 */   156,  157,  158,  159,  160,   72,   94,   95,   96,   97,
 /*   790 */    98,  167,  168,  101,  201,  202,  172,  173,  174,  175,
 /*   800 */   176,  177,  102,  154,  155,  156,  157,  158,  159,  160,
 /*   810 */     2,   70,   75,   41,   72,  102,  167,  168,  210,  211,
 /*   820 */    73,  172,  173,  174,  175,  176,  177,  154,  155,  156,
 /*   830 */   157,  158,  159,  160,   73,   94,   95,   96,   97,   98,
 /*   840 */   167,  168,  101,   89,  102,  172,  173,  174,  175,  176,
 /*   850 */   177,   79,  154,  155,  156,  157,  158,  159,  160,  105,
 /*   860 */   106,  107,  108,  109,   81,  167,  168,  130,  131,   70,
 /*   870 */   172,  173,  174,  175,  176,  177,   94,   95,   96,   97,
 /*   880 */    98,   81,  163,  101,   72,  154,  155,  156,  157,  158,
 /*   890 */   159,  160,  101,   94,   95,   96,   97,   98,  167,  168,
 /*   900 */   101,  182,   75,  172,  173,  174,  175,  176,  177,  154,
 /*   910 */   155,  156,  157,  158,  159,  160,   76,  105,  106,  107,
 /*   920 */   108,  109,  167,  168,   76,   26,   73,  172,  173,  174,
 /*   930 */   175,  176,  177,  154,  155,  156,  157,  158,  159,  160,
 /*   940 */    41,   42,  184,  185,   90,   91,  167,  168,   73,  193,
 /*   950 */   194,   73,  173,  174,  175,  176,  177,  130,  213,  214,
 /*   960 */    61,   62,  183,  154,  155,  156,  157,  158,  159,  160,
 /*   970 */   213,  214,   73,  213,  214,   73,  167,  168,   79,  213,
 /*   980 */   214,   72,  173,  174,  175,  176,  177,   73,  170,  171,
 /*   990 */    73,   73,  183,  136,  137,  154,  155,  156,  157,  158,
 /*  1000 */   159,  160,   73,   94,   95,   96,   97,   98,  167,  168,
 /*  1010 */   101,   72,   65,   73,  173,  174,  175,  176,  177,  105,
 /*  1020 */   106,  107,  108,  109,  183,  154,  155,  156,  157,  158,
 /*  1030 */   159,  160,   73,   94,   95,   96,   97,   98,  167,  168,
 /*  1040 */   101,   73,   73,   73,  173,  174,  175,  176,  177,   94,
 /*  1050 */    95,   96,   97,   98,  183,   73,  101,   73,  154,  155,
 /*  1060 */   156,  157,  158,  159,  160,   73,  105,  106,  107,  108,
 /*  1070 */   109,  167,  168,  163,   26,  130,  155,  173,  174,  175,
 /*  1080 */   176,  177,  105,  106,  107,  108,  109,  183,  154,  155,
 /*  1090 */   156,  157,  158,  159,  160,  158,   94,   95,   96,   97,
 /*  1100 */    98,  167,  168,  101,  131,  130,  221,  173,  174,  175,
 /*  1110 */   176,  177,  105,  106,  107,  108,  109,  183,  154,  155,
 /*  1120 */   156,  157,  158,  159,  160,  105,  106,  107,  108,  109,
 /*  1130 */   158,  167,  168,  170,  171,   41,  158,  173,  174,  175,
 /*  1140 */   176,  177,   94,   95,   96,   97,   98,  183,  224,  101,
 /*  1150 */   154,  155,  156,  157,  158,  159,  160,   27,  195,  196,
 /*  1160 */   197,  198,  199,  167,  168,   61,  225,   62,  155,  173,
 /*  1170 */   174,  175,  176,  177,  105,  106,  107,  108,  109,  183,
 /*  1180 */   154,  155,  156,  157,  158,  159,  160,  225,  155,  224,
 /*  1190 */   224,  192,  225,  167,  168,  170,  171,  221,  224,  173,
 /*  1200 */   174,  175,  176,  177,   72,   75,  225,    2,  199,  183,
 /*  1210 */    27,    2,  223,  154,  155,  156,  157,  158,  159,  160,
 /*  1220 */   195,  196,  197,  198,  199,  222,  167,  168,  199,   42,
 /*  1230 */    26,  225,  173,  174,  175,  176,  177,  105,  106,  107,
 /*  1240 */   108,  109,  183,  154,  155,  156,  157,  158,  159,  160,
 /*  1250 */   223,  170,  171,  221,  224,  224,  167,  168,   75,  222,
 /*  1260 */   130,  172,  173,  174,  175,  176,  177,  225,  221,  223,
 /*  1270 */   225,  222,  154,  155,  156,  157,  158,  159,  160,  170,
 /*  1280 */   171,  224,  224,  155,  224,  167,  168,  206,  207,  208,
 /*  1290 */   172,  173,  174,  175,  176,  177,  154,  155,  156,  157,
 /*  1300 */   158,  159,  160,  225,  195,  196,  197,  198,  199,  167,
 /*  1310 */   168,  225,  224,  130,  172,  173,  174,  175,  176,  177,
 /*  1320 */   225,  224,  221,  225,  154,  155,  156,  157,  158,  159,
 /*  1330 */   160,  170,  171,  223,  222,  224,  155,  167,  168,  225,
 /*  1340 */   221,  224,  172,  173,  174,  175,  176,  177,  154,  155,
 /*  1350 */   156,  157,  158,  159,  160,  222,  195,  196,  197,  198,
 /*  1360 */   199,  167,  168,  223,  225,  221,  172,  173,  174,  175,
 /*  1370 */   176,  177,  154,  155,  156,  157,  158,  159,  160,  155,
 /*  1380 */   170,  171,  223,  225,  222,  167,  168,  224,  224,  194,
 /*  1390 */   172,  173,  174,  175,  176,  177,  225,  154,  155,  156,
 /*  1400 */   157,  158,  159,  160,  170,  171,  196,  197,  198,  199,
 /*  1410 */   167,  168,  155,  225,  221,  172,  173,  174,  175,  176,
 /*  1420 */   177,  154,  155,  156,  157,  158,  159,  160,  157,  185,
 /*  1430 */   196,  197,  198,  199,  167,  168,  171,  171,  221,  172,
 /*  1440 */   173,  174,  175,  176,  177,   69,  171,  171,   72,  154,
 /*  1450 */   155,  156,  157,  158,  159,  160,  171,   81,   82,   83,
 /*  1460 */   165,   85,   86,   87,   88,  154,  155,  156,  157,  158,
 /*  1470 */   159,  160,  171,  154,  155,  156,  157,  158,  159,  160,
 /*  1480 */   169,  105,  106,  107,  108,  109,  167,  168,  171,   73,
 /*  1490 */   179,  180,  181,  174,  175,  171,  171,  154,  155,  156,
 /*  1500 */   157,  158,  159,  160,  171,  171,  225,  188,  189,  190,
 /*  1510 */   167,  168,  221,    2,  222,  222,  222,  174,  175,  222,
 /*  1520 */   221,  105,  106,  107,  108,  109,    2,   69,  244,  244,
 /*  1530 */   244,  188,  189,  190,  244,  244,  244,  244,  244,   81,
 /*  1540 */    82,   83,  244,   85,   86,   87,   88,  153,  154,  155,
 /*  1550 */   156,  157,  158,  159,  160,  161,  162,  244,  244,   72,
 /*  1560 */   244,  244,  244,  105,  106,  107,  108,  109,   81,   82,
 /*  1570 */    83,  244,   85,   86,   87,   88,    1,    2,  244,  244,
 /*  1580 */     5,    6,   81,   82,   83,  244,   85,   86,   87,   88,
 /*  1590 */   244,  244,  105,  106,  107,  108,  109,  244,  154,  155,
 /*  1600 */   156,  157,  158,  159,  160,  244,  105,  106,  107,  108,
 /*  1610 */   109,  167,  168,   81,   82,   83,  244,   85,   86,   87,
 /*  1620 */    88,  244,  244,   48,   49,   50,   81,   82,   83,  244,
 /*  1630 */    85,   86,   87,   88,  244,  244,  244,  105,  106,  107,
 /*  1640 */   108,  109,  244,  244,  244,  244,   71,  244,  244,  244,
 /*  1650 */   105,  106,  107,  108,  109,    1,    2,  244,  244,    5,
 /*  1660 */     6,  244,   81,   82,   83,  244,   85,   86,   87,   88,
 /*  1670 */   244,  244,  153,  154,  155,  156,  157,  158,  159,  160,
 /*  1680 */   105,  162,  244,  244,  244,  110,  105,  106,  107,  108,
 /*  1690 */   109,    1,    2,  244,  154,  155,  156,  157,  158,  159,
 /*  1700 */   160,  244,   73,  244,  244,  130,  131,  132,  244,  169,
 /*  1710 */   154,  155,  156,  157,  158,  159,  160,   63,   64,  179,
 /*  1720 */   180,  181,  244,  167,  168,   71,  244,  154,  155,  156,
 /*  1730 */   157,  158,  159,  160,  105,  106,  107,  108,  109,  244,
 /*  1740 */   244,  244,  169,  154,  155,  156,  157,  158,  159,  160,
 /*  1750 */   244,  244,  179,  180,  181,  244,  244,  244,  169,  105,
 /*  1760 */   244,   71,  244,  244,  110,  244,  244,  244,  179,  180,
 /*  1770 */   181,  244,  154,  155,  156,  157,  158,  159,  160,  244,
 /*  1780 */   244,  244,  244,  244,  130,  131,  132,  169,  154,  155,
 /*  1790 */   156,  157,  158,  159,  160,  105,  244,  179,  180,  181,
 /*  1800 */   110,  244,  244,  169,  244,  154,  155,  156,  157,  158,
 /*  1810 */   159,  160,  244,  179,  180,  181,  244,  244,  244,  244,
 /*  1820 */   169,  154,  155,  156,  157,  158,  159,  160,  244,  244,
 /*  1830 */   179,  180,  181,  170,  171,  244,  169,  244,  244,  244,
 /*  1840 */   244,  244,  244,  244,  244,  244,  179,  180,  181,  154,
 /*  1850 */   155,  156,  157,  158,  159,  160,  244,  244,  244,  196,
 /*  1860 */   197,  198,  199,  244,  169,  244,  154,  155,  156,  157,
 /*  1870 */   158,  159,  160,  158,  179,  180,  181,  165,  163,  244,
 /*  1880 */   154,  155,  156,  157,  158,  159,  160,  153,  154,  155,
 /*  1890 */   156,  157,  158,  159,  160,  169,  162,  182,  244,  244,
 /*  1900 */   244,  186,  187,  244,  244,  179,  180,  181,  153,  154,
 /*  1910 */   155,  156,  157,  158,  159,  160,  161,  162,  153,  154,
 /*  1920 */   155,  156,  157,  158,  159,  160,  244,  162,  153,  154,
 /*  1930 */   155,  156,  157,  158,  159,  160,  244,  162,  153,  154,
 /*  1940 */   155,  156,  157,  158,  159,  160,  244,  162,  153,  154,
 /*  1950 */   155,  156,  157,  158,  159,  160,  244,  162,  244,  153,
 /*  1960 */   154,  155,  156,  157,  158,  159,  160,  244,  162,  153,
 /*  1970 */   154,  155,  156,  157,  158,  159,  160,  244,  162,  153,
 /*  1980 */   154,  155,  156,  157,  158,  159,  160,  244,  162,  244,
 /*  1990 */   153,  154,  155,  156,  157,  158,  159,  160,  244,  162,
 /*  2000 */   153,  154,  155,  156,  157,  158,  159,  160,  244,  162,
 /*  2010 */   244,  244,  244,  244,  244,  244,  244,  244,  153,  154,
 /*  2020 */   155,  156,  157,  158,  159,  160,  244,  162,  153,  154,
 /*  2030 */   155,  156,  157,  158,  159,  160,  244,  162,  153,  154,
 /*  2040 */   155,  156,  157,  158,  159,  160,  244,  162,  153,  154,
 /*  2050 */   155,  156,  157,  158,  159,  160,  244,  162,  244,  153,
 /*  2060 */   154,  155,  156,  157,  158,  159,  160,  244,  162,  153,
 /*  2070 */   154,  155,  156,  157,  158,  159,  160,  244,  162,  153,
 /*  2080 */   154,  155,  156,  157,  158,  159,  160,  244,  162,  153,
 /*  2090 */   154,  155,  156,  157,  158,  159,  160,  244,  162,  153,
 /*  2100 */   154,  155,  156,  157,  158,  159,  160,  244,  162,  153,
 /*  2110 */   154,  155,  156,  157,  158,  159,  160,  244,  162,  153,
 /*  2120 */   154,  155,  156,  157,  158,  159,  160,  244,  162,  153,
 /*  2130 */   154,  155,  156,  157,  158,  159,  160,  244,  162,  153,
 /*  2140 */   154,  155,  156,  157,  158,  159,  160,  244,  162,  153,
 /*  2150 */   154,  155,  156,  157,  158,  159,  160,  244,  162,  153,
 /*  2160 */   154,  155,  156,  157,  158,  159,  160,  244,  162,  244,
 /*  2170 */   244,  244,  153,  154,  155,  156,  157,  158,  159,  160,
 /*  2180 */   244,  162,  153,  154,  155,  156,  157,  158,  159,  160,
 /*  2190 */   244,  162,  153,  154,  155,  156,  157,  158,  159,  160,
 /*  2200 */   244,  162,  153,  154,  155,  156,  157,  158,  159,  160,
 /*  2210 */   244,  162,  244,  153,  154,  155,  156,  157,  158,  159,
 /*  2220 */   160,  244,  162,  153,  154,  155,  156,  157,  158,  159,
 /*  2230 */   160,  244,  162,  153,  154,  155,  156,  157,  158,  159,
 /*  2240 */   160,  244,  162,  153,  154,  155,  156,  157,  158,  159,
 /*  2250 */   160,  244,  162,  153,  154,  155,  156,  157,  158,  159,
 /*  2260 */   160,  244,  162,  153,  154,  155,  156,  157,  158,  159,
 /*  2270 */   160,  244,  162,  153,  154,  155,  156,  157,  158,  159,
 /*  2280 */   160,  244,  162,  153,  154,  155,  156,  157,  158,  159,
 /*  2290 */   160,  244,  162,  153,  154,  155,  156,  157,  158,  159,
 /*  2300 */   160,  244,  162,  153,  154,  155,  156,  157,  158,  159,
 /*  2310 */   160,  244,  162,  153,  154,  155,  156,  157,  158,  159,
 /*  2320 */   160,  244,  162,  244,  244,  244,  153,  154,  155,  156,
 /*  2330 */   157,  158,  159,  160,  244,  162,  153,  154,  155,  156,
 /*  2340 */   157,  158,  159,  160,  244,  162,  153,  154,  155,  156,
 /*  2350 */   157,  158,  159,  160,  244,  162,  153,  154,  155,  156,
 /*  2360 */   157,  158,  159,  160,  244,  162,  244,  153,  154,  155,
 /*  2370 */   156,  157,  158,  159,  160,  244,  162,  153,  154,  155,
 /*  2380 */   156,  157,  158,  159,  160,  244,  162,  153,  154,  155,
 /*  2390 */   156,  157,  158,  159,  160,  244,  162,  153,  154,  155,
 /*  2400 */   156,  157,  158,  159,  160,  244,  162,  153,  154,  155,
 /*  2410 */   156,  157,  158,  159,  160,  244,  162,  153,  154,  155,
 /*  2420 */   156,  157,  158,  159,  160,  244,  162,  153,  154,  155,
 /*  2430 */   156,  157,  158,  159,  160,  244,  162,  153,  154,  155,
 /*  2440 */   156,  157,  158,  159,  160,  244,  162,  153,  154,  155,
 /*  2450 */   156,  157,  158,  159,  160,  244,  162,  153,  154,  155,
 /*  2460 */   156,  157,  158,  159,  160,  244,  162,  153,  154,  155,
 /*  2470 */   156,  157,  158,  159,  160,  244,  162,  244,  244,  244,
 /*  2480 */   153,  154,  155,  156,  157,  158,  159,  160,  244,  162,
 /*  2490 */   154,  155,  156,  157,  158,  159,  160,  244,  244,  244,
 /*  2500 */   244,  165,  154,  155,  156,  157,  158,  159,  160,  244,
 /*  2510 */   244,  244,  244,  165,  154,  155,  156,  157,  158,  159,
 /*  2520 */   160,  244,  244,  244,  244,  165,  154,  155,  156,  157,
 /*  2530 */   158,  159,  160,  244,  244,  244,  244,  165,  154,  155,
 /*  2540 */   156,  157,  158,  159,  160,  244,  244,  244,  244,  165,
 /*  2550 */   244,  154,  155,  156,  157,  158,  159,  160,  244,  244,
 /*  2560 */   244,  244,  165,  154,  155,  156,  157,  158,  159,  160,
 /*  2570 */   244,  244,  244,  244,  165,  154,  155,  156,  157,  158,
 /*  2580 */   159,  160,  244,  244,  244,  244,  165,  154,  155,  156,
 /*  2590 */   157,  158,  159,  160,  244,  244,  244,  244,  165,  154,
 /*  2600 */   155,  156,  157,  158,  159,  160,  244,  244,  244,  244,
 /*  2610 */   165,  154,  155,  156,  157,  158,  159,  160,  244,  244,
 /*  2620 */   244,  244,  165,  154,  155,  156,  157,  158,  159,  160,
 /*  2630 */   244,  244,  244,  244,  165,  154,  155,  156,  157,  158,
 /*  2640 */   159,  160,  244,  244,  244,  244,  165,  154,  155,  156,
 /*  2650 */   157,  158,  159,  160,  244,  244,  244,  244,  165,  154,
 /*  2660 */   155,  156,  157,  158,  159,  160,  244,  244,  244,  244,
 /*  2670 */   165,  154,  155,  156,  157,  158,  159,  160,  244,  244,
 /*  2680 */   244,  244,  165,  154,  155,  156,  157,  158,  159,  160,
 /*  2690 */     1,    2,    1,    1,  165,  154,  155,  156,  157,  158,
 /*  2700 */   159,  160,  244,  244,  244,  244,  165,  154,  155,  156,
 /*  2710 */   157,  158,  159,  160,  244,  244,  244,  244,  165,  154,
 /*  2720 */   155,  156,  157,  158,  159,  160,  244,  244,  244,  244,
 /*  2730 */   165,  154,  155,  156,  157,  158,  159,  160,  244,  244,
 /*  2740 */   244,  244,  165,  154,  155,  156,  157,  158,  159,  160,
 /*  2750 */   244,  244,  244,  244,  165,  244,  244,  244,  244,  244,
 /*  2760 */    71,  244,   71,   71,  244,  244,  244,  244,  244,  244,
 /*  2770 */   244,  244,  244,  244,  244,  244,  244,  244,  244,  244,
 /*  2780 */   244,  244,  244,  244,  244,  244,  244,  244,  244,  244,
 /*  2790 */   244,  244,  244,  244,  105,  244,  105,  105,  244,  110,
 /*  2800 */   244,  110,  110,
};
#define YY_SHIFT_USE_DFLT (-82)
#define YY_SHIFT_COUNT (468)
#define YY_SHIFT_MIN   (-81)
#define YY_SHIFT_MAX   (2692)
static const short yy_shift_ofst[] = {
 /*     0 */   -82,  108,  148,  148,  180,  180,  180,  180,  180,  180,
 /*    10 */   180,  180,  180,  180,  259,  259,  259,  259,  259,  259,
 /*    20 */   259,  259,  259,  259,  180,  180,  180,  180,  180,  180,
 /*    30 */   180,  180,  180,  180,  180,  180,  180,  365,  365,  365,
 /*    40 */   365,  471,  471,  471,  471,  471,  471,  471,  471,  471,
 /*    50 */   471,  550,  550,  550,  550,  550,  550,  550,  550,  550,
 /*    60 */   550,  550,  550,  550,  550,  550,  550,  550,  550,  550,
 /*    70 */   550,  550,  550,  550,  550,  550,  550,  550,  550,  550,
 /*    80 */   550,  550,  550,  550,  550,  550,  550,  550,  550,  550,
 /*    90 */   550,  550,  550,  550,  550,  550,  550,  550,  550,  550,
 /*   100 */   550,  550,  550,  550,  550,  550,  550,  550,  550,  550,
 /*   110 */   550,  550,  550, 1575, 1575, 1575, 1654,  291, 1654, 1654,
 /*   120 */  1654, 1654, 1654, 1654, 1654, 1654, 1654, 1654, 1654, 1654,
 /*   130 */  1654, 1654, 1654, 1654, 1654, 1654, 1654, 1654, 1654, 1654,
 /*   140 */  1654, 1654, 2689, 2689, 2689, 2689, 1690, 2689, 2689, 2689,
 /*   150 */  1690,  737,  737, 1183, 1130,  243, 1690,  583,  583,  827,
 /*   160 */   345, 1524, 1511, 2692,  345,  345,  345,  345,  767,  229,
 /*   170 */   854,  827,  827, 1524, 1511, 1205,  537, 1048, 1048, 1048,
 /*   180 */  1048,  627,  424,  424, 2691, 2691, 2691, 2691, 2691, 2691,
 /*   190 */  2691, 2691, 2691, 2691, 2691,  331,  321,  772,  767, 1105,
 /*   200 */   975, 1105, 1104, 1105, 1104, 1204, 1187, 1094,  975, 1105,
 /*   210 */  1104, 1204, 1187, 1094,  975, 1105, 1104, 1204, 1187, 1094,
 /*   220 */   975, 1105, 1104, 1105, 1104, 1105, 1104, 1105, 1104, 1105,
 /*   230 */  1104, 1204, 1187, 1094, 1105, 1104, 1204, 1187, 1094, 1105,
 /*   240 */  1104, 1204, 1187, 1209, 1209, 1205, 1105, 1104, 1094, 1105,
 /*   250 */  1104,  975, 1105, 1104,  975, 1105, 1104,  973,  973, 1094,
 /*   260 */   975,  973,  945,  477,  -39, 1376, 1487, 1458,  485, 1581,
 /*   270 */  1545, 1532, 1501,  441,  939,  909,  611,  799,  741,  692,
 /*   280 */   644,   72,  899, 1629,  782,  782, 1002, 1002, 1002, 1416,
 /*   290 */   914, 1132,  754,  955,  782,  812,  127,  -71,  668, 1069,
 /*   300 */   443, 1020, 1020, 1020, 1020, 1020, 1020, 1020,  961,  961,
 /*   310 */   961,  961,  961,  302,  961,  961,  961,  961, 1020, 1020,
 /*   320 */  1020,  961,  961,  961,  961,  961,  961,  961,  961,  961,
 /*   330 */   961,  961,  961,  961,  961,  961,  961,  961,  961,  961,
 /*   340 */   961,  961, 1007,  977,  961,  961,  101,  355,  355,  103,
 /*   350 */   -34,  -34,  165,  165,  -57,  -57,  124,  124,  -57,  -57,
 /*   360 */   -81,  -81,  -81,  -81,  -81,  -81,  -81,  -81,  246,  653,
 /*   370 */   591,  514,  181,  367,  367,  367,  367,  586,  563,  547,
 /*   380 */   742,  520,  522,  362,  713,  585,  316,  517,  144,   73,
 /*   390 */    10,  455,  478,   67,  -28,  369,   76,   53,  300,  263,
 /*   400 */   254,  215,   39,  947,  992,  984,  982,  970,  969,  968,
 /*   410 */   959,  940,  929,  918,  917,  902,  878,  875,  853,  848,
 /*   420 */   840,  791,  791,  800,  783,  761,  747,  808,  700,  654,
 /*   430 */   666,  658,  633,  645,  625,  506,  506,  592,  628,  524,
 /*   440 */   490,  506,  479,  473,  418,  472,  470,  460,  401,  408,
 /*   450 */   364,  226,  226,  274,  271,  253,  190,  190,  189,  171,
 /*   460 */   167,  125,   65,  149,   64,   17,    5,  -40,    2,
};
#define YY_REDUCE_USE_DFLT (-138)
#define YY_REDUCE_COUNT (262)
#define YY_REDUCE_MIN   (-137)
#define YY_REDUCE_MAX   (2589)
static const short yy_reduce_ofst[] = {
 /*     0 */   857, -135,  -96,  250,  350,  326, 1267, 1243, 1218, 1194,
 /*    10 */  1170, 1142, 1118, 1089, 1059, 1026,  996,  964,  934,  904,
 /*    20 */   871,  841,  809,  779,  755,  731,  698,  673,  649,  624,
 /*    30 */   600,  576,  543,  518,  494,  469,   33, 1343, 1319,  186,
 /*    40 */   114, 1726, 1695, 1667, 1651, 1634, 1618, 1589, 1573, 1540,
 /*    50 */  1311, 1755, 1394,  289,  224,  -29, 2327, 2314, 2304, 2294,
 /*    60 */  2284, 2274, 2264, 2254, 2244, 2234, 2224, 2214, 2203, 2193,
 /*    70 */  2183, 2173, 2160, 2150, 2140, 2130, 2120, 2110, 2100, 2090,
 /*    80 */  2080, 2070, 2060, 2049, 2039, 2029, 2019, 2006, 1996, 1986,
 /*    90 */  1976, 1966, 1956, 1946, 1936, 1926, 1916, 1906, 1895, 1885,
 /*   100 */  1875, 1865, 1847, 1837, 1826, 1816, 1806, 1795, 1785, 1775,
 /*   110 */  1765, 1734, 1519, 1556, 1444,  450,  141, -114, 2589, 2577,
 /*   120 */  2565, 2553, 2541, 2529, 2517, 2505, 2493, 2481, 2469, 2457,
 /*   130 */  2445, 2433, 2421, 2409, 2397, 2384, 2372, 2360, 2348, 2336,
 /*   140 */  1712, 1295, 1161, 1109, 1025,  963,  187, 1663, 1234, 1210,
 /*   150 */  1081, 1715,  329,  205,  104,   49,   86,  572,  422,  272,
 /*   160 */   218,  128,   75,  818,  766,  760,  757,  745,  756, -137,
 /*   170 */   758,  719,  406,  608,  593,  287, 1299, 1297, 1294, 1293,
 /*   180 */  1292, 1281, 1291, 1217, 1334, 1333, 1325, 1324, 1317, 1301,
 /*   190 */  1285, 1276, 1275, 1266, 1265, 1271, 1244, 1193, 1195, 1188,
 /*   200 */  1257, 1171, 1164, 1158, 1163, 1162, 1159, 1144, 1224, 1139,
 /*   210 */  1117, 1133, 1140, 1119, 1181, 1114, 1111, 1112, 1110, 1101,
 /*   220 */  1128, 1098, 1097, 1095, 1088, 1086, 1060, 1078, 1058, 1045,
 /*   230 */  1057, 1049, 1046, 1047, 1042, 1031, 1037, 1027, 1032, 1006,
 /*   240 */  1030, 1003,  989, 1029, 1009,  999,  981,  974,  976,  967,
 /*   250 */   966, 1033,  962,  965, 1013,  941,  924,  978,  972,  885,
 /*   260 */   921,  937,  910,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */   728, 1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102,
 /*    10 */  1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102,
 /*    20 */  1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102,
 /*    30 */  1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102,
 /*    40 */  1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102,
 /*    50 */  1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102,  932,
 /*    60 */   936,  931,  935,  953,  949,  954,  950, 1102, 1102, 1102,
 /*    70 */  1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102,
 /*    80 */  1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102,
 /*    90 */  1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102,
 /*   100 */  1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102,
 /*   110 */  1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102,
 /*   120 */  1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102,
 /*   130 */  1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102,
 /*   140 */  1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102,
 /*   150 */  1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102,
 /*   160 */  1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102,
 /*   170 */  1102, 1102, 1102, 1102, 1102, 1102, 1051, 1053, 1053, 1053,
 /*   180 */  1053, 1059, 1051, 1051, 1102, 1102, 1102, 1102, 1102, 1102,
 /*   190 */  1102, 1102, 1102, 1102, 1102, 1102, 1102, 1051, 1102, 1059,
 /*   200 */  1102, 1059, 1057, 1059, 1057, 1053, 1055, 1051, 1102, 1059,
 /*   210 */  1057, 1053, 1055, 1051, 1102, 1059, 1057, 1053, 1055, 1051,
 /*   220 */  1102, 1059, 1057, 1059, 1057, 1059, 1057, 1059, 1057, 1059,
 /*   230 */  1057, 1053, 1055, 1051, 1059, 1057, 1053, 1055, 1051, 1059,
 /*   240 */  1057, 1053, 1055, 1102, 1102, 1102, 1059, 1057, 1051, 1059,
 /*   250 */  1057, 1102, 1059, 1057, 1102, 1059, 1057, 1102, 1102, 1051,
 /*   260 */  1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102,
 /*   270 */   885,  885, 1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102,
 /*   280 */  1102, 1102,  794, 1102, 1052, 1054, 1043, 1040,  925, 1102,
 /*   290 */  1102, 1102, 1102, 1058, 1050, 1102, 1102, 1102,  922,  839,
 /*   300 */   899,  911,  910,  909,  908,  907,  906,  905,  891,  934,
 /*   310 */   938,  933,  937,  856,  955,  951,  956,  952,  914,  767,
 /*   320 */   768,  871,  870,  869,  868,  867,  866,  865,  887,  884,
 /*   330 */   883,  882,  881,  880,  879,  878,  877,  876,  875,  874,
 /*   340 */   873,  872, 1102, 1102,  764,  763,  947,  924,  923, 1102,
 /*   350 */   844,  845,  901,  900,  821,  820,  858,  857,  834,  835,
 /*   360 */   796,  795,  801,  800,  806,  805,  780,  781, 1102, 1102,
 /*   370 */   840, 1102, 1102, 1020, 1024, 1023, 1021, 1102, 1102, 1102,
 /*   380 */  1102, 1102, 1102,  840, 1102, 1102, 1102, 1102, 1102,  968,
 /*   390 */  1102, 1102, 1102, 1102, 1102, 1102,  748, 1102, 1102, 1102,
 /*   400 */  1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102,
 /*   410 */  1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102,
 /*   420 */   789,  921,  920, 1102, 1102, 1102, 1102, 1102, 1022, 1102,
 /*   430 */  1009,  986,  988, 1102, 1001,  967,  966,  984, 1102, 1102,
 /*   440 */   983,  982, 1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102,
 /*   450 */  1102,  898,  897,  889,  885,  752,  855,  854, 1102, 1102,
 /*   460 */  1102, 1102,  912,  766,  762,  760,  756,  751, 1102, 1101,
 /*   470 */  1100, 1099, 1098, 1097, 1096, 1095, 1094, 1093, 1092, 1091,
 /*   480 */  1090,  948, 1089, 1088, 1087, 1086, 1080, 1079, 1081, 1078,
 /*   490 */  1077, 1076, 1075, 1074, 1073, 1072, 1071, 1070, 1069, 1068,
 /*   500 */  1067, 1066, 1065, 1064, 1063, 1062, 1061, 1060, 1036, 1035,
 /*   510 */  1042, 1041, 1049, 1048, 1045, 1044, 1039, 1047,  916,  918,
 /*   520 */   919,  917,  915,  792, 1038, 1037, 1031, 1030, 1032, 1029,
 /*   530 */  1034, 1033, 1028, 1026, 1025, 1027, 1019, 1014, 1017, 1018,
 /*   540 */  1016, 1015, 1013, 1004, 1007, 1012, 1010, 1008, 1006, 1005,
 /*   550 */  1003,  979,  987,  989, 1002, 1000,  999,  998,  997,  996,
 /*   560 */   995,  994,  993,  992,  991,  990,  985,  981,  977,  976,
 /*   570 */   975,  974,  973,  972,  971,  756,  970,  969,  846,  848,
 /*   580 */   847,  843,  842,  841,  840,  980,  978,  958,  961,  962,
 /*   590 */   965,  964,  963,  960,  959,  957, 1085, 1084, 1083, 1082,
 /*   600 */   893,  895,  904,  903,  902,  896,  894,  892,  819,  818,
 /*   610 */   817,  816,  815,  814,  824,  823,  822,  813,  812,  811,
 /*   620 */   810, 1056,  888,  890,  753,  850,  852,  927,  930,  929,
 /*   630 */   928,  926,  864,  863,  862,  861,  860,  859,  853,  851,
 /*   640 */   849,  792,  947,  946,  945,  944,  943,  942,  886,  940,
 /*   650 */   941,  939,  913,  836,  838,  837,  833,  832,  831,  830,
 /*   660 */   829,  828,  827,  826,  825,  765,  799,  798,  797,  804,
 /*   670 */   803,  802,  794,  793,  792,  791,  790,  789,  809,  808,
 /*   680 */   807,  788,  787,  786,  785,  782,  784,  783,  779,  778,
 /*   690 */   777,  776,  775,  774,  773,  772,  771,  770,  769,  761,
 /*   700 */   759,  758,  757,  755,  754,  750,  746,  745,  749,  748,
 /*   710 */   747,  744,  743,  742,  741,  740,  739,  738,  737,  736,
 /*   720 */   735,  734,  733,  732,  731,  730,  729,
};

/* The next table maps tokens into fallback tokens.  If a construct
** like the following:
** 
**      %fallback ID X Y Z.
**
** appears in the grammar, then ID becomes a fallback token for X, Y,
** and Z.  Whenever one of the tokens X, Y, or Z is input to the parser
** but it does not parse, the type of the token is changed to ID and
** the parse is retried before an error is thrown.
*/
#ifdef YYFALLBACK
static const YYCODETYPE yyFallback[] = {
    0,  /*          $ => nothing */
    0,  /*    INTEGER => nothing */
    0,  /* IDENTIFIER => nothing */
    0,  /* POUND_IDENTIFIER => nothing */
    0,  /* POUND_INTEGER => nothing */
    0,  /* AT_IDENTIFIER => nothing */
    0,  /* STRING_LITERAL => nothing */
    0,  /*     ASP_GR => nothing */
    0,  /*     ASP_CP => nothing */
    0,  /*     LUA_GR => nothing */
    0,  /*     LUA_CP => nothing */
    0,  /*    F2LP_GR => nothing */
    0,  /*    F2LP_CP => nothing */
    0,  /*    COMMENT => nothing */
    0,  /*  CONSTANTS => nothing */
    0,  /*    INCLUDE => nothing */
    0,  /*     MACROS => nothing */
    0,  /*    OBJECTS => nothing */
    0,  /*      QUERY => nothing */
    0,  /*       SHOW => nothing */
    0,  /*      SORTS => nothing */
    0,  /*  VARIABLES => nothing */
    0,  /*   ABACTION => nothing */
    0,  /*     ACTION => nothing */
    0,  /* ADDITIVEACTION => nothing */
    0,  /* ADDITIVEFLUENT => nothing */
    0,  /*      AFTER => nothing */
    0,  /*        ALL => nothing */
    0,  /*     ALWAYS => nothing */
    0,  /*   ASSUMING => nothing */
    0,  /*  ATTRIBUTE => nothing */
    0,  /*         BY => nothing */
    0,  /*     CAUSED => nothing */
    0,  /*     CAUSES => nothing */
    0,  /* IMPOSSIBLE => nothing */
    0,  /* CONSTRAINT => nothing */
    0,  /* DECREMENTS => nothing */
    0,  /*    DEFAULT => nothing */
    0,  /* EXTERNALACTION => nothing */
    0,  /*  EXOGENOUS => nothing */
    0,  /* EXOGENOUSACTION => nothing */
    0,  /*         IF => nothing */
    0,  /*     IFCONS => nothing */
    0,  /* INCREMENTS => nothing */
    0,  /*   INERTIAL => nothing */
    0,  /* INERTIALFLUENT => nothing */
    0,  /*      LABEL => nothing */
    0,  /*  MAY_CAUSE => nothing */
    0,  /* MAXADDITIVE => nothing */
    0,  /* MAXAFVALUE => nothing */
    0,  /*    MAXSTEP => nothing */
    0,  /*      NEVER => nothing */
    0,  /* NOCONCURRENCY => nothing */
    0,  /* STRONG_NOCONCURRENCY => nothing */
    0,  /* NONEXECUTABLE => nothing */
    0,  /*         OF => nothing */
    0,  /* POSSIBLY_CAUSED => nothing */
    0,  /*      RIGID => nothing */
    0,  /*   SDFLUENT => nothing */
    0,  /* SIMPLEFLUENT => nothing */
    0,  /* EXTERNALFLUENT => nothing */
    0,  /*     UNLESS => nothing */
    0,  /*      WHERE => nothing */
    0,  /*      FALSE => nothing */
    0,  /*       TRUE => nothing */
    0,  /*         AT => nothing */
    0,  /*  BRACKET_L => nothing */
    0,  /*  BRACKET_R => nothing */
    0,  /* COLON_DASH => nothing */
    0,  /* CBRACKET_L => nothing */
    0,  /* CBRACKET_R => nothing */
    0,  /*    PAREN_L => nothing */
    0,  /*    PAREN_R => nothing */
    0,  /*     PERIOD => nothing */
    0,  /* MACRO_STRING => nothing */
    0,  /*      TILDE => nothing */
    0,  /*  DBL_COLON => nothing */
    0,  /*  ARROW_LEQ => nothing */
    0,  /*  ARROW_REQ => nothing */
    0,  /* ARROW_LDASH => nothing */
    0,  /*      COLON => nothing */
    0,  /*         EQ => nothing */
    0,  /*     DBL_EQ => nothing */
    0,  /*        NEQ => nothing */
    0,  /*     NOT_EQ => nothing */
    0,  /*      LTHAN => nothing */
    0,  /*      GTHAN => nothing */
    0,  /*   LTHAN_EQ => nothing */
    0,  /*   GTHAN_EQ => nothing */
    0,  /* DBL_PERIOD => nothing */
    0,  /*   BIG_CONJ => nothing */
    0,  /*   BIG_DISJ => nothing */
    0,  /*      POUND => nothing */
    0,  /*  SEMICOLON => nothing */
    0,  /*      EQUIV => nothing */
    0,  /*       IMPL => nothing */
    0,  /* ARROW_RDASH => nothing */
    0,  /*   DBL_PLUS => nothing */
    0,  /*       PIPE => nothing */
    0,  /*  DBL_GTHAN => nothing */
    0,  /*  DBL_LTHAN => nothing */
    0,  /*        AMP => nothing */
    0,  /*      COMMA => nothing */
    0,  /*    DBL_AMP => nothing */
    0,  /*        NOT => nothing */
    0,  /*       DASH => nothing */
    0,  /*       PLUS => nothing */
    0,  /*       STAR => nothing */
    0,  /*    INT_DIV => nothing */
    0,  /*        MOD => nothing */
    0,  /*        ABS => nothing */
    0,  /*     CARROT => nothing */
    0,  /*     UMINUS => nothing */
    0,  /*     PREC_4 => nothing */
    0,  /*     PREC_3 => nothing */
    0,  /*     PREC_2 => nothing */
    0,  /*     PREC_1 => nothing */
    0,  /*     PREC_0 => nothing */
    0,  /*        EOF => nothing */
    0,  /*     ERR_IO => nothing */
    0,  /* ERR_UNKNOWN_SYMBOL => nothing */
    0,  /* ERR_UNTERMINATED_STRING => nothing */
    0,  /* ERR_UNTERMINATED_ASP => nothing */
    0,  /* ERR_UNTERMINATED_LUA => nothing */
    0,  /* ERR_UNTERMINATED_F2LP => nothing */
    0,  /* ERR_UNTERMINATED_BLK_COMMENT => nothing */
    0,  /* ERR_SYNTAX => nothing */
    0,  /* ERR_PAREN_MISMATCH => nothing */
    0,  /*        ARG => nothing */
    0,  /*       NOOP => nothing */
    2,  /* CONSTANT_ID => IDENTIFIER */
    2,  /* VARIABLE_ID => IDENTIFIER */
    2,  /*  OBJECT_ID => IDENTIFIER */
};
#endif /* YYFALLBACK */

/* The following structure represents a single element of the
** parser's stack.  Information stored includes:
**
**   +  The state number for the parser at this level of the stack.
**
**   +  The value of the token stored at this level of the stack.
**      (In other words, the "major" token.)
**
**   +  The semantic value stored at this level of the stack.  This is
**      the information used by the action routines in the grammar.
**      It is sometimes called the "minor" token.
*/
struct yyStackEntry {
  YYACTIONTYPE stateno;  /* The state-number */
  YYCODETYPE major;      /* The major token value.  This is the code
                         ** number for the token at this stack level */
  YYMINORTYPE minor;     /* The user-supplied minor token value.  This
                         ** is the value of the token  */
};
typedef struct yyStackEntry yyStackEntry;

/* The state of the parser is completely contained in an instance of
** the following structure */
struct yyParser {
  int yyidx;                    /* Index of top element in stack */
#ifdef YYTRACKMAXSTACKDEPTH
  int yyidxMax;                 /* Maximum value of yyidx */
#endif
  int yyerrcnt;                 /* Shifts left before out of the error */
  lemon_parserARG_SDECL                /* A place to hold %extra_argument */
#if YYSTACKDEPTH<=0
  int yystksz;                  /* Current side of the stack */
  yyStackEntry *yystack;        /* The parser's stack */
#else
  yyStackEntry yystack[YYSTACKDEPTH];  /* The parser's stack */
#endif

  int yylookmajor;				/* major token type for the lookahead token */
  YYMINORTYPE yylookminor;		/* minor token type for the lookahead token */
  int yysyntaxerr;				/* a flag used to trigger a syntax error */

};
typedef struct yyParser yyParser;

#ifndef NDEBUG
#include <stdio.h>
static FILE *yyTraceFILE = 0;
static char const*yyTracePrompt = 0;
#endif /* NDEBUG */

#ifndef NDEBUG
/* 
** Turn parser tracing on by giving a stream to which to write the trace
** and a prompt to preface each trace message.  Tracing is turned off
** by making either argument NULL 
**
** Inputs:
** <ul>
** <li> A FILE* to which trace output should be written.
**      If NULL, then tracing is turned off.
** <li> A prefix string written at the beginning of every
**      line of trace output.  If NULL, then tracing is
**      turned off.
** </ul>
**
** Outputs:
** None.
*/
void lemon_parserTrace(FILE *TraceFILE, char const*zTracePrompt){
  yyTraceFILE = TraceFILE;
  yyTracePrompt = zTracePrompt;
  if( yyTraceFILE==0 ) yyTracePrompt = 0;
  else if( yyTracePrompt==0 ) yyTraceFILE = 0;
}
#endif /* NDEBUG */

//#ifndef NDEBUG
/* For tracing shifts, the names of all terminals and nonterminals
** are required.  The following table supplies these names */
static const char *const yyTokenName[] = { 
  "$",             "INTEGER",       "IDENTIFIER",    "POUND_IDENTIFIER",
  "POUND_INTEGER",  "AT_IDENTIFIER",  "STRING_LITERAL",  "ASP_GR",      
  "ASP_CP",        "LUA_GR",        "LUA_CP",        "F2LP_GR",     
  "F2LP_CP",       "COMMENT",       "CONSTANTS",     "INCLUDE",     
  "MACROS",        "OBJECTS",       "QUERY",         "SHOW",        
  "SORTS",         "VARIABLES",     "ABACTION",      "ACTION",      
  "ADDITIVEACTION",  "ADDITIVEFLUENT",  "AFTER",         "ALL",         
  "ALWAYS",        "ASSUMING",      "ATTRIBUTE",     "BY",          
  "CAUSED",        "CAUSES",        "IMPOSSIBLE",    "CONSTRAINT",  
  "DECREMENTS",    "DEFAULT",       "EXTERNALACTION",  "EXOGENOUS",   
  "EXOGENOUSACTION",  "IF",            "IFCONS",        "INCREMENTS",  
  "INERTIAL",      "INERTIALFLUENT",  "LABEL",         "MAY_CAUSE",   
  "MAXADDITIVE",   "MAXAFVALUE",    "MAXSTEP",       "NEVER",       
  "NOCONCURRENCY",  "STRONG_NOCONCURRENCY",  "NONEXECUTABLE",  "OF",          
  "POSSIBLY_CAUSED",  "RIGID",         "SDFLUENT",      "SIMPLEFLUENT",
  "EXTERNALFLUENT",  "UNLESS",        "WHERE",         "FALSE",       
  "TRUE",          "AT",            "BRACKET_L",     "BRACKET_R",   
  "COLON_DASH",    "CBRACKET_L",    "CBRACKET_R",    "PAREN_L",     
  "PAREN_R",       "PERIOD",        "MACRO_STRING",  "TILDE",       
  "DBL_COLON",     "ARROW_LEQ",     "ARROW_REQ",     "ARROW_LDASH", 
  "COLON",         "EQ",            "DBL_EQ",        "NEQ",         
  "NOT_EQ",        "LTHAN",         "GTHAN",         "LTHAN_EQ",    
  "GTHAN_EQ",      "DBL_PERIOD",    "BIG_CONJ",      "BIG_DISJ",    
  "POUND",         "SEMICOLON",     "EQUIV",         "IMPL",        
  "ARROW_RDASH",   "DBL_PLUS",      "PIPE",          "DBL_GTHAN",   
  "DBL_LTHAN",     "AMP",           "COMMA",         "DBL_AMP",     
  "NOT",           "DASH",          "PLUS",          "STAR",        
  "INT_DIV",       "MOD",           "ABS",           "CARROT",      
  "UMINUS",        "PREC_4",        "PREC_3",        "PREC_2",      
  "PREC_1",        "PREC_0",        "EOF",           "ERR_IO",      
  "ERR_UNKNOWN_SYMBOL",  "ERR_UNTERMINATED_STRING",  "ERR_UNTERMINATED_ASP",  "ERR_UNTERMINATED_LUA",
  "ERR_UNTERMINATED_F2LP",  "ERR_UNTERMINATED_BLK_COMMENT",  "ERR_SYNTAX",    "ERR_PAREN_MISMATCH",
  "ARG",           "NOOP",          "CONSTANT_ID",   "VARIABLE_ID", 
  "OBJECT_ID",     "HIDE",          "OBSERVED",      "error",       
  "start",         "statement_lst",  "statement",     "stmt_macro_def",
  "stmt_constant_def",  "stmt_object_def",  "stmt_variable_def",  "stmt_sort_def",
  "stmt_code_blk",  "stmt_law",      "stmt_show",     "stmt_hide",   
  "stmt_noconcurrency",  "stmt_strong_noconcurrency",  "stmt_maxafvalue",  "stmt_maxadditive",
  "stmt_query",    "base_elem",     "base_elem_no_const",  "constant",    
  "object",        "object_nullary",  "variable",      "lua",         
  "undeclared",    "term_lst",      "term",          "constant_one_const",
  "term_no_const_lst",  "term_no_const",  "const_anon",    "term_strong", 
  "term_strong_candidate",  "term_no_const_strong",  "num_range",     "term_numeric",
  "formula",       "formula_base",  "comparison",    "atomic_formula",
  "formula_quant",  "formula_card",  "atomic_formula_anon",  "formula_no_const",
  "formula_no_const_base",  "comparison_no_const",  "atomic_formula_one_const",  "formula_temporal",
  "quant_lst",     "quant_op",      "card_var_lst",  "card_var_lst_inner",
  "head_formula",  "atomic_head_formula",  "formula_smpl_card",  "macro_def_lst",
  "macro_bnd",     "macro_args",    "macro_arg",     "sort_lst",    
  "sort",          "sort_id_nr",    "sort_nr",       "sort_id",     
  "constant_bnd_lst",  "constant_bnd",  "constant_dcl_lst",  "constant_dcl_type",
  "attrib_spec",   "object_bnd_lst",  "object_bnd",    "object_lst",  
  "object_spec",   "variable_bnd_lst",  "variable_bnd",  "variable_lst",
  "sort_bnd_lst",  "sort_bnd",      "sort_dcl_lst",  "show_lst",    
  "show_elem",     "query_lst",     "query_maxstep_decl",  "query_label_decl",
  "query_label_Decl",  "clause_if",     "clause_after",  "clause_ifcons",
  "clause_unless",  "clause_where",  "law_basic",     "law_caused",  
  "law_pcaused",   "law_impl",      "law_causes",    "law_increments",
  "law_decrements",  "law_mcause",    "law_always",    "law_constraint",
  "law_impossible",  "law_never",     "law_default",   "law_exogenous",
  "law_inertial",  "law_nonexecutable",  "law_rigid",     "law_observed",
};
//#endif /* NDEBUG */

#ifndef NDEBUG
/* For tracing reduce actions, the names of all rules are required.
*/
static const char *const yyRuleName[] = {
 /*   0 */ "start ::= statement_lst EOF",
 /*   1 */ "statement_lst ::=",
 /*   2 */ "statement_lst ::= statement_lst error",
 /*   3 */ "statement_lst ::= statement_lst statement",
 /*   4 */ "statement ::= stmt_macro_def",
 /*   5 */ "statement ::= stmt_constant_def",
 /*   6 */ "statement ::= stmt_object_def",
 /*   7 */ "statement ::= stmt_variable_def",
 /*   8 */ "statement ::= stmt_sort_def",
 /*   9 */ "statement ::= stmt_code_blk",
 /*  10 */ "statement ::= stmt_law",
 /*  11 */ "statement ::= stmt_show",
 /*  12 */ "statement ::= stmt_hide",
 /*  13 */ "statement ::= stmt_noconcurrency",
 /*  14 */ "statement ::= stmt_strong_noconcurrency",
 /*  15 */ "statement ::= stmt_maxafvalue",
 /*  16 */ "statement ::= stmt_maxadditive",
 /*  17 */ "statement ::= stmt_query",
 /*  18 */ "base_elem ::= constant",
 /*  19 */ "base_elem ::= base_elem_no_const",
 /*  20 */ "base_elem_no_const ::= object",
 /*  21 */ "base_elem_no_const ::= variable",
 /*  22 */ "base_elem_no_const ::= lua",
 /*  23 */ "constant ::= CONSTANT_ID PAREN_L term_lst PAREN_R",
 /*  24 */ "constant ::= CONSTANT_ID",
 /*  25 */ "const_anon ::= IDENTIFIER",
 /*  26 */ "const_anon ::= IDENTIFIER PAREN_L term_lst PAREN_R",
 /*  27 */ "object ::= OBJECT_ID PAREN_L term_lst PAREN_R",
 /*  28 */ "object ::= object_nullary",
 /*  29 */ "object_nullary ::= OBJECT_ID",
 /*  30 */ "object ::= undeclared",
 /*  31 */ "variable ::= VARIABLE_ID",
 /*  32 */ "lua ::= AT_IDENTIFIER PAREN_L term_lst PAREN_R",
 /*  33 */ "lua ::= AT_IDENTIFIER",
 /*  34 */ "undeclared ::= IDENTIFIER PAREN_L term_lst PAREN_R",
 /*  35 */ "undeclared ::= IDENTIFIER",
 /*  36 */ "term_lst ::= term",
 /*  37 */ "term_lst ::= term_lst COMMA term",
 /*  38 */ "constant_one_const ::= CONSTANT_ID PAREN_L term_no_const_lst PAREN_R",
 /*  39 */ "constant_one_const ::= CONSTANT_ID",
 /*  40 */ "term_no_const_lst ::= term_no_const",
 /*  41 */ "term_no_const_lst ::= term_no_const_lst COMMA term_no_const",
 /*  42 */ "term ::= base_elem",
 /*  43 */ "term ::= INTEGER",
 /*  44 */ "term ::= STRING_LITERAL",
 /*  45 */ "term ::= PAREN_L term PAREN_R",
 /*  46 */ "term ::= TRUE",
 /*  47 */ "term ::= FALSE",
 /*  48 */ "term ::= MAXSTEP",
 /*  49 */ "term ::= MAXADDITIVE",
 /*  50 */ "term ::= MAXAFVALUE",
 /*  51 */ "term ::= DASH term",
 /*  52 */ "term ::= ABS term",
 /*  53 */ "term ::= term DASH term",
 /*  54 */ "term ::= term PLUS term",
 /*  55 */ "term ::= term STAR term",
 /*  56 */ "term ::= term INT_DIV term",
 /*  57 */ "term ::= term MOD term",
 /*  58 */ "term_strong ::= base_elem_no_const",
 /*  59 */ "term_strong ::= INTEGER",
 /*  60 */ "term_strong ::= STRING_LITERAL",
 /*  61 */ "term_strong ::= PAREN_L term_strong PAREN_R",
 /*  62 */ "term_strong ::= MAXSTEP",
 /*  63 */ "term_strong ::= MAXADDITIVE",
 /*  64 */ "term_strong ::= MAXAFVALUE",
 /*  65 */ "term_strong ::= DASH term_strong",
 /*  66 */ "term_strong ::= ABS term",
 /*  67 */ "term_strong_candidate ::= DASH constant",
 /*  68 */ "term_strong ::= term_strong_candidate DASH term",
 /*  69 */ "term_strong ::= term_strong_candidate PLUS term",
 /*  70 */ "term_strong ::= term_strong_candidate STAR term",
 /*  71 */ "term_strong ::= term_strong_candidate INT_DIV term",
 /*  72 */ "term_strong ::= term_strong_candidate MOD term",
 /*  73 */ "term_strong ::= constant DASH term",
 /*  74 */ "term_strong ::= constant PLUS term",
 /*  75 */ "term_strong ::= constant STAR term",
 /*  76 */ "term_strong ::= constant INT_DIV term",
 /*  77 */ "term_strong ::= constant MOD term",
 /*  78 */ "term_strong ::= term_strong DASH term",
 /*  79 */ "term_strong ::= term_strong PLUS term",
 /*  80 */ "term_strong ::= term_strong STAR term",
 /*  81 */ "term_strong ::= term_strong INT_DIV term",
 /*  82 */ "term_strong ::= term_strong MOD term",
 /*  83 */ "term_no_const_strong ::= base_elem_no_const",
 /*  84 */ "term_no_const_strong ::= INTEGER",
 /*  85 */ "term_no_const_strong ::= STRING_LITERAL",
 /*  86 */ "term_no_const_strong ::= PAREN_L term_no_const_strong PAREN_R",
 /*  87 */ "term_no_const_strong ::= MAXSTEP",
 /*  88 */ "term_no_const_strong ::= MAXADDITIVE",
 /*  89 */ "term_no_const_strong ::= MAXAFVALUE",
 /*  90 */ "term_no_const_strong ::= constant",
 /*  91 */ "term_no_const_strong ::= DASH term_no_const_strong",
 /*  92 */ "term_no_const_strong ::= ABS term_no_const",
 /*  93 */ "term_no_const_strong ::= term_no_const_strong DASH term_no_const",
 /*  94 */ "term_no_const_strong ::= term_no_const_strong PLUS term_no_const",
 /*  95 */ "term_no_const_strong ::= term_no_const_strong STAR term_no_const",
 /*  96 */ "term_no_const_strong ::= term_no_const_strong INT_DIV term_no_const",
 /*  97 */ "term_no_const_strong ::= term_no_const_strong MOD term_no_const",
 /*  98 */ "term_no_const ::= base_elem_no_const",
 /*  99 */ "term_no_const ::= INTEGER",
 /* 100 */ "term_no_const ::= STRING_LITERAL",
 /* 101 */ "term_no_const ::= PAREN_L term_no_const PAREN_R",
 /* 102 */ "term_no_const ::= TRUE",
 /* 103 */ "term_no_const ::= FALSE",
 /* 104 */ "term_no_const ::= constant",
 /* 105 */ "term_no_const ::= DASH term_no_const",
 /* 106 */ "term_no_const ::= ABS term_no_const",
 /* 107 */ "term_no_const ::= term_no_const DASH term_no_const",
 /* 108 */ "term_no_const ::= term_no_const PLUS term_no_const",
 /* 109 */ "term_no_const ::= term_no_const STAR term_no_const",
 /* 110 */ "term_no_const ::= term_no_const INT_DIV term_no_const",
 /* 111 */ "term_no_const ::= term_no_const MOD term_no_const",
 /* 112 */ "num_range ::= term_numeric DBL_PERIOD term_numeric",
 /* 113 */ "term_numeric ::= INTEGER",
 /* 114 */ "term_numeric ::= PAREN_L term_numeric PAREN_R",
 /* 115 */ "term_numeric ::= DASH term_numeric",
 /* 116 */ "term_numeric ::= ABS term_numeric",
 /* 117 */ "term_numeric ::= term_numeric DASH term_numeric",
 /* 118 */ "term_numeric ::= term_numeric PLUS term_numeric",
 /* 119 */ "term_numeric ::= term_numeric STAR term_numeric",
 /* 120 */ "term_numeric ::= term_numeric INT_DIV term_numeric",
 /* 121 */ "term_numeric ::= term_numeric MOD term_numeric",
 /* 122 */ "formula ::= formula_base",
 /* 123 */ "formula ::= PAREN_L formula PAREN_R",
 /* 124 */ "formula ::= NOT formula",
 /* 125 */ "formula ::= DASH formula",
 /* 126 */ "formula ::= formula AMP formula",
 /* 127 */ "formula ::= formula DBL_PLUS formula",
 /* 128 */ "formula ::= formula PIPE formula",
 /* 129 */ "formula ::= formula EQUIV formula",
 /* 130 */ "formula ::= formula IMPL formula",
 /* 131 */ "formula ::= formula ARROW_RDASH formula",
 /* 132 */ "formula_base ::= comparison",
 /* 133 */ "formula_base ::= atomic_formula",
 /* 134 */ "formula_base ::= formula_quant",
 /* 135 */ "formula_base ::= formula_card",
 /* 136 */ "formula_base ::= TRUE",
 /* 137 */ "formula_base ::= FALSE",
 /* 138 */ "comparison ::= term_strong EQ term",
 /* 139 */ "comparison ::= term_strong DBL_EQ term",
 /* 140 */ "comparison ::= term_strong NEQ term",
 /* 141 */ "comparison ::= term_strong LTHAN term",
 /* 142 */ "comparison ::= term_strong GTHAN term",
 /* 143 */ "comparison ::= term_strong LTHAN_EQ term",
 /* 144 */ "comparison ::= term_strong GTHAN_EQ term",
 /* 145 */ "comparison ::= term_strong_candidate EQ term",
 /* 146 */ "comparison ::= term_strong_candidate DBL_EQ term",
 /* 147 */ "comparison ::= term_strong_candidate NEQ term",
 /* 148 */ "comparison ::= term_strong_candidate LTHAN term",
 /* 149 */ "comparison ::= term_strong_candidate GTHAN term",
 /* 150 */ "comparison ::= term_strong_candidate LTHAN_EQ term",
 /* 151 */ "comparison ::= term_strong_candidate GTHAN_EQ term",
 /* 152 */ "comparison ::= constant DBL_EQ term",
 /* 153 */ "comparison ::= constant NEQ term",
 /* 154 */ "comparison ::= constant LTHAN term",
 /* 155 */ "comparison ::= constant GTHAN term",
 /* 156 */ "comparison ::= constant LTHAN_EQ term",
 /* 157 */ "comparison ::= constant GTHAN_EQ term",
 /* 158 */ "atomic_formula ::= constant",
 /* 159 */ "atomic_formula ::= TILDE constant",
 /* 160 */ "atomic_formula ::= constant EQ term",
 /* 161 */ "atomic_formula_anon ::= atomic_formula",
 /* 162 */ "atomic_formula_anon ::= const_anon",
 /* 163 */ "atomic_formula_anon ::= TILDE const_anon",
 /* 164 */ "atomic_formula_anon ::= const_anon EQ term",
 /* 165 */ "formula_no_const ::= formula_no_const_base",
 /* 166 */ "formula_no_const ::= PAREN_L formula_no_const PAREN_R",
 /* 167 */ "formula_no_const ::= NOT formula_no_const",
 /* 168 */ "formula_no_const ::= DASH formula_no_const",
 /* 169 */ "formula_no_const ::= formula_no_const AMP formula_no_const",
 /* 170 */ "formula_no_const ::= formula_no_const DBL_PLUS formula_no_const",
 /* 171 */ "formula_no_const ::= formula_no_const PIPE formula_no_const",
 /* 172 */ "formula_no_const ::= formula_no_const EQUIV formula_no_const",
 /* 173 */ "formula_no_const ::= formula_no_const IMPL formula_no_const",
 /* 174 */ "formula_no_const ::= formula_no_const ARROW_RDASH formula_no_const",
 /* 175 */ "formula_no_const_base ::= comparison_no_const",
 /* 176 */ "formula_no_const_base ::= TRUE",
 /* 177 */ "formula_no_const_base ::= FALSE",
 /* 178 */ "comparison_no_const ::= term_no_const_strong EQ term_no_const",
 /* 179 */ "comparison_no_const ::= term_no_const_strong DBL_EQ term_no_const",
 /* 180 */ "comparison_no_const ::= term_no_const_strong NEQ term_no_const",
 /* 181 */ "comparison_no_const ::= term_no_const_strong LTHAN term_no_const",
 /* 182 */ "comparison_no_const ::= term_no_const_strong GTHAN term_no_const",
 /* 183 */ "comparison_no_const ::= term_no_const_strong LTHAN_EQ term_no_const",
 /* 184 */ "comparison_no_const ::= term_no_const_strong GTHAN_EQ term_no_const",
 /* 185 */ "atomic_formula_one_const ::= constant_one_const",
 /* 186 */ "atomic_formula_one_const ::= TILDE constant_one_const",
 /* 187 */ "atomic_formula_one_const ::= constant_one_const EQ term_no_const",
 /* 188 */ "formula_temporal ::= formula_base",
 /* 189 */ "formula_temporal ::= PAREN_L formula_temporal PAREN_R",
 /* 190 */ "formula_temporal ::= NOT formula_temporal",
 /* 191 */ "formula_temporal ::= DASH formula_temporal",
 /* 192 */ "formula_temporal ::= formula_temporal AMP formula_temporal",
 /* 193 */ "formula_temporal ::= formula_temporal DBL_PLUS formula_temporal",
 /* 194 */ "formula_temporal ::= formula_temporal PIPE formula_temporal",
 /* 195 */ "formula_temporal ::= formula_temporal EQUIV formula_temporal",
 /* 196 */ "formula_temporal ::= formula_temporal IMPL formula_temporal",
 /* 197 */ "formula_temporal ::= formula_temporal ARROW_RDASH formula_temporal",
 /* 198 */ "formula_temporal ::= term_strong COLON formula_temporal",
 /* 199 */ "formula_quant ::= BRACKET_L quant_lst PIPE formula BRACKET_R",
 /* 200 */ "quant_lst ::= quant_op variable",
 /* 201 */ "quant_lst ::= quant_lst quant_op variable",
 /* 202 */ "quant_op ::= BIG_CONJ",
 /* 203 */ "quant_op ::= BIG_DISJ",
 /* 204 */ "formula_card ::= CBRACKET_L card_var_lst formula CBRACKET_R",
 /* 205 */ "formula_card ::= term_strong CBRACKET_L card_var_lst formula CBRACKET_R",
 /* 206 */ "formula_card ::= CBRACKET_L card_var_lst formula CBRACKET_R term",
 /* 207 */ "formula_card ::= term_strong CBRACKET_L card_var_lst formula CBRACKET_R term",
 /* 208 */ "formula_card ::= CBRACKET_L formula CBRACKET_R",
 /* 209 */ "formula_card ::= term_strong CBRACKET_L formula CBRACKET_R",
 /* 210 */ "formula_card ::= CBRACKET_L formula CBRACKET_R term",
 /* 211 */ "formula_card ::= term_strong CBRACKET_L formula CBRACKET_R term",
 /* 212 */ "card_var_lst ::= card_var_lst_inner PIPE",
 /* 213 */ "card_var_lst_inner ::= variable",
 /* 214 */ "card_var_lst_inner ::= card_var_lst_inner COMMA variable",
 /* 215 */ "head_formula ::= comparison",
 /* 216 */ "head_formula ::= atomic_head_formula",
 /* 217 */ "head_formula ::= formula_smpl_card",
 /* 218 */ "head_formula ::= TRUE",
 /* 219 */ "head_formula ::= FALSE",
 /* 220 */ "atomic_head_formula ::= atomic_formula",
 /* 221 */ "atomic_head_formula ::= DASH constant",
 /* 222 */ "formula_smpl_card ::= CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R",
 /* 223 */ "formula_smpl_card ::= term_strong CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R",
 /* 224 */ "formula_smpl_card ::= CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R term",
 /* 225 */ "formula_smpl_card ::= term_strong CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R term",
 /* 226 */ "formula_smpl_card ::= CBRACKET_L atomic_formula_one_const CBRACKET_R",
 /* 227 */ "formula_smpl_card ::= term_strong CBRACKET_L atomic_formula_one_const CBRACKET_R",
 /* 228 */ "formula_smpl_card ::= CBRACKET_L atomic_formula_one_const CBRACKET_R term",
 /* 229 */ "formula_smpl_card ::= term_strong CBRACKET_L atomic_formula_one_const CBRACKET_R term",
 /* 230 */ "stmt_macro_def ::= COLON_DASH MACROS macro_def_lst PERIOD",
 /* 231 */ "macro_def_lst ::= macro_bnd",
 /* 232 */ "macro_def_lst ::= macro_def_lst SEMICOLON macro_bnd",
 /* 233 */ "macro_bnd ::= IDENTIFIER PAREN_L macro_args PAREN_R ARROW_RDASH MACRO_STRING",
 /* 234 */ "macro_bnd ::= IDENTIFIER ARROW_RDASH MACRO_STRING",
 /* 235 */ "macro_args ::= macro_arg",
 /* 236 */ "macro_args ::= macro_args COMMA macro_arg",
 /* 237 */ "macro_arg ::= POUND_INTEGER",
 /* 238 */ "macro_arg ::= POUND_IDENTIFIER",
 /* 239 */ "sort_lst ::= sort",
 /* 240 */ "sort_lst ::= sort_lst COMMA sort",
 /* 241 */ "sort ::= sort_id_nr",
 /* 242 */ "sort ::= sort_id_nr STAR",
 /* 243 */ "sort ::= sort_id_nr CARROT",
 /* 244 */ "sort ::= sort PLUS object_nullary",
 /* 245 */ "sort ::= sort PLUS IDENTIFIER",
 /* 246 */ "sort ::= sort PLUS INTEGER",
 /* 247 */ "sort_id_nr ::= sort_id",
 /* 248 */ "sort_id_nr ::= sort_nr",
 /* 249 */ "sort_nr ::= num_range",
 /* 250 */ "sort_id ::= IDENTIFIER",
 /* 251 */ "stmt_constant_def ::= COLON_DASH CONSTANTS constant_bnd_lst PERIOD",
 /* 252 */ "constant_bnd_lst ::= constant_bnd",
 /* 253 */ "constant_bnd_lst ::= constant_bnd_lst SEMICOLON constant_bnd",
 /* 254 */ "constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type PAREN_L sort PAREN_R",
 /* 255 */ "constant_bnd ::= constant_dcl_lst DBL_COLON sort",
 /* 256 */ "constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type",
 /* 257 */ "constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER",
 /* 258 */ "constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER PAREN_L sort_lst PAREN_R",
 /* 259 */ "constant_dcl_lst ::= IDENTIFIER",
 /* 260 */ "constant_dcl_lst ::= IDENTIFIER PAREN_L sort_lst PAREN_R",
 /* 261 */ "constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER",
 /* 262 */ "constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER PAREN_L sort_lst PAREN_R",
 /* 263 */ "constant_dcl_type ::= ABACTION",
 /* 264 */ "constant_dcl_type ::= ACTION",
 /* 265 */ "constant_dcl_type ::= ADDITIVEACTION",
 /* 266 */ "constant_dcl_type ::= ADDITIVEFLUENT",
 /* 267 */ "constant_dcl_type ::= EXTERNALACTION",
 /* 268 */ "constant_dcl_type ::= EXTERNALFLUENT",
 /* 269 */ "constant_dcl_type ::= EXOGENOUSACTION",
 /* 270 */ "constant_dcl_type ::= INERTIALFLUENT",
 /* 271 */ "constant_dcl_type ::= RIGID",
 /* 272 */ "constant_dcl_type ::= SIMPLEFLUENT",
 /* 273 */ "constant_dcl_type ::= SDFLUENT",
 /* 274 */ "attrib_spec ::= ATTRIBUTE",
 /* 275 */ "attrib_spec ::= ATTRIBUTE PAREN_L sort PAREN_R",
 /* 276 */ "stmt_object_def ::= COLON_DASH OBJECTS object_bnd_lst PERIOD",
 /* 277 */ "object_bnd_lst ::= object_bnd",
 /* 278 */ "object_bnd_lst ::= object_bnd_lst SEMICOLON object_bnd",
 /* 279 */ "object_bnd ::= object_lst DBL_COLON sort_id",
 /* 280 */ "object_lst ::= object_spec",
 /* 281 */ "object_lst ::= object_lst COMMA object_spec",
 /* 282 */ "object_spec ::= IDENTIFIER",
 /* 283 */ "object_spec ::= IDENTIFIER PAREN_L sort_lst PAREN_R",
 /* 284 */ "object_spec ::= INTEGER",
 /* 285 */ "object_spec ::= num_range",
 /* 286 */ "stmt_variable_def ::= COLON_DASH VARIABLES variable_bnd_lst PERIOD",
 /* 287 */ "variable_bnd_lst ::= variable_bnd",
 /* 288 */ "variable_bnd_lst ::= variable_bnd_lst SEMICOLON variable_bnd",
 /* 289 */ "variable_bnd ::= variable_lst DBL_COLON sort_id",
 /* 290 */ "variable_lst ::= IDENTIFIER",
 /* 291 */ "variable_lst ::= variable_lst COMMA IDENTIFIER",
 /* 292 */ "stmt_sort_def ::= COLON_DASH SORTS sort_bnd_lst PERIOD",
 /* 293 */ "sort_bnd_lst ::= sort_bnd",
 /* 294 */ "sort_bnd_lst ::= sort_bnd_lst SEMICOLON sort_bnd",
 /* 295 */ "sort_bnd ::= sort_dcl_lst",
 /* 296 */ "sort_bnd ::= sort_bnd DBL_LTHAN sort_bnd",
 /* 297 */ "sort_bnd ::= sort_bnd DBL_GTHAN sort_bnd",
 /* 298 */ "sort_bnd ::= PAREN_L sort_bnd PAREN_R",
 /* 299 */ "sort_dcl_lst ::= IDENTIFIER",
 /* 300 */ "sort_dcl_lst ::= sort_dcl_lst COMMA IDENTIFIER",
 /* 301 */ "stmt_show ::= COLON_DASH SHOW show_lst PERIOD",
 /* 302 */ "stmt_show ::= COLON_DASH SHOW ALL PERIOD",
 /* 303 */ "stmt_hide ::= COLON_DASH HIDE show_lst PERIOD",
 /* 304 */ "stmt_hide ::= COLON_DASH HIDE ALL PERIOD",
 /* 305 */ "show_lst ::= show_elem",
 /* 306 */ "show_lst ::= show_lst COMMA show_elem",
 /* 307 */ "show_elem ::= atomic_formula_one_const",
 /* 308 */ "stmt_noconcurrency ::= NOCONCURRENCY PERIOD",
 /* 309 */ "stmt_strong_noconcurrency ::= STRONG_NOCONCURRENCY PERIOD",
 /* 310 */ "stmt_maxafvalue ::= COLON_DASH MAXAFVALUE EQ term_numeric PERIOD",
 /* 311 */ "stmt_maxadditive ::= COLON_DASH MAXADDITIVE EQ term_numeric PERIOD",
 /* 312 */ "stmt_query ::= COLON_DASH QUERY query_lst PERIOD",
 /* 313 */ "query_lst ::= formula_temporal",
 /* 314 */ "query_lst ::= query_maxstep_decl",
 /* 315 */ "query_lst ::= query_label_decl",
 /* 316 */ "query_lst ::= query_lst SEMICOLON formula_temporal",
 /* 317 */ "query_lst ::= query_lst SEMICOLON query_maxstep_decl",
 /* 318 */ "query_lst ::= query_lst SEMICOLON query_label_decl",
 /* 319 */ "query_maxstep_decl ::= MAXSTEP DBL_COLON INTEGER",
 /* 320 */ "query_maxstep_decl ::= MAXSTEP DBL_COLON num_range",
 /* 321 */ "query_label_decl ::= LABEL DBL_COLON INTEGER",
 /* 322 */ "query_label_decl ::= LABEL DBL_COLON IDENTIFIER",
 /* 323 */ "clause_if ::= IF formula",
 /* 324 */ "clause_if ::=",
 /* 325 */ "clause_after ::= AFTER formula",
 /* 326 */ "clause_after ::=",
 /* 327 */ "clause_ifcons ::= IFCONS formula",
 /* 328 */ "clause_ifcons ::=",
 /* 329 */ "clause_unless ::= UNLESS atomic_formula_anon",
 /* 330 */ "clause_unless ::=",
 /* 331 */ "clause_where ::= WHERE formula_no_const",
 /* 332 */ "clause_where ::=",
 /* 333 */ "stmt_law ::= law_basic",
 /* 334 */ "stmt_law ::= law_caused",
 /* 335 */ "stmt_law ::= law_pcaused",
 /* 336 */ "stmt_law ::= law_impl",
 /* 337 */ "stmt_law ::= law_causes",
 /* 338 */ "stmt_law ::= law_increments",
 /* 339 */ "stmt_law ::= law_decrements",
 /* 340 */ "stmt_law ::= law_mcause",
 /* 341 */ "stmt_law ::= law_always",
 /* 342 */ "stmt_law ::= law_constraint",
 /* 343 */ "stmt_law ::= law_impossible",
 /* 344 */ "stmt_law ::= law_never",
 /* 345 */ "stmt_law ::= law_default",
 /* 346 */ "stmt_law ::= law_exogenous",
 /* 347 */ "stmt_law ::= law_inertial",
 /* 348 */ "stmt_law ::= law_nonexecutable",
 /* 349 */ "stmt_law ::= law_rigid",
 /* 350 */ "stmt_law ::= law_observed",
 /* 351 */ "law_basic ::= head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 352 */ "law_caused ::= CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 353 */ "law_pcaused ::= POSSIBLY_CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 354 */ "law_impl ::= head_formula ARROW_LDASH formula clause_where PERIOD",
 /* 355 */ "law_causes ::= atomic_formula CAUSES head_formula clause_if clause_unless clause_where PERIOD",
 /* 356 */ "law_increments ::= atomic_formula INCREMENTS constant BY term clause_if clause_unless clause_where PERIOD",
 /* 357 */ "law_decrements ::= atomic_formula DECREMENTS constant BY term clause_if clause_unless clause_where PERIOD",
 /* 358 */ "law_mcause ::= atomic_formula MAY_CAUSE head_formula clause_if clause_unless clause_where PERIOD",
 /* 359 */ "law_always ::= ALWAYS formula clause_after clause_unless clause_where PERIOD",
 /* 360 */ "law_constraint ::= CONSTRAINT formula clause_after clause_unless clause_where PERIOD",
 /* 361 */ "law_impossible ::= IMPOSSIBLE formula clause_after clause_unless clause_where PERIOD",
 /* 362 */ "law_never ::= NEVER formula clause_after clause_unless clause_where PERIOD",
 /* 363 */ "law_default ::= DEFAULT atomic_head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 364 */ "law_exogenous ::= EXOGENOUS constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 365 */ "law_inertial ::= INERTIAL constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 366 */ "law_nonexecutable ::= NONEXECUTABLE formula clause_if clause_unless clause_where PERIOD",
 /* 367 */ "law_rigid ::= RIGID constant clause_where PERIOD",
 /* 368 */ "law_observed ::= OBSERVED atomic_head_formula AT term_no_const PERIOD",
 /* 369 */ "stmt_code_blk ::= ASP_GR",
 /* 370 */ "stmt_code_blk ::= ASP_CP",
 /* 371 */ "stmt_code_blk ::= F2LP_GR",
 /* 372 */ "stmt_code_blk ::= F2LP_CP",
 /* 373 */ "stmt_code_blk ::= LUA_GR",
 /* 374 */ "stmt_code_blk ::= LUA_CP",
};
#endif /* NDEBUG */


#if YYSTACKDEPTH<=0
/*
** Try to increase the size of the parser stack.
*/
static void yyGrowStack(yyParser *p){
  int newSize;
  yyStackEntry *pNew;

  newSize = p->yystksz*2 + 100;
  pNew = realloc(p->yystack, newSize*sizeof(pNew[0]));
  if( pNew ){
    p->yystack = pNew;
    p->yystksz = newSize;
#ifndef NDEBUG
    if( yyTraceFILE ){
      fprintf(yyTraceFILE,"%sStack grows to %d entries!\n",
              yyTracePrompt, p->yystksz);
    }
#endif
  }
}
#endif

/* 
** This function allocates a new parser.
** The only argument is a pointer to a function which works like
** malloc.
**
** Inputs:
** A pointer to the function used to allocate memory.
**
** Outputs:
** A pointer to a parser.  This pointer is used in subsequent calls
** to lemon_parser and lemon_parserFree.
*/
void *lemon_parserAlloc(void *(*mallocProc)(size_t)){
  yyParser *pParser;
  pParser = (yyParser*)(*mallocProc)( (size_t)sizeof(yyParser) );
  if( pParser ){
    pParser->yyidx = -1;
#ifdef YYTRACKMAXSTACKDEPTH
    pParser->yyidxMax = 0;
#endif
#if YYSTACKDEPTH<=0
    pParser->yystack = NULL;
    pParser->yystksz = 0;
    yyGrowStack(pParser);
#endif
	pParser->yylookmajor = YYNOCODE;
	pParser->yylookminor = yyzerominor;
	pParser->yysyntaxerr = 0;
  }
  return pParser;
}

/* The following function deletes the value associated with a
** symbol.  The symbol can be either a terminal or nonterminal.
** "yymajor" is the symbol code, and "yypminor" is a pointer to
** the value.
*/
static void yy_destructor(
  yyParser *yypParser,    /* The parser */
  YYCODETYPE yymajor,     /* Type code for object to destroy */
  YYMINORTYPE *yypminor   /* The object to be destroyed */
){
  lemon_parserARG_FETCH;
  switch( yymajor ){
    /* Here is inserted the actions which take place when a
    ** terminal or non-terminal is destroyed.  This can happen
    ** when the symbol is popped from the stack during a
    ** reduce or during error processing or when a parser is 
    ** being destroyed before it is finished parsing.
    **
    ** Note: during a reduce, the only symbols destroyed are those
    ** which appear on the RHS of the rule, but which are not used
    ** inside the C code.
    */
      /* TERMINAL Destructor */
    case 1: /* INTEGER */
    case 2: /* IDENTIFIER */
    case 3: /* POUND_IDENTIFIER */
    case 4: /* POUND_INTEGER */
    case 5: /* AT_IDENTIFIER */
    case 6: /* STRING_LITERAL */
    case 7: /* ASP_GR */
    case 8: /* ASP_CP */
    case 9: /* LUA_GR */
    case 10: /* LUA_CP */
    case 11: /* F2LP_GR */
    case 12: /* F2LP_CP */
    case 13: /* COMMENT */
    case 14: /* CONSTANTS */
    case 15: /* INCLUDE */
    case 16: /* MACROS */
    case 17: /* OBJECTS */
    case 18: /* QUERY */
    case 19: /* SHOW */
    case 20: /* SORTS */
    case 21: /* VARIABLES */
    case 22: /* ABACTION */
    case 23: /* ACTION */
    case 24: /* ADDITIVEACTION */
    case 25: /* ADDITIVEFLUENT */
    case 26: /* AFTER */
    case 27: /* ALL */
    case 28: /* ALWAYS */
    case 29: /* ASSUMING */
    case 30: /* ATTRIBUTE */
    case 31: /* BY */
    case 32: /* CAUSED */
    case 33: /* CAUSES */
    case 34: /* IMPOSSIBLE */
    case 35: /* CONSTRAINT */
    case 36: /* DECREMENTS */
    case 37: /* DEFAULT */
    case 38: /* EXTERNALACTION */
    case 39: /* EXOGENOUS */
    case 40: /* EXOGENOUSACTION */
    case 41: /* IF */
    case 42: /* IFCONS */
    case 43: /* INCREMENTS */
    case 44: /* INERTIAL */
    case 45: /* INERTIALFLUENT */
    case 46: /* LABEL */
    case 47: /* MAY_CAUSE */
    case 48: /* MAXADDITIVE */
    case 49: /* MAXAFVALUE */
    case 50: /* MAXSTEP */
    case 51: /* NEVER */
    case 52: /* NOCONCURRENCY */
    case 53: /* STRONG_NOCONCURRENCY */
    case 54: /* NONEXECUTABLE */
    case 55: /* OF */
    case 56: /* POSSIBLY_CAUSED */
    case 57: /* RIGID */
    case 58: /* SDFLUENT */
    case 59: /* SIMPLEFLUENT */
    case 60: /* EXTERNALFLUENT */
    case 61: /* UNLESS */
    case 62: /* WHERE */
    case 63: /* FALSE */
    case 64: /* TRUE */
    case 65: /* AT */
    case 66: /* BRACKET_L */
    case 67: /* BRACKET_R */
    case 68: /* COLON_DASH */
    case 69: /* CBRACKET_L */
    case 70: /* CBRACKET_R */
    case 71: /* PAREN_L */
    case 72: /* PAREN_R */
    case 73: /* PERIOD */
    case 74: /* MACRO_STRING */
    case 75: /* TILDE */
    case 76: /* DBL_COLON */
    case 77: /* ARROW_LEQ */
    case 78: /* ARROW_REQ */
    case 79: /* ARROW_LDASH */
    case 80: /* COLON */
    case 81: /* EQ */
    case 82: /* DBL_EQ */
    case 83: /* NEQ */
    case 84: /* NOT_EQ */
    case 85: /* LTHAN */
    case 86: /* GTHAN */
    case 87: /* LTHAN_EQ */
    case 88: /* GTHAN_EQ */
    case 89: /* DBL_PERIOD */
    case 90: /* BIG_CONJ */
    case 91: /* BIG_DISJ */
    case 92: /* POUND */
    case 93: /* SEMICOLON */
    case 94: /* EQUIV */
    case 95: /* IMPL */
    case 96: /* ARROW_RDASH */
    case 97: /* DBL_PLUS */
    case 98: /* PIPE */
    case 99: /* DBL_GTHAN */
    case 100: /* DBL_LTHAN */
    case 101: /* AMP */
    case 102: /* COMMA */
    case 103: /* DBL_AMP */
    case 104: /* NOT */
    case 105: /* DASH */
    case 106: /* PLUS */
    case 107: /* STAR */
    case 108: /* INT_DIV */
    case 109: /* MOD */
    case 110: /* ABS */
    case 111: /* CARROT */
    case 112: /* UMINUS */
    case 113: /* PREC_4 */
    case 114: /* PREC_3 */
    case 115: /* PREC_2 */
    case 116: /* PREC_1 */
    case 117: /* PREC_0 */
    case 118: /* EOF */
    case 119: /* ERR_IO */
    case 120: /* ERR_UNKNOWN_SYMBOL */
    case 121: /* ERR_UNTERMINATED_STRING */
    case 122: /* ERR_UNTERMINATED_ASP */
    case 123: /* ERR_UNTERMINATED_LUA */
    case 124: /* ERR_UNTERMINATED_F2LP */
    case 125: /* ERR_UNTERMINATED_BLK_COMMENT */
    case 126: /* ERR_SYNTAX */
    case 127: /* ERR_PAREN_MISMATCH */
    case 128: /* ARG */
    case 129: /* NOOP */
    case 130: /* CONSTANT_ID */
    case 131: /* VARIABLE_ID */
    case 132: /* OBJECT_ID */
    case 133: /* HIDE */
    case 134: /* OBSERVED */
{
#line 197 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy0));								
#line 2342 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 136: /* start */
    case 137: /* statement_lst */
    case 160: /* undeclared */
{
#line 207 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */			
#line 2351 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 138: /* statement */
    case 144: /* stmt_code_blk */
    case 145: /* stmt_law */
    case 146: /* stmt_show */
    case 147: /* stmt_hide */
    case 150: /* stmt_maxafvalue */
    case 151: /* stmt_maxadditive */
{
#line 211 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy224));								
#line 2364 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 139: /* stmt_macro_def */
{
#line 232 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy455));								
#line 2371 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 140: /* stmt_constant_def */
{
#line 234 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy55));								
#line 2378 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 141: /* stmt_object_def */
{
#line 236 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy296));								
#line 2385 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 142: /* stmt_variable_def */
{
#line 238 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy171));								
#line 2392 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 143: /* stmt_sort_def */
{
#line 240 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy469));								
#line 2399 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 148: /* stmt_noconcurrency */
{
#line 250 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy210));								
#line 2406 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 149: /* stmt_strong_noconcurrency */
{
#line 252 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy482));								
#line 2413 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 152: /* stmt_query */
{
#line 258 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy146));								
#line 2420 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 153: /* base_elem */
    case 154: /* base_elem_no_const */
    case 162: /* term */
    case 165: /* term_no_const */
    case 167: /* term_strong */
    case 168: /* term_strong_candidate */
    case 169: /* term_no_const_strong */
{
#line 292 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy163));								
#line 2433 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 155: /* constant */
    case 163: /* constant_one_const */
    case 166: /* const_anon */
{
#line 296 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy345));								
#line 2442 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 156: /* object */
    case 157: /* object_nullary */
{
#line 298 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy238));								
#line 2450 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 158: /* variable */
{
#line 302 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy453));								
#line 2457 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 159: /* lua */
{
#line 304 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy313));								
#line 2464 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 161: /* term_lst */
    case 164: /* term_no_const_lst */
{
#line 308 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy187));								
#line 2472 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 170: /* num_range */
{
#line 706 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy309));								
#line 2479 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 171: /* term_numeric */
{
#line 708 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy416));								
#line 2486 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 172: /* formula */
    case 173: /* formula_base */
    case 174: /* comparison */
    case 177: /* formula_card */
    case 179: /* formula_no_const */
    case 180: /* formula_no_const_base */
    case 181: /* comparison_no_const */
    case 183: /* formula_temporal */
    case 188: /* head_formula */
{
#line 769 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy353));								
#line 2501 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 175: /* atomic_formula */
    case 178: /* atomic_formula_anon */
    case 182: /* atomic_formula_one_const */
    case 189: /* atomic_head_formula */
{
#line 775 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy138));								
#line 2511 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 176: /* formula_quant */
{
#line 777 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy285));								
#line 2518 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 184: /* quant_lst */
{
#line 978 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy261));								
#line 2525 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 185: /* quant_op */
{
#line 980 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */			
#line 2532 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 186: /* card_var_lst */
    case 187: /* card_var_lst_inner */
{
#line 1017 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy319));								
#line 2540 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 190: /* formula_smpl_card */
{
#line 1095 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy369));								
#line 2547 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 191: /* macro_def_lst */
{
#line 1142 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy105));                              
#line 2554 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 192: /* macro_bnd */
{
#line 1144 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy315));                              
#line 2561 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 193: /* macro_args */
{
#line 1146 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy354));                              
#line 2568 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 194: /* macro_arg */
{
#line 1148 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy251));                              
#line 2575 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 195: /* sort_lst */
{
#line 1238 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy411));							
#line 2582 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 196: /* sort */
    case 197: /* sort_id_nr */
    case 198: /* sort_nr */
    case 199: /* sort_id */
{
#line 1240 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */		
#line 2592 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 200: /* constant_bnd_lst */
    case 201: /* constant_bnd */
{
#line 1368 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy97));									
#line 2600 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 202: /* constant_dcl_lst */
{
#line 1372 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy42));									
#line 2607 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 203: /* constant_dcl_type */
{
#line 1374 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */				
#line 2614 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 204: /* attrib_spec */
{
#line 1376 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */				
#line 2621 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 205: /* object_bnd_lst */
{
#line 1732 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy198));									
#line 2628 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 206: /* object_bnd */
{
#line 1734 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy70));									
#line 2635 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 207: /* object_lst */
    case 208: /* object_spec */
{
#line 1736 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy341));									
#line 2643 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 209: /* variable_bnd_lst */
    case 210: /* variable_bnd */
{
#line 1861 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy117));									
#line 2651 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 211: /* variable_lst */
{
#line 1865 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy152));									
#line 2658 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 212: /* sort_bnd_lst */
    case 213: /* sort_bnd */
    case 214: /* sort_dcl_lst */
{
#line 1948 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy320));									
#line 2667 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 215: /* show_lst */
{
#line 2052 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy451));									
#line 2674 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 216: /* show_elem */
    case 224: /* clause_unless */
{
#line 2054 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy138));									
#line 2682 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 217: /* query_lst */
{
#line 2199 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy301).l); DEALLOC((yypminor->yy301).maxstep); DEALLOC((yypminor->yy301).label);	
#line 2689 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 218: /* query_maxstep_decl */
{
#line 2201 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy392));												
#line 2696 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 220: /* query_label_Decl */
{
#line 2203 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy0));												
#line 2703 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 221: /* clause_if */
    case 222: /* clause_after */
    case 223: /* clause_ifcons */
    case 225: /* clause_where */
{
#line 2357 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy353));									
#line 2713 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 226: /* law_basic */
    case 227: /* law_caused */
    case 228: /* law_pcaused */
    case 229: /* law_impl */
    case 230: /* law_causes */
    case 231: /* law_increments */
    case 232: /* law_decrements */
    case 233: /* law_mcause */
    case 234: /* law_always */
    case 235: /* law_constraint */
    case 236: /* law_impossible */
    case 237: /* law_never */
    case 238: /* law_default */
    case 239: /* law_exogenous */
    case 240: /* law_inertial */
    case 241: /* law_nonexecutable */
    case 242: /* law_rigid */
    case 243: /* law_observed */
{
#line 2398 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy224));									
#line 2737 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    default:  break;   /* If no destructor action specified: do nothing */
  }
  lemon_parserARG_STORE;
}

/*
** Pop the parser's stack once.
**
** If there is a destructor routine associated with the token which
** is popped from the stack, then call it.
**
** Return the major token number for the symbol popped.
*/
static int yy_pop_parser_stack(yyParser *pParser){
  YYCODETYPE yymajor;
  yyStackEntry *yytos = &pParser->yystack[pParser->yyidx];

  if( pParser->yyidx<0 ) return 0;
#ifndef NDEBUG
  if( yyTraceFILE && pParser->yyidx>=0 ){
    fprintf(yyTraceFILE,"%sPopping %s\n",
      yyTracePrompt,
      yyTokenName[yytos->major]);
  }
#endif
  yymajor = yytos->major;
  yy_destructor(pParser, yymajor, &yytos->minor);
  pParser->yyidx--;
  return yymajor;
}

/* 
** Deallocate and destroy a parser.  Destructors are all called for
** all stack elements before shutting the parser down.
**
** Inputs:
** <ul>
** <li>  A pointer to the parser.  This should be a pointer
**       obtained from lemon_parserAlloc.
** <li>  A pointer to a function used to reclaim memory obtained
**       from malloc.
** </ul>
*/
void lemon_parserFree(
  void *p,                    /* The parser to be deleted */
  void (*freeProc)(void*)     /* Function used to reclaim memory */
){
  yyParser *pParser = (yyParser*)p;
  if( pParser==0 ) return;
  if( pParser->yylookmajor != YYNOCODE ) yy_destructor(pParser, (YYCODETYPE)pParser->yylookmajor, &pParser->yylookminor);
  while( pParser->yyidx>=0 ) yy_pop_parser_stack(pParser);
#if YYSTACKDEPTH<=0
  free(pParser->yystack);
#endif
  (*freeProc)((void*)pParser);
}

/*
** Return the peak depth of the stack for a parser.
*/
#ifdef YYTRACKMAXSTACKDEPTH
int lemon_parserStackPeak(void *p){
  yyParser *pParser = (yyParser*)p;
  return pParser->yyidxMax;
}
#endif

/*
** Find the appropriate action for a parser given the terminal
** look-ahead token iLookAhead.
**
** If the look-ahead token is YYNOCODE, then check to see if the action is
** independent of the look-ahead.  If it is, return the action, otherwise
** return YY_NO_ACTION.
*/
static int yy_find_shift_action(
  yyParser *pParser,        /* The parser */
  YYCODETYPE iLookAhead     /* The look-ahead token */
){
  int i;
  int stateno = pParser->yystack[pParser->yyidx].stateno;
 
  if( stateno>YY_SHIFT_COUNT
   || (i = yy_shift_ofst[stateno])==YY_SHIFT_USE_DFLT ){
    return yy_default[stateno];
  }
  assert( iLookAhead!=YYNOCODE );
  i += iLookAhead;
  if( i<0 || i>=YY_ACTTAB_COUNT || yy_lookahead[i]!=iLookAhead ){
    if( iLookAhead>0 ){
#ifdef YYFALLBACK
      YYCODETYPE iFallback;            /* Fallback token */
      if( iLookAhead<sizeof(yyFallback)/sizeof(yyFallback[0])
             && (iFallback = yyFallback[iLookAhead])!=0 ){
#ifndef NDEBUG
        if( yyTraceFILE ){
          fprintf(yyTraceFILE, "%sFALLBACK %s => %s\n",
             yyTracePrompt, yyTokenName[iLookAhead], yyTokenName[iFallback]);
        }
#endif
        return yy_find_shift_action(pParser, iFallback);
      }
#endif
#ifdef YYWILDCARD
      {
        int j = i - iLookAhead + YYWILDCARD;
        if( 
#if YY_SHIFT_MIN+YYWILDCARD<0
          j>=0 &&
#endif
#if YY_SHIFT_MAX+YYWILDCARD>=YY_ACTTAB_COUNT
          j<YY_ACTTAB_COUNT &&
#endif
          yy_lookahead[j]==YYWILDCARD
        ){
#ifndef NDEBUG
          if( yyTraceFILE ){
            fprintf(yyTraceFILE, "%sWILDCARD %s => %s\n",
               yyTracePrompt, yyTokenName[iLookAhead], yyTokenName[YYWILDCARD]);
          }
#endif /* NDEBUG */
          return yy_action[j];
        }
      }
#endif /* YYWILDCARD */
    }
    return yy_default[stateno];
  }else{
    return yy_action[i];
  }
}

/*
** Find the appropriate action for a parser given the non-terminal
** look-ahead token iLookAhead.
**
** If the look-ahead token is YYNOCODE, then check to see if the action is
** independent of the look-ahead.  If it is, return the action, otherwise
** return YY_NO_ACTION.
*/
static int yy_find_reduce_action(
  int stateno,              /* Current state number */
  YYCODETYPE iLookAhead     /* The look-ahead token */
){
  int i;
#ifdef YYERRORSYMBOL
  if( stateno>YY_REDUCE_COUNT ){
    return yy_default[stateno];
  }
#else
  assert( stateno<=YY_REDUCE_COUNT );
#endif
  i = yy_reduce_ofst[stateno];
  assert( i!=YY_REDUCE_USE_DFLT );
  if (iLookAhead == YYNOCODE) return YY_NO_ACTION;
  assert( iLookAhead!=YYNOCODE );
  i += iLookAhead;
#ifdef YYERRORSYMBOL
  if( i<0 || i>=YY_ACTTAB_COUNT || yy_lookahead[i]!=iLookAhead ){
    return yy_default[stateno];
  }
#else
  assert( i>=0 && i<YY_ACTTAB_COUNT );
  assert( yy_lookahead[i]==iLookAhead );
#endif
  return yy_action[i];
}

/*
** The following routine is called if the stack overflows.
*/
static void yyStackOverflow(yyParser *yypParser /*, YYMINORTYPE *yypMinor */){
   lemon_parserARG_FETCH;
   yypParser->yyidx--;
#ifndef NDEBUG
   if( yyTraceFILE ){
     fprintf(yyTraceFILE,"%sStack Overflow!\n",yyTracePrompt);
   }
#endif
   while( yypParser->yyidx>=0 ) yy_pop_parser_stack(yypParser);
   /* Here code is inserted which will execute if the parser
   ** stack every overflows */
   lemon_parserARG_STORE; /* Suppress warning about unused %extra_argument var */
}

/*
** Perform a shift action.
*/
static void yy_shift(
  yyParser *yypParser,          /* The parser to be shifted */
  int yyNewState,               /* The new state to shift in */
  int yyMajor,                  /* The major token to shift in */
  YYMINORTYPE *yypMinor         /* Pointer to the minor token to shift in */
){
  yyStackEntry *yytos;
  yypParser->yyidx++;
#ifdef YYTRACKMAXSTACKDEPTH
  if( yypParser->yyidx>yypParser->yyidxMax ){
    yypParser->yyidxMax = yypParser->yyidx;
  }
#endif
#if YYSTACKDEPTH>0 
  if( yypParser->yyidx>=YYSTACKDEPTH ){
    yyStackOverflow(yypParser/*, yypMinor */);
    return;
  }
#else
  if( yypParser->yyidx>=yypParser->yystksz ){
    yyGrowStack(yypParser);
    if( yypParser->yyidx>=yypParser->yystksz ){
      yyStackOverflow(yypParser/*, yypMinor */);
      return;
    }
  }
#endif
  yytos = &yypParser->yystack[yypParser->yyidx];
  yytos->stateno = (YYACTIONTYPE)yyNewState;
  yytos->major = (YYCODETYPE)yyMajor;
  yytos->minor = *yypMinor;
#ifndef NDEBUG
  if( yyTraceFILE && yypParser->yyidx>0 ){
    int i;
    fprintf(yyTraceFILE,"%sShift %d\n",yyTracePrompt,yyNewState);
    fprintf(yyTraceFILE,"%sStack:",yyTracePrompt);
    for(i=1; i<=yypParser->yyidx; i++)
      fprintf(yyTraceFILE," %s",yyTokenName[yypParser->yystack[i].major]);
    fprintf(yyTraceFILE,"\n");
  }
#endif
}

/* The following table contains information about every rule that
** is used during the reduce.
*/
static const struct {
  YYCODETYPE lhs;         /* Symbol on the left-hand side of the rule */
  unsigned char nrhs;     /* Number of right-hand side symbols in the rule */
} yyRuleInfo[] = {
  { 136, 2 },
  { 137, 0 },
  { 137, 2 },
  { 137, 2 },
  { 138, 1 },
  { 138, 1 },
  { 138, 1 },
  { 138, 1 },
  { 138, 1 },
  { 138, 1 },
  { 138, 1 },
  { 138, 1 },
  { 138, 1 },
  { 138, 1 },
  { 138, 1 },
  { 138, 1 },
  { 138, 1 },
  { 138, 1 },
  { 153, 1 },
  { 153, 1 },
  { 154, 1 },
  { 154, 1 },
  { 154, 1 },
  { 155, 4 },
  { 155, 1 },
  { 166, 1 },
  { 166, 4 },
  { 156, 4 },
  { 156, 1 },
  { 157, 1 },
  { 156, 1 },
  { 158, 1 },
  { 159, 4 },
  { 159, 1 },
  { 160, 4 },
  { 160, 1 },
  { 161, 1 },
  { 161, 3 },
  { 163, 4 },
  { 163, 1 },
  { 164, 1 },
  { 164, 3 },
  { 162, 1 },
  { 162, 1 },
  { 162, 1 },
  { 162, 3 },
  { 162, 1 },
  { 162, 1 },
  { 162, 1 },
  { 162, 1 },
  { 162, 1 },
  { 162, 2 },
  { 162, 2 },
  { 162, 3 },
  { 162, 3 },
  { 162, 3 },
  { 162, 3 },
  { 162, 3 },
  { 167, 1 },
  { 167, 1 },
  { 167, 1 },
  { 167, 3 },
  { 167, 1 },
  { 167, 1 },
  { 167, 1 },
  { 167, 2 },
  { 167, 2 },
  { 168, 2 },
  { 167, 3 },
  { 167, 3 },
  { 167, 3 },
  { 167, 3 },
  { 167, 3 },
  { 167, 3 },
  { 167, 3 },
  { 167, 3 },
  { 167, 3 },
  { 167, 3 },
  { 167, 3 },
  { 167, 3 },
  { 167, 3 },
  { 167, 3 },
  { 167, 3 },
  { 169, 1 },
  { 169, 1 },
  { 169, 1 },
  { 169, 3 },
  { 169, 1 },
  { 169, 1 },
  { 169, 1 },
  { 169, 1 },
  { 169, 2 },
  { 169, 2 },
  { 169, 3 },
  { 169, 3 },
  { 169, 3 },
  { 169, 3 },
  { 169, 3 },
  { 165, 1 },
  { 165, 1 },
  { 165, 1 },
  { 165, 3 },
  { 165, 1 },
  { 165, 1 },
  { 165, 1 },
  { 165, 2 },
  { 165, 2 },
  { 165, 3 },
  { 165, 3 },
  { 165, 3 },
  { 165, 3 },
  { 165, 3 },
  { 170, 3 },
  { 171, 1 },
  { 171, 3 },
  { 171, 2 },
  { 171, 2 },
  { 171, 3 },
  { 171, 3 },
  { 171, 3 },
  { 171, 3 },
  { 171, 3 },
  { 172, 1 },
  { 172, 3 },
  { 172, 2 },
  { 172, 2 },
  { 172, 3 },
  { 172, 3 },
  { 172, 3 },
  { 172, 3 },
  { 172, 3 },
  { 172, 3 },
  { 173, 1 },
  { 173, 1 },
  { 173, 1 },
  { 173, 1 },
  { 173, 1 },
  { 173, 1 },
  { 174, 3 },
  { 174, 3 },
  { 174, 3 },
  { 174, 3 },
  { 174, 3 },
  { 174, 3 },
  { 174, 3 },
  { 174, 3 },
  { 174, 3 },
  { 174, 3 },
  { 174, 3 },
  { 174, 3 },
  { 174, 3 },
  { 174, 3 },
  { 174, 3 },
  { 174, 3 },
  { 174, 3 },
  { 174, 3 },
  { 174, 3 },
  { 174, 3 },
  { 175, 1 },
  { 175, 2 },
  { 175, 3 },
  { 178, 1 },
  { 178, 1 },
  { 178, 2 },
  { 178, 3 },
  { 179, 1 },
  { 179, 3 },
  { 179, 2 },
  { 179, 2 },
  { 179, 3 },
  { 179, 3 },
  { 179, 3 },
  { 179, 3 },
  { 179, 3 },
  { 179, 3 },
  { 180, 1 },
  { 180, 1 },
  { 180, 1 },
  { 181, 3 },
  { 181, 3 },
  { 181, 3 },
  { 181, 3 },
  { 181, 3 },
  { 181, 3 },
  { 181, 3 },
  { 182, 1 },
  { 182, 2 },
  { 182, 3 },
  { 183, 1 },
  { 183, 3 },
  { 183, 2 },
  { 183, 2 },
  { 183, 3 },
  { 183, 3 },
  { 183, 3 },
  { 183, 3 },
  { 183, 3 },
  { 183, 3 },
  { 183, 3 },
  { 176, 5 },
  { 184, 2 },
  { 184, 3 },
  { 185, 1 },
  { 185, 1 },
  { 177, 4 },
  { 177, 5 },
  { 177, 5 },
  { 177, 6 },
  { 177, 3 },
  { 177, 4 },
  { 177, 4 },
  { 177, 5 },
  { 186, 2 },
  { 187, 1 },
  { 187, 3 },
  { 188, 1 },
  { 188, 1 },
  { 188, 1 },
  { 188, 1 },
  { 188, 1 },
  { 189, 1 },
  { 189, 2 },
  { 190, 4 },
  { 190, 5 },
  { 190, 5 },
  { 190, 6 },
  { 190, 3 },
  { 190, 4 },
  { 190, 4 },
  { 190, 5 },
  { 139, 4 },
  { 191, 1 },
  { 191, 3 },
  { 192, 6 },
  { 192, 3 },
  { 193, 1 },
  { 193, 3 },
  { 194, 1 },
  { 194, 1 },
  { 195, 1 },
  { 195, 3 },
  { 196, 1 },
  { 196, 2 },
  { 196, 2 },
  { 196, 3 },
  { 196, 3 },
  { 196, 3 },
  { 197, 1 },
  { 197, 1 },
  { 198, 1 },
  { 199, 1 },
  { 140, 4 },
  { 200, 1 },
  { 200, 3 },
  { 201, 6 },
  { 201, 3 },
  { 201, 3 },
  { 201, 5 },
  { 201, 8 },
  { 202, 1 },
  { 202, 4 },
  { 202, 3 },
  { 202, 6 },
  { 203, 1 },
  { 203, 1 },
  { 203, 1 },
  { 203, 1 },
  { 203, 1 },
  { 203, 1 },
  { 203, 1 },
  { 203, 1 },
  { 203, 1 },
  { 203, 1 },
  { 203, 1 },
  { 204, 1 },
  { 204, 4 },
  { 141, 4 },
  { 205, 1 },
  { 205, 3 },
  { 206, 3 },
  { 207, 1 },
  { 207, 3 },
  { 208, 1 },
  { 208, 4 },
  { 208, 1 },
  { 208, 1 },
  { 142, 4 },
  { 209, 1 },
  { 209, 3 },
  { 210, 3 },
  { 211, 1 },
  { 211, 3 },
  { 143, 4 },
  { 212, 1 },
  { 212, 3 },
  { 213, 1 },
  { 213, 3 },
  { 213, 3 },
  { 213, 3 },
  { 214, 1 },
  { 214, 3 },
  { 146, 4 },
  { 146, 4 },
  { 147, 4 },
  { 147, 4 },
  { 215, 1 },
  { 215, 3 },
  { 216, 1 },
  { 148, 2 },
  { 149, 2 },
  { 150, 5 },
  { 151, 5 },
  { 152, 4 },
  { 217, 1 },
  { 217, 1 },
  { 217, 1 },
  { 217, 3 },
  { 217, 3 },
  { 217, 3 },
  { 218, 3 },
  { 218, 3 },
  { 219, 3 },
  { 219, 3 },
  { 221, 2 },
  { 221, 0 },
  { 222, 2 },
  { 222, 0 },
  { 223, 2 },
  { 223, 0 },
  { 224, 2 },
  { 224, 0 },
  { 225, 2 },
  { 225, 0 },
  { 145, 1 },
  { 145, 1 },
  { 145, 1 },
  { 145, 1 },
  { 145, 1 },
  { 145, 1 },
  { 145, 1 },
  { 145, 1 },
  { 145, 1 },
  { 145, 1 },
  { 145, 1 },
  { 145, 1 },
  { 145, 1 },
  { 145, 1 },
  { 145, 1 },
  { 145, 1 },
  { 145, 1 },
  { 145, 1 },
  { 226, 7 },
  { 227, 8 },
  { 228, 8 },
  { 229, 5 },
  { 230, 7 },
  { 231, 9 },
  { 232, 9 },
  { 233, 7 },
  { 234, 6 },
  { 235, 6 },
  { 236, 6 },
  { 237, 6 },
  { 238, 8 },
  { 239, 8 },
  { 240, 8 },
  { 241, 6 },
  { 242, 4 },
  { 243, 5 },
  { 144, 1 },
  { 144, 1 },
  { 144, 1 },
  { 144, 1 },
  { 144, 1 },
  { 144, 1 },
};

/*
** Flags that a syntax error has occurred. 
*/
#define YYERROR { yypParser->yysyntaxerr = 1; yypParser->yyerrcnt = 3; }

static void yy_accept(yyParser*);  /* Forward Declaration */


/*
** Perform a reduce action and the shift that must immediately
** follow the reduce.
*/
static void yy_reduce(
  yyParser *yypParser,         /* The parser */
  int yyruleno                 /* Number of the rule by which to reduce */
){
  int yygoto;                     /* The next state */
  int yyact;                      /* The next action */
  YYMINORTYPE yygotominor;        /* The LHS of the rule reduced */
  yyStackEntry *yymsp;            /* The top of the parser's stack */
  int yysize;                     /* Amount to pop the stack */
  lemon_parserARG_FETCH;
  yymsp = &yypParser->yystack[yypParser->yyidx];
#ifndef NDEBUG
  if( yyTraceFILE && yyruleno>=0 
        && yyruleno<(int)(sizeof(yyRuleName)/sizeof(yyRuleName[0])) ){
    fprintf(yyTraceFILE, "%sReduce [%s].\n", yyTracePrompt,
      yyRuleName[yyruleno]);
  }
#endif /* NDEBUG */

  /* Silence complaints from purify about yygotominor being uninitialized
  ** in some cases when it is copied into the stack after the following
  ** switch.  yygotominor is uninitialized when a rule reduces that does
  ** not set the value of its left-hand side nonterminal.  Leaving the
  ** value of the nonterminal uninitialized is utterly harmless as long
  ** as the value is never used.  So really the only thing this code
  ** accomplishes is to quieten purify.  
  **
  ** 2007-01-16:  The wireshark project (www.wireshark.org) reports that
  ** without this code, their parser segfaults.  I'm not sure what there
  ** parser is doing to make this happen.  This is the second bug report
  ** from wireshark this week.  Clearly they are stressing Lemon in ways
  ** that it has not been previously stressed...  (SQLite ticket #2172)
  */
  /*memset(&yygotominor, 0, sizeof(yygotominor));*/
  yygotominor = yyzerominor;

  switch( yyruleno ){
  /* Beginning here are the reduction cases.  A typical example
  ** follows:
  **   case 0:
  **  #line <lineno> <grammarfile>
  **     { ... }           // User supplied code
  **  #line <lineno> <thisfile>
  **     break;
  */
      case 0: /* start ::= statement_lst EOF */
#line 213 "bcplus/parser/detail/lemon_parser.y"
{
  yy_destructor(yypParser,118,&yymsp[0].minor);
}
#line 3417 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 3: /* statement_lst ::= statement_lst statement */
#line 218 "bcplus/parser/detail/lemon_parser.y"
{
			ref_ptr<Statement> ptr = yymsp[0].minor.yy224;
			yymsp[0].minor.yy224  = NULL;
			parser->_handle_stmt(ptr);
		}
#line 3426 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 4: /* statement ::= stmt_macro_def */
#line 261 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy224 = yymsp[0].minor.yy455; }
#line 3431 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 5: /* statement ::= stmt_constant_def */
#line 262 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy224 = yymsp[0].minor.yy55; }
#line 3436 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 6: /* statement ::= stmt_object_def */
#line 263 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy224 = yymsp[0].minor.yy296; }
#line 3441 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 7: /* statement ::= stmt_variable_def */
#line 264 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy224 = yymsp[0].minor.yy171; }
#line 3446 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 8: /* statement ::= stmt_sort_def */
#line 265 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy224 = yymsp[0].minor.yy469; }
#line 3451 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 9: /* statement ::= stmt_code_blk */
      case 10: /* statement ::= stmt_law */ yytestcase(yyruleno==10);
      case 11: /* statement ::= stmt_show */ yytestcase(yyruleno==11);
      case 12: /* statement ::= stmt_hide */ yytestcase(yyruleno==12);
      case 15: /* statement ::= stmt_maxafvalue */ yytestcase(yyruleno==15);
      case 16: /* statement ::= stmt_maxadditive */ yytestcase(yyruleno==16);
#line 266 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy224 = yymsp[0].minor.yy224; }
#line 3461 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 13: /* statement ::= stmt_noconcurrency */
#line 270 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy224 = yymsp[0].minor.yy210; }
#line 3466 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 14: /* statement ::= stmt_strong_noconcurrency */
#line 271 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy224 = yymsp[0].minor.yy482; }
#line 3471 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 17: /* statement ::= stmt_query */
#line 274 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy224 = yymsp[0].minor.yy146; }
#line 3476 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 18: /* base_elem ::= constant */
#line 320 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy163 = yymsp[0].minor.yy345; }
#line 3481 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 19: /* base_elem ::= base_elem_no_const */
      case 42: /* term ::= base_elem */ yytestcase(yyruleno==42);
      case 58: /* term_strong ::= base_elem_no_const */ yytestcase(yyruleno==58);
      case 83: /* term_no_const_strong ::= base_elem_no_const */ yytestcase(yyruleno==83);
      case 98: /* term_no_const ::= base_elem_no_const */ yytestcase(yyruleno==98);
#line 321 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy163 = yymsp[0].minor.yy163; }
#line 3490 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 20: /* base_elem_no_const ::= object */
#line 323 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy163 = yymsp[0].minor.yy238;	}
#line 3495 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 21: /* base_elem_no_const ::= variable */
#line 324 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy163 = yymsp[0].minor.yy453; }
#line 3500 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 22: /* base_elem_no_const ::= lua */
#line 325 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy163 = yymsp[0].minor.yy313; }
#line 3505 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 23: /* constant ::= CONSTANT_ID PAREN_L term_lst PAREN_R */
      case 38: /* constant_one_const ::= CONSTANT_ID PAREN_L term_no_const_lst PAREN_R */ yytestcase(yyruleno==38);
#line 442 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy345, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy187, yymsp[0].minor.yy0, Symbol::Type::CONSTANT, Constant, ConstantSymbol);	}
#line 3511 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 24: /* constant ::= CONSTANT_ID */
      case 39: /* constant_one_const ::= CONSTANT_ID */ yytestcase(yyruleno==39);
#line 443 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy345, yymsp[0].minor.yy0, NULL, NULL, NULL, Symbol::Type::CONSTANT, Constant, ConstantSymbol); }
#line 3517 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 25: /* const_anon ::= IDENTIFIER */
#line 445 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF9(yygotominor.yy345, yymsp[0].minor.yy0, NULL, NULL, NULL, Symbol::Type::CONSTANT, Constant, ConstantSymbol, true); }
#line 3522 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 26: /* const_anon ::= IDENTIFIER PAREN_L term_lst PAREN_R */
#line 446 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF9(yygotominor.yy345, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy187, yymsp[0].minor.yy0, Symbol::Type::CONSTANT, Constant, ConstantSymbol, true);	}
#line 3527 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 27: /* object ::= OBJECT_ID PAREN_L term_lst PAREN_R */
#line 449 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy238, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy187, yymsp[0].minor.yy0, Symbol::Type::OBJECT, Object, ObjectSymbol);	}
#line 3532 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 28: /* object ::= object_nullary */
#line 450 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy238 = yymsp[0].minor.yy238; }
#line 3537 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 29: /* object_nullary ::= OBJECT_ID */
#line 451 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy238, yymsp[0].minor.yy0, NULL, NULL, NULL, Symbol::Type::OBJECT, Object, ObjectSymbol); }
#line 3542 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 30: /* object ::= undeclared */
#line 452 "bcplus/parser/detail/lemon_parser.y"
{ /* never called */ }
#line 3547 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 31: /* variable ::= VARIABLE_ID */
#line 455 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy453 = NULL;
		ref_ptr<const Token> id_ptr = yymsp[0].minor.yy0;
		
		if (!parser->lang()->support(Language::Feature::VARIABLE)) {
			parser->_feature_error(Language::Feature::VARIABLE, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else {
			BASE_ELEM_BARE_DEF(yygotominor.yy453, yymsp[0].minor.yy0, Symbol::Type::VARIABLE, Variable, VariableSymbol); 
		}
	}
#line 3562 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 32: /* lua ::= AT_IDENTIFIER PAREN_L term_lst PAREN_R */
#line 466 "bcplus/parser/detail/lemon_parser.y"
{ BASE_LUA_ELEM(yygotominor.yy313, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy187, yymsp[0].minor.yy0); }
#line 3567 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 33: /* lua ::= AT_IDENTIFIER */
#line 467 "bcplus/parser/detail/lemon_parser.y"
{ BASE_LUA_ELEM(yygotominor.yy313, yymsp[0].minor.yy0, NULL, NULL, NULL); }
#line 3572 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 34: /* undeclared ::= IDENTIFIER PAREN_L term_lst PAREN_R */
#line 468 "bcplus/parser/detail/lemon_parser.y"
{ UNDECLARED(yygotominor.yy209, yymsp[-3].minor.yy0, yymsp[-1].minor.yy187);   yy_destructor(yypParser,71,&yymsp[-2].minor);
  yy_destructor(yypParser,72,&yymsp[0].minor);
}
#line 3579 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 35: /* undeclared ::= IDENTIFIER */
#line 469 "bcplus/parser/detail/lemon_parser.y"
{ UNDECLARED(yygotominor.yy209, yymsp[0].minor.yy0, NULL); }
#line 3584 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 36: /* term_lst ::= term */
      case 40: /* term_no_const_lst ::= term_no_const */ yytestcase(yyruleno==40);
#line 472 "bcplus/parser/detail/lemon_parser.y"
{
			yygotominor.yy187 = new TermList();
			yygotominor.yy187->push_back(yymsp[0].minor.yy163);
		}
#line 3593 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 37: /* term_lst ::= term_lst COMMA term */
      case 41: /* term_no_const_lst ::= term_no_const_lst COMMA term_no_const */ yytestcase(yyruleno==41);
#line 478 "bcplus/parser/detail/lemon_parser.y"
{
			yygotominor.yy187 = yymsp[-2].minor.yy187;
			yymsp[-2].minor.yy187->push_back(yymsp[0].minor.yy163);
		  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 3603 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 43: /* term ::= INTEGER */
      case 59: /* term_strong ::= INTEGER */ yytestcase(yyruleno==59);
      case 84: /* term_no_const_strong ::= INTEGER */ yytestcase(yyruleno==84);
      case 99: /* term_no_const ::= INTEGER */ yytestcase(yyruleno==99);
#line 577 "bcplus/parser/detail/lemon_parser.y"
{ BASIC_TERM(yygotominor.yy163, yymsp[0].minor.yy0);	}
#line 3611 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 44: /* term ::= STRING_LITERAL */
      case 46: /* term ::= TRUE */ yytestcase(yyruleno==46);
      case 47: /* term ::= FALSE */ yytestcase(yyruleno==47);
      case 60: /* term_strong ::= STRING_LITERAL */ yytestcase(yyruleno==60);
      case 85: /* term_no_const_strong ::= STRING_LITERAL */ yytestcase(yyruleno==85);
      case 100: /* term_no_const ::= STRING_LITERAL */ yytestcase(yyruleno==100);
      case 102: /* term_no_const ::= TRUE */ yytestcase(yyruleno==102);
      case 103: /* term_no_const ::= FALSE */ yytestcase(yyruleno==103);
#line 578 "bcplus/parser/detail/lemon_parser.y"
{ BASIC_TERM(yygotominor.yy163, yymsp[0].minor.yy0); }
#line 3623 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 45: /* term ::= PAREN_L term PAREN_R */
      case 61: /* term_strong ::= PAREN_L term_strong PAREN_R */ yytestcase(yyruleno==61);
      case 86: /* term_no_const_strong ::= PAREN_L term_no_const_strong PAREN_R */ yytestcase(yyruleno==86);
      case 101: /* term_no_const ::= PAREN_L term_no_const PAREN_R */ yytestcase(yyruleno==101);
#line 579 "bcplus/parser/detail/lemon_parser.y"
{ TERM_PARENS(yygotominor.yy163, yymsp[-2].minor.yy0, yymsp[-1].minor.yy163, yymsp[0].minor.yy0); }
#line 3631 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 48: /* term ::= MAXSTEP */
      case 62: /* term_strong ::= MAXSTEP */ yytestcase(yyruleno==62);
      case 87: /* term_no_const_strong ::= MAXSTEP */ yytestcase(yyruleno==87);
#line 582 "bcplus/parser/detail/lemon_parser.y"
{ NULLARY_TERM(yygotominor.yy163, yymsp[0].minor.yy0, Language::Feature::MAXSTEP, NullaryTerm::Operator::MAXSTEP); }
#line 3638 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 49: /* term ::= MAXADDITIVE */
      case 63: /* term_strong ::= MAXADDITIVE */ yytestcase(yyruleno==63);
      case 88: /* term_no_const_strong ::= MAXADDITIVE */ yytestcase(yyruleno==88);
#line 583 "bcplus/parser/detail/lemon_parser.y"
{ NULLARY_TERM(yygotominor.yy163, yymsp[0].minor.yy0, Language::Feature::MAXADDITIVE, NullaryTerm::Operator::MAXADDITIVE); }
#line 3645 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 50: /* term ::= MAXAFVALUE */
      case 64: /* term_strong ::= MAXAFVALUE */ yytestcase(yyruleno==64);
      case 89: /* term_no_const_strong ::= MAXAFVALUE */ yytestcase(yyruleno==89);
#line 584 "bcplus/parser/detail/lemon_parser.y"
{ NULLARY_TERM(yygotominor.yy163, yymsp[0].minor.yy0, Language::Feature::MAXAFVALUE, NullaryTerm::Operator::MAXAFVALUE); }
#line 3652 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 51: /* term ::= DASH term */
      case 65: /* term_strong ::= DASH term_strong */ yytestcase(yyruleno==65);
      case 91: /* term_no_const_strong ::= DASH term_no_const_strong */ yytestcase(yyruleno==91);
      case 105: /* term_no_const ::= DASH term_no_const */ yytestcase(yyruleno==105);
#line 588 "bcplus/parser/detail/lemon_parser.y"
{ UNARY_ARITH(yygotominor.yy163, yymsp[-1].minor.yy0, yymsp[0].minor.yy163, UnaryTerm::Operator::NEGATIVE); }
#line 3660 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 52: /* term ::= ABS term */
      case 66: /* term_strong ::= ABS term */ yytestcase(yyruleno==66);
      case 92: /* term_no_const_strong ::= ABS term_no_const */ yytestcase(yyruleno==92);
      case 106: /* term_no_const ::= ABS term_no_const */ yytestcase(yyruleno==106);
#line 589 "bcplus/parser/detail/lemon_parser.y"
{ UNARY_ARITH(yygotominor.yy163, yymsp[-1].minor.yy0, yymsp[0].minor.yy163, UnaryTerm::Operator::ABS); }
#line 3668 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 53: /* term ::= term DASH term */
      case 68: /* term_strong ::= term_strong_candidate DASH term */ yytestcase(yyruleno==68);
      case 78: /* term_strong ::= term_strong DASH term */ yytestcase(yyruleno==78);
      case 93: /* term_no_const_strong ::= term_no_const_strong DASH term_no_const */ yytestcase(yyruleno==93);
      case 107: /* term_no_const ::= term_no_const DASH term_no_const */ yytestcase(yyruleno==107);
#line 593 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy163, yymsp[-2].minor.yy163, yymsp[-1].minor.yy0, yymsp[0].minor.yy163, BinaryTerm::Operator::MINUS); }
#line 3677 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 54: /* term ::= term PLUS term */
      case 69: /* term_strong ::= term_strong_candidate PLUS term */ yytestcase(yyruleno==69);
      case 79: /* term_strong ::= term_strong PLUS term */ yytestcase(yyruleno==79);
      case 94: /* term_no_const_strong ::= term_no_const_strong PLUS term_no_const */ yytestcase(yyruleno==94);
      case 108: /* term_no_const ::= term_no_const PLUS term_no_const */ yytestcase(yyruleno==108);
#line 594 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy163, yymsp[-2].minor.yy163, yymsp[-1].minor.yy0, yymsp[0].minor.yy163, BinaryTerm::Operator::PLUS); }
#line 3686 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 55: /* term ::= term STAR term */
      case 70: /* term_strong ::= term_strong_candidate STAR term */ yytestcase(yyruleno==70);
      case 80: /* term_strong ::= term_strong STAR term */ yytestcase(yyruleno==80);
      case 95: /* term_no_const_strong ::= term_no_const_strong STAR term_no_const */ yytestcase(yyruleno==95);
      case 109: /* term_no_const ::= term_no_const STAR term_no_const */ yytestcase(yyruleno==109);
#line 595 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy163, yymsp[-2].minor.yy163, yymsp[-1].minor.yy0, yymsp[0].minor.yy163, BinaryTerm::Operator::TIMES); }
#line 3695 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 56: /* term ::= term INT_DIV term */
      case 71: /* term_strong ::= term_strong_candidate INT_DIV term */ yytestcase(yyruleno==71);
      case 81: /* term_strong ::= term_strong INT_DIV term */ yytestcase(yyruleno==81);
      case 96: /* term_no_const_strong ::= term_no_const_strong INT_DIV term_no_const */ yytestcase(yyruleno==96);
      case 110: /* term_no_const ::= term_no_const INT_DIV term_no_const */ yytestcase(yyruleno==110);
#line 596 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy163, yymsp[-2].minor.yy163, yymsp[-1].minor.yy0, yymsp[0].minor.yy163, BinaryTerm::Operator::DIVIDE); }
#line 3704 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 57: /* term ::= term MOD term */
      case 72: /* term_strong ::= term_strong_candidate MOD term */ yytestcase(yyruleno==72);
      case 82: /* term_strong ::= term_strong MOD term */ yytestcase(yyruleno==82);
      case 97: /* term_no_const_strong ::= term_no_const_strong MOD term_no_const */ yytestcase(yyruleno==97);
      case 111: /* term_no_const ::= term_no_const MOD term_no_const */ yytestcase(yyruleno==111);
#line 597 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy163, yymsp[-2].minor.yy163, yymsp[-1].minor.yy0, yymsp[0].minor.yy163, BinaryTerm::Operator::MOD); }
#line 3713 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 67: /* term_strong_candidate ::= DASH constant */
#line 616 "bcplus/parser/detail/lemon_parser.y"
{ UNARY_ARITH(yygotominor.yy163, yymsp[-1].minor.yy0, yymsp[0].minor.yy345, UnaryTerm::Operator::NEGATIVE); }
#line 3718 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 73: /* term_strong ::= constant DASH term */
#line 625 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy163, yymsp[-2].minor.yy345, yymsp[-1].minor.yy0, yymsp[0].minor.yy163, BinaryTerm::Operator::MINUS); }
#line 3723 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 74: /* term_strong ::= constant PLUS term */
#line 626 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy163, yymsp[-2].minor.yy345, yymsp[-1].minor.yy0, yymsp[0].minor.yy163, BinaryTerm::Operator::PLUS); }
#line 3728 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 75: /* term_strong ::= constant STAR term */
#line 627 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy163, yymsp[-2].minor.yy345, yymsp[-1].minor.yy0, yymsp[0].minor.yy163, BinaryTerm::Operator::TIMES); }
#line 3733 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 76: /* term_strong ::= constant INT_DIV term */
#line 628 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy163, yymsp[-2].minor.yy345, yymsp[-1].minor.yy0, yymsp[0].minor.yy163, BinaryTerm::Operator::DIVIDE); }
#line 3738 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 77: /* term_strong ::= constant MOD term */
#line 629 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy163, yymsp[-2].minor.yy345, yymsp[-1].minor.yy0, yymsp[0].minor.yy163, BinaryTerm::Operator::MOD); }
#line 3743 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 90: /* term_no_const_strong ::= constant */
#line 651 "bcplus/parser/detail/lemon_parser.y"
{
		// error handling for constants so they don'yygotominor.yy163 default to undeclared identifiers
		yygotominor.yy163 = NULL;
		ref_ptr<const Referenced> c_ptr = yymsp[0].minor.yy345;
		parser->_parse_error("Encountered unexpected constant symbol.", &yymsp[0].minor.yy345->beginLoc());
		YYERROR;
	}
#line 3754 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 104: /* term_no_const ::= constant */
#line 681 "bcplus/parser/detail/lemon_parser.y"
{
		// error handline for constants so they don'yygotominor.yy163 default to undeclared identifiers
		yygotominor.yy163 = NULL;
		ref_ptr<const Referenced> c_ptr = yymsp[0].minor.yy345;
		parser->_parse_error("Encountered unexpected constant symbol.", &yymsp[0].minor.yy345->beginLoc());
		YYERROR;
	}
#line 3765 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 112: /* num_range ::= term_numeric DBL_PERIOD term_numeric */
#line 711 "bcplus/parser/detail/lemon_parser.y"
{
	ref_ptr<const Referenced> l_ptr = yymsp[-2].minor.yy416, r_ptr = yymsp[0].minor.yy416, s_ptr = yymsp[-1].minor.yy0;

	yygotominor.yy309 = new NumberRange(yymsp[-2].minor.yy416->val(), yymsp[0].minor.yy416->val(), yymsp[-2].minor.yy416->beginLoc(), yymsp[0].minor.yy416->endLoc());

}
#line 3775 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 113: /* term_numeric ::= INTEGER */
#line 719 "bcplus/parser/detail/lemon_parser.y"
{
	ref_ptr<const Referenced> i_ptr = yymsp[0].minor.yy0;

	yygotominor.yy416 = 0;
	try {
		yygotominor.yy416 = new Number(boost::lexical_cast<int>(*yymsp[0].minor.yy0->str()), yymsp[0].minor.yy0->beginLoc());

	} catch (boost::bad_lexical_cast const& e) {
		parser->_parse_error("INTERNAL ERROR: Failed to parse integer \"" + *yymsp[0].minor.yy0->str() + "\".", &yymsp[0].minor.yy0->beginLoc());
		YYERROR;
	}
}
#line 3791 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 114: /* term_numeric ::= PAREN_L term_numeric PAREN_R */
#line 732 "bcplus/parser/detail/lemon_parser.y"
{ 
	ref_ptr<const Referenced> pl_ptr = yymsp[-2].minor.yy0, pr_ptr = yymsp[0].minor.yy0;
	yygotominor.yy416 = yymsp[-1].minor.yy416;  
	yygotominor.yy416->beginLoc(yymsp[-2].minor.yy0->beginLoc());
	yygotominor.yy416->endLoc(yymsp[0].minor.yy0->endLoc());
}
#line 3801 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 115: /* term_numeric ::= DASH term_numeric */
#line 752 "bcplus/parser/detail/lemon_parser.y"
{ NUM_UOP(yygotominor.yy416, yymsp[0].minor.yy416, -1 * yymsp[0].minor.yy416->val());   yy_destructor(yypParser,105,&yymsp[-1].minor);
}
#line 3807 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 116: /* term_numeric ::= ABS term_numeric */
#line 753 "bcplus/parser/detail/lemon_parser.y"
{ NUM_UOP(yygotominor.yy416, yymsp[0].minor.yy416, yymsp[0].minor.yy416->val() < 0 ? - yymsp[0].minor.yy416->val() : yymsp[0].minor.yy416->val());   yy_destructor(yypParser,110,&yymsp[-1].minor);
}
#line 3813 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 117: /* term_numeric ::= term_numeric DASH term_numeric */
#line 755 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy416, yymsp[-2].minor.yy416, yymsp[0].minor.yy416, yymsp[-2].minor.yy416->val() - yymsp[0].minor.yy416->val());   yy_destructor(yypParser,105,&yymsp[-1].minor);
}
#line 3819 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 118: /* term_numeric ::= term_numeric PLUS term_numeric */
#line 756 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy416, yymsp[-2].minor.yy416, yymsp[0].minor.yy416, yymsp[-2].minor.yy416->val() + yymsp[0].minor.yy416->val());   yy_destructor(yypParser,106,&yymsp[-1].minor);
}
#line 3825 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 119: /* term_numeric ::= term_numeric STAR term_numeric */
#line 757 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy416, yymsp[-2].minor.yy416, yymsp[0].minor.yy416, yymsp[-2].minor.yy416->val() * yymsp[0].minor.yy416->val());   yy_destructor(yypParser,107,&yymsp[-1].minor);
}
#line 3831 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 120: /* term_numeric ::= term_numeric INT_DIV term_numeric */
#line 758 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy416, yymsp[-2].minor.yy416, yymsp[0].minor.yy416, yymsp[-2].minor.yy416->val() / yymsp[0].minor.yy416->val());   yy_destructor(yypParser,108,&yymsp[-1].minor);
}
#line 3837 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 121: /* term_numeric ::= term_numeric MOD term_numeric */
#line 759 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy416, yymsp[-2].minor.yy416, yymsp[0].minor.yy416, yymsp[-2].minor.yy416->val() % yymsp[0].minor.yy416->val());   yy_destructor(yypParser,109,&yymsp[-1].minor);
}
#line 3843 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 122: /* formula ::= formula_base */
      case 165: /* formula_no_const ::= formula_no_const_base */ yytestcase(yyruleno==165);
      case 188: /* formula_temporal ::= formula_base */ yytestcase(yyruleno==188);
#line 817 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy353 = yymsp[0].minor.yy353;				}
#line 3850 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 123: /* formula ::= PAREN_L formula PAREN_R */
      case 166: /* formula_no_const ::= PAREN_L formula_no_const PAREN_R */ yytestcase(yyruleno==166);
      case 189: /* formula_temporal ::= PAREN_L formula_temporal PAREN_R */ yytestcase(yyruleno==189);
#line 818 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy353 = yymsp[-1].minor.yy353; yygotominor.yy353->parens(true); 	  yy_destructor(yypParser,71,&yymsp[-2].minor);
  yy_destructor(yypParser,72,&yymsp[0].minor);
}
#line 3859 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 124: /* formula ::= NOT formula */
      case 167: /* formula_no_const ::= NOT formula_no_const */ yytestcase(yyruleno==167);
      case 190: /* formula_temporal ::= NOT formula_temporal */ yytestcase(yyruleno==190);
#line 819 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_UOP(yygotominor.yy353, yymsp[-1].minor.yy0, yymsp[0].minor.yy353, UnaryFormula::Operator::NOT, Language::Feature::FORMULA_NOT_KEYWORD); }
#line 3866 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 125: /* formula ::= DASH formula */
      case 168: /* formula_no_const ::= DASH formula_no_const */ yytestcase(yyruleno==168);
      case 191: /* formula_temporal ::= DASH formula_temporal */ yytestcase(yyruleno==191);
#line 820 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_UOP(yygotominor.yy353, yymsp[-1].minor.yy0, yymsp[0].minor.yy353, UnaryFormula::Operator::NOT, Language::Feature::FORMULA_NOT_DASH); }
#line 3873 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 126: /* formula ::= formula AMP formula */
      case 169: /* formula_no_const ::= formula_no_const AMP formula_no_const */ yytestcase(yyruleno==169);
      case 192: /* formula_temporal ::= formula_temporal AMP formula_temporal */ yytestcase(yyruleno==192);
#line 821 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy353 = new BinaryFormula(BinaryFormula::Operator::AND, yymsp[-2].minor.yy353, yymsp[0].minor.yy353, yymsp[-2].minor.yy353->beginLoc(), yymsp[0].minor.yy353->endLoc());   yy_destructor(yypParser,101,&yymsp[-1].minor);
}
#line 3881 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 127: /* formula ::= formula DBL_PLUS formula */
      case 128: /* formula ::= formula PIPE formula */ yytestcase(yyruleno==128);
      case 170: /* formula_no_const ::= formula_no_const DBL_PLUS formula_no_const */ yytestcase(yyruleno==170);
      case 171: /* formula_no_const ::= formula_no_const PIPE formula_no_const */ yytestcase(yyruleno==171);
      case 193: /* formula_temporal ::= formula_temporal DBL_PLUS formula_temporal */ yytestcase(yyruleno==193);
      case 194: /* formula_temporal ::= formula_temporal PIPE formula_temporal */ yytestcase(yyruleno==194);
#line 822 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_BOP(yygotominor.yy353, yymsp[-2].minor.yy353, yymsp[-1].minor.yy0, yymsp[0].minor.yy353, BinaryFormula::Operator::OR); }
#line 3891 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 129: /* formula ::= formula EQUIV formula */
      case 172: /* formula_no_const ::= formula_no_const EQUIV formula_no_const */ yytestcase(yyruleno==172);
      case 195: /* formula_temporal ::= formula_temporal EQUIV formula_temporal */ yytestcase(yyruleno==195);
#line 824 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_BOP(yygotominor.yy353, yymsp[-2].minor.yy353, yymsp[-1].minor.yy0, yymsp[0].minor.yy353, BinaryFormula::Operator::EQUIV); }
#line 3898 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 130: /* formula ::= formula IMPL formula */
      case 131: /* formula ::= formula ARROW_RDASH formula */ yytestcase(yyruleno==131);
      case 173: /* formula_no_const ::= formula_no_const IMPL formula_no_const */ yytestcase(yyruleno==173);
      case 174: /* formula_no_const ::= formula_no_const ARROW_RDASH formula_no_const */ yytestcase(yyruleno==174);
      case 196: /* formula_temporal ::= formula_temporal IMPL formula_temporal */ yytestcase(yyruleno==196);
      case 197: /* formula_temporal ::= formula_temporal ARROW_RDASH formula_temporal */ yytestcase(yyruleno==197);
#line 825 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_BOP(yygotominor.yy353, yymsp[-2].minor.yy353, yymsp[-1].minor.yy0, yymsp[0].minor.yy353, BinaryFormula::Operator::IMPL); }
#line 3908 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 132: /* formula_base ::= comparison */
      case 175: /* formula_no_const_base ::= comparison_no_const */ yytestcase(yyruleno==175);
      case 215: /* head_formula ::= comparison */ yytestcase(yyruleno==215);
#line 828 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy353 = yymsp[0].minor.yy353; }
#line 3915 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 133: /* formula_base ::= atomic_formula */
      case 216: /* head_formula ::= atomic_head_formula */ yytestcase(yyruleno==216);
#line 829 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy353 = yymsp[0].minor.yy138; }
#line 3921 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 134: /* formula_base ::= formula_quant */
#line 830 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy353 = yymsp[0].minor.yy285; }
#line 3926 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 135: /* formula_base ::= formula_card */
#line 832 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy353 = yymsp[0].minor.yy353;
		if (!parser->lang()->support(Language::Feature::FORMULA_CARDINALITY_BODY)) {
			parser->_feature_error(Language::Feature::FORMULA_CARDINALITY_BODY, &yymsp[0].minor.yy353->beginLoc());
			YYERROR;
		}
	}
#line 3937 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 136: /* formula_base ::= TRUE */
      case 176: /* formula_no_const_base ::= TRUE */ yytestcase(yyruleno==176);
      case 218: /* head_formula ::= TRUE */ yytestcase(yyruleno==218);
#line 839 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy353 = new NullaryFormula(NullaryFormula::Operator::TRUE, yymsp[0].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc()); }
#line 3944 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 137: /* formula_base ::= FALSE */
      case 177: /* formula_no_const_base ::= FALSE */ yytestcase(yyruleno==177);
      case 219: /* head_formula ::= FALSE */ yytestcase(yyruleno==219);
#line 840 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy353 = new NullaryFormula(NullaryFormula::Operator::FALSE, yymsp[0].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc()); }
#line 3951 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 138: /* comparison ::= term_strong EQ term */
      case 145: /* comparison ::= term_strong_candidate EQ term */ yytestcase(yyruleno==145);
      case 178: /* comparison_no_const ::= term_no_const_strong EQ term_no_const */ yytestcase(yyruleno==178);
#line 842 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy353 = new ComparisonFormula(ComparisonFormula::Operator::EQ, yymsp[-2].minor.yy163, yymsp[0].minor.yy163, yymsp[-2].minor.yy163->beginLoc(), yymsp[0].minor.yy163->endLoc());   yy_destructor(yypParser,81,&yymsp[-1].minor);
}
#line 3959 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 139: /* comparison ::= term_strong DBL_EQ term */
      case 146: /* comparison ::= term_strong_candidate DBL_EQ term */ yytestcase(yyruleno==146);
      case 179: /* comparison_no_const ::= term_no_const_strong DBL_EQ term_no_const */ yytestcase(yyruleno==179);
#line 843 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy353 = new ComparisonFormula(ComparisonFormula::Operator::EQ, yymsp[-2].minor.yy163, yymsp[0].minor.yy163, yymsp[-2].minor.yy163->beginLoc(), yymsp[0].minor.yy163->endLoc());   yy_destructor(yypParser,82,&yymsp[-1].minor);
}
#line 3967 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 140: /* comparison ::= term_strong NEQ term */
      case 147: /* comparison ::= term_strong_candidate NEQ term */ yytestcase(yyruleno==147);
      case 180: /* comparison_no_const ::= term_no_const_strong NEQ term_no_const */ yytestcase(yyruleno==180);
#line 844 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy353 = new ComparisonFormula(ComparisonFormula::Operator::NEQ, yymsp[-2].minor.yy163, yymsp[0].minor.yy163, yymsp[-2].minor.yy163->beginLoc(), yymsp[0].minor.yy163->endLoc());   yy_destructor(yypParser,83,&yymsp[-1].minor);
}
#line 3975 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 141: /* comparison ::= term_strong LTHAN term */
      case 148: /* comparison ::= term_strong_candidate LTHAN term */ yytestcase(yyruleno==148);
      case 181: /* comparison_no_const ::= term_no_const_strong LTHAN term_no_const */ yytestcase(yyruleno==181);
#line 845 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy353 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN, yymsp[-2].minor.yy163, yymsp[0].minor.yy163, yymsp[-2].minor.yy163->beginLoc(), yymsp[0].minor.yy163->endLoc());   yy_destructor(yypParser,85,&yymsp[-1].minor);
}
#line 3983 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 142: /* comparison ::= term_strong GTHAN term */
      case 149: /* comparison ::= term_strong_candidate GTHAN term */ yytestcase(yyruleno==149);
      case 182: /* comparison_no_const ::= term_no_const_strong GTHAN term_no_const */ yytestcase(yyruleno==182);
#line 846 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy353 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN, yymsp[-2].minor.yy163, yymsp[0].minor.yy163, yymsp[-2].minor.yy163->beginLoc(), yymsp[0].minor.yy163->endLoc());   yy_destructor(yypParser,86,&yymsp[-1].minor);
}
#line 3991 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 143: /* comparison ::= term_strong LTHAN_EQ term */
      case 150: /* comparison ::= term_strong_candidate LTHAN_EQ term */ yytestcase(yyruleno==150);
      case 183: /* comparison_no_const ::= term_no_const_strong LTHAN_EQ term_no_const */ yytestcase(yyruleno==183);
#line 847 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy353 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN_EQ, yymsp[-2].minor.yy163, yymsp[0].minor.yy163, yymsp[-2].minor.yy163->beginLoc(), yymsp[0].minor.yy163->endLoc());   yy_destructor(yypParser,87,&yymsp[-1].minor);
}
#line 3999 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 144: /* comparison ::= term_strong GTHAN_EQ term */
      case 151: /* comparison ::= term_strong_candidate GTHAN_EQ term */ yytestcase(yyruleno==151);
      case 184: /* comparison_no_const ::= term_no_const_strong GTHAN_EQ term_no_const */ yytestcase(yyruleno==184);
#line 848 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy353 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN_EQ, yymsp[-2].minor.yy163, yymsp[0].minor.yy163, yymsp[-2].minor.yy163->beginLoc(), yymsp[0].minor.yy163->endLoc());   yy_destructor(yypParser,88,&yymsp[-1].minor);
}
#line 4007 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 152: /* comparison ::= constant DBL_EQ term */
#line 856 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy353 = new ComparisonFormula(ComparisonFormula::Operator::EQ, yymsp[-2].minor.yy345, yymsp[0].minor.yy163, yymsp[-2].minor.yy345->beginLoc(), yymsp[0].minor.yy163->endLoc());   yy_destructor(yypParser,82,&yymsp[-1].minor);
}
#line 4013 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 153: /* comparison ::= constant NEQ term */
#line 857 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy353 = new ComparisonFormula(ComparisonFormula::Operator::NEQ, yymsp[-2].minor.yy345, yymsp[0].minor.yy163, yymsp[-2].minor.yy345->beginLoc(), yymsp[0].minor.yy163->endLoc());   yy_destructor(yypParser,83,&yymsp[-1].minor);
}
#line 4019 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 154: /* comparison ::= constant LTHAN term */
#line 858 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy353 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN, yymsp[-2].minor.yy345, yymsp[0].minor.yy163, yymsp[-2].minor.yy345->beginLoc(), yymsp[0].minor.yy163->endLoc());   yy_destructor(yypParser,85,&yymsp[-1].minor);
}
#line 4025 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 155: /* comparison ::= constant GTHAN term */
#line 859 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy353 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN, yymsp[-2].minor.yy345, yymsp[0].minor.yy163, yymsp[-2].minor.yy345->beginLoc(), yymsp[0].minor.yy163->endLoc());   yy_destructor(yypParser,86,&yymsp[-1].minor);
}
#line 4031 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 156: /* comparison ::= constant LTHAN_EQ term */
#line 860 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy353 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN_EQ, yymsp[-2].minor.yy345, yymsp[0].minor.yy163, yymsp[-2].minor.yy345->beginLoc(), yymsp[0].minor.yy163->endLoc());   yy_destructor(yypParser,87,&yymsp[-1].minor);
}
#line 4037 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 157: /* comparison ::= constant GTHAN_EQ term */
#line 861 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy353 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN_EQ, yymsp[-2].minor.yy345, yymsp[0].minor.yy163, yymsp[-2].minor.yy345->beginLoc(), yymsp[0].minor.yy163->endLoc());   yy_destructor(yypParser,88,&yymsp[-1].minor);
}
#line 4043 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 158: /* atomic_formula ::= constant */
      case 162: /* atomic_formula_anon ::= const_anon */ yytestcase(yyruleno==162);
      case 185: /* atomic_formula_one_const ::= constant_one_const */ yytestcase(yyruleno==185);
#line 888 "bcplus/parser/detail/lemon_parser.y"
{ ATOMIC_FORMULA(yygotominor.yy138, yymsp[0].minor.yy345, true); }
#line 4050 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 159: /* atomic_formula ::= TILDE constant */
      case 163: /* atomic_formula_anon ::= TILDE const_anon */ yytestcase(yyruleno==163);
      case 186: /* atomic_formula_one_const ::= TILDE constant_one_const */ yytestcase(yyruleno==186);
#line 889 "bcplus/parser/detail/lemon_parser.y"
{ ATOMIC_FORMULA(yygotominor.yy138, yymsp[0].minor.yy345, false);   yy_destructor(yypParser,75,&yymsp[-1].minor);
}
#line 4058 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 160: /* atomic_formula ::= constant EQ term */
      case 164: /* atomic_formula_anon ::= const_anon EQ term */ yytestcase(yyruleno==164);
      case 187: /* atomic_formula_one_const ::= constant_one_const EQ term_no_const */ yytestcase(yyruleno==187);
#line 890 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy138 = new AtomicFormula(yymsp[-2].minor.yy345, yymsp[0].minor.yy163, yymsp[-2].minor.yy345->beginLoc(), yymsp[0].minor.yy163->endLoc());	  yy_destructor(yypParser,81,&yymsp[-1].minor);
}
#line 4066 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 161: /* atomic_formula_anon ::= atomic_formula */
      case 220: /* atomic_head_formula ::= atomic_formula */ yytestcase(yyruleno==220);
      case 307: /* show_elem ::= atomic_formula_one_const */ yytestcase(yyruleno==307);
#line 892 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy138 = yymsp[0].minor.yy138; }
#line 4073 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 198: /* formula_temporal ::= term_strong COLON formula_temporal */
#line 971 "bcplus/parser/detail/lemon_parser.y"
{ BINDING(yygotominor.yy353, yymsp[-2].minor.yy163, yymsp[-1].minor.yy0, yymsp[0].minor.yy353); }
#line 4078 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 199: /* formula_quant ::= BRACKET_L quant_lst PIPE formula BRACKET_R */
#line 983 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy285=NULL;
		ref_ptr<const Token> bl_ptr = yymsp[-4].minor.yy0;
		ref_ptr<QuantifierFormula::QuantifierList> lst_ptr = yymsp[-3].minor.yy261;
		ref_ptr<Formula> sub_ptr = yymsp[-1].minor.yy353;
		ref_ptr<const Token> br_ptr = yymsp[0].minor.yy0;

		if (!parser->lang()->support(Language::Feature::FORMULA_QUANTIFIER)) {
			parser->_feature_error(Language::Feature::FORMULA_QUANTIFIER, &yymsp[-4].minor.yy0->beginLoc());
			YYERROR;
		} else yygotominor.yy285 = new QuantifierFormula(yymsp[-3].minor.yy261, yymsp[-1].minor.yy353, yymsp[-4].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
	  yy_destructor(yypParser,98,&yymsp[-2].minor);
}
#line 4095 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 200: /* quant_lst ::= quant_op variable */
#line 997 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy261 = new QuantifierFormula::QuantifierList();
		yygotominor.yy261->push_back(QuantifierFormula::Quantifier(yymsp[-1].minor.yy249, yymsp[0].minor.yy453));
	}
#line 4103 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 201: /* quant_lst ::= quant_lst quant_op variable */
#line 1003 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy261 = yymsp[-2].minor.yy261;
		yygotominor.yy261->push_back(QuantifierFormula::Quantifier(yymsp[-1].minor.yy249, yymsp[0].minor.yy453));
	}
#line 4111 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 202: /* quant_op ::= BIG_CONJ */
#line 1008 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy249 = QuantifierFormula::Operator::CONJ;   yy_destructor(yypParser,90,&yymsp[0].minor);
}
#line 4117 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 203: /* quant_op ::= BIG_DISJ */
#line 1009 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy249 = QuantifierFormula::Operator::DISJ;   yy_destructor(yypParser,91,&yymsp[0].minor);
}
#line 4123 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 204: /* formula_card ::= CBRACKET_L card_var_lst formula CBRACKET_R */
#line 1055 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy353, NULL, yymsp[-3].minor.yy0, yymsp[-2].minor.yy319, yymsp[-1].minor.yy353, yymsp[0].minor.yy0, NULL);  }
#line 4128 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 205: /* formula_card ::= term_strong CBRACKET_L card_var_lst formula CBRACKET_R */
#line 1056 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy353, yymsp[-4].minor.yy163, yymsp[-3].minor.yy0, yymsp[-2].minor.yy319, yymsp[-1].minor.yy353,  yymsp[0].minor.yy0, NULL);  }
#line 4133 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 206: /* formula_card ::= CBRACKET_L card_var_lst formula CBRACKET_R term */
#line 1057 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy353, NULL, yymsp[-4].minor.yy0, yymsp[-3].minor.yy319, yymsp[-2].minor.yy353, yymsp[-1].minor.yy0, yymsp[0].minor.yy163); 	}
#line 4138 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 207: /* formula_card ::= term_strong CBRACKET_L card_var_lst formula CBRACKET_R term */
#line 1058 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy353, yymsp[-5].minor.yy163, yymsp[-4].minor.yy0, yymsp[-3].minor.yy319, yymsp[-2].minor.yy353,  yymsp[-1].minor.yy0, yymsp[0].minor.yy163); 	}
#line 4143 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 208: /* formula_card ::= CBRACKET_L formula CBRACKET_R */
#line 1059 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy353, NULL, yymsp[-2].minor.yy0, NULL, yymsp[-1].minor.yy353, yymsp[0].minor.yy0, NULL);  }
#line 4148 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 209: /* formula_card ::= term_strong CBRACKET_L formula CBRACKET_R */
#line 1060 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy353, yymsp[-3].minor.yy163, yymsp[-2].minor.yy0, NULL, yymsp[-1].minor.yy353,  yymsp[0].minor.yy0, NULL);  }
#line 4153 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 210: /* formula_card ::= CBRACKET_L formula CBRACKET_R term */
#line 1061 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy353, NULL, yymsp[-3].minor.yy0, NULL, yymsp[-2].minor.yy353, yymsp[-1].minor.yy0, yymsp[0].minor.yy163); 	}
#line 4158 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 211: /* formula_card ::= term_strong CBRACKET_L formula CBRACKET_R term */
#line 1062 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy353, yymsp[-4].minor.yy163, yymsp[-3].minor.yy0, NULL, yymsp[-2].minor.yy353,  yymsp[-1].minor.yy0, yymsp[0].minor.yy163); 	}
#line 4163 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 212: /* card_var_lst ::= card_var_lst_inner PIPE */
#line 1066 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy319 = yymsp[-1].minor.yy319;
	  yy_destructor(yypParser,98,&yymsp[0].minor);
}
#line 4171 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 213: /* card_var_lst_inner ::= variable */
#line 1071 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> v_ptr = yymsp[0].minor.yy453;
		yygotominor.yy319 = new CardinalityFormula::VariableList();
		yygotominor.yy319->push_back(yymsp[0].minor.yy453->symbol());
	}
#line 4180 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 214: /* card_var_lst_inner ::= card_var_lst_inner COMMA variable */
#line 1078 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> v_ptr = yymsp[0].minor.yy453;
		yygotominor.yy319 = yymsp[-2].minor.yy319;
		yygotominor.yy319->push_back(yymsp[0].minor.yy453->symbol());
	  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 4190 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 217: /* head_formula ::= formula_smpl_card */
#line 1100 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy353 = yymsp[0].minor.yy369;
		if (!parser->lang()->support(Language::Feature::FORMULA_CARDINALITY_HEAD)) {
			parser->_feature_error(Language::Feature::FORMULA_CARDINALITY_HEAD, &yymsp[0].minor.yy369->beginLoc());
			YYERROR;
		}
	}
#line 4201 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 221: /* atomic_head_formula ::= DASH constant */
#line 1113 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy138 = NULL;
		ref_ptr<const Token> d_ptr = yymsp[-1].minor.yy0;
		ref_ptr<Constant> c_ptr = yymsp[0].minor.yy345;

		if (!parser->lang()->support(Language::Feature::FORMULA_NOT_DASH_HEAD)) {
			parser->_feature_error(Language::Feature::FORMULA_NOT_DASH_HEAD);
			YYERROR;
		} else {
			ATOMIC_FORMULA(yygotominor.yy138, yymsp[0].minor.yy345, false); 
		}
	}
#line 4217 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 222: /* formula_smpl_card ::= CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R */
#line 1126 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy369, NULL, yymsp[-3].minor.yy0, yymsp[-2].minor.yy319, yymsp[-1].minor.yy138, yymsp[0].minor.yy0, NULL);  }
#line 4222 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 223: /* formula_smpl_card ::= term_strong CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R */
#line 1127 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy369, yymsp[-4].minor.yy163, yymsp[-3].minor.yy0, yymsp[-2].minor.yy319, yymsp[-1].minor.yy138,  yymsp[0].minor.yy0, NULL);  }
#line 4227 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 224: /* formula_smpl_card ::= CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R term */
#line 1128 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy369, NULL, yymsp[-4].minor.yy0, yymsp[-3].minor.yy319, yymsp[-2].minor.yy138, yymsp[-1].minor.yy0, yymsp[0].minor.yy163); 	}
#line 4232 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 225: /* formula_smpl_card ::= term_strong CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R term */
#line 1129 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy369, yymsp[-5].minor.yy163, yymsp[-4].minor.yy0, yymsp[-3].minor.yy319, yymsp[-2].minor.yy138,  yymsp[-1].minor.yy0, yymsp[0].minor.yy163); 	}
#line 4237 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 226: /* formula_smpl_card ::= CBRACKET_L atomic_formula_one_const CBRACKET_R */
#line 1130 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy369, NULL, yymsp[-2].minor.yy0, NULL, yymsp[-1].minor.yy138, yymsp[0].minor.yy0, NULL);  }
#line 4242 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 227: /* formula_smpl_card ::= term_strong CBRACKET_L atomic_formula_one_const CBRACKET_R */
#line 1131 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy369, yymsp[-3].minor.yy163, yymsp[-2].minor.yy0, NULL, yymsp[-1].minor.yy138,  yymsp[0].minor.yy0, NULL);  }
#line 4247 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 228: /* formula_smpl_card ::= CBRACKET_L atomic_formula_one_const CBRACKET_R term */
#line 1132 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy369, NULL, yymsp[-3].minor.yy0, NULL, yymsp[-2].minor.yy138, yymsp[-1].minor.yy0, yymsp[0].minor.yy163); 	}
#line 4252 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 229: /* formula_smpl_card ::= term_strong CBRACKET_L atomic_formula_one_const CBRACKET_R term */
#line 1133 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy369, yymsp[-4].minor.yy163, yymsp[-3].minor.yy0, NULL, yymsp[-2].minor.yy138,  yymsp[-1].minor.yy0, yymsp[0].minor.yy163); 	}
#line 4257 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 230: /* stmt_macro_def ::= COLON_DASH MACROS macro_def_lst PERIOD */
#line 1152 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy455 = NULL;
        ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0;
        ref_ptr<const Token> kw_ptr = yymsp[-2].minor.yy0;
        ref_ptr<MacroDeclaration::ElementList> l_ptr = yymsp[-1].minor.yy105;
        ref_ptr<const Token> p_ptr = yymsp[0].minor.yy0;

        if (!parser->lang()->support(Language::Feature::DECL_MACRO)) {
            parser->_feature_error(Language::Feature::DECL_MACRO, &yymsp[-2].minor.yy0->beginLoc());
            YYERROR;
        } else {
		    BOOST_FOREACH(symbols::MacroSymbol* m, *yymsp[-1].minor.yy105) {
			    if (!parser->symtab()->create(m)) {
	    	        // Check if it's a duplicate
	    	        symbols::MacroSymbol* m2 = (symbols::MacroSymbol*)parser->symtab()->resolve(symbols::Symbol::Type::MACRO, *m->base(), m->arity());
		            if (!m2 || m2 != m) {
		                parser->_parse_error("Detected conflicting definition of symbol \"" + *m->name() + "\".", &yygotominor.yy455->beginLoc());
		            } else {
		                parser->_parse_error("Detected a duplicate definition of symbol \"" + *m->name() + "\".", &yygotominor.yy455->beginLoc());
		            }
		        }
		    }

			yygotominor.yy455 = new MacroDeclaration(yymsp[-1].minor.yy105, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
        }
    }
#line 4287 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 231: /* macro_def_lst ::= macro_bnd */
#line 1180 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy105 = new MacroDeclaration::ElementList();
        yygotominor.yy105->push_back(yymsp[0].minor.yy315);
    }
#line 4295 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 232: /* macro_def_lst ::= macro_def_lst SEMICOLON macro_bnd */
#line 1186 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy105 = yymsp[-2].minor.yy105;
        yygotominor.yy105->push_back(yymsp[0].minor.yy315);
      yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 4304 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 233: /* macro_bnd ::= IDENTIFIER PAREN_L macro_args PAREN_R ARROW_RDASH MACRO_STRING */
#line 1192 "bcplus/parser/detail/lemon_parser.y"
{
        ref_ptr<const Token> id_ptr = yymsp[-5].minor.yy0;
        ref_ptr<MacroSymbol::ArgumentList> args_ptr = yymsp[-3].minor.yy354;
        ref_ptr<const Token> def_ptr = yymsp[0].minor.yy0;

        yygotominor.yy315 = new MacroSymbol(yymsp[-5].minor.yy0->str(), yymsp[0].minor.yy0->str(), yymsp[-3].minor.yy354);
      yy_destructor(yypParser,71,&yymsp[-4].minor);
  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,96,&yymsp[-1].minor);
}
#line 4318 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 234: /* macro_bnd ::= IDENTIFIER ARROW_RDASH MACRO_STRING */
#line 1201 "bcplus/parser/detail/lemon_parser.y"
{
        ref_ptr<const Token> id_ptr = yymsp[-2].minor.yy0;
        ref_ptr<const Token> def_ptr = yymsp[0].minor.yy0;

        yygotominor.yy315 = new MacroSymbol(yymsp[-2].minor.yy0->str(), yymsp[0].minor.yy0->str());
      yy_destructor(yypParser,96,&yymsp[-1].minor);
}
#line 4329 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 235: /* macro_args ::= macro_arg */
#line 1209 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy354 = new MacroSymbol::ArgumentList();
        yygotominor.yy354->push_back(yymsp[0].minor.yy251->str());
        delete yymsp[0].minor.yy251;
    }
#line 4338 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 236: /* macro_args ::= macro_args COMMA macro_arg */
#line 1215 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy354 = yymsp[-2].minor.yy354;
        yygotominor.yy354->push_back(yymsp[0].minor.yy251->str());
        delete yymsp[0].minor.yy251;
      yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 4348 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 237: /* macro_arg ::= POUND_INTEGER */
      case 238: /* macro_arg ::= POUND_IDENTIFIER */ yytestcase(yyruleno==238);
#line 1222 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy251 = yymsp[0].minor.yy0;
    }
#line 4356 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 239: /* sort_lst ::= sort */
#line 1249 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy411 = new ConstantSymbol::SortList();
		yygotominor.yy411->push_back(yymsp[0].minor.yy393);
	}
#line 4364 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 240: /* sort_lst ::= sort_lst COMMA sort */
#line 1254 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy411 = yymsp[-2].minor.yy411;
		yygotominor.yy411->push_back(yymsp[0].minor.yy393);
	  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 4373 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 241: /* sort ::= sort_id_nr */
      case 247: /* sort_id_nr ::= sort_id */ yytestcase(yyruleno==247);
      case 248: /* sort_id_nr ::= sort_nr */ yytestcase(yyruleno==248);
#line 1279 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy393 = yymsp[0].minor.yy393; }
#line 4380 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 242: /* sort ::= sort_id_nr STAR */
#line 1280 "bcplus/parser/detail/lemon_parser.y"
{ DYNAMIC_SORT_PLUS(yygotominor.yy393, yymsp[-1].minor.yy393, yymsp[0].minor.yy0, Language::Feature::STAR_SORT, parser->symtab()->bobj(SymbolTable::BuiltinObject::NONE)); }
#line 4385 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 243: /* sort ::= sort_id_nr CARROT */
#line 1281 "bcplus/parser/detail/lemon_parser.y"
{ DYNAMIC_SORT_PLUS(yygotominor.yy393, yymsp[-1].minor.yy393, yymsp[0].minor.yy0, Language::Feature::CARROT_SORT, parser->symtab()->bobj(SymbolTable::BuiltinObject::UNKNOWN)); }
#line 4390 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 244: /* sort ::= sort PLUS object_nullary */
#line 1283 "bcplus/parser/detail/lemon_parser.y"
{ u::ref_ptr<const Object> o_ptr = yymsp[0].minor.yy238; DYNAMIC_SORT_PLUS(yygotominor.yy393, yymsp[-2].minor.yy393, yymsp[-1].minor.yy0, Language::Feature::SORT_PLUS, yymsp[0].minor.yy238->symbol()); }
#line 4395 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 245: /* sort ::= sort PLUS IDENTIFIER */
#line 1286 "bcplus/parser/detail/lemon_parser.y"
{
												  u::ref_ptr<const Referenced> s_ptr = yymsp[-2].minor.yy393, op_ptr = yymsp[-1].minor.yy0, id_ptr = yymsp[0].minor.yy0;
												  u::ref_ptr<const ObjectSymbol> obj = parser->symtab()->resolveOrCreate(new ObjectSymbol(yymsp[0].minor.yy0->str()));
												  if(!obj) {
													if (parser->lang()->support(Language::Feature::SORT_PLUS)) 
														parser->_parse_error("\"" + *yymsp[0].minor.yy0->str() + "\" could not be declared as an object as this conflicts with a previous declarations of this identifier.", &yymsp[0].minor.yy0->beginLoc());
													else 
														parser->_feature_error(Language::Feature::SORT_PLUS, &yymsp[-1].minor.yy0->beginLoc());
													YYERROR;
												  } else {
													DYNAMIC_SORT_PLUS(yygotominor.yy393, yymsp[-2].minor.yy393, yymsp[-1].minor.yy0, Language::Feature::SORT_PLUS, obj);
												  }
												}
#line 4412 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 246: /* sort ::= sort PLUS INTEGER */
#line 1300 "bcplus/parser/detail/lemon_parser.y"
{ 
												  ref_ptr<const Object> t_ptr;
												  BASIC_TERM(t_ptr, yymsp[0].minor.yy0);
												  DYNAMIC_SORT_PLUS(yygotominor.yy393, yymsp[-2].minor.yy393, yymsp[-1].minor.yy0, Language::Feature::SORT_PLUS, t_ptr->symbol()); 
												}
#line 4421 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 249: /* sort_nr ::= num_range */
#line 1311 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> nr_ptr = yymsp[0].minor.yy309;

		yygotominor.yy393 = NULL;

		if (!parser->lang()->support(Language::Feature::NUMRANGE_SORT)) {
			parser->_feature_error(Language::Feature::NUMRANGE_SORT, &yymsp[0].minor.yy309->beginLoc());
			YYERROR;
		}

		// X..Y becomes __sort_X_Y__
		std::string name = "__sort_" + boost::lexical_cast<std::string>(yymsp[0].minor.yy309->min()) + "__" + boost::lexical_cast<std::string>(yymsp[0].minor.yy309->max()) + "__";

		ref_ptr<SortSymbol::ObjectList> objs = new SortSymbol::ObjectList();

		// Generate the objects that it will have
		for (int i = yymsp[0].minor.yy309->min(); i <= yymsp[0].minor.yy309->max(); i++) {
			std::string obj_name = boost::lexical_cast<std::string>(i);
			ObjectSymbol const* sym = parser->symtab()->resolveOrCreate(new ObjectSymbol(new ReferencedString(obj_name)));

			if (!sym) {
				yygotominor.yy393 = NULL;
				parser->_parse_error("An error occurred creating symbol \"" + obj_name + "/0\".", &yymsp[0].minor.yy309->beginLoc());
				YYERROR;
			}
			objs->insert(sym);
		}		

		// dynamically declare the sort
		yygotominor.yy393 = parser->symtab()->resolveOrCreate(new SortSymbol(new ReferencedString(name), objs));
		if (!yygotominor.yy393) {
				parser->_parse_error("An error occurred creating symbol \"" + name + "/0\".", &yymsp[0].minor.yy309->beginLoc());
				YYERROR;
		} 
	}
#line 4460 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 250: /* sort_id ::= IDENTIFIER */
#line 1348 "bcplus/parser/detail/lemon_parser.y"
{
		// dynamically declare the sort
		yygotominor.yy393 = (SortSymbol*)parser->symtab()->resolve(Symbol::Type::SORT, *yymsp[0].minor.yy0->str());
		if (!yygotominor.yy393) {
			parser->_parse_error("\"" + Symbol::genName(*yymsp[0].minor.yy0->str(),0) + "\" is not a declared sort.", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
		delete yymsp[0].minor.yy0;
	}
#line 4473 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 251: /* stmt_constant_def ::= COLON_DASH CONSTANTS constant_bnd_lst PERIOD */
#line 1379 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0;
		ref_ptr<const Token> kw_ptr = yymsp[-2].minor.yy0;
		ref_ptr<ConstantDeclaration::ElementList> l_ptr = yymsp[-1].minor.yy97;
		ref_ptr<const Token> p_ptr = yymsp[0].minor.yy0;
		
		if (!parser->lang()->support(Language::Feature::DECL_CONSTANT)) {
			yygotominor.yy55 = NULL;
			parser->_feature_error(Language::Feature::DECL_CONSTANT, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy55 = new ConstantDeclaration(yymsp[-1].minor.yy97, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());

		}
	}
#line 4492 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 252: /* constant_bnd_lst ::= constant_bnd */
#line 1396 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy97 = yymsp[0].minor.yy97;
	}
#line 4499 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 253: /* constant_bnd_lst ::= constant_bnd_lst SEMICOLON constant_bnd */
#line 1401 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<ConstantDeclaration::ElementList> bnd_ptr = yymsp[0].minor.yy97;
		yygotominor.yy97 = yymsp[-2].minor.yy97;
		yygotominor.yy97->splice(yygotominor.yy97->end(), *yymsp[0].minor.yy97);
	  yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 4509 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 254: /* constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type PAREN_L sort PAREN_R */
#line 1421 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const SortSymbol> s_ptr = yymsp[-1].minor.yy393;
		ref_ptr<const Referenced> names_ptr = yymsp[-5].minor.yy42;
		yygotominor.yy97 = new ConstantDeclaration::ElementList();

		// NOTE: additive constants default to the additive sort, not the boolean sort
		if (yymsp[-3].minor.yy310 & ConstantSymbol::Type::M_ADDITIVE) s_ptr = parser->symtab()->bsort(SymbolTable::BuiltinSort::ADDITIVE);

		// external constants should have "unknown" in their sort
		else if (yymsp[-3].minor.yy310 & ConstantSymbol::Type::M_EXTERNAL) s_ptr = parser->symtab()->carrot(yymsp[-1].minor.yy393);

		// non-boolean abActions should contain "none"
		else if (yymsp[-3].minor.yy310 == ConstantSymbol::Type::ABACTION && s_ptr->domainType() != DomainType::BOOLEAN) s_ptr = parser->symtab()->star(yymsp[-1].minor.yy393);

		BOOST_FOREACH(IdentifierDecl& decl, *yymsp[-5].minor.yy42) {
			// attempt to declare each symbol
			ref_ptr<ConstantSymbol> c = new ConstantSymbol(yymsp[-3].minor.yy310, decl.first->str(), s_ptr, decl.second);
			yygotominor.yy97->push_back(c);
			CONSTANT_DECL(c, decl.first->beginLoc());
		}
	  yy_destructor(yypParser,76,&yymsp[-4].minor);
  yy_destructor(yypParser,71,&yymsp[-2].minor);
  yy_destructor(yypParser,72,&yymsp[0].minor);
}
#line 4537 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 255: /* constant_bnd ::= constant_dcl_lst DBL_COLON sort */
#line 1443 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> names_ptr = yymsp[-2].minor.yy42, s_ptr = yymsp[0].minor.yy393;
		yygotominor.yy97 = new ConstantDeclaration::ElementList();
		BOOST_FOREACH(IdentifierDecl& decl, *yymsp[-2].minor.yy42) {
			// attempt to declare each symbol
			ref_ptr<ConstantSymbol> c = new ConstantSymbol(ConstantSymbol::Type::RIGID, decl.first->str(), yymsp[0].minor.yy393, decl.second);
			yygotominor.yy97->push_back(c);
			CONSTANT_DECL(c, decl.first->beginLoc());
		}
	  yy_destructor(yypParser,76,&yymsp[-1].minor);
}
#line 4552 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 256: /* constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type */
#line 1454 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> names_ptr = yymsp[-2].minor.yy42;
		yygotominor.yy97 = new ConstantDeclaration::ElementList();
		BOOST_FOREACH(IdentifierDecl& decl, *yymsp[-2].minor.yy42) {
			// attempt to declare each symbol
			ref_ptr<SortSymbol> s = parser->symtab()->bsort(SymbolTable::BuiltinSort::BOOLEAN);

			// NOTE: additive constants default to the additive sort, not the boolean sort
			if (yymsp[0].minor.yy310 & ConstantSymbol::Type::M_ADDITIVE) s = parser->symtab()->bsort(SymbolTable::BuiltinSort::ADDITIVE);

			// external constants should have "unknown" in their sort
			else if (yymsp[0].minor.yy310 & ConstantSymbol::Type::M_EXTERNAL) s = parser->symtab()->carrot(s);

			// non-boolean abActions should contain "none"
			else if (yymsp[0].minor.yy310 == ConstantSymbol::Type::ABACTION && s->domainType() != DomainType::BOOLEAN) s = parser->symtab()->star(s);


			ref_ptr<ConstantSymbol> c = new ConstantSymbol(yymsp[0].minor.yy310, decl.first->str(), parser->symtab()->bsort(SymbolTable::BuiltinSort::BOOLEAN), decl.second);
			yygotominor.yy97->push_back(c);
			CONSTANT_DECL(c, decl.first->beginLoc());
		}
	  yy_destructor(yypParser,76,&yymsp[-1].minor);
}
#line 4579 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 257: /* constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER */
#line 1477 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy97 = NULL;
		ref_ptr<const Referenced> names_ptr = yymsp[-4].minor.yy42, s_ptr = yymsp[-2].minor.yy228, id_ptr = yymsp[0].minor.yy0;


		// attempt to resolve the attribute parent symbol
		ConstantSymbol const* c = (ConstantSymbol const*) parser->symtab()->resolve(Symbol::Type::CONSTANT, *yymsp[0].minor.yy0->str());

		if (!c) {
			parser->_parse_error("\"" + Symbol::genName(*yymsp[0].minor.yy0->str(), 0) + "\" is not a valid constant symbol.", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else if (c->constType() != ConstantSymbol::Type::ABACTION && c->constType() != ConstantSymbol::Type::ACTION && c->constType() != ConstantSymbol::Type::EXOGENOUSACTION) {
			parser->_parse_error("Unexpected constant type of attribute parent \"" + Symbol::genName(*yymsp[0].minor.yy0->str(), 0) + "\". Attribute parents must be an \"abAction\", \"action\", or \"exogenousAction\".", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy97 = new ConstantDeclaration::ElementList();
			BOOST_FOREACH(IdentifierDecl& decl, *yymsp[-4].minor.yy42) {
				ref_ptr<ConstantSymbol> c= new AttributeSymbol(decl.first->str(), yymsp[-2].minor.yy228, c, decl.second);
				yygotominor.yy97->push_back(c);
				CONSTANT_DECL(c, decl.first->beginLoc());
			}
		}
	  yy_destructor(yypParser,76,&yymsp[-3].minor);
  yy_destructor(yypParser,55,&yymsp[-1].minor);
}
#line 4608 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 258: /* constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1501 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy97 = NULL;
		ref_ptr<const Referenced> names_ptr = yymsp[-7].minor.yy42, s_ptr = yymsp[-5].minor.yy228, id_ptr = yymsp[-3].minor.yy0, lst_ptr = yymsp[-1].minor.yy411;

		// attempt to resolve the attribute parent symbol
		ConstantSymbol const* c = (ConstantSymbol const*) parser->symtab()->resolve(Symbol::Type::CONSTANT, *yymsp[-3].minor.yy0->str(), yymsp[-1].minor.yy411->size());

		if (!c) {
			parser->_parse_error("\"" + Symbol::genName(*yymsp[-3].minor.yy0->str(), yymsp[-1].minor.yy411->size()) + "\" is not a valid constant symbol.", &yymsp[-3].minor.yy0->beginLoc());
			YYERROR;
		} else if (c->constType() != ConstantSymbol::Type::ABACTION && c->constType() != ConstantSymbol::Type::ACTION && c->constType() != ConstantSymbol::Type::EXOGENOUSACTION) {
			parser->_parse_error("Unexpected constant type of attribute parent \"" + Symbol::genName(*yymsp[-3].minor.yy0->str(), yymsp[-1].minor.yy411->size()) + "\". Attribute parents must be an \"abAction\", \"action\", or \"exogenousAction\".", &yymsp[-3].minor.yy0->beginLoc());
			YYERROR;
		} else {
			// ensure that the sorts match the declaration of the symbol
			SortList::const_iterator it = yymsp[-1].minor.yy411->begin();
			BOOST_FOREACH(SortSymbol const* sort, *c) {
				if (*it != sort) {
					// check to see if it'yymsp[-5].minor.yy228 a subsort, which is also permissable
					bool found = false;
					for (SortSymbol::SortList::const_iterator it2 = sort->beginSubSorts(); it2 != sort->endSubSorts(); it2++) {
						if (*it == *it2) {
							found = true;
							break;
						}
					}

					if (!found) {
						parser->_parse_error("Detected a sort mismatch in an attribute parent declaration. \"" + *(*it)->base() + "\" is not an explicit subsort of \"" + *sort->base() + "\".", &yymsp[-3].minor.yy0->beginLoc());
						YYERROR;
					}
				}
				it++;
			}

			yygotominor.yy97 = new ConstantDeclaration::ElementList();
			BOOST_FOREACH(IdentifierDecl& decl, *yymsp[-7].minor.yy42) {
				// ensure that the sorts match the start of the sort list for each of the symbols
				if (decl.second->size() < yymsp[-1].minor.yy411->size()) {
					parser->_parse_error("Detected a malformed attribute declaration. An attribute must duplicate its parent'yymsp[-5].minor.yy228 parameters.", &decl.first->beginLoc());
					YYERROR;
				} else {
					bool good_sort = true;		
					it = decl.second->begin();
					BOOST_FOREACH(SortSymbol const* sort, *yymsp[-1].minor.yy411) {
						if (*it != sort) {
							// check to see if it'yymsp[-5].minor.yy228 a subsort, which is also permissable
							bool found = false;
							for (SortSymbol::SortList::const_iterator it2 = sort->beginSubSorts(); it2 != sort->endSubSorts(); it2++) {
								if (*it == *it2) {
									found = true;
									break;
								}
							}
							if (!found) {
								good_sort = false;
								parser->_parse_error("Detected a sort mismatch in an attribute declaration. \"" + *(*it)->base() + "\" is not an explicit subsort of \"" + *sort->base() + "\".", &decl.first->beginLoc());
								YYERROR;
							}
						}
						it++;	
					}

					if (good_sort) {
						ref_ptr<ConstantSymbol> sym = new AttributeSymbol(decl.first->str(), yymsp[-5].minor.yy228, c, decl.second);
						yygotominor.yy97->push_back(sym);
						CONSTANT_DECL(sym, decl.first->beginLoc());

					}
				}
			}
		}
	  yy_destructor(yypParser,76,&yymsp[-6].minor);
  yy_destructor(yypParser,55,&yymsp[-4].minor);
  yy_destructor(yypParser,71,&yymsp[-2].minor);
  yy_destructor(yypParser,72,&yymsp[0].minor);
}
#line 4689 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 259: /* constant_dcl_lst ::= IDENTIFIER */
#line 1577 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy42 = new IdentifierDeclList();
		yygotominor.yy42->push_back(IdentifierDecl(yymsp[0].minor.yy0, NULL));
	}
#line 4697 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 260: /* constant_dcl_lst ::= IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1582 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy42 = new IdentifierDeclList();
		yygotominor.yy42->push_back(IdentifierDecl(yymsp[-3].minor.yy0, yymsp[-1].minor.yy411));
	  yy_destructor(yypParser,71,&yymsp[-2].minor);
  yy_destructor(yypParser,72,&yymsp[0].minor);
}
#line 4707 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 261: /* constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER */
#line 1587 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy42 = yymsp[-2].minor.yy42;
		yygotominor.yy42->push_back(IdentifierDecl(yymsp[0].minor.yy0, NULL));
	  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 4716 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 262: /* constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1592 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy42 = yymsp[-5].minor.yy42;
		yygotominor.yy42->push_back(IdentifierDecl(yymsp[-3].minor.yy0, yymsp[-1].minor.yy411));
	  yy_destructor(yypParser,102,&yymsp[-4].minor);
  yy_destructor(yypParser,71,&yymsp[-2].minor);
  yy_destructor(yypParser,72,&yymsp[0].minor);
}
#line 4727 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 263: /* constant_dcl_type ::= ABACTION */
#line 1599 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy310 = ConstantSymbol::Type::ABACTION;
		if (!parser->lang()->support(Language::Feature::CONST_ABACTION)) {
			parser->_feature_error(Language::Feature::CONST_ABACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4739 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 264: /* constant_dcl_type ::= ACTION */
#line 1608 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy310 = ConstantSymbol::Type::ACTION;
		if (!parser->lang()->support(Language::Feature::CONST_ACTION)) {
			parser->_feature_error(Language::Feature::CONST_ACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4751 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 265: /* constant_dcl_type ::= ADDITIVEACTION */
#line 1617 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy310 = ConstantSymbol::Type::ADDITIVEACTION;
		if (!parser->lang()->support(Language::Feature::CONST_ADDITIVEACTION)) {
			parser->_feature_error(Language::Feature::CONST_ADDITIVEACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4763 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 266: /* constant_dcl_type ::= ADDITIVEFLUENT */
#line 1626 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy310 = ConstantSymbol::Type::ADDITIVEFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_ADDITIVEFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_ADDITIVEFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4775 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 267: /* constant_dcl_type ::= EXTERNALACTION */
#line 1635 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy310 = ConstantSymbol::Type::EXTERNALACTION;
		if (!parser->lang()->support(Language::Feature::CONST_EXTERNALACTION)) {
			parser->_feature_error(Language::Feature::CONST_EXTERNALACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4787 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 268: /* constant_dcl_type ::= EXTERNALFLUENT */
#line 1644 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy310 = ConstantSymbol::Type::EXTERNALFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_EXTERNALFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_EXTERNALFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4799 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 269: /* constant_dcl_type ::= EXOGENOUSACTION */
#line 1653 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy310 = ConstantSymbol::Type::EXOGENOUSACTION;
		if (!parser->lang()->support(Language::Feature::CONST_EXOGENOUSACTION)) {
			parser->_feature_error(Language::Feature::CONST_EXOGENOUSACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4811 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 270: /* constant_dcl_type ::= INERTIALFLUENT */
#line 1662 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy310 = ConstantSymbol::Type::INERTIALFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_INERTIALFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_INERTIALFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4823 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 271: /* constant_dcl_type ::= RIGID */
#line 1671 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy310 = ConstantSymbol::Type::RIGID;
		if (!parser->lang()->support(Language::Feature::CONST_RIGID)) {
			parser->_feature_error(Language::Feature::CONST_RIGID, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4835 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 272: /* constant_dcl_type ::= SIMPLEFLUENT */
#line 1680 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy310 = ConstantSymbol::Type::SIMPLEFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_SIMPLEFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_SIMPLEFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4847 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 273: /* constant_dcl_type ::= SDFLUENT */
#line 1690 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy310 = ConstantSymbol::Type::SDFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_SDFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_SDFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4859 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 274: /* attrib_spec ::= ATTRIBUTE */
#line 1700 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy228 = NULL;
		ref_ptr<const Referenced> kw_ptr = yymsp[0].minor.yy0;
		if (!parser->lang()->support(Language::Feature::CONST_ATTRIBUTE)) {
			parser->_feature_error(Language::Feature::CONST_ATTRIBUTE, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else {
			// grab the boolean sort and provide it
			yygotominor.yy228 = parser->symtab()->star(parser->symtab()->bsort(SymbolTable::BuiltinSort::BOOLEAN));
		}
	}
#line 4874 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 275: /* attrib_spec ::= ATTRIBUTE PAREN_L sort PAREN_R */
#line 1713 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy228 = NULL;
		ref_ptr<const Referenced> kw_ptr = yymsp[-3].minor.yy0, s_ptr = yymsp[-1].minor.yy393;
		if (!parser->lang()->support(Language::Feature::CONST_ATTRIBUTE)) {
			parser->_feature_error(Language::Feature::CONST_ATTRIBUTE, &yymsp[-3].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy228 = parser->symtab()->star(yymsp[-1].minor.yy393);
		}
	  yy_destructor(yypParser,71,&yymsp[-2].minor);
  yy_destructor(yypParser,72,&yymsp[0].minor);
}
#line 4890 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 276: /* stmt_object_def ::= COLON_DASH OBJECTS object_bnd_lst PERIOD */
#line 1741 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0;
		ref_ptr<const Token> p_ptr = yymsp[0].minor.yy0;
		ref_ptr<const Token> kw_ptr = yymsp[-2].minor.yy0;
		ref_ptr<ObjectDeclaration::ElementList> l_ptr = yymsp[-1].minor.yy198;
		
		if (!parser->lang()->support(Language::Feature::DECL_OBJECT)) {
			yygotominor.yy296 = NULL;
			parser->_feature_error(Language::Feature::DECL_OBJECT, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy296 = new ObjectDeclaration(yymsp[-1].minor.yy198, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());

			// Go ahead and add them to the symbol table
			BOOST_FOREACH(ObjectDeclaration::Element* bnd, *yymsp[-1].minor.yy198) {
				BOOST_FOREACH(ObjectSymbol const* o, *bnd) {
						bnd->sort()->add(o);
				}
			}
		}
	}
#line 4915 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 277: /* object_bnd_lst ::= object_bnd */
#line 1764 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy198 = new ObjectDeclaration::ElementList();
		yygotominor.yy198->push_back(yymsp[0].minor.yy70);
	}
#line 4923 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 278: /* object_bnd_lst ::= object_bnd_lst SEMICOLON object_bnd */
#line 1770 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy198 = yymsp[-2].minor.yy198;
		yygotominor.yy198->push_back(yymsp[0].minor.yy70);
	  yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 4932 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 279: /* object_bnd ::= object_lst DBL_COLON sort_id */
#line 1776 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy70 = new ObjectDeclaration::Element(yymsp[0].minor.yy393, yymsp[-2].minor.yy341);
	  yy_destructor(yypParser,76,&yymsp[-1].minor);
}
#line 4940 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 280: /* object_lst ::= object_spec */
#line 1781 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy341 = yymsp[0].minor.yy341;
	}
#line 4947 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 281: /* object_lst ::= object_lst COMMA object_spec */
#line 1785 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy341 = yymsp[-2].minor.yy341;
		yygotominor.yy341->splice(yygotominor.yy341->end(), *yymsp[0].minor.yy341);
		delete yymsp[0].minor.yy341;
	  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 4957 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 282: /* object_spec ::= IDENTIFIER */
#line 1794 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Token> id_ptr = yymsp[0].minor.yy0;
		yygotominor.yy341 = NULL;
		ref_ptr<const ObjectSymbol> o = parser->symtab()->resolveOrCreate(new ObjectSymbol(yymsp[0].minor.yy0->str()));
		if (!o) {
			parser->_parse_error("Detected a conflicting definition of \"" + Symbol::genName(*yymsp[0].minor.yy0->str(),0) + "\".", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy341 = new ObjectDeclaration::Element::ObjectList();
			yygotominor.yy341->push_back(o);
		}
	}
#line 4973 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 283: /* object_spec ::= IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1807 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy341 = NULL;
		ref_ptr<ObjectSymbol::SortList> lst_ptr = yymsp[-1].minor.yy411;
		ref_ptr<const Token> id_ptr = yymsp[-3].minor.yy0;
		ref_ptr<const ObjectSymbol> o = parser->symtab()->resolveOrCreate(new ObjectSymbol(yymsp[-3].minor.yy0->str(), yymsp[-1].minor.yy411));
		if (!o) {
			parser->_parse_error("Detected a conflicting definition of \"" + Symbol::genName(*yymsp[-3].minor.yy0->str(),yymsp[-1].minor.yy411->size()) + "\".", &yymsp[-3].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy341 = new  ObjectDeclaration::Element::ObjectList();
			yygotominor.yy341->push_back(o);
		}
	  yy_destructor(yypParser,71,&yymsp[-2].minor);
  yy_destructor(yypParser,72,&yymsp[0].minor);
}
#line 4992 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 284: /* object_spec ::= INTEGER */
#line 1822 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy341 = NULL;
		ref_ptr<const Token> id_ptr = yymsp[0].minor.yy0;
		ref_ptr<const ObjectSymbol> o = parser->symtab()->resolveOrCreate(new ObjectSymbol(yymsp[0].minor.yy0->str()));
		if (!o) {
			parser->_parse_error("Detected a conflicting definition of \"" + Symbol::genName(*yymsp[0].minor.yy0->str(),0) + "\".", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy341 = new ObjectDeclaration::Element::ObjectList();
			yygotominor.yy341->push_back(o);
		}
	}
#line 5008 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 285: /* object_spec ::= num_range */
#line 1836 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy341 = new ObjectDeclaration::Element::ObjectList();
		ref_ptr<const Referenced> nr_ptr = yymsp[0].minor.yy309;

		// iterate over the range and add it to the list
		for (int i = yymsp[0].minor.yy309->min(); i <= yymsp[0].minor.yy309->max(); i++) {
			std::string name = boost::lexical_cast<std::string>(i);
			ref_ptr<const ObjectSymbol> o = parser->symtab()->resolveOrCreate(new ObjectSymbol(new ReferencedString(name)));
			if (!o) {
				parser->_parse_error("INTERNAL ERROR: Could not create object symbol \"" + Symbol::genName(name, 0) + "\".", &yymsp[0].minor.yy309->beginLoc());
				YYERROR;
			} else {
				yygotominor.yy341->push_back(o);
			}
		}
	}
#line 5028 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 286: /* stmt_variable_def ::= COLON_DASH VARIABLES variable_bnd_lst PERIOD */
#line 1868 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0;
		ref_ptr<const Token> p_ptr = yymsp[0].minor.yy0;
		ref_ptr<const Token> kw_ptr = yymsp[-2].minor.yy0;
		ref_ptr<VariableDeclaration::ElementList> l_ptr = yymsp[-1].minor.yy117;
		
		if (!parser->lang()->support(Language::Feature::DECL_VARIABLE)) {
			yygotominor.yy171 = NULL;
			parser->_feature_error(Language::Feature::DECL_VARIABLE, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {

			VariableSymbol* v2;

			// Go ahead and add them to the symbol table
			BOOST_FOREACH(ref_ptr<VariableSymbol>& v, *yymsp[-1].minor.yy117) {
				if (!(v2 = parser->symtab()->resolveOrCreate(v))) {
					// Check if it's a duplicate
					v2 = (VariableSymbol*)parser->symtab()->resolve(Symbol::Type::VARIABLE, *v->base());
					if (!v2 || v2 != v) {
						parser->_parse_error("Detected conflicting definition of symbol \"" + *v->name() + "\".", &yymsp[-3].minor.yy0->beginLoc());
					} else {
						parser->_parse_error("Detected a duplicate definition of symbol \"" + *v->name() + "\".", &yymsp[-3].minor.yy0->beginLoc());
					}
				} else {
					v = v2;
				}
			}

			yygotominor.yy171 = new VariableDeclaration(yymsp[-1].minor.yy117, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());


		}
	}
#line 5066 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 287: /* variable_bnd_lst ::= variable_bnd */
#line 1904 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy117 = yymsp[0].minor.yy117;
	}
#line 5073 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 288: /* variable_bnd_lst ::= variable_bnd_lst SEMICOLON variable_bnd */
#line 1909 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy117 = yymsp[-2].minor.yy117;
		yygotominor.yy117->splice(yygotominor.yy117->end(), *yymsp[0].minor.yy117);
		delete yymsp[0].minor.yy117;
	  yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 5083 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 289: /* variable_bnd ::= variable_lst DBL_COLON sort_id */
#line 1916 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy117 = new VariableDeclaration::ElementList();

		BOOST_FOREACH(Token const* tok, *yymsp[-2].minor.yy152) {
			yygotominor.yy117->push_back(new VariableSymbol(tok->str(), yymsp[0].minor.yy393));
		}



		delete yymsp[-2].minor.yy152;
	  yy_destructor(yypParser,76,&yymsp[-1].minor);
}
#line 5099 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 290: /* variable_lst ::= IDENTIFIER */
#line 1929 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy152 = new TokenList();
		yygotominor.yy152->push_back(yymsp[0].minor.yy0);
	}
#line 5107 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 291: /* variable_lst ::= variable_lst COMMA IDENTIFIER */
#line 1934 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy152 = yymsp[-2].minor.yy152;
		yygotominor.yy152->push_back(yymsp[0].minor.yy0);
	  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 5116 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 292: /* stmt_sort_def ::= COLON_DASH SORTS sort_bnd_lst PERIOD */
#line 1955 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0;
		ref_ptr<const Token> p_ptr = yymsp[0].minor.yy0;
		ref_ptr<const Token> kw_ptr = yymsp[-2].minor.yy0;
		ref_ptr<SortDeclaration::ElementList> l_ptr = yymsp[-1].minor.yy320;
		
		if (!parser->lang()->support(Language::Feature::DECL_SORT)) {
			yygotominor.yy469 = NULL;
			parser->_feature_error(Language::Feature::DECL_SORT, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy469 = new SortDeclaration(yymsp[-1].minor.yy320, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		}
	}
#line 5134 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 293: /* sort_bnd_lst ::= sort_bnd */
      case 295: /* sort_bnd ::= sort_dcl_lst */ yytestcase(yyruleno==295);
#line 1971 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy320 = yymsp[0].minor.yy320;
	}
#line 5142 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 294: /* sort_bnd_lst ::= sort_bnd_lst SEMICOLON sort_bnd */
#line 1976 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy320 = yymsp[-2].minor.yy320;
		yygotominor.yy320->splice(yygotominor.yy320->end(), *yymsp[0].minor.yy320);
		delete yymsp[0].minor.yy320;
	  yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 5152 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 296: /* sort_bnd ::= sort_bnd DBL_LTHAN sort_bnd */
#line 1988 "bcplus/parser/detail/lemon_parser.y"
{
		BOOST_FOREACH(SortSymbol* sym, *yymsp[-2].minor.yy320) {
			BOOST_FOREACH(SortSymbol* sym2, *yymsp[0].minor.yy320) {
				sym2->addSubSort(sym);
			}
		}
		yygotominor.yy320 = yymsp[-2].minor.yy320;
		yygotominor.yy320->splice(yymsp[-2].minor.yy320->end(), *yymsp[0].minor.yy320);
		delete yymsp[0].minor.yy320;

	  yy_destructor(yypParser,100,&yymsp[-1].minor);
}
#line 5168 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 297: /* sort_bnd ::= sort_bnd DBL_GTHAN sort_bnd */
#line 2000 "bcplus/parser/detail/lemon_parser.y"
{
		BOOST_FOREACH(SortSymbol* sym, *yymsp[-2].minor.yy320) {
			BOOST_FOREACH(SortSymbol* sym2, *yymsp[0].minor.yy320) {
				sym->addSubSort(sym2);
			}
		}
		yygotominor.yy320 = yymsp[-2].minor.yy320;
		yygotominor.yy320->splice(yymsp[-2].minor.yy320->end(), *yymsp[0].minor.yy320);
		delete yymsp[0].minor.yy320;
	  yy_destructor(yypParser,99,&yymsp[-1].minor);
}
#line 5183 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 298: /* sort_bnd ::= PAREN_L sort_bnd PAREN_R */
#line 2011 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy320 = yymsp[-1].minor.yy320;
	  yy_destructor(yypParser,71,&yymsp[-2].minor);
  yy_destructor(yypParser,72,&yymsp[0].minor);
}
#line 5192 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 299: /* sort_dcl_lst ::= IDENTIFIER */
#line 2016 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<SortSymbol> s = parser->symtab()->resolveOrCreate(new SortSymbol(yymsp[0].minor.yy0->str()));
		if (!s) {
			yygotominor.yy320 = NULL;
			parser->_parse_error("Detected conflicting definition of sort symbol \"" + Symbol::genName(*yymsp[0].minor.yy0->str(),0) + "\".", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy320 = new SortDeclaration::ElementList();
			yygotominor.yy320->push_back(s);
		}

		delete yymsp[0].minor.yy0;
	}
#line 5209 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 300: /* sort_dcl_lst ::= sort_dcl_lst COMMA IDENTIFIER */
#line 2030 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy320 = yymsp[-2].minor.yy320;
		ref_ptr<SortSymbol> s = parser->symtab()->resolveOrCreate(new SortSymbol(yymsp[0].minor.yy0->str()));
		if (!s) {
			yymsp[-2].minor.yy320 = NULL;
			parser->_parse_error("Detected conflicting definition of sort symbol \"" + Symbol::genName(*yymsp[0].minor.yy0->str(),0) + "\".", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yymsp[-2].minor.yy320->push_back(s);
		}

		delete yymsp[0].minor.yy0;

	  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 5228 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 301: /* stmt_show ::= COLON_DASH SHOW show_lst PERIOD */
#line 2057 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy224 = NULL;
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0, kw_ptr = yymsp[-2].minor.yy0, p_ptr = yymsp[0].minor.yy0;
		ref_ptr<ShowStatement::ElementList> lst_ptr = yymsp[-1].minor.yy451;

		if (!parser->lang()->support(Language::Feature::DECL_SHOW)) {
			parser->_feature_error(Language::Feature::DECL_SHOW, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy224 = new ShowStatement(yymsp[-1].minor.yy451, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		}
	}
#line 5244 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 302: /* stmt_show ::= COLON_DASH SHOW ALL PERIOD */
#line 2071 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy224 = NULL;
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0, kw_ptr = yymsp[-2].minor.yy0, p_ptr = yymsp[0].minor.yy0, all_ptr = yymsp[-1].minor.yy0;
		
		if (!parser->lang()->support(Language::Feature::DECL_SHOW)) {
			parser->_feature_error(Language::Feature::DECL_SHOW, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else if (!parser->lang()->support(Language::Feature::DECL_SHOW_ALL)) {
			parser->_feature_error(Language::Feature::DECL_SHOW_ALL, &yymsp[-1].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy224 = new ShowAllStatement(yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		}
	}
#line 5262 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 303: /* stmt_hide ::= COLON_DASH HIDE show_lst PERIOD */
#line 2088 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy224 = NULL;
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0, kw_ptr = yymsp[-2].minor.yy0, p_ptr = yymsp[0].minor.yy0;
		ref_ptr<HideStatement::ElementList> lst_ptr = yymsp[-1].minor.yy451;

		if (!parser->lang()->support(Language::Feature::DECL_HIDE)) {
			parser->_feature_error(Language::Feature::DECL_HIDE, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy224 = new HideStatement(yymsp[-1].minor.yy451, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		}
	}
#line 5278 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 304: /* stmt_hide ::= COLON_DASH HIDE ALL PERIOD */
#line 2102 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy224 = NULL;
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0, kw_ptr = yymsp[-2].minor.yy0, p_ptr = yymsp[0].minor.yy0, all_ptr = yymsp[-1].minor.yy0;
		
		if (!parser->lang()->support(Language::Feature::DECL_HIDE)) {
			parser->_feature_error(Language::Feature::DECL_HIDE, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else if (!parser->lang()->support(Language::Feature::DECL_HIDE_ALL)) {
			parser->_feature_error(Language::Feature::DECL_HIDE_ALL, &yymsp[-1].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy224 = new HideAllStatement(yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		}
	}
#line 5296 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 305: /* show_lst ::= show_elem */
#line 2120 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy451 = new ShowStatement::ElementList();
		yygotominor.yy451->push_back(yymsp[0].minor.yy138);
	}
#line 5304 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 306: /* show_lst ::= show_lst COMMA show_elem */
#line 2125 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy451 = yymsp[-2].minor.yy451;
		yygotominor.yy451->push_back(yymsp[0].minor.yy138);
	  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 5313 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 308: /* stmt_noconcurrency ::= NOCONCURRENCY PERIOD */
#line 2153 "bcplus/parser/detail/lemon_parser.y"
{ NC_STATEMENT(yygotominor.yy210, yymsp[-1].minor.yy0, yymsp[0].minor.yy0, Language::Feature::NOCONCURRENCY, NCStatement); }
#line 5318 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 309: /* stmt_strong_noconcurrency ::= STRONG_NOCONCURRENCY PERIOD */
#line 2154 "bcplus/parser/detail/lemon_parser.y"
{ NC_STATEMENT(yygotominor.yy482, yymsp[-1].minor.yy0, yymsp[0].minor.yy0, Language::Feature::STRONG_NOCONCURRENCY, StrongNCStatement); }
#line 5323 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 310: /* stmt_maxafvalue ::= COLON_DASH MAXAFVALUE EQ term_numeric PERIOD */
#line 2180 "bcplus/parser/detail/lemon_parser.y"
{ VALUE_DECL(yygotominor.yy224, yymsp[-4].minor.yy0, yymsp[-3].minor.yy0, yymsp[-1].minor.yy416, yymsp[0].minor.yy0, Language::Feature::DECL_MAXAFVALUE, MaxAFValueStatement);   yy_destructor(yypParser,81,&yymsp[-2].minor);
}
#line 5329 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 311: /* stmt_maxadditive ::= COLON_DASH MAXADDITIVE EQ term_numeric PERIOD */
#line 2181 "bcplus/parser/detail/lemon_parser.y"
{ VALUE_DECL(yygotominor.yy224, yymsp[-4].minor.yy0, yymsp[-3].minor.yy0, yymsp[-1].minor.yy416, yymsp[0].minor.yy0, Language::Feature::DECL_MAXADDITIVE, MaxAdditiveStatement);   yy_destructor(yypParser,81,&yymsp[-2].minor);
}
#line 5335 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 312: /* stmt_query ::= COLON_DASH QUERY query_lst PERIOD */
#line 2206 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy146 = NULL;
		ref_ptr<const Referenced> cd_ptr = yymsp[-3].minor.yy0, kw_ptr = yymsp[-2].minor.yy0, data_l_ptr = yymsp[-1].minor.yy301.l, p_ptr = yymsp[0].minor.yy0;
		ref_ptr<const Referenced> data_maxstep_ptr = yymsp[-1].minor.yy301.maxstep, data_label_ptr = yymsp[-1].minor.yy301.label;

		ref_ptr<const ReferencedString> label;
		if (yymsp[-1].minor.yy301.label) label = yymsp[-1].minor.yy301.label->str();
		else label = new ReferencedString("0");

		int min = -1, max = -1;
		if (yymsp[-1].minor.yy301.maxstep) {
			min = yymsp[-1].minor.yy301.maxstep->min();
			max = yymsp[-1].minor.yy301.maxstep->max();
		}

		if (!parser->lang()->support(Language::Feature::DECL_QUERY)) {
			parser->_feature_error(Language::Feature::DECL_QUERY, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			bool good = true;

			// resolve the query label
			ref_ptr<QuerySymbol> sym = new QuerySymbol(label, min, max);
			if (!parser->symtab()->create(sym)) {
				parser->_parse_error("Could not create query, the label \"" + *label + "\" already exists.", (yymsp[-1].minor.yy301.label ? &yymsp[-1].minor.yy301.label->beginLoc() : &yymsp[-2].minor.yy0->beginLoc()));
				good = false;
				YYERROR;
			}


			if (good) yygotominor.yy146 = new QueryStatement(sym, yymsp[-1].minor.yy301.l, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		}
	}
#line 5372 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 313: /* query_lst ::= formula_temporal */
#line 2242 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy301.l = new QueryStatement::FormulaList();
		yygotominor.yy301.maxstep = NULL;
		yygotominor.yy301.label = NULL;

		yygotominor.yy301.l->push_back(yymsp[0].minor.yy353);
	}
#line 5383 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 314: /* query_lst ::= query_maxstep_decl */
#line 2251 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy301.l = new QueryStatement::FormulaList();
		yygotominor.yy301.maxstep = yymsp[0].minor.yy392;
		yygotominor.yy301.label = NULL;
	}
#line 5392 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 315: /* query_lst ::= query_label_decl */
#line 2258 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy301.l = new QueryStatement::FormulaList();
		yygotominor.yy301.maxstep = NULL;
		yygotominor.yy301.label = yymsp[0].minor.yy251;
	}
#line 5401 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 316: /* query_lst ::= query_lst SEMICOLON formula_temporal */
#line 2265 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy301 = yymsp[-2].minor.yy301;
		yymsp[-2].minor.yy301.l->push_back(yymsp[0].minor.yy353);
	  yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 5410 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 317: /* query_lst ::= query_lst SEMICOLON query_maxstep_decl */
#line 2271 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy301 = yymsp[-2].minor.yy301;

		if (yygotominor.yy301.maxstep) {
			parser->_parse_error("Encountered multiple maxstep definitions within a query.", &yymsp[0].minor.yy392->beginLoc());
			delete yymsp[0].minor.yy392;
			YYERROR;
		} else {
			yygotominor.yy301.maxstep = yymsp[0].minor.yy392;
		}
	  yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 5426 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 318: /* query_lst ::= query_lst SEMICOLON query_label_decl */
#line 2284 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy301 = yymsp[-2].minor.yy301;
		if (yygotominor.yy301.label) {
			parser->_parse_error("Encountered multiple maxstep definitions within a query.", &yymsp[0].minor.yy251->beginLoc());
			delete yymsp[0].minor.yy251;
			YYERROR;

		} else {
			yygotominor.yy301.label = yymsp[0].minor.yy251;
		}
	  yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 5442 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 319: /* query_maxstep_decl ::= MAXSTEP DBL_COLON INTEGER */
#line 2310 "bcplus/parser/detail/lemon_parser.y"
{ 
	yygotominor.yy392 = NULL;
	ref_ptr<const Referenced> kw_ptr = yymsp[-2].minor.yy0, i_ptr = yymsp[0].minor.yy0;


	if (!parser->lang()->support(Language::Feature::QUERY_MAXSTEP)) {
		parser->_feature_error(Language::Feature::QUERY_MAXSTEP, &yymsp[-2].minor.yy0->beginLoc());
		YYERROR;
	} else {

		int max = -1;
		try {
			max = boost::lexical_cast<int>(*yymsp[0].minor.yy0->str());
			yygotominor.yy392 = new NumberRange(-1, max, yymsp[0].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		} catch (boost::bad_lexical_cast const& e) {
			parser->_parse_error("INTERNAL ERROR: An error occurred extracting an integer from \"" + *yymsp[0].minor.yy0->str() + "\".", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
  yy_destructor(yypParser,76,&yymsp[-1].minor);
}
#line 5467 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 320: /* query_maxstep_decl ::= MAXSTEP DBL_COLON num_range */
#line 2331 "bcplus/parser/detail/lemon_parser.y"
{
	yygotominor.yy392 = NULL;
	ref_ptr<const Referenced> kw_ptr = yymsp[-2].minor.yy0, nr_ptr = yymsp[0].minor.yy309;

	if (!parser->lang()->support(Language::Feature::QUERY_MAXSTEP)) {
		parser->_feature_error(Language::Feature::QUERY_MAXSTEP, &yymsp[-2].minor.yy0->beginLoc());
		YYERROR;
	} else {
		yygotominor.yy392 = yymsp[0].minor.yy309;
		nr_ptr.release();
	}
  yy_destructor(yypParser,76,&yymsp[-1].minor);
}
#line 5484 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 321: /* query_label_decl ::= LABEL DBL_COLON INTEGER */
      case 322: /* query_label_decl ::= LABEL DBL_COLON IDENTIFIER */ yytestcase(yyruleno==322);
#line 2345 "bcplus/parser/detail/lemon_parser.y"
{ QUERY_DECL(yygotominor.yy251, yymsp[-2].minor.yy0, yymsp[0].minor.yy0, Language::Feature::QUERY_LABEL);   yy_destructor(yypParser,76,&yymsp[-1].minor);
}
#line 5491 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 323: /* clause_if ::= IF formula */
#line 2380 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy353, yymsp[-1].minor.yy0, yymsp[0].minor.yy353, Language::Feature::CLAUSE_IF); 		}
#line 5496 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 324: /* clause_if ::= */
      case 326: /* clause_after ::= */ yytestcase(yyruleno==326);
      case 328: /* clause_ifcons ::= */ yytestcase(yyruleno==328);
      case 332: /* clause_where ::= */ yytestcase(yyruleno==332);
#line 2381 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy353 = NULL; }
#line 5504 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 325: /* clause_after ::= AFTER formula */
#line 2382 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy353, yymsp[-1].minor.yy0, yymsp[0].minor.yy353, Language::Feature::CLAUSE_AFTER);	}
#line 5509 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 327: /* clause_ifcons ::= IFCONS formula */
#line 2384 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy353, yymsp[-1].minor.yy0, yymsp[0].minor.yy353, Language::Feature::CLAUSE_IFCONS); 	}
#line 5514 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 329: /* clause_unless ::= UNLESS atomic_formula_anon */
#line 2386 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy138, yymsp[-1].minor.yy0, yymsp[0].minor.yy138, Language::Feature::CLAUSE_UNLESS); 	}
#line 5519 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 330: /* clause_unless ::= */
#line 2387 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy138 = NULL; }
#line 5524 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 331: /* clause_where ::= WHERE formula_no_const */
#line 2388 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy353, yymsp[-1].minor.yy0, yymsp[0].minor.yy353, Language::Feature::CLAUSE_WHERE); 	}
#line 5529 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 333: /* stmt_law ::= law_basic */
      case 334: /* stmt_law ::= law_caused */ yytestcase(yyruleno==334);
      case 335: /* stmt_law ::= law_pcaused */ yytestcase(yyruleno==335);
      case 336: /* stmt_law ::= law_impl */ yytestcase(yyruleno==336);
      case 337: /* stmt_law ::= law_causes */ yytestcase(yyruleno==337);
      case 338: /* stmt_law ::= law_increments */ yytestcase(yyruleno==338);
      case 339: /* stmt_law ::= law_decrements */ yytestcase(yyruleno==339);
      case 340: /* stmt_law ::= law_mcause */ yytestcase(yyruleno==340);
      case 341: /* stmt_law ::= law_always */ yytestcase(yyruleno==341);
      case 342: /* stmt_law ::= law_constraint */ yytestcase(yyruleno==342);
      case 343: /* stmt_law ::= law_impossible */ yytestcase(yyruleno==343);
      case 344: /* stmt_law ::= law_never */ yytestcase(yyruleno==344);
      case 345: /* stmt_law ::= law_default */ yytestcase(yyruleno==345);
      case 346: /* stmt_law ::= law_exogenous */ yytestcase(yyruleno==346);
      case 347: /* stmt_law ::= law_inertial */ yytestcase(yyruleno==347);
      case 348: /* stmt_law ::= law_nonexecutable */ yytestcase(yyruleno==348);
      case 349: /* stmt_law ::= law_rigid */ yytestcase(yyruleno==349);
      case 350: /* stmt_law ::= law_observed */ yytestcase(yyruleno==350);
#line 2434 "bcplus/parser/detail/lemon_parser.y"
{yygotominor.yy224 = yymsp[0].minor.yy224;}
#line 5551 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 351: /* law_basic ::= head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2551 "bcplus/parser/detail/lemon_parser.y"
{ 
		if (yymsp[-5].minor.yy353 || yymsp[-4].minor.yy353 || yymsp[-3].minor.yy353 || yymsp[-2].minor.yy138 || yymsp[-1].minor.yy353) {
			LAW_BASIC_FORM(yygotominor.yy224, NULL, yymsp[-6].minor.yy353, yymsp[-5].minor.yy353, yymsp[-4].minor.yy353, yymsp[-3].minor.yy353, 
				yymsp[-2].minor.yy138, yymsp[-1].minor.yy353, yymsp[0].minor.yy0, Language::Feature::LAW_BASIC_S, 
				Language::Feature::LAW_BASIC_D, BasicLaw); 
		} else {
			LAW_BASIC_FORM(yygotominor.yy224, NULL, yymsp[-6].minor.yy353, yymsp[-5].minor.yy353, yymsp[-4].minor.yy353, yymsp[-3].minor.yy353, 
				yymsp[-2].minor.yy138, yymsp[-1].minor.yy353, yymsp[0].minor.yy0, Language::Feature::LAW_BASIC_FACT, 
				Language::Feature::LAW_BASIC_FACT, BasicLaw); 
		}
	}
#line 5566 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 352: /* law_caused ::= CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2563 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy224, yymsp[-7].minor.yy0, yymsp[-6].minor.yy353, yymsp[-5].minor.yy353, yymsp[-4].minor.yy353, yymsp[-3].minor.yy353, 
																																														yymsp[-2].minor.yy138, yymsp[-1].minor.yy353, yymsp[0].minor.yy0, Language::Feature::LAW_CAUSED_S, 
																																															Language::Feature::LAW_CAUSED_D, CausedLaw); }
#line 5573 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 353: /* law_pcaused ::= POSSIBLY_CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2567 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy224, yymsp[-7].minor.yy0, yymsp[-6].minor.yy353, yymsp[-5].minor.yy353, yymsp[-4].minor.yy353, yymsp[-3].minor.yy353, 
																																														yymsp[-2].minor.yy138, yymsp[-1].minor.yy353, yymsp[0].minor.yy0, Language::Feature::LAW_PCAUSED_S, 
																																															Language::Feature::LAW_PCAUSED_D, PossiblyCausedLaw); }
#line 5580 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 354: /* law_impl ::= head_formula ARROW_LDASH formula clause_where PERIOD */
#line 2571 "bcplus/parser/detail/lemon_parser.y"
{ LAW_IMPL_FORM(yygotominor.yy224, yymsp[-4].minor.yy353, yymsp[-3].minor.yy0, yymsp[-2].minor.yy353, yymsp[-1].minor.yy353, yymsp[0].minor.yy0, 
																																														Language::Feature::LAW_IMPL, ImplicationLaw); }
#line 5586 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 355: /* law_causes ::= atomic_formula CAUSES head_formula clause_if clause_unless clause_where PERIOD */
#line 2574 "bcplus/parser/detail/lemon_parser.y"
{ LAW_DYNAMIC_FORM(yygotominor.yy224, yymsp[-6].minor.yy138, yymsp[-5].minor.yy0, yymsp[-4].minor.yy353, yymsp[-3].minor.yy353, yymsp[-2].minor.yy138, yymsp[-1].minor.yy353, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_CAUSES, CausesLaw); }
#line 5592 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 356: /* law_increments ::= atomic_formula INCREMENTS constant BY term clause_if clause_unless clause_where PERIOD */
#line 2578 "bcplus/parser/detail/lemon_parser.y"
{ LAW_INCREMENTAL_FORM(yygotominor.yy224, yymsp[-8].minor.yy138, yymsp[-7].minor.yy0, yymsp[-6].minor.yy345, yymsp[-4].minor.yy163, yymsp[-3].minor.yy353, yymsp[-2].minor.yy138, yymsp[-1].minor.yy353, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_INCREMENTS, IncrementsLaw);   yy_destructor(yypParser,31,&yymsp[-5].minor);
}
#line 5599 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 357: /* law_decrements ::= atomic_formula DECREMENTS constant BY term clause_if clause_unless clause_where PERIOD */
#line 2581 "bcplus/parser/detail/lemon_parser.y"
{ LAW_INCREMENTAL_FORM(yygotominor.yy224, yymsp[-8].minor.yy138, yymsp[-7].minor.yy0, yymsp[-6].minor.yy345, yymsp[-4].minor.yy163, yymsp[-3].minor.yy353, yymsp[-2].minor.yy138, yymsp[-1].minor.yy353, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_DECREMENTS, DecrementsLaw);   yy_destructor(yypParser,31,&yymsp[-5].minor);
}
#line 5606 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 358: /* law_mcause ::= atomic_formula MAY_CAUSE head_formula clause_if clause_unless clause_where PERIOD */
#line 2585 "bcplus/parser/detail/lemon_parser.y"
{ LAW_DYNAMIC_FORM(yygotominor.yy224, yymsp[-6].minor.yy138, yymsp[-5].minor.yy0, yymsp[-4].minor.yy353, yymsp[-3].minor.yy353, yymsp[-2].minor.yy138, yymsp[-1].minor.yy353, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_MCAUSE, MayCauseLaw); }
#line 5612 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 359: /* law_always ::= ALWAYS formula clause_after clause_unless clause_where PERIOD */
#line 2589 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy224, yymsp[-5].minor.yy0, yymsp[-4].minor.yy353, yymsp[-3].minor.yy353, yymsp[-2].minor.yy138, yymsp[-1].minor.yy353, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_ALWAYS_S, 
																																															Language::Feature::LAW_ALWAYS_D, AlwaysLaw); }
#line 5619 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 360: /* law_constraint ::= CONSTRAINT formula clause_after clause_unless clause_where PERIOD */
#line 2593 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy224, yymsp[-5].minor.yy0, yymsp[-4].minor.yy353, yymsp[-3].minor.yy353, yymsp[-2].minor.yy138, yymsp[-1].minor.yy353, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_CONSTRAINT_S, 
																																															Language::Feature::LAW_CONSTRAINT_D, ConstraintLaw); }
#line 5626 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 361: /* law_impossible ::= IMPOSSIBLE formula clause_after clause_unless clause_where PERIOD */
#line 2597 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy224, yymsp[-5].minor.yy0, yymsp[-4].minor.yy353, yymsp[-3].minor.yy353, yymsp[-2].minor.yy138, yymsp[-1].minor.yy353, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_IMPOSSIBLE_S, 
																																															Language::Feature::LAW_IMPOSSIBLE_D, ImpossibleLaw); }
#line 5633 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 362: /* law_never ::= NEVER formula clause_after clause_unless clause_where PERIOD */
#line 2601 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy224, yymsp[-5].minor.yy0, yymsp[-4].minor.yy353, yymsp[-3].minor.yy353, yymsp[-2].minor.yy138, yymsp[-1].minor.yy353, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_NEVER_S, 
																																															Language::Feature::LAW_NEVER_D, NeverLaw); }
#line 5640 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 363: /* law_default ::= DEFAULT atomic_head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2605 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy224, yymsp[-7].minor.yy0, yymsp[-6].minor.yy138, yymsp[-5].minor.yy353, yymsp[-4].minor.yy353, yymsp[-3].minor.yy353, 
																																														yymsp[-2].minor.yy138, yymsp[-1].minor.yy353, yymsp[0].minor.yy0, Language::Feature::LAW_DEFAULT_S,
																																															Language::Feature::LAW_DEFAULT_D, DefaultLaw); }
#line 5647 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 364: /* law_exogenous ::= EXOGENOUS constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2609 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy224, yymsp[-7].minor.yy0, yymsp[-6].minor.yy345, yymsp[-5].minor.yy353, yymsp[-4].minor.yy353, yymsp[-3].minor.yy353, 
																																														yymsp[-2].minor.yy138, yymsp[-1].minor.yy353, yymsp[0].minor.yy0, Language::Feature::LAW_EXOGENOUS_S,
																																															Language::Feature::LAW_EXOGENOUS_D, ExogenousLaw); }
#line 5654 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 365: /* law_inertial ::= INERTIAL constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2613 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy224, yymsp[-7].minor.yy0, yymsp[-6].minor.yy345, yymsp[-5].minor.yy353, yymsp[-4].minor.yy353, yymsp[-3].minor.yy353, 
																																														yymsp[-2].minor.yy138, yymsp[-1].minor.yy353, yymsp[0].minor.yy0, Language::Feature::LAW_INERTIAL_S,
																																															Language::Feature::LAW_INERTIAL_D, InertialLaw); }
#line 5661 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 366: /* law_nonexecutable ::= NONEXECUTABLE formula clause_if clause_unless clause_where PERIOD */
#line 2617 "bcplus/parser/detail/lemon_parser.y"
{ LAW_DYNAMIC_CONSTRAINT_FORM(yygotominor.yy224, yymsp[-5].minor.yy0, yymsp[-4].minor.yy353, yymsp[-3].minor.yy353, yymsp[-2].minor.yy138, yymsp[-1].minor.yy353,
																																														yymsp[0].minor.yy0, Language::Feature::LAW_NONEXECUTABLE, NonexecutableLaw); }
#line 5667 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 367: /* law_rigid ::= RIGID constant clause_where PERIOD */
#line 2621 "bcplus/parser/detail/lemon_parser.y"
{ LAW_SIMPLE_FORM(yygotominor.yy224, yymsp[-3].minor.yy0, yymsp[-2].minor.yy345, yymsp[-1].minor.yy353, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_RIGID, RigidLaw); }
#line 5673 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 368: /* law_observed ::= OBSERVED atomic_head_formula AT term_no_const PERIOD */
#line 2626 "bcplus/parser/detail/lemon_parser.y"
{ 
			yygotominor.yy224 = NULL;
			ref_ptr<const Token> kw_ptr = yymsp[-4].minor.yy0, p_ptr = yymsp[0].minor.yy0;
			ref_ptr<AtomicFormula> head_ptr = yymsp[-3].minor.yy138;
			ref_ptr<Term> t_ptr = yymsp[-1].minor.yy163;

			// make sure that the At clause is integral
			if (yymsp[-1].minor.yy163->domainType() != DomainType::INTEGRAL) {
				parser->_parse_error("Expected an integral expression.", &yymsp[-1].minor.yy163->beginLoc());
				YYERROR;
			} else {
				LAW_SIMPLE_FORM(yygotominor.yy224, yymsp[-4].minor.yy0, yymsp[-3].minor.yy138, yymsp[-1].minor.yy163, yymsp[0].minor.yy0, Language::Feature::LAW_OBSERVED, ObservedLaw); 
			}
		  yy_destructor(yypParser,65,&yymsp[-2].minor);
}
#line 5692 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 369: /* stmt_code_blk ::= ASP_GR */
#line 2660 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy224, yymsp[0].minor.yy0, Language::Feature::CODE_ASP_GR, ASPBlock);	}
#line 5697 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 370: /* stmt_code_blk ::= ASP_CP */
#line 2661 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy224, yymsp[0].minor.yy0, Language::Feature::CODE_ASP_CP, ASPBlock);	}
#line 5702 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 371: /* stmt_code_blk ::= F2LP_GR */
#line 2662 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy224, yymsp[0].minor.yy0, Language::Feature::CODE_F2LP_GR, F2LPBlock);	}
#line 5707 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 372: /* stmt_code_blk ::= F2LP_CP */
#line 2663 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy224, yymsp[0].minor.yy0, Language::Feature::CODE_F2LP_CP, F2LPBlock); }
#line 5712 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 373: /* stmt_code_blk ::= LUA_GR */
#line 2664 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy224, yymsp[0].minor.yy0, Language::Feature::CODE_LUA_GR, LUABlock);   }
#line 5717 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 374: /* stmt_code_blk ::= LUA_CP */
#line 2665 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy224, yymsp[0].minor.yy0, Language::Feature::CODE_LUA_CP, LUABlock);   }
#line 5722 "bcplus/parser/detail/lemon_parser.c"
        break;
      default:
      /* (1) statement_lst ::= */ yytestcase(yyruleno==1);
      /* (2) statement_lst ::= statement_lst error */ yytestcase(yyruleno==2);
        break;
  };
  yygoto = yyRuleInfo[yyruleno].lhs;
  yysize = yyRuleInfo[yyruleno].nrhs;
  yypParser->yyidx -= yysize;
  yyact = yy_find_reduce_action(yymsp[-yysize].stateno,(YYCODETYPE)yygoto);
  if( yyact < YYNSTATE ){
#ifdef NDEBUG
    /* If we are not debugging and the reduce action popped at least
    ** one element off the stack, then we can push the new element back
    ** onto the stack here, and skip the stack overflow test in yy_shift().
    ** That gives a significant speed improvement. */
    if( yysize ){
      yypParser->yyidx++;
      yymsp -= yysize-1;
      yymsp->stateno = (YYACTIONTYPE)yyact;
      yymsp->major = (YYCODETYPE)yygoto;
      yymsp->minor = yygotominor;
    }else
#endif
    {
      yy_shift(yypParser,yyact,yygoto,&yygotominor);
    }
  }else{
    assert( yyact == YYNSTATE + YYNRULE + 1 );
    yy_accept(yypParser);
  }
}

/*
** The following code executes when the parse fails
*/
#ifndef YYNOERRORRECOVERY
static void yy_parse_failed(
  yyParser *yypParser           /* The parser */
){
  lemon_parserARG_FETCH;
#ifndef NDEBUG
  if( yyTraceFILE ){
    fprintf(yyTraceFILE,"%sFail!\n",yyTracePrompt);
  }
#endif
  while( yypParser->yyidx>=0 ) yy_pop_parser_stack(yypParser);
  /* Here code is inserted which will be executed whenever the
  ** parser fails */
  lemon_parserARG_STORE; /* Suppress warning about unused %extra_argument variable */
}
#endif /* YYNOERRORRECOVERY */

/*
** The following code executes when a syntax error first occurs.
*/
static void yy_syntax_error(
  yyParser *yypParser,           /* The parser */
  int yymajor,                   /* The major type of the error token */
  YYMINORTYPE yyminor            /* The minor type of the error token */
){
  lemon_parserARG_FETCH;
#define TOKEN (yyminor.yy0)
#line 198 "bcplus/parser/detail/lemon_parser.y"
 parser->_parse_error("Syntax error.");	
#line 5788 "bcplus/parser/detail/lemon_parser.c"
  lemon_parserARG_STORE; /* Suppress warning about unused %extra_argument variable */
}

/*
** The following is executed when the parser accepts
*/
static void yy_accept(
  yyParser *yypParser           /* The parser */
){
  lemon_parserARG_FETCH;
#ifndef NDEBUG
  if( yyTraceFILE ){
    fprintf(yyTraceFILE,"%sAccept!\n",yyTracePrompt);
  }
#endif
  while( yypParser->yyidx>=0 ) yy_pop_parser_stack(yypParser);
  /* Here code is inserted which will be executed whenever the
  ** parser accepts */
  lemon_parserARG_STORE; /* Suppress warning about unused %extra_argument variable */
}


/*
** Handles a syntax error within the parser.
*/
static void yy_handle_err(yyParser* yypParser, int* yyerrorhit) {
      int yyact;
#ifdef YYERRORSYMBOL
  int yymx;
#endif
#ifndef NDEBUG
      if( yyTraceFILE ){
        fprintf(yyTraceFILE,"%sSyntax Error!\n",yyTracePrompt);
      }
#endif
#ifdef YYERRORSYMBOL
      /* A syntax error has occurred.
      ** The response to an error depends upon whether or not the
      ** grammar defines an error token "ERROR".  
      **
      ** This is what we do if the grammar does define ERROR:
      **
      **  * Call the %syntax_error function.
      **
      **  * Begin popping the stack until we enter a state where
      **    it is legal to shift the error symbol, then shift
      **    the error symbol.
      **
      **  * Set the error count to three.
      **
      **  * Begin accepting and shifting new tokens.  No new error
      **    processing will occur until three tokens have been
      **    shifted successfully.
      **
      */
	  yyact = YY_ERROR_ACTION;
      if( yypParser->yyerrcnt<0 ){
        yy_syntax_error(yypParser,yypParser->yylookmajor,yypParser->yylookminor);
      }
      yymx = yypParser->yystack[yypParser->yyidx].major;
      if( yymx==YYERRORSYMBOL || *yyerrorhit ){
#ifndef NDEBUG
        if( yyTraceFILE ){
          fprintf(yyTraceFILE,"%sDiscard input token %s\n",
             yyTracePrompt,yyTokenName[yypParser->yylookmajor]);
        }
#endif
        yy_destructor(yypParser, (YYCODETYPE)yypParser->yylookmajor,&yypParser->yylookminor);
        yypParser->yylookmajor = YYNOCODE;
        yypParser->yylookminor = yyzerominor;
      }else{
         while(
          yypParser->yyidx >= 0 &&
          yymx != YYERRORSYMBOL &&
          (yyact = yy_find_reduce_action(
                        yypParser->yystack[yypParser->yyidx].stateno,
                        YYERRORSYMBOL)) >= YYNSTATE
        ){
          yy_pop_parser_stack(yypParser);
        }
        if( yypParser->yyidx < 0 || yypParser->yylookmajor==0 ){
          yy_destructor(yypParser,(YYCODETYPE)yypParser->yylookmajor,&yypParser->yylookminor);
          yy_parse_failed(yypParser);
          yypParser->yylookmajor = YYNOCODE;
          yypParser->yylookminor = yyzerominor;
        }else if( yymx!=YYERRORSYMBOL ){
          YYMINORTYPE u2;
          u2.YYERRSYMDT = 0;
          yy_shift(yypParser,yyact,YYERRORSYMBOL,&u2);
        }
      }
      yypParser->yyerrcnt = 3;
      *yyerrorhit = 1;
#elif defined(YYNOERRORRECOVERY)
      /* If the YYNOERRORRECOVERY macro is defined, then do not attempt to
      ** do any kind of error recovery.  Instead, simply invoke the syntax
      ** error routine and continue going as if nothing had happened.
      **
      ** Applications can set this macro (for example inside %include) if
      ** they intend to abandon the parse upon the first syntax error seen.
      */
      yy_syntax_error(yypParser,yypParser->yylookmajor,yypParser->yylookminor);
      yy_destructor(yypParser,(YYCODETYPE)yypParser->yylookmajor,&yypParser->yylookminor);
      yypParser->yylookmajor = YYNOCODE;
      yypParser->yylookminor = yyzerominor;
      
#else  /* YYERRORSYMBOL is not defined */
      /* This is what we do if the grammar does not define ERROR:
      **
      **  * Report an error message, and throw away the input token.
      **
      **  * If the input token is $, then fail the parse.
      **
      ** As before, subsequent error messages are suppressed until
      ** three input tokens have been successfully shifted.
      */
      if( yypParser->yyerrcnt<=0 ){
        yy_syntax_error(yypParser,yypParser->yylookmajor,yypParser->yylookminor);
      }
      yypParser->yyerrcnt = 3;
      yy_destructor(yypParser,(YYCODETYPE)yypParser->yylookmajor,&yypParser->yylookminor);
      if( yyendofinput ){
        yy_parse_failed(yypParser);
      }
      yypParser->yylookmajor = YYNOCODE;
      yypParser->yylookminor = yyzerominor;
#endif
      yypParser->yysyntaxerr = 0;
}


/*
** Prepares the parser to accept tokens injected at the current
** location by extracting the lookahead token so that it can be
** reintroduced into the stream.
** Also pops the latest symbol off the parser's stack if the pop
** option is asserted.
** 
** returns the major type of the lookahead token that has been 
** cleared from the parser or YYNOCODE and sets the lookahead minor
** type appropriately.
*/
int lemon_parserPreInject(void* yyp, int pop, lemon_parserTOKENTYPE* lookahead) {
	yyParser* pParser = (yyParser*)yyp;
	int code = pParser->yylookmajor;
	if (pop && pParser->yyidx) yy_pop_parser_stack(pParser);
	if (code != YYNOCODE) {
		*lookahead = pParser->yylookminor.yy0;
		pParser->yylookmajor = YYNOCODE;
	    pParser->yylookminor = yyzerominor;
		return code;
	} else {
		*lookahead = yyzerominor.yy0;
		return 0;

	}
}

/*
** Gets the name of the provided token.
** Primarily for debugging purposes.
**
*/
char const* lemon_parserTokenName(int tok) {
	if (tok < 1) return "<INVALID_TOKEN>";
	else if (tok == YYNOCODE) return "<NOCODE_TOKEN>";
#ifdef YYERRORSYMBOL
	else if (tok == YYERRORSYMBOL) return "<ERROR_TOKEN>";
#endif
	return yyTokenName[tok];
}


/*
** Checks to see if there is a next-token independent reduction rule
** and executes it.
*/
void lemon_parserAttemptReduce(void* yyp lemon_parserARG_PDECL) {
	yyParser* yypParser = (yyParser*)yyp;
	lemon_parserARG_STORE;
	int act = 0;
	int yyerrorhit = 0;
	do {
		yypParser->yysyntaxerr = 0;
		act = yy_find_reduce_action(yypParser->yystack[yypParser->yyidx].stateno, YYNOCODE);
		if (act >= YYNSTATE && act < YYNSTATE + YYNRULE) {
			// There is a reduce action. Do it.
			yy_reduce(yypParser, act-YYNSTATE);	
		}

		if (yypParser->yysyntaxerr) {
			yy_handle_err(yypParser, &yyerrorhit);
		}

	} while (act >= YYNSTATE && act < YYNSTATE + YYNRULE);
}

/* The main parser program.
** The first argument is a pointer to a structure obtained from
** "lemon_parserAlloc" which describes the current state of the parser.
** The second argument is the major token number.  The third is
** the minor token.  The fourth optional argument is whatever the
** user wants (and specified in the grammar) and is available for
** use by the action routines.
**
** Inputs:
** <ul>
** <li> A pointer to the parser (an opaque structure.)
** <li> The major token number.
** <li> The minor token number.
** <li> An option argument of a grammar-specified type.
** </ul>
**
** Outputs:
** None.
*/
void lemon_parser(
  void *yyp,                   /* The parser */
  int yymajor,                 /* The major token code number */
  lemon_parserTOKENTYPE yyminor       /* The value for the token */
  lemon_parserARG_PDECL               /* Optional %extra_argument parameter */
){
  int yyact;            /* The parser action. */
  int yyendofinput;     /* True if we are at the end of input */
  int yyerrorhit = 0;
  yyParser *yypParser;  /* The parser */

  /* (re)initialize the parser, if necessary */
  yypParser = (yyParser*)yyp;
  if( yypParser->yyidx<0 ){
#if YYSTACKDEPTH<=0
    if( yypParser->yystksz <=0 ){
      /*memset(&yyminorunion, 0, sizeof(yyminorunion));*/
      yypParser->yylookmajor = YYNOCODE;
      yypParser->yylookminor = yyzerominor;
      yyStackOverflow(yypParser/*, &yyminorunion */);
      return;
    }
#endif
    yypParser->yyidx = 0;
    yypParser->yyerrcnt = -1;
    yypParser->yystack[0].stateno = 0;
    yypParser->yystack[0].major = 0;
  }
  yypParser->yylookmajor = yymajor;
  yypParser->yylookminor.yy0 = yyminor;
  yyendofinput = (yypParser->yylookmajor==0);
  lemon_parserARG_STORE;

#ifndef NDEBUG
  if( yyTraceFILE ){
    fprintf(yyTraceFILE,"%sInput %s\n",yyTracePrompt,yyTokenName[yypParser->yylookmajor]);
  }
#endif

  do{
    yyact = yy_find_shift_action(yypParser,(YYCODETYPE)yypParser->yylookmajor);
    if( yyact<YYNSTATE ){
      assert( !yyendofinput );  /* Impossible to shift the $ token */
      yy_shift(yypParser,yyact,yypParser->yylookmajor,&yypParser->yylookminor);
      yypParser->yyerrcnt--;
      yypParser->yylookmajor = YYNOCODE;
      yypParser->yylookminor = yyzerominor;
    }else if( yyact < YYNSTATE + YYNRULE ) {
      yy_reduce(yypParser,yyact-YYNSTATE);
    } else {
      	assert( yyact == YY_ERROR_ACTION );
		yypParser->yysyntaxerr = 1; 
	}
	
	if (yypParser->yysyntaxerr) {
		yy_handle_err(yypParser, &yyerrorhit);
    }
  }while( yypParser->yylookmajor!=YYNOCODE && yypParser->yyidx>=0 );
  return;
}
