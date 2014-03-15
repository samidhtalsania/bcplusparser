#include "bcplus/languages/Language.h"

namespace bcplus {
namespace languages {

char const* Language::Feature::descr(Value feature) {
	switch (feature) {
		case DECL_INCLUDE:               return "include statements";
		case DECL_MACRO:                 return "macro expansion";
		case DECL_SORT:                  return "sorts";
		case DECL_OBJECT:                return "objects";
		case DECL_CONSTANT:              return "constants";
		case DECL_VARIABLE:              return "variables";
		case DECL_SHOW:                  return "show statements";
		case DECL_MAXAFVALUE:            return "maxAFValue declarations";
		case DECL_MAXADDITIVE:           return "maxAdditive declarations";

		case FORMULA_NESTED:             return "arbitrary nested formulas";
		case FORMULA_CHOICE:             return "choice rules";

		case CLAUSE_IF:                  return "\"if\" clauses";
		case CLAUSE_AFTER:               return "\"after\" clauses";
		case CLAUSE_IFCONS:              return "\"ifcons\" clauses";
		case CLAUSE_ASSUMING:            return "\"assuming\" clauses";
		case CLAUSE_UNLESS:              return "\"unless\" clauses";
		case CLAUSE_WHERE:               return "\"where\" clauses";

		case LAW_BASIC_S:                return "static basic laws (F if G)";
		case LAW_BASIC_D:                return "dynamic basic laws (F if G after H)";

		case LAW_CAUSED_S:               return "static causal laws (caused F if G)";
		case LAW_CAUSED_D:               return "dynamic causal laws (caused F if G after H)";
		case LAW_PCAUSED_S:              return "defeasible static causal laws (possibly caused F if G)";
		case LAW_PCAUSED_D:              return "defeasible dynamic causal laws (possibly caused F if G after H)";

		case LAW_IMPL:                   return "basic implication laws (F <- G)";

		case LAW_CAUSES:                 return "\"causes\" shorthand laws (G causes F if H)";
		case LAW_INCREMENTS:             return "additive \"increments\"laws (G increments c by V if H)";
		case LAW_MCAUSE:                 return "defeasible \"may cause\" shorthand laws (G may cause F if H)";

		case LAW_ALWAYS_S:               return "static \"always\" constraint laws (always G)";
		case LAW_ALWAYS_D:               return "dynamic \"always\" constraint laws (always G after H)";
		case LAW_CONSTRAINT_S:           return "static \"constraint\" laws (constraint G)";
		case LAW_CONSTRAINT_D:           return "dynamic \"constraint\" laws (constraint G after H)";
		case LAW_IMPOSSIBLE_S:           return "static \"impossible\" constraint laws (impossible G)";
		case LAW_IMPOSSIBLE_D:           return "dynamic \"impossible\" constraint laws (impossible G after H)";
		case LAW_NEVER_S:                return "static \"never\" constraint laws (never G)";
		case LAW_NEVER_D:                return "dynamic \"never\" constraint laws (never G after H)";

		case LAW_DEFAULT_S:              return "static default laws (default c=v if G)";
		case LAW_DEFAULT_D:              return "dynamic default laws (default c=v if G after H)";
		case LAW_EXOGENOUS_S:            return "static exogenous laws (exogenous c if G)";
		case LAW_EXOGENOUS_D:            return "dynamic exogenous laws (exogenous c if G after H)";
		case LAW_INERTIAL_S:             return "static inertial laws (inertial c if G)";
		case LAW_INERTIAL_D:             return "dynamic inertial laws (inertial c if G after H)";

		case LAW_NONEXECUTABLE:          return "\"nonexecutable\" constraint laws (nonexecutable F if G)";
		case LAW_RIGID:                  return "\"rigid\" constraint laws (rigid c)";

		case LAW_OBSERVED:               return "observational laws (observed c-v at t)";

		case NOCONCURRENCY:              return "no concurrency declarations";
		case STRONG_NOCONCURRENCY:       return "strong no concurrency declarations";

		case CODE_ASP_CP:                return "ASP code blocks (:- begin asp. ... :- end asp.)";
		case CODE_ASP_GR:                return "ASP code blocks (#begin_asp ... #end_asp.)";
		case CODE_F2LP_CP:               return "F2LP code blocks (:- begin f2lp. ... :- end f2lp.)";
		case CODE_F2LP_GR:               return "F2LP code blocks (#begin_f2lp ... #end_f2lp.)";
		case CODE_LUA_CP:                return "LUA code blocks (:- begin lua. ... :- end lua.)";
		case CODE_LUA_GR:                return "LUA code blocks (#begin_lua ... #end_lua.)";
	}

}








}}

