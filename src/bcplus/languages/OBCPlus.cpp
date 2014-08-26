
#include "bcplus/languages/Language.h"
#include "bcplus/languages/OBCPlus.h"

namespace bcplus {
namespace languages {

const size_t OBCPlus::ID = (size_t)"BC+ Observation Streams";


char const* OBCPlus::name() const {
	return "BC+ Observation Streams";
}

bool OBCPlus::support(Feature::type feature) const {
	switch (feature) {
		case Feature::LAW_OBSERVED:               
		case Feature::CONST_ABACTION:
		case Feature::CONST_ACTION:
		case Feature::CONST_ADDITIVEFLUENT:
		case Feature::CONST_ADDITIVEACTION:
		case Feature::CONST_ATTRIBUTE:
		case Feature::CONST_EXTERNALACTION:
		case Feature::CONST_EXTERNALFLUENT:
		case Feature::CONST_EXOGENOUSACTION:
		case Feature::CONST_INERTIALFLUENT:
		case Feature::CONST_RIGID:
		case Feature::CONST_SIMPLEFLUENT:
		case Feature::CONST_SDFLUENT:
		case Feature::FORMULA_NESTED:
		case Feature::FORMULA_CARDINALITY_BODY:
		case Feature::FORMULA_CARDINALITY_HEAD:
		case Feature::FORMULA_QUANTIFIER:
		case Feature::FORMULA_NOT_KEYWORD:
		case Feature::FORMULA_NOT_DASH:
		case Feature::FORMULA_NOT_DASH_HEAD:
		case Feature::FORMULA_TILDE_STRONG_NEG:
		case Feature::FORMULA_CONSTANT_ARGS:
		case Feature::MACRO:
		case Feature::CLAUSE_IF:                  
		case Feature::CLAUSE_AFTER:               
		case Feature::CLAUSE_IFCONS:              
		case Feature::CLAUSE_UNLESS:              
		case Feature::CLAUSE_WHERE:               
		case Feature::MAXAFVALUE:
		case Feature::MAXSTEP:
		case Feature::MAXADDITIVE:
			return true;

		case Feature::DECL_INCLUDE:               
		case Feature::DECL_MACRO:                 
		case Feature::DECL_SORT:                  
		case Feature::DECL_OBJECT:                
		case Feature::DECL_CONSTANT:              
		case Feature::DECL_VARIABLE:              
		case Feature::DECL_SHOW:                  
		case Feature::DECL_SHOW_ALL:
		case Feature::DECL_HIDE:
		case Feature::DECL_HIDE_ALL:
		case Feature::DECL_MAXADDITIVE:           
		case Feature::DECL_QUERY:
		case Feature::NUMRANGE_SORT:
		case Feature::VARIABLE:
		case Feature::STAR_SORT:
		case Feature::CARROT_SORT:
		case Feature::SORT_PLUS:
		case Feature::QUERY_BIND_STEP:
		case Feature::QUERY_MAXSTEP:
		case Feature::QUERY_LABEL:
		case Feature::LAW_BASIC_S:                
		case Feature::LAW_BASIC_D:                
		case Feature::LAW_BASIC_FACT:                
		case Feature::LAW_CAUSES:                 
		case Feature::LAW_INCREMENTS:             
		case Feature::LAW_DECREMENTS:             
		case Feature::LAW_MCAUSE:                 
		case Feature::LAW_ALWAYS_S:               
		case Feature::LAW_ALWAYS_D:               
		case Feature::LAW_CONSTRAINT_S:           
		case Feature::LAW_CONSTRAINT_D:           
		case Feature::LAW_IMPOSSIBLE_S:           
		case Feature::LAW_IMPOSSIBLE_D:           
		case Feature::LAW_NEVER_S:                
		case Feature::LAW_NEVER_D:                
		case Feature::LAW_DEFAULT_S:              
		case Feature::LAW_DEFAULT_D:              
		case Feature::LAW_EXOGENOUS_S:            
		case Feature::LAW_EXOGENOUS_D:            
		case Feature::LAW_INERTIAL_S:             
		case Feature::LAW_INERTIAL_D:             
		case Feature::LAW_NONEXECUTABLE:          
		case Feature::LAW_RIGID:                  
		case Feature::NOCONCURRENCY:              
		case Feature::CODE_ASP_CP:                
		case Feature::CODE_ASP_GR:                
		case Feature::CODE_F2LP_CP:               
		case Feature::CODE_F2LP_GR:               
		case Feature::CODE_LUA_CP:                
		case Feature::CODE_LUA_GR: 
		case Feature::STRONG_NOCONCURRENCY:       
		case Feature::DECL_MAXAFVALUE:            
		case Feature::LAW_CAUSED_S:               
		case Feature::LAW_CAUSED_D:               
		case Feature::LAW_PCAUSED_S:              
		case Feature::LAW_PCAUSED_D:              
		case Feature::LAW_IMPL:                   
		default:
			return false;
	}
}

size_t OBCPlus::id() const {
	return ID;
}


}}


