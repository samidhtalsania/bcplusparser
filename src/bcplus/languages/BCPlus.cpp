
#include "bcplus/languages/Language.h"
#include "bcplus/languages/BCPlus.h"

namespace bcplus {
namespace languages {

char const* BCPlus::name() const {
	return "BC+";
}

bool BCPlus::support(Feature::Value feature) const {
	switch (feature) {
		case Feature::DECL_INCLUDE:               
		case Feature::DECL_MACRO:                 
		case Feature::DECL_SORT:                  
		case Feature::DECL_OBJECT:                
		case Feature::DECL_CONSTANT:              
		case Feature::DECL_VARIABLE:              
		case Feature::DECL_SHOW:                  
		case Feature::DECL_MAXAFVALUE:            
		case Feature::DECL_MAXADDITIVE:           
		case Feature::FORMULA_NESTED:             
		case Feature::FORMULA_CHOICE:             
		case Feature::CLAUSE_IF:                  
		case Feature::CLAUSE_AFTER:               
		case Feature::CLAUSE_IFCONS:              
		case Feature::CLAUSE_ASSUMING:            
		case Feature::CLAUSE_UNLESS:              
		case Feature::CLAUSE_WHERE:               
		case Feature::LAW_BASIC_S:                
		case Feature::LAW_BASIC_D:                
		case Feature::LAW_CAUSES:                 
		case Feature::LAW_INCREMENTS:             
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
			return true;

		case Feature::LAW_CAUSED_S:               
		case Feature::LAW_CAUSED_D:               
		case Feature::LAW_PCAUSED_S:              
		case Feature::LAW_PCAUSED_D:              
		case Feature::LAW_IMPL:                   
		case Feature::LAW_OBSERVED:               
		case Feature::STRONG_NOCONCURRENCY:       
		default:
			return false;
	}
}


}}


