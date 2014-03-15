#pragma once

#include "babb/utils/memory.h"

namespace bcplus {
namespace languages {

class Language : public babb::utils::Referenced {
public:
	/******************************************************************************/
	/* Public Types */
	/******************************************************************************/
	/// Container for language features to be supported
	struct Feature {
		enum Value {
			DECL_INCLUDE,				///< include statements.
			DECL_MACRO,					///< macro definitions and expansion
			DECL_SORT,					///< sort definition
			DECL_OBJECT,				///< object definition
			DECL_CONSTANT,				///< constant definition
			DECL_VARIABLE,				///< variable definition
			DECL_SHOW,					///< show statement
			DECL_MAXAFVALUE,			///< Maximum additive fluent value (:- maxAFValue)
			DECL_MAXADDITIVE,			///< Maximum additive fluent value (:- maxAdditive)

			FORMULA_NESTED,				///< nested formulas (otherwise only conjunction is allowed)
			FORMULA_CHOICE,				///< choice rules in the head of laws
		
			CLAUSE_IF,					///< "if G" optional rule body
			CLAUSE_AFTER,				///< "after H" optional rule body
			CLAUSE_IFCONS,				///< "ifcons M" optional rule body
			CLAUSE_ASSUMING,			///< "assuming M" optional rule body
			CLAUSE_UNLESS,				///< "unless ab" optional rule body
			CLAUSE_WHERE,				///< "where M" optional rule body

			LAW_BASIC_S,				///< F if G
			LAW_BASIC_D,				///< F if G after H

			LAW_CAUSED_S,				///< caused F if G.
			LAW_CAUSED_D,				///< caused F if G after H
			LAW_PCAUSED_S,				///< possibly caused F if G
			LAW_PCAUSED_D,				///< possibly caused F if G
			
			LAW_IMPL,					///< F <- G (also: <- G).

			LAW_CAUSES,					///< G causes F if H.
			LAW_INCREMENTS,				///< G increments c by v if H.
			LAW_MCAUSE,					///< G may cause F if H.

			LAW_ALWAYS_S,				///< always G
			LAW_ALWAYS_D,				///< always G after H
			LAW_CONSTRAINT_S,			///< co	nstraint G
			LAW_CONSTRAINT_D,			///< constraint G after H
			LAW_IMPOSSIBLE_S,			///< impossible G
			LAW_IMPOSSIBLE_D,			///< impossible G after H
			LAW_NEVER_S,				///< never G
			LAW_NEVER_D,				///< never G after H
	
			LAW_DEFAULT_S,				///< default F if G
			LAW_DEFAULT_D,				///< default F if G after H
			LAW_EXOGENOUS_S,			///< exogenous c if G
			LAW_EXOGENOUS_D,			///< exogenous c if G after H
			LAW_INERTIAL_S,				///< inertial c if G
			LAW_INERTIAL_D,				///< inertial c if G after H

			LAW_NONEXECUTABLE,			///< nonexecutable F if G		
			LAW_RIGID,					///< rigid c
			
			LAW_OBSERVED,				///< observed c=v at v2.

			NOCONCURRENCY,				///< noconcurrency.
			STRONG_NOCONCURRENCY,		///< strong noconcurrency.

			CODE_ASP_CP,				///< CCalc style ASP code block
			CODE_ASP_GR,				///< Gringo style ASP code block
			CODE_F2LP_CP,				///< CCalc style F2LP code block
			CODE_F2LP_GR,				///< Gringo style F2LP code block
			CODE_LUA_CP,				///< CCalc style LUA code block
			CODE_LUA_GR					///< Gringo style LUA code block
		};


		/// Get a description of the provided feature
		/// @param feature The feature to retrieve the description of
		/// @return A short description of the feature
		static char const* descr(Value feature);

	};


	/// Get the description of the provided feature
	char const* featureDescription(Feature::Value feature) const				{ return Feature::descr(feature); }

	/// Get the name of this language.
	/// @return The language name
	virtual char const* name() const = 0;

	/// Determine if this language supports the provided feature.
	/// @param feature The feature to check
	/// @param True if the language supports the feature, false otherwise.
	virtual bool support(Feature::Value feature) const = 0;


};

}}
