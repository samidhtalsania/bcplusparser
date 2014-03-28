
#include "bcplus/languages/Language.h"

namespace bcplus {
namespace languages {

/// BC+ language
class BCPlus : public Language {

	virtual char const* name() const;
	virtual bool support(Feature::type feature) const;

};

}}
