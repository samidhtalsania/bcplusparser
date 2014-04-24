#include "babb/utils/memory.h"
#include "bcplus/Location.h"

namespace bcplus {
namespace parser {
namespace detail {

/// A simple class to track an integral token
class NumberRange : public babb::utils::Referenced
{
private:
	/************************************************************/
	/* Private Members */
	/************************************************************/

	/// The token's minimum and maximum values
	int _min, _max;

	/// Beginning locaiton of the numeric token
	Location _begin;

	/// Ending location of the numeric token
	Location _end;

public:
	/************************************************************/
	/* Constructors / Destructors */
	/************************************************************/
	NumberRange(int min, int max, Location const& begin = Location(NULL, 0, 0), Location const& end = Location(NULL, 0, 0));

	virtual ~NumberRange();


	/************************************************************/
	/* Public Members */
	/************************************************************/

	/// Get the value of the token
	inline int min() const						{ return _min; }
	inline int max() const						{ return _max; }
	

	/// Get/set the location of the token
	inline Location const& beginLoc() const		{ return _begin; }
	inline Location const& endLoc() const			{ return _end; }
	
	inline void beginLoc(Location const& v)		{ _begin = v; }
	inline void endLoc(Location const& v) 			{ _end = v; }


};


}}}
