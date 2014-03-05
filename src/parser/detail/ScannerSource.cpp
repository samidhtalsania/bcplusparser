#include "parser/ScannerSource.h"


namespace bcplus {
namespace parser {
namespace detail {

ScannerSource::ScannerSource(Configuration* config)
	: _config(config), _cursor(NULL), _limit(NULL), 
	  _token(NULL), _marker(NULL) {
	// Intentionally left blank
}

ScannerSource::~ScannerSource() {
	// Intentionally left blank
}

}}}


