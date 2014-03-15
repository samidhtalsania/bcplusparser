
#include "bcplus/parser/Token.h"
#include "bcplus/parser/detail/ScannerSource.h"
#include "bcplus/parser/detail/ScannerTokenSource.h"

namespace bcplus {
namespace parser {
namespace detail {

ScannerTokenSource::ScannerTokenSource(TokenList* stream)
	: ScannerSource(ScannerSource::Type::TOKENS), _stream(stream) {
	/* Intentionally left blank */
}

ScannerTokenSource::ScannerTokenSource(Token const* token)
	: ScannerSource(ScannerSource::Type::TOKENS) {
	_stream = new TokenList();
	if (token) _stream->push_back(token);
}

ScannerTokenSource::~ScannerTokenSource() {
	/* Intentionally left blank */
}

void ScannerTokenSource::push_front(TokenList const* tokens) {
	for (const_iterator it = tokens->end(); it != tokens->begin(); it--) {
		_stream->push_front(*it);
	}

}

void ScannerTokenSource::push_back(TokenList const* tokens) {
	for (const_iterator it = tokens->begin(); it != tokens->end(); it++) {
		_stream->push_back(*it);
	}
}


Token const* ScannerTokenSource::next() {
	if (size()) {
		Token const* t = _stream->front().release();
		_stream->pop_front();
		return t;
	} else return NULL;

}

Token const* ScannerTokenSource::peak() const {
	if (size()) return _stream->front().release();
	else return NULL;
}

Location ScannerTokenSource::loc() const {
	return (size() ? _stream->front()->begin() : Location(NULL, 0, 0));
}



}}}


