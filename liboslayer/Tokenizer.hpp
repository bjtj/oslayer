#ifndef __TOKENIZER_HPP__
#define __TOKENIZER_HPP__

#include <string>
#include <vector>

namespace osl {
    class Tokenizer
    {
    private:
	std::string _text;
	std::string _delim;
	std::vector<std::string> _tokens;
    public:
	Tokenizer(const std::string & text, const std::string & delim);
	virtual ~Tokenizer();
	virtual void tokenize();
	std::string & text();
	std::string text() const;
	std::string & delim();
	std::string delim() const;
	std::vector<std::string> & tokens();
	std::vector<std::string> tokens() const;
	std::string & token(size_t idx);
	std::string token(size_t idx) const;
	size_t size() const;
    };

}

#endif
