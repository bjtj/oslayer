#ifndef __XML_PARSER_HPP__
#define __XML_PARSER_HPP__

#include <vector>
#include <map>
#include <string>
#include "AutoRef.hpp"
#include "XmlEncoderDecoder.hpp"

namespace osl {


	/**
	 *
	 */
	class XmlNode {
	public:
		const static int NONE = 0;
		const static int ELEMENT = 1;
		const static int TEXT = 2;

	private:
		int type;
		XmlNode * parent;
		std::string _ns;
		std::string _tagName;
		std::map<std::string, std::string> _attrs;
		std::vector<osl::AutoRef<XmlNode> > _children;
		std::string _text;
	
	public:
		XmlNode() : type(NONE), parent(NULL) {}
		virtual ~XmlNode() {
			for (std::vector<osl::AutoRef<XmlNode> >::iterator iter = _children.begin(); iter != _children.end();) {
				iter = _children.erase(iter);
			}
		}
		void testType(int type) {
			if (this->type != type) {
				throw osl::Exception("type not matched / exptected: " + getTypeString(type) + ", but: " + getTypeString(this->type));
			}
		}
		bool nil() {
			return this->type == NONE;
		}
		std::string getTypeString(int type) {
			switch (type) {
			case ELEMENT:
				return "ELEMENT";
			case TEXT:
				return "TEXT";
			default:
				return "Unknown";
			}
		}
		void setType(int type) {
			this->type = type;
		}
		int getType() {
			return type;
		}
		bool valid() {
			return (isElement() || isText());
		}
		bool isElement() {
			return type == ELEMENT;
		}
		bool isText() {
			return type == TEXT;
		}
		void setParent(XmlNode * parent) {
			this->parent = parent;
		}
		XmlNode * getParent() {
			return parent;
		}
		size_t childrenCount() {
			testType(ELEMENT);
			return _children.size();
		}
		std::string & ns() {
			testType(ELEMENT);
			return _ns;
		}
		std::string & tagName() {
			testType(ELEMENT);
			return _tagName;
		}
		std::string & text() {
			testType(TEXT);
			return _text;
		}
		bool hasAttr(const std::string & name) {
			return _attrs.find(name) != _attrs.end();
		}
		std::string & attr(const std::string & name) {
			testType(ELEMENT);
			return _attrs[name];
		}
		std::map<std::string, std::string> & attrs() {
			return _attrs;
		}
		osl::AutoRef<XmlNode> child(size_t idx) {
			testType(ELEMENT);
			return _children[idx];
		}
		void addChild(osl::AutoRef<XmlNode> node) {
			testType(ELEMENT);
			node->setParent(this);
			_children.push_back(node);
		}
		void removeChild(size_t idx) {
			testType(ELEMENT);
			_children.erase(_children.begin() + idx);
		}
		osl::AutoRef<XmlNode> getFirstChild() {
			return *_children.begin();
		}
		osl::AutoRef<XmlNode> getFirstChildElement() {
			for (std::vector<osl::AutoRef<XmlNode> >::iterator iter = _children.begin(); iter != _children.end(); iter++) {
				if ((*iter)->isElement()) {
					return *iter;
				}
			}
			return osl::AutoRef<XmlNode>();
		}
		osl::AutoRef<XmlNode> getFirstChildText() {
			for (std::vector<osl::AutoRef<XmlNode> >::iterator iter = _children.begin(); iter != _children.end(); iter++) {
				if ((*iter)->isText()) {
					return *iter;
				}
			}
			return osl::AutoRef<XmlNode>();
		}
		osl::AutoRef<XmlNode> getLastChild() {
			return *_children.rbegin();
		}
		std::vector<osl::AutoRef<XmlNode> > & children() {
			testType(ELEMENT);
			return _children;
		}
		osl::AutoRef<XmlNode> getElementByTagNameInDepth(const std::string & tagName, int depth) {
			if (--depth <= 0) {
				return osl::AutoRef<XmlNode>();
			}
			for (std::vector<osl::AutoRef<XmlNode> >::iterator iter = _children.begin(); iter != _children.end(); iter++) {
				if ((*iter)->isElement() && (*iter)->tagName() == tagName) {
					return *iter;
				}
				osl::AutoRef<XmlNode> elem = (*iter)->getElementByTagNameInDepth(tagName, depth);
				if (elem.nil() == false) {
					return elem;
				}
			}
			return osl::AutoRef<XmlNode>();
		}
		osl::AutoRef<XmlNode> getElementByTagName(const std::string & tagName) {
			for (std::vector<osl::AutoRef<XmlNode> >::iterator iter = _children.begin(); iter != _children.end(); iter++) {
				if ((*iter)->isElement() && (*iter)->tagName() == tagName) {
					return *iter;
				}
				osl::AutoRef<XmlNode> elem = (*iter)->getElementByTagName(tagName);
				if (elem.nil() == false) {
					return elem;
				}
			}
			return osl::AutoRef<XmlNode>();
		}
		std::vector<osl::AutoRef<XmlNode> > getElementsByTagNameInDepth(const std::string & tagName, int depth) {
			std::vector<osl::AutoRef<XmlNode> > lst;
			if (depth-- <= 0) {
				return lst;
			}
			for (std::vector<osl::AutoRef<XmlNode> >::iterator iter = _children.begin(); iter != _children.end(); iter++) {
				if ((*iter)->isElement() && (*iter)->tagName() == tagName) {
					lst.push_back(*iter);
				}
				std::vector<osl::AutoRef<XmlNode> > ret = (*iter)->getElementsByTagNameInDepth(tagName, depth);
				if (ret.size() > 0) {
					lst.insert(lst.end(), ret.begin(), ret.end());
				}
			}
			return lst;
		}
		std::vector<osl::AutoRef<XmlNode> > getElementsByTagName(const std::string & tagName) {
			std::vector<osl::AutoRef<XmlNode> > lst;
			for (std::vector<osl::AutoRef<XmlNode> >::iterator iter = _children.begin(); iter != _children.end(); iter++) {
				if ((*iter)->isElement() && (*iter)->tagName() == tagName) {
					lst.push_back(*iter);
				}
				std::vector<osl::AutoRef<XmlNode> > ret = (*iter)->getElementsByTagName(tagName);
				if (ret.size() > 0) {
					lst.insert(lst.end(), ret.begin(), ret.end());
				}
			}
			return lst;
		}
		std::string toString() {
			switch (type) {
			case ELEMENT:
			{
				std::string rest;
				for (std::map<std::string, std::string>::iterator iter = _attrs.begin(); iter != _attrs.end(); iter++) {
					rest.append(rest.empty() ? " @ " : ", ");
					rest.append(iter->first + " => \"" + iter->second + "\"");
				}
				if (!ns().empty()) {
					return ns() + ":" + tagName() + rest;
				} else {
					return tagName() + rest;
				}
			}
			break;
			case TEXT:
				return text();
			default:
				return "";
			}
		}
	};

	/**
	 *
	 */
	class XmlDocument {
	private:
		std::string _firstLine;
		osl::AutoRef<XmlNode> _rootNode;
	public:
		XmlDocument() {}
		virtual ~XmlDocument() {}
		std::string & firstLine() {
			return _firstLine;
		}
		osl::AutoRef<XmlNode> & rootNode() {
			return _rootNode;
		}
		static std::string escapeString(const std::string & str) {
			std::string ret;
			for (std::string::const_iterator iter = str.begin(); iter != str.end(); iter++) {
				if (*iter == '\"') {
					ret.append("\\\"");
				} else {
					ret.append(1, *iter);
				}
			}
			return ret;
		}
		static std::string toFullTagString(osl::AutoRef<XmlNode> node) {
			std::string xml = toTagNameString(node);
			if (node->attrs().size() > 0) {
				xml.append(" ");
			}
			for (std::map<std::string, std::string>::iterator iter = node->attrs().begin(); iter != node->attrs().end(); iter++) {
				xml.append(iter->first + "=\"" + escapeString(iter->second) + "\"");
			}
			return xml;
		}
		static std::string toTagNameString(osl::AutoRef<XmlNode> node) {
			if (!node->ns().empty()) {
				return node->ns() + ":" + node->tagName();
			}
			return node->tagName();
		}
		static std::string toString(osl::AutoRef<XmlNode> node) {
			std::string xml;
			if (node.nil() == false) {
				if (node->isElement()) {
					xml.append("<" + toFullTagString(node) + ">");
					for (size_t i = 0; i < node->childrenCount(); i++) {
						xml.append(toString(node->child(i)));
					}
					xml.append("</" + toTagNameString(node) + ">");
				} else {
					xml.append(node->text());
				}
			}
			return xml;
		}
		std::string toString() {
			std::string xml;
			if (!_firstLine.empty()) {
				xml.append("<!" + _firstLine + ">\r\n");
			}
			xml.append(toString(_rootNode));
			return xml;
		}
	};

	/**
	 *
	 */
	class XmlNodeCursor {
	private:
		osl::AutoRef<XmlNode> _root;
		XmlNode * _cursor;
	public:
		XmlNodeCursor() : _cursor(NULL) {}
		XmlNodeCursor(osl::AutoRef<XmlNode> root) : _root(root), _cursor(&root) {}
		virtual ~XmlNodeCursor() {}
		void enter(osl::AutoRef<XmlNode> element) {
			element->testType(XmlNode::ELEMENT);
			if (_root.nil()) {
				_root = element;
			} else {
				_cursor->addChild(element);
			}
			_cursor = &element;
		}
		void leave() {
			if (_cursor->getParent()) {
				_cursor = _cursor->getParent();
			}
		}
		void append(osl::AutoRef<XmlNode> node) {
			_cursor->addChild(node);
		}
		XmlNode * cursor() {
			return _cursor;
		}
		osl::AutoRef<XmlNode> root() {
			return _root;
		}
	};

	/**
	 *
	 */
	class DomParser {
	private:
	public:
		DomParser() {}
		virtual ~DomParser() {}

		static std::string trim(const std::string & text) {
			if (text.empty()) {
				return text;
			}
			size_t s = text.find_first_not_of(" \t\r\n");
			size_t e = text.find_last_not_of(" \t\r\n");
			return text.substr(s, e - s + 1);
		}

		static bool isSpace(char ch) {
			return ch == ' ' || ch == '\t' || ch == '\r' || ch == '\n';
		}

		static std::vector<std::string> tokenize(const std::string & str) {
			std::vector<std::string> tokens;
			for (std::string::const_iterator iter = str.begin(); iter != str.end(); iter++) {
				if (*iter == '\"') {
					std::string token;
					iter++;
					if (iter == str.end()) {
						throw osl::Exception("unexpected end of string");
					}
					for(; *iter != '\"'; iter++) {
						char ch = *iter;
						if (iter == str.end()) {
							throw osl::Exception("unexpected end of string");
						}
						if (*iter == '\\') {
							ch = *(++iter);
							switch(ch) {
							case 't':
								ch = '\t';
								break;
							case 'r':
								ch = '\r';
								break;
							case 'n':
								ch = '\n';
								break;
							default:
								break;
							}
						}
						token.append(1, ch);
					}
					tokens.push_back(token);
				} else if (*iter == '=') {
					tokens.push_back("=");
				} else if (!isSpace(*iter)) {
					std::string token;
					for(; iter != str.end() && !isSpace(*iter) && *iter != '='; iter++) {
						token.append(1, *iter);
					}
					tokens.push_back(token);
					iter--;
				}
			}
			return tokens;
		}

		static void parseAttrs(const std::string & attrs, osl::AutoRef<XmlNode> node) {
			if (attrs.empty()) {
				return;
			}
			std::vector<std::string> tokens = tokenize(attrs);

			for (std::vector<std::string>::iterator iter = tokens.begin(); iter != tokens.end(); iter++) {
				std::string name = *iter;
				std::string value;
				if ((iter + 1) != tokens.end() && *(iter + 1) == "=") {
					value = *(iter + 2);
					iter += 2;
				}
				node->attr(name) = value;
			}
		}

		static void parseTag(const std::string & tag, osl::AutoRef<XmlNode> node) {
			std::string ns;
			std::string name;
			std::string attrs;
			size_t sep = tag.find_first_of(" \t\r\n");
			if (sep != std::string::npos) {
				name = tag.substr(0, sep);
				attrs = trim(tag.substr(sep + 1));
			} else {
				name = tag;
			}
			sep = name.find(":");
			if (sep != std::string::npos) {
				ns = name.substr(0, sep);
				name = name.substr(sep + 1);
			}

			node->ns() = ns;
			node->tagName() = name;

			parseAttrs(attrs, node);
		}

		static XmlDocument parse(const std::string & text) {

			XmlDocument doc;
			XmlNodeCursor cursor;

			size_t s = text.find("<");
			size_t l = 0;

			if (text[s + 1] == '!') {
				size_t e = text.find(">", s);
				doc.firstLine() = text.substr(s + 2, e - (s + 2));
				l = e + 1;
				s = text.find("<", e + 1);
			}
		
			while (s != std::string::npos) {
				size_t e = text.find(">", s);

				if (e == std::string::npos) {
					throw osl::Exception("Wrong xml format");
				}
				
				if (text[e - 1] == '/') {
					// atom
					osl::AutoRef<XmlNode> node(new XmlNode);
					node->setType(XmlNode::ELEMENT);
					parseTag(trim(text.substr(s + 1, e - (s + 1) - 1)), node);
					cursor.append(node);
				} else if (text[s + 1] == '/') {
					{
						osl::AutoRef<XmlNode> node(new XmlNode);
						node->setType(XmlNode::TEXT);
						node->text() = XmlDecoder::decode(text.substr(l, s - l));
						cursor.append(node);
					}
					// end tag
					cursor.leave();
				} else {
					if (cursor.root().nil() == false) {
						osl::AutoRef<XmlNode> node(new XmlNode);
						node->setType(XmlNode::TEXT);
						node->text() = XmlDecoder::decode(text.substr(l, s - l));
						cursor.append(node);
					}
					// start tag
					osl::AutoRef<XmlNode> node(new XmlNode);
					node->setType(XmlNode::ELEMENT);
					parseTag(trim(text.substr(s + 1, e - (s + 1))), node);
					cursor.enter(node);
				}

				l = e + 1;
				s = text.find("<", e + 1);
			}

			doc.rootNode() = osl::AutoRef<XmlNode>(cursor.root());
			return doc;
		}
	};
}

#endif
