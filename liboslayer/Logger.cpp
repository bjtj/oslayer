#include <iostream>
#include "Date.hpp"
#include "Logger.hpp"
#include "Text.hpp"

namespace UTIL {

	using namespace std;
	using namespace OS;

	const int LogLevel::FATAL_LEVEL = 0;
	const int LogLevel::ERROR_LEVEL = 1;
	const int LogLevel::WARN_LEVEL = 2;
	const int LogLevel::INFO_LEVEL = 3;
	const int LogLevel::DEBUG_LEVEL = 4;
	const int LogLevel::TRACE_LEVEL = 5;
	const int LogLevel::VERBOSE_LEVEL = 6;

	LogLevel::LogLevel(int level) : level(level) {
	}
	LogLevel::~LogLevel() {
	}
	int LogLevel::getLevel() const {
		return level;
	}
	void LogLevel::setLevel(int level) {
	}
	string LogLevel::getShortName() {
		return getShortName(level);
	}
	string LogLevel::getName() {
		return getName(level);
	}
	string LogLevel::getShortName(int level) {
		switch (level) {
		case FATAL_LEVEL:
			return "F";
		case ERROR_LEVEL:
			return "E";
		case WARN_LEVEL:
			return "W";
		case INFO_LEVEL:
			return "I";
		case DEBUG_LEVEL:
			return "D";
		case TRACE_LEVEL:
			return "T";
		case VERBOSE_LEVEL:
			return "V";
		default:
			break;
		}
		throw Exception("unknown level");
	}
	string LogLevel::getName(int level) {
		switch (level) {
		case FATAL_LEVEL:
			return "FATAL";
		case ERROR_LEVEL:
			return "ERROR";
		case WARN_LEVEL:
			return "WARN";
		case INFO_LEVEL:
			return "INFO";
		case DEBUG_LEVEL:
			return "DEBUG";
		case TRACE_LEVEL:
			return "TRACE";
		case VERBOSE_LEVEL:
			return "VERBOSE";
		default:
			break;
		}
		throw Exception("unknown level");
	}
	void LogLevel::testLevel(int level) {
		switch (level) {
		case FATAL_LEVEL:
		case ERROR_LEVEL:
		case WARN_LEVEL:
		case INFO_LEVEL:
		case DEBUG_LEVEL:
		case TRACE_LEVEL:
		case VERBOSE_LEVEL:
			break;
		default:
			throw Exception("unknown level");
		}
	}
	vector<int> LogLevel::getLevels() {
		vector<int> levels;
		levels.push_back(FATAL_LEVEL);
		levels.push_back(ERROR_LEVEL);
		levels.push_back(WARN_LEVEL);
		levels.push_back(INFO_LEVEL);
		levels.push_back(DEBUG_LEVEL);
		levels.push_back(TRACE_LEVEL);
		levels.push_back(VERBOSE_LEVEL);
		return levels;
	}
	

	LogFormatter::LogFormatter() {
	}
	LogFormatter::~LogFormatter() {
	}
	string LogFormatter::format(const LogSession & session, const string & msg) {
		return "";
	}


	
	LogPrinter::LogPrinter() {
	}
	LogPrinter::~LogPrinter() {
	}
	void LogPrinter::print(const LogSession & session, const string & msg) {
	}
	


	LogSession::LogSession(const LogLevel & level) : enabled(false), level(level) {
	}
	LogSession::LogSession(int level) : enabled(false), level(level) {
	}
	LogSession::~LogSession() {
	}
	void LogSession::log(const string & msg) const {
		if (enabled) {
			printer->print(*this, formatter->format(*this, msg));
		}
	}
	void LogSession::setEnable(bool enable) {
		this->enabled = enable;
	}
	bool LogSession::isEnable() {
		return enabled;
	}
	AutoRef<LogFormatter> LogSession::getFormatter() {
		return formatter;
	}
	AutoRef<LogPrinter> LogSession::getPrinter() {
		return printer;
	}
	void LogSession::setFormatter(AutoRef<LogFormatter> formatter) {
		this->formatter = formatter;
	}
	void LogSession::setPrinter(AutoRef<LogPrinter> printer) {
		this->printer = printer;
	}
	LogLevel LogSession::getLevel() const {
		return level;
	}

	

	Logger::Logger(LoggerFactory * factory, const string & name) : factory(factory), name(name) {
	}
	Logger::Logger(const string & name) : factory(NULL), name(name) {
	}
	Logger::~Logger() {
		if (factory) {
			factory->removeObserver(this);
		}
	}
	void Logger::logf(const string & msg) const {
		_fatal->log(msg);
	}
	void Logger::loge(const string & msg) const {
		_error->log(msg);
	}
	void Logger::logw(const string & msg) const {
		_warn->log(msg);
	}
	void Logger::logi(const string & msg) const {
		_info->log(msg);
	}
	void Logger::logd(const string & msg) const {
		_debug->log(msg);
	}
	void Logger::logt(const string & msg) const {
		_trace->log(msg);
	}
	void Logger::logv(const string & msg) const {
		_verbose->log(msg);
	}
	AutoRef<LogSession> & Logger::session(int level) {
		switch (level) {
		case LogLevel::FATAL_LEVEL:
			return _fatal;
		case LogLevel::ERROR_LEVEL:
			return _error;
		case LogLevel::WARN_LEVEL:
			return _warn;
		case LogLevel::INFO_LEVEL:
			return _info;
		case LogLevel::DEBUG_LEVEL:
			return _debug;
		case LogLevel::TRACE_LEVEL:
			return _trace;
		case LogLevel::VERBOSE_LEVEL:
			return _verbose;
		default:
			break;
		}
		throw Exception("unknown log level");
	}
	void Logger::observe(LoggerFactory * factory) {
		this->factory = factory;
		factory->addObserver(this);
	}
	void Logger::update(Observable * target) {

		if (!target) {
			this->factory = NULL;
			return;
		}
		
		LoggerFactory * f = (LoggerFactory*)target;
		AutoRef<Logger> logger = f->getLoggerWithoutObserve(name);
		_fatal = logger->session(LogLevel::FATAL_LEVEL);
		_error = logger->session(LogLevel::ERROR_LEVEL);
		_warn = logger->session(LogLevel::WARN_LEVEL);
		_info = logger->session(LogLevel::INFO_LEVEL);
		_debug = logger->session(LogLevel::DEBUG_LEVEL);
		_trace = logger->session(LogLevel::TRACE_LEVEL);
		_verbose = logger->session(LogLevel::VERBOSE_LEVEL);
	}
	

	LoggerDescriptor::LoggerDescriptor() {
	}
	LoggerDescriptor::LoggerDescriptor(const string & pattern) : pattern(pattern) {
	}
	LoggerDescriptor::~LoggerDescriptor() {
	}
	bool LoggerDescriptor::match(const string & keyword) const {
		return Text::match(pattern, keyword);
	}
	AutoRef<Logger> LoggerDescriptor::makeLogger(LoggerFactory & factory, const string & name) {
		AutoRef<Logger> logger(new Logger(name));
		vector<int> levels = LogLevel::getLevels();
		for (vector<int>::iterator iter = levels.begin(); iter != levels.end(); iter++) {
			logger->session(*iter) = makeSession(factory, *iter);
		}
		return logger;
	}
	AutoRef<LogSession> LoggerDescriptor::makeSession(LoggerFactory & factory, int level) {
		AutoRef<LogSession> session(new LogSession(level));
		session->setEnable(true);
		if (factory.getFormatter(formatters[level]).nil()) {
			session->setFormatter(AutoRef<LogFormatter>(new LogFormatter));
		} else {
			session->setFormatter(factory.getFormatter(formatters[level]));

		}
		if (factory.getPrinter(printers[level]).nil()) {
			session->setPrinter(AutoRef<LogPrinter>(new LogPrinter));
		} else {
			session->setPrinter(factory.getPrinter(printers[level]));
		}
		return session;
	}
	void LoggerDescriptor::setFormatter(int level, const string & name) {
		LogLevel::testLevel(level);
		formatters[level] = name;
	}
	void LoggerDescriptor::setPrinter(int level, const string & name) {
		LogLevel::testLevel(level);
		printers[level] = name;
	}
	void LoggerDescriptor::setAllFormatter(const string & name) {
		vector<int> levels = LogLevel::getLevels();
		for (vector<int>::iterator iter = levels.begin(); iter != levels.end(); iter++) {
			setFormatter(*iter, name);
		}
	}
	void LoggerDescriptor::setAllPrinter(const string & name) {
		vector<int> levels = LogLevel::getLevels();
		for (vector<int>::iterator iter = levels.begin(); iter != levels.end(); iter++) {
			setPrinter(*iter, name);
		}
	}






	class PlainFormatter : public LogFormatter {
	public:
		PlainFormatter() {}
		virtual ~PlainFormatter() {}
		virtual string format(LogSession & session, const string & msg) {
			return msg;
		}
	};

	/**
	 * @brief 
	 */
	class BasicFormatter : public LogFormatter {
	public:
		BasicFormatter() {}
		virtual ~BasicFormatter() {}

		virtual string getDate() {
			return Date::format(Date::now(), "%Y-%c-%d %H:%i:%s.%f");
		}

		virtual string format(const LogSession & session, const string & msg) {
			return "[" + getDate() + "] " + session.getLevel().getShortName() + " " + msg;
		}
	};


	class ConsolePrinter : public LogPrinter {
	public:
		ConsolePrinter() {}
		virtual ~ConsolePrinter() {}
		virtual void print(const LogSession & session, const string & msg) {
			cout << msg << endl;
		}
	};


	
	
	
	LoggerFactory::LoggerFactory() {
		setFormatter("plain", AutoRef<LogFormatter>(new PlainFormatter));
		setFormatter("basic", AutoRef<LogFormatter>(new BasicFormatter));
		setPrinter("console", AutoRef<LogPrinter>(new ConsolePrinter));
		
	}
	LoggerFactory::~LoggerFactory() {
		notifyObservers(NULL);
	}
	LoggerFactory & LoggerFactory::getInstance() {
		static LoggerFactory factory;
		return factory;
	}
	AutoRef<Logger> LoggerFactory::getLogger(const string & name) {
		for (vector<LoggerDescriptor>::iterator iter = descriptors.begin(); iter != descriptors.end(); iter++) {
			if (iter->match(name)) {
				AutoRef<Logger> logger = iter->makeLogger(*this, name);
				logger->observe(this);
				return logger;
			}
		}
		LoggerDescriptor descriptor;
		AutoRef<Logger> logger = descriptor.makeLogger(*this, name);
		logger->observe(this);
		return logger;
	}
	AutoRef<Logger> LoggerFactory::getLoggerWithoutObserve(const string & name) {
		for (vector<LoggerDescriptor>::iterator iter = descriptors.begin(); iter != descriptors.end(); iter++) {
			if (iter->match(name)) {
				AutoRef<Logger> logger = iter->makeLogger(*this, name);
				return logger;
			}
		}
		LoggerDescriptor descriptor;
		AutoRef<Logger> logger = descriptor.makeLogger(*this, name);
		return logger;
	}
	void LoggerFactory::setLoggerDescriptorSimple(const string & pattern, const string & formatterName, const string & printerName) {
		LoggerDescriptor descriptor(pattern);
		descriptor.setAllFormatter(formatterName);
		descriptor.setAllPrinter(printerName);
		setLoggerDescriptor(descriptor);
	}
	void LoggerFactory::setLoggerDescriptor(const LoggerDescriptor & descriptor) {
		descriptors.push_back(descriptor);
		notifyObservers();
	}
	void LoggerFactory::setFormatter(const string & name, AutoRef<LogFormatter> formatter) {
		formatters[name] = formatter;
		notifyObservers();
	}
	void LoggerFactory::setPrinter(const string & name, AutoRef<LogPrinter> printer) {
		printers[name] = printer;
		notifyObservers();
	}
	AutoRef<LogFormatter> LoggerFactory::getFormatter(const string & name) {
		return formatters[name];
	}
	AutoRef<LogPrinter> LoggerFactory::getPrinter(const string & name) {
		return printers[name];
	}
}
