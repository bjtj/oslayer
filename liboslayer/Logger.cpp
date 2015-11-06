#include "Logger.hpp"
#include <iostream>
#include <algorithm>

namespace UTIL {

	using namespace std;

	/**
	 * @brief
	 */

	LogLevel::LogLevel() :level(0) {
	}
	LogLevel::LogLevel(int level) : level(level) {
	}
	LogLevel::~LogLevel() {
	}

	string LogLevel::toString() const {
		switch (level) {
		case ERROR_LEVEL:
			return "ERROR";
		case WARN_LEVEL:
			return "WARN";
		case INFO_LEVEL:
			return "INFO";
		case DEBUG_LEVEL:
			return "DEBUG";
		case VERBOSE_LEVEL:
			return "TRACE";
		}
		return "(UNKNOWN)";
	}

	string LogLevel::toMinialString() const {
		switch (level) {
		case ERROR_LEVEL:
			return "E";
		case WARN_LEVEL:
			return "W";
		case INFO_LEVEL:
			return "I";
		case DEBUG_LEVEL:
			return "D";
		case VERBOSE_LEVEL:
			return "V";
		}
		return "X";
	}

	/**
	 * @brief
	 */
	LogMessage::LogMessage() {
	}

	LogMessage::LogMessage(int level, const std::string & message) : level(level), message(message) {
	}

	LogMessage::~LogMessage() {
	}

	void LogMessage::setLogLevel(const LogLevel & level) {
		this->level = level;
	}

	void LogMessage::setMessage(const string & message) {
		this->message = message;
	}

	string LogMessage::toString() const {
		return level.toMinialString() + " " + message;
	}


	/**
	 * @brief
	 */
	LogMessagePrinter::LogMessagePrinter() {
	}
	LogMessagePrinter::~LogMessagePrinter() {
	}

	string LogMessagePrinter::print(const LogMessage & message) {
		return message.toString();
	}

	/**
	 * @brief
	 */
	LogMessagePrinterDecorator::LogMessagePrinterDecorator(LogMessagePrinter & printer) : printer(printer) {
	}

	LogMessagePrinterDecorator::~LogMessagePrinterDecorator() {
	}


	/**
	 * @brief
	 */
	LogMessageWriter::LogMessageWriter() {
	}

	LogMessageWriter::~LogMessageWriter() {
	}

	/**
	 * @brief
	 */
	LogMessageWriterDecorator::LogMessageWriterDecorator(LogMessageWriter & writer) : writer(writer) {
	}
	LogMessageWriterDecorator::~LogMessageWriterDecorator() {
	}

	/**
	 * @brief
	 */
	StandardIOLogMessageWriter::StandardIOLogMessageWriter() {
	}
	StandardIOLogMessageWriter::~StandardIOLogMessageWriter() {
	}
	void StandardIOLogMessageWriter::write(const string & message) {
		cout << message << endl;
	}

	/**
	 * @brief
	 */
	
	Logger::Logger() : printer(NULL) {
	}
	Logger::~Logger() {
	}

	void Logger::log(const LogMessage & msg) const {
		string logMessage = printer->print(msg);
		writeMessage(logMessage);
	}

	void Logger::log(int level, const std::string & msg) const {
		log(LogMessage(level, msg));
	}

	void Logger::loge(const string & msg) const {
		log(LogLevel::ERROR_LEVEL, msg);
	}
	void Logger::logw(const string & msg) const {
		log(LogLevel::WARN_LEVEL, msg);
	}
	void Logger::logi(const string & msg) const {
		log(LogLevel::INFO_LEVEL, msg);
	}
	void Logger::logd(const string & msg) const {
		log(LogLevel::DEBUG_LEVEL, msg);
	}
	void Logger::logv(const string & msg) const {
		log(LogLevel::VERBOSE_LEVEL, msg);
	}

	void Logger::writeMessage(const string & msg) const {
		for (size_t i = 0; i < writers.size(); i++) {
			writers[i]->write(msg);
		}
	}

	void Logger::setPrinter(LogMessagePrinter * printer) {
		this->printer = printer;
	}

	void Logger::addWriter(LogMessageWriter * writer) {
		writers.push_back(writer);
	}
	void Logger::removeWriter(LogMessageWriter * writer) {
		writers.erase(std::remove(writers.begin(), writers.end(), writer), writers.end());
	}

	/**
	 * @brief
	 */

	class DefaultLogger : public Logger {
	private:
		LogMessagePrinter printer;
		StandardIOLogMessageWriter writer;
	public:
		DefaultLogger() {
			this->setPrinter(&printer);
			this->addWriter(&writer);
		}
		virtual ~DefaultLogger() {
		}
	};
	static DefaultLogger s_defaultLogger;

	/**
	 * @brief
	 */

	LoggerFactory::LoggerFactory() {
	}
	LoggerFactory::~LoggerFactory() {
	}
	const Logger & LoggerFactory::getDefaultLogger() {
		return s_defaultLogger;
	}
}
