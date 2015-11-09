#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <liboslayer/os.hpp>
#include <liboslayer/PollablePool.hpp>

using namespace std;
using namespace OS;
using namespace UTIL;

class Poller;
class SharedSelector;

class Pollee {

private:
public:

	Pollee();
	virtual ~Pollee();

	virtual void listen(Poller & poller);
};

class Poller {

private:
	vector<Pollee*> pollees;
public:

	Poller();
	virtual ~Poller();

	virtual void registerPollee(Pollee * pollee);
	virtual void unregisterPollee(Pollee * pollee);
	virtual void poll(unsigned long timeout) = 0;
	virtual void listen();
};

Poller::Poller() {
}
Poller::~Poller() {
}

void Poller::registerPollee(Pollee * pollee) {
	pollees.push_back(pollee);
}
void Poller::unregisterPollee(Pollee * pollee) {
	pollees.erase(std::remove(pollees.begin(), pollees.end(), pollee), pollees.end());
}
void Poller::listen() {
	for (size_t i = 0; i < pollees.size(); i++) {
		Pollee * pollee = pollees[i];
		pollee->listen(*this);
	}
}


class SharedSelector : public Poller {

private:

	Selector selector;

public:

	SharedSelector();
	virtual ~SharedSelector();


	virtual void poll(unsigned long timeout);
};


SharedSelector::SharedSelector() {
}
SharedSelector::~SharedSelector() {
}

void SharedSelector::poll(unsigned long timeout) {
	if (selector.select(timeout) > 0) {
		listen();
	}
}


int main(int argc, char * args[]) {
	return 0;
}