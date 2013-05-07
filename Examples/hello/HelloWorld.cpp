#include "tbb/pipeline.h"
#include "tbb/task_scheduler_init.h"
#include "tbb/tbb_allocator.h"
#include <iostream>

using namespace std;
static int iter;
#define tbb_return(val){if (iter++ >= 10000)return NULL;return val;}

class filt17: public tbb::filter {
public:
	filt17();
private:
	void* operator()(void* item);
};

filt17::filt17() : tbb::filter(serial_in_order) {}

void* filt17::operator()(void* item) {
/* pop 1*/
	std::cout << *static_cast<int*>(item) << std::endl;
}


class filt16: public tbb::filter {
public:
	filt16();
private:
	int var1;
	void* operator()(void* item);
};

filt16::filt16() : tbb::filter(serial_in_order) {
	var1 = 0;
}

void* filt16::operator()(void* item) {
/* push 1*/
	var1 = (var1 + 1);
	tbb_return(&var1);
}


int main(int argc, char* argv[]) {
	tbb::task_scheduler_init init_serial(1);
	tbb::pipeline pipeline;
	filt16 filt16_;
	pipeline.add_filter(filt16_);
	filt17 filt17_;
	pipeline.add_filter(filt17_);
	pipeline.run(1);
}

