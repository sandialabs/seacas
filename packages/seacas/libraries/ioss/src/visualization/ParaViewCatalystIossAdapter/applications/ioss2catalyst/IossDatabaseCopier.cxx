#include "IossDatabaseCopier.h"
#include <Ioss_Region.h>

IossDatabaseCopier::IossDatabaseCopier(Ioss::Region * input,
    Ioss::Region * output) {
    this->inputRegion = input;
    this->outputRegion = output;
}

IossDatabaseCopier::~IossDatabaseCopier() {
}

void IossDatabaseCopier::copyInputIOSSDatabaseToOutputIOSSDatabase() {
    this->copyDefineModel();
    this->copyModel();
    auto state_count = inputRegion->get_property("state_count").get_int();
    for(auto state=1; state <= state_count; ++state) {
        inputRegion->begin_state(state);
        this->copyDefineTransient();
        this->copyTransient();
        inputRegion->end_state(state);
    }
}

void IossDatabaseCopier::copyDefineModel() {
    this->outputRegion->begin_mode(Ioss::STATE_DEFINE_MODEL);



    this->outputRegion->end_mode(Ioss::STATE_DEFINE_MODEL);
}

void IossDatabaseCopier::copyModel() {
    //this->outputRegion->begin_mode(Ioss::STATE_MODEL);



    //this->outputRegion->end_mode(Ioss::STATE_MODEL);
}
void IossDatabaseCopier::copyDefineTransient() {
    //this->outputRegion->begin_mode(Ioss::STATE_DEFINE_TRANSIENT);


    //this->outputRegion->end_mode(Ioss::STATE_DEFINE_TRANSIENT);
}
void IossDatabaseCopier::copyTransient() {
    //this->outputRegion->begin_mode(Ioss::STATE_TRANSIENT);



    //this->outputRegion->end_mode(Ioss::STATE_TRANSIENT);
}
