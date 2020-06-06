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
    this->copyDefineTransient();
    auto state_count = inputRegion->get_property("state_count").get_int();
    for(auto state=1; state <= state_count; ++state) {
        this->inputRegion->begin_state(state);
        this->outputRegion->add_state(this->inputRegion->get_state_time());
        this->outputRegion->begin_state(state);

        this->copyTransient();

        this->outputRegion->end_state(state);
        this->inputRegion->end_state(state);
    }
}

void IossDatabaseCopier::copyDefineModel() {
    this->outputRegion->begin_mode(Ioss::STATE_DEFINE_MODEL);

    this->copyExplicitProperties(*this->inputRegion, *this->outputRegion);

    for(auto nb : inputRegion->get_node_blocks()) {
        this->createOutputNodeBlock(*nb);
    }
    for(auto eb : inputRegion->get_element_blocks()) {
        this->createOutputElementBlock(*eb);
    }

    this->outputRegion->end_mode(Ioss::STATE_DEFINE_MODEL);
}

void IossDatabaseCopier::copyModel() {
    this->outputRegion->begin_mode(Ioss::STATE_MODEL);

    for(auto nb : inputRegion->get_node_blocks()) {
        Ioss::NodeBlock* outputNodeBlock = this->outputRegion->
            get_node_block(nb->name());
        this->copyModelFieldData(*nb, *outputNodeBlock);
    }
    for(auto eb : inputRegion->get_element_blocks()) {
        Ioss::ElementBlock* outputElementBlock = this->outputRegion->
            get_element_block(eb->name());
        this->copyModelFieldData(*eb, *outputElementBlock);
    }

    this->outputRegion->end_mode(Ioss::STATE_MODEL);
}
void IossDatabaseCopier::copyDefineTransient() {
    this->outputRegion->begin_mode(Ioss::STATE_DEFINE_TRANSIENT);

    for(auto nb : inputRegion->get_node_blocks()) {
        Ioss::NodeBlock* outputNodeBlock = this->outputRegion->
            get_node_block(nb->name());
        this->addTransientFields(*nb, *outputNodeBlock);
    }
    for(auto eb : inputRegion->get_element_blocks()) {
        Ioss::ElementBlock* outputElementBlock = this->outputRegion->
            get_element_block(eb->name());
        this->addTransientFields(*eb, *outputElementBlock);
    }

    this->outputRegion->end_mode(Ioss::STATE_DEFINE_TRANSIENT);
}
void IossDatabaseCopier::copyTransient() {
    this->outputRegion->begin_mode(Ioss::STATE_TRANSIENT);

    for(auto nb : inputRegion->get_node_blocks()) {
        Ioss::NodeBlock* outputNodeBlock = this->outputRegion->
            get_node_block(nb->name());
        this->copyTransientFieldData(*nb, *outputNodeBlock);
    }
    for(auto eb : inputRegion->get_element_blocks()) {
        Ioss::ElementBlock* outputElementBlock = this->outputRegion->
            get_element_block(eb->name());
        this->copyTransientFieldData(*eb, *outputElementBlock);
    }

    this->outputRegion->end_mode(Ioss::STATE_TRANSIENT);
}

void IossDatabaseCopier::createOutputNodeBlock(
    const Ioss::NodeBlock& nb) {
    int entity_count = nb.get_property("entity_count").get_int();
    std::string name = nb.get_property("name").get_string();
    int component_degree = nb.get_property("component_degree").get_int();

    Ioss::NodeBlock *outputNodeBlock = new Ioss::NodeBlock(
        this->outputRegion->get_database(), name, entity_count,
            component_degree);
    this->copyExplicitProperties(nb, *outputNodeBlock);
    this->addModelFields(nb, *outputNodeBlock);
    this->outputRegion->add(outputNodeBlock);
}

void IossDatabaseCopier::createOutputElementBlock(
    const Ioss::ElementBlock& eb) {
    int entity_count = eb.get_property("entity_count").get_int();
    std::string name = eb.get_property("name").get_string();
    std::string original_topology_type = eb.get_property(
        "original_topology_type").get_string();

    Ioss::ElementBlock *outputElementBlock = new Ioss::ElementBlock(
        this->outputRegion->get_database(), name,
            original_topology_type, entity_count);
    this->copyExplicitProperties(eb, *outputElementBlock);
    this->addModelFields(eb, *outputElementBlock);
    this->outputRegion->add(outputElementBlock);
}

void IossDatabaseCopier::copyExplicitProperties(
    const Ioss::GroupingEntity& input, Ioss::GroupingEntity& output) {

    std::vector<std::string> inputPropertyNames;
    input.property_describe(&inputPropertyNames);
    for(auto name : inputPropertyNames) {
        Ioss::Property inputProperty = input.get_property(name);
        if(!output.property_exists(name) &&
           inputProperty.is_explicit()) {
           output.property_add(inputProperty);
        }
    }
}

void IossDatabaseCopier::addModelFields(const Ioss::GroupingEntity& input,
    Ioss::GroupingEntity& output) {

}

void IossDatabaseCopier::copyModelFieldData(const Ioss::GroupingEntity& input,
    Ioss::GroupingEntity& output) {

}

void IossDatabaseCopier::addTransientFields(const Ioss::GroupingEntity& input,
    Ioss::GroupingEntity& output) {

}

void IossDatabaseCopier::copyTransientFieldData(
    const Ioss::GroupingEntity& input, Ioss::GroupingEntity& output) {

}
