#ifndef Ioss_Database_Copier_h
#define Ioss_Database_Copier_h

#include <Ioss_Region.h>
#include <Ioss_NodeBlock.h>
#include <Ioss_ElementBlock.h>
#include <vector>

class IossDatabaseCopier {
public:
    IossDatabaseCopier(Ioss::Region * input,
        Ioss::Region * output);
    ~IossDatabaseCopier();
    void copyInputIOSSDatabaseToOutputIOSSDatabase();
private:
    IossDatabaseCopier();

    void copyDefineModel();
    void copyModel();
    void copyDefineTransient();
    void copyTransient();

    void copyExplicitProperties(const Ioss::GroupingEntity& input,
        Ioss::GroupingEntity& output);

    void addModelFields(const Ioss::GroupingEntity& input,
        Ioss::GroupingEntity& output);
    void copyModelFieldData(const Ioss::GroupingEntity& input,
        Ioss::GroupingEntity& output);

    void addTransientFields(const Ioss::GroupingEntity& input,
        Ioss::GroupingEntity& output);
    void copyTransientFieldData(const Ioss::GroupingEntity& input,
        Ioss::GroupingEntity& output);

    void createOutputNodeBlock(const Ioss::NodeBlock& nb);
    void createOutputElementBlock(const Ioss::ElementBlock& eb);

    Ioss::Region* inputRegion;
    Ioss::Region* outputRegion;

    std::vector<Ioss::Field::RoleType> transient_roles = {
        Ioss::Field::RoleType::REDUCTION,
        Ioss::Field::RoleType::TRANSIENT};

    std::vector<Ioss::Field::RoleType> model_roles = {
        Ioss::Field::RoleType::INTERNAL,
        Ioss::Field::RoleType::MESH,
        Ioss::Field::RoleType::ATTRIBUTE,
        Ioss::Field::RoleType::COMMUNICATION,
        Ioss::Field::RoleType::MESH_REDUCTION};
};

#endif
