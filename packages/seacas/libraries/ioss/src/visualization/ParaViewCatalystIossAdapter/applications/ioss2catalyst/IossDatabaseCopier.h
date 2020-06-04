#ifndef Ioss_Database_Copier_h
#define Ioss_Database_Copier_h

#include <Ioss_Region.h>

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
    Ioss::Region* inputRegion;
    Ioss::Region* outputRegion;
};

#endif
