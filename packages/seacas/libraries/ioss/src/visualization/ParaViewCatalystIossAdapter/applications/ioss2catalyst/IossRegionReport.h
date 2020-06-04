#ifndef Ioss_Region_Report_h
#define Ioss_Region_Report_h

#include <iostream>
#include <string>
#include <vector>
#include <Ioss_Region.h>

namespace ioss_region_report {

using Message = std::string;
using Key = std::string;

struct Messages {
    std::string begin{""};
    std::vector<Message> messages;

    Messages& operator+=(const Message & rhs) {
        messages.push_back(begin + rhs);
        return *this;
    }

    Messages& operator+=(const Messages & rhs) {
        for(auto msg : rhs.messages)
            messages.push_back(begin + msg);
        return *this;
    }
};

std::ostream& operator<<(std::ostream& os, const Messages& messages);
Messages region_report(const Ioss::Region& region);

}

#endif
