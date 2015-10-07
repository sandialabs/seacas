# -*- mode: cmake -*-

# 
# MSTK Print variable
# Shamelessly stolen from Amanzi (Open Source) https://software.lanl.gov/ascem/trac
#

function(PRINT_VARIABLE VAR_NAME)
    message("==> " "${VAR_NAME}=${${VAR_NAME}}")
endfunction()    

