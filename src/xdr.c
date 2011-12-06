#include <Rinternals.h>

int REFSXP =            255;
int NILVALUE_SXP =      254;
int GLOBALENV_SXP =     253;
int UNBOUNDVALUE_SXP =  252;
int MISSINGARG_SXP =    251;
int BASENAMESPACE_SXP = 250;
int NAMESPACESXP =      249;
int PACKAGESXP =        248;
int PERSISTSXP =        247;
int CLASSREFSXP =       246;
int GENERICREFSXP =     245;
int EMPTYENV_SXP =	242;
int BASEENV_SXP =	241;

/****************************************/

/****************************************/

char *
readString(SEXP r_data)
{
    return(NULL);
}

SEXP
R_convertXDRInteger(SEXP r_data)
{
    void *data;
    int ans;

    data = RAW(r_data);
    ans = R_XDRDecodeInteger(data);
    return(ScalarInteger(ans));
}


SEXP
R_convertXDRDouble(SEXP r_data)
{
    void *data;
    double ans;

    data = RAW(r_data);
    ans = R_XDRDecodeDouble(data);
    return(ScalarReal(ans));
}
