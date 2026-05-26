#ifndef DYORITE_H
#define DYORITE_H

#ifndef DYORITE_API
#ifdef _WIN32
#define DYORITE_API __declspec(dllexport)
#else
#define DYORITE_API extern
#endif // _WIN32
#endif // DYORITE_API

#include <stdbool.h>

// Any binary-representable number
typedef double                 Dyorite_Scalar;
/// A complex number of the form a + bi
typedef struct Dyorite_Complex Dyorite_Complex;
/// A binary value - either '1' or '0' / 'true' or 'false'
typedef bool                   Dyorite_Boolean;
/// A sequence of homogenous values
typedef struct Dyorite_Vector  Dyorite_Vector;
/// A table of homogenous values
typedef struct Dyorite_Matrix  Dyorite_Matrix;

/// An umbrella/generic type for all types in the language
typedef struct Dyorite_Value Dyorite_Value;

#endif // DYORITE_H
