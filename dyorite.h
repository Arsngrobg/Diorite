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
#include <stddef.h>

// ============================================================================
//                               State Management
// ============================================================================

/// A stateful Dyorite execution context
typedef struct Dyorite_Context Dyorite_Context;

/// Creates a Dyorite context
DYORITE_API
Dyorite_Context *dyorite_context_create(void);

/// Deletes a Dyorite context
DYORITE_API
Dyorite_Context *dyorite_context_delete(void);

// ============================================================================
//                                 Evaluation
// ============================================================================

/// Evaluate the given Dyorite expression with the supplied context
DYORITE_API
Dyorite_Value *dyorite_eval(Dyorite_Context *ctx, const char *expression);

// ============================================================================
//                             Types
// ============================================================================

/// A binary value - either '1' or '0' / 'true' or 'false'
typedef bool                   Dyorite_Boolean;
/// Any binary-representable number
typedef double                 Dyorite_Scalar;
/// A complex number of the form a + bi
typedef struct Dyorite_Complex Dyorite_Complex;
/// A sequence of homogenous values
typedef struct Dyorite_Vector  Dyorite_Vector;
/// A table of homogenous values
typedef struct Dyorite_Matrix  Dyorite_Matrix;

/// An umbrella/generic type for all types in the language
typedef struct Dyorite_Value Dyorite_Value;

// ============================================================================
//                             Value Constructors
// ============================================================================

/// Represents the 'true' value / '1' binary digit (singleton)
DYORITE_API
Dyorite_Value *dyorite_true(void);

/// Represents the 'false' value / '0' binary digit (singleton)
DYORITE_API
Dyorite_Value *dyorite_false(void);

/// Represents the absence of a definition for a value (singleton)
DYORITE_API
Dyorite_Value *dyorite_undefined(void);

/// Creates a value umbrella type from the literal 'scalar' value
DYORITE_API
Dyorite_Value *dyorite_scalar(Dyorite_Scalar scalar);

/// Creates a value umbrella type from the pair of literal 'a' & 'b' values
DYORITE_API
Dyorite_Value *dyorite_complex(Dyorite_Scalar a, Dyorite_Scalar b);

// ============================================================================
//                             Vector Constructors
// ============================================================================

/// Creates a 0D vector (empty) (singleton)
DYORITE_API
Dyorite_Value *dyorite_vec0(void);

/// Creates a 1D vector
DYORITE_API
Dyorite_Value *dyorite_vec1(Dyorite_Scalar x);

/// Creates a 2D vector
DYORITE_API
Dyorite_Value *dyorite_vec2(Dyorite_Scalar x, Dyorite_Scalar y);

/// Creates a 3D vector
DYORITE_API
Dyorite_Value *dyorite_vec3(Dyorite_Scalar x, Dyorite_Scalar y, Dyorite_Scalar z);

/// Creates a 4D vector
DYORITE_API
Dyorite_Value *dyorite_vec4(Dyorite_Scalar x, Dyorite_Scalar y, Dyorite_Scalar z, Dyorite_Scalar w);

/// Creates an ND vector
DYORITE_API
Dyorite_Value *dyorite_vec(size_t n, ...);

// ============================================================================
//                             Matrix Constructors
// ============================================================================

/// Creates a matrix of M rows and N columns filled with zeros
DYORITE_API
Dyorite_Value *dyorite_mat_scalar_zeros(size_t m, size_t n);

/// Creates an identity matrix of the given size
DYORITE_API
Dyorite_Value *dyorite_mat_scalar_identity(size_t size);

/// Creates a matrix of size M x N containing the scalar values
DYORITE_API
Dyorite_Value *dyorite_mat_scalar(size_t m, size_t n, const Dyorite_Scalar *data);

/// Creates a matrix of M rows and N columns filled with false values
DYORITE_API
Dyorite_Value *dyorite_mat_boolean_zeros(size_t m, size_t n);

/// Creates an identity matrix of the given size
DYORITE_API
Dyorite_Value *dyorite_mat_boolean_identity(size_t size);

/// Creates a matrix of size M x N containing the boolean values
DYORITE_API
Dyorite_Value *dyorite_mat_boolean(size_t m, size_t n, const Dyorite_Boolean *data);

#endif // DYORITE_H
