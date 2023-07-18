# Premise

All program data exists in a conceptual 3D grid. The X and Y dimensions are fixed at compile time, but the Z dimension is dynamic.

Instead of defining structs, you define `shape`s. A shape is an arrangement of typed data.

Shapes are allocated in the grid at compile time by being children of the root shape.
