# About
The language is similar to the one implemented in https://gnuu.org/2009/09/18/writing-your-own-toy-compiler/. It is a c-like syntax with additional support for int and double and treats the last expression as the return value for a function. The main function must be defined as part of the language. The `printf` takes in one argument, checks the type, and uses the C printf to print (with newline).

The following is an example to calculate volume of cylinder:
```
double circle(double r) {
    3.14 * r * r
}

double cyl(double r, double h) {
    h * circle(r)
}

int main() {
    double vol = cyl(2.0, 5.0)
    printf(vol)
    0
}

```

Members: Daniel Volya and Nicholas Suhlman

# Dependencies: 
    - llvm (VERSION 8), llvm.analysis, llvm.bitwriter, menhir
    - llvm-dis, llc, clang

# To run:
    `make tests`

# The output:
    `tests/%.llvm` - human-readable llvm
    `tests/%.exe` - the executable
    `tests/%.out` - the output of the running the executable
