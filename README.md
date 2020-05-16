# mathematica-cov-reader

This Mathematica package contains function to read Ibex binary COV files.

To use:
```
<< IbexCovReader`
cov = readCovFile["file.cov"]
```

The function `readCovFile` returns an association containing the data associated to the COV.

You can see other utility functions with
```
IbexCovReader::usage
```

